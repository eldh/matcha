/*
 * Runtime - Layout renderer, main event loop and application lifecycle
 *
 * This module provides the entry point for running a Matcha terminal
 * application. It manages:
 * - Terminal setup and cleanup
 * - The render loop (interactive `start`, and headless `startHeadless`)
 * - Keyboard input dispatch
 * - Signal handling (terminal resize)
 * - Effect lifecycle
 *
 * Usage:
 *   Runtime.start((module MyApp));
 *
 * ============================================================================
 * THE RENDER MODEL
 * ============================================================================
 *
 * ONE RENDERER, TWO MODES
 * -----------------------
 * [renderElement] is the single recursive renderer. It takes the element, the
 * root context, the layout [constraints] for this subtree and the tree [~path],
 * and returns the subtree as a string.
 *
 * ~measuring=false (REAL MODE) applies layout: stacks resolve child sizes,
 * distribute flex space, apply gap/align/justify, and pad or truncate each
 * child to its allocation. This is what a frame prints.
 *
 * ~measuring=true (MEASURING MODE) is layout-free: VStack joins children with
 * newlines, HStack concatenates them, and sizes, gaps, align and justify are
 * ignored. The result is the NATURAL content size of the subtree, which is what
 * Auto sizing has to ask for (see [measureContentSize]). Laying out during a
 * measurement would answer the wrong question - under AlignStretch every line
 * gets padded to the full available width, so every Auto child would measure as
 * wide as its parent.
 *
 * The mode changes the LAYOUT only. The component machinery is identical in
 * both: same path, same stable ID, same context, same effect queue.
 *
 * IDENTITY = TREE PATH
 * --------------------
 * A component instance is identified by where it sits in the element tree, NOT
 * by render order. [childPath] appends a stack child's index, [componentPath]
 * appends a component's type ID (emitted by the [@component] ppx) and its
 * optional key. Because a path depends only on the shape of the tree ABOVE a
 * component, a conditional sibling appearing or disappearing does not shift any
 * other component's identity.
 *
 * The path is mapped to a stable numeric ID through the per-instance registry
 * [Hooks.instanceState.componentIdRegistry] (path -> componentId), and that ID
 * keys the component's hooks context. The registry belongs to the instance, so
 * two applications started in the same process never share identities.
 *
 * RENDER ALWAYS, NEVER CACHE
 * --------------------------
 * A visited component is ALWAYS rendered: resolve id -> get/create context ->
 * beginRender -> run the body -> enqueueEffects -> restore the previous
 * context. There is no per-component output cache, deliberately. Element trees
 * are rebuilt from scratch every frame, so nothing survives to be cached; and
 * serving a cached string would skip the descendants' visits, which means
 * [recordRenderedComponent] would not see them and
 * [Hooks.cleanupUnmountedComponents] would reap their contexts - losing their
 * state - while a stateful descendant's update would be masked by its cached
 * ancestor.
 *
 * HSTACK RENDERS ITS CHILDREN TWICE
 * ---------------------------------
 * A stack with Auto children must know their natural sizes before it can hand
 * out space, so [calculateChildSizes] measures each Auto child (measuring mode)
 * and [renderElement] then renders the children again for real. Both passes
 * walk the SAME paths, so a component child resolves to one identity, one
 * context and one hook array across both. That is what makes the double visit
 * safe. (VStack does the same for Auto heights.)
 *
 * DETACHED RENDERING
 * ------------------
 * Application code may call [Element.render] on a subtree it built by hand.
 * Element cannot run a component body itself, so it delegates through the
 * [Element.componentRenderer] ref, which this module fills in with
 * [renderDetachedComponent] at module-initialization time. The subtree has no
 * position in the element tree, so it is given a path under "detached" derived
 * from the component that OWNS the call - stable across frames - and it renders
 * in measuring mode, which is what Element.render documents.
 *
 * EFFECTS: RENDER MANY TIMES, COMMIT ONCE
 * ---------------------------------------
 * Rendering a component body only SCHEDULES its effects onto its context and
 * queues that context ([Hooks.enqueueEffects]). Nothing runs during the walk.
 * After the whole tree has rendered, the loop calls [Hooks.commitEffects],
 * which drains the queue in the order renders finished (children before
 * parents, root last) and runs each context's pending effects exactly once -
 * so a component rendered twice in a frame (measure + real) still commits one
 * time. Only then does the frame call [Hooks.cleanupUnmountedComponents] with
 * the recorded IDs and [Hooks.collectKeyHandlers] to rebuild the dispatch list.
 */

/* Module signature for a top-level application component.
 * The component must have a make function that takes unit
 * and returns an Element.t.
 */
module type HooksComponent = {
  let make: unit => Element.t;
};

/* ============================================================================
 * Wake Pipe for Background Thread State Updates
 * ============================================================================ */

/* Self-pipe for waking the main loop when state changes from background threads */
let wakePipe: ref(option((Unix.file_descr, Unix.file_descr))) = ref(None);

/* Initialize the wake pipe */
let initWakePipe = (): unit => {
  let (readFd, writeFd) = Unix.pipe();
  Unix.set_nonblock(readFd);
  Unix.set_nonblock(writeFd);
  wakePipe := Some((readFd, writeFd));
};

/* Drain any pending bytes from the wake pipe */
let drainWakePipe = (): unit => {
  switch (wakePipe^) {
  | Some((readFd, _)) =>
    let buf = Bytes.create(64);
    let rec drain = () => {
      try({
        let _ = Unix.read(readFd, buf, 0, 64);
        drain(); /* Keep draining until empty */
      }) {
      | Unix.Unix_error(Unix.EAGAIN, _, _) => () /* No more data */
      | Unix.Unix_error(Unix.EWOULDBLOCK, _, _) => () /* No more data */
      | _ => ()
      };
    };
    drain();
  | None => ()
  };
};

/* Wake the main loop by writing to the pipe */
let wakeMainLoop = (): unit => {
  switch (wakePipe^) {
  | Some((_, writeFd)) =>
    try(ignore(Unix.write(writeFd, Bytes.of_string("w"), 0, 1))) {
    | _ => () /* Ignore errors (pipe full, etc.) */
    }
  | None => ()
  };
};

/* Register the wake function with Hooks */
let () = Hooks.setWakeMainLoop(wakeMainLoop);

/* ============================================================================
 * Component ID Management
 * ============================================================================ */

/* Component IDs rendered in the current pass, most recent first.
 * Lives in the current Hooks instance so that two applications started in the
 * same process do not share it. */
let renderedComponentIds = (): list(Element.componentId) =>
  Hooks.instance().renderedComponentIds^;

/* ---------------------------------------------------------------------------
 * Tree paths
 *
 * A component's identity is its path through the element tree, not its
 * position in a flat render-order counter. A path is a string built from
 * segments: one segment per stack child (its index) and one segment per
 * component (its type ID plus optional key). Because the path only depends on
 * the shape of the tree above a component, inserting or removing a
 * conditional sibling no longer shifts the identity of later components.
 *
 * Separators are ASCII control characters (unit/record/group separator) so
 * that ordinary keys and type IDs cannot forge a segment boundary. A key that
 * literally contains one of these bytes could still collide; that is not
 * considered worth guarding against.
 * ------------------------------------------------------------------------- */

let pathChildSep = "\031"; /* US - precedes a stack child index */
let pathComponentSep = "\030"; /* RS - precedes a component type ID */
let pathKeySep = "\029"; /* GS - separates a component key from its type ID */

/* Path of the i-th child of a stack at [path] */
let childPath = (path: string, i: int): string =>
  path ++ pathChildSep ++ string_of_int(i);

/* Identity path of a component at [path] */
let componentPath =
    (path: string, typeId: option(string), key: option(string)): string =>
  path
  ++ pathComponentSep
  ++ (
    switch (typeId) {
    | Some(t) => t
    | None => "?"
    }
  )
  ++ (
    switch (key) {
    | Some(k) => pathKeySep ++ k
    | None => ""
    }
  );

/* ============================================================================
 * Overlay queue (B2)
 * ============================================================================ */

/* One floating layer, as the render walk produced it: the lines to paint, the
 * box they go in (FRAME coordinates, already clipped to the frame), and the
 * bits [Hooks] needs to route input to it.
 *
 * - ofPath: the Overlay node's tree path. It is the queue key, so a subtree
 *   visited twice in one frame (an HStack measures its children, then renders
 *   them for real) records ONE layer, not two - the recordPendingStatic rule.
 * - ofLines: exactly ofH lines, each already clipped to ofH rows. They are
 *   NOT padded to ofW here; the compositor pads every row it splices, which
 *   is what makes the layer opaque.
 * - ofMembers: the component ids rendered INSIDE the layer's child. This is
 *   the layer's membership - see the Overlay case for how it is captured.
 * - ofShadow / ofOnDismiss: straight from the node's options. */
type overlayFrame = {
  ofPath: string,
  ofLines: list(string),
  ofX: int,
  ofY: int,
  ofW: int,
  ofH: int,
  ofShadow: bool,
  ofOnDismiss: option(unit => unit),
  ofMembers: list(Element.componentId),
};

/* This frame's layers, BOTTOM-MOST FIRST (first-visit, i.e. tree, order) -
 * which is also paint order, so a later sibling floats over an earlier one.
 * Transient within a frame; cleared by resetComponentTracking. */
let overlayQueue: ref(list(overlayFrame)) = ref([]);

/* Queue (or re-queue) the layer for the Overlay node at [f.ofPath].
 *
 * REPLACES any entry already recorded for the same path in this frame,
 * keeping its position, exactly like Hooks.recordPendingStatic and for
 * exactly the same reason: a stack that measures an Auto child and then
 * renders it for real walks the subtree twice, and the second walk computes
 * the same layer. New paths append, which makes the queue's order the
 * first-visit (tree) order. */
let recordOverlay = (f: overlayFrame): unit => {
  let replaced = ref(false);
  let updated =
    List.map(
      (existing: overlayFrame) =>
        if (existing.ofPath == f.ofPath) {
          replaced := true;
          f;
        } else {
          existing;
        },
      overlayQueue^,
    );
  overlayQueue := (replaced^ ? updated : updated @ [f]);
};

/* Reset component tracking at start of render */
let resetComponentTracking = (): unit => {
  Hooks.instance().renderedComponentIds := [];
  /* Mouse (B4): last frame's bounds describe a frame that is about to be
     replaced. Clearing here (rather than after the paint) means a component
     that disappears this frame leaves no stale rect behind for
     Hooks.dispatchMouse to hit. */
  Hashtbl.reset(Hooks.instance().componentBounds);
  /* Don't clear the registry - it keeps component IDs stable across renders of
     the same instance. It is per-instance, so a new application start begins
     with an empty one. */
  /* Focus (B1): start this render's useFocus registrations from empty, same
     as renderedComponentIds above - Hooks.commitFocus consumes the previous
     render's list right after this frame's collectKeyHandlers. */
  Hooks.resetFocusRegistrations();
  /* Overlays (B2): last frame's layers describe a frame that is about to be
     replaced. A modal that closed this frame must leave no layer behind, or
     the composite would paint it again and Hooks would still be routing
     input to it. */
  overlayQueue := [];
};

/* Generate (or reuse) the stable component ID for a component identity path.
 * The path already embeds the component's type ID and key, so a match on the
 * path is a match on the identity. */
let generateStableComponentId = (path: string): Element.componentId => {
  let registry = Hooks.instance().componentIdRegistry;
  switch (Hashtbl.find_opt(registry, path)) {
  | Some(stableId) => stableId
  | None =>
    let newId = Hooks.generateComponentId();
    Hashtbl.replace(registry, path, newId);
    newId;
  };
};

/* Track a component ID as rendered during the current pass */
let recordRenderedComponent = (componentId: Element.componentId): unit => {
  let rendered = Hooks.instance().renderedComponentIds;
  rendered := [componentId, ...rendered^];
};

/* ============================================================================
 * Layout Constraints
 * ============================================================================ */

/* Layout constraints passed down during rendering */
type constraints = {
  availWidth: int,
  availHeight: int,
};

/* Default layout constraints, used until the first render sets real ones */
let defaultConstraints: constraints = {availWidth: 80, availHeight: 24};

/* Current layout constraints - accessible by components during render.
 * Transient within a frame (saved and restored around every renderElement
 * call), so it stays a module-level ref; each application start resets it. */
let currentConstraints: ref(constraints) = ref(defaultConstraints);

/* Get the current layout constraints (available width/height for this component) */
let getConstraints = (): constraints => currentConstraints^;

/* ============================================================================
 * The frame (B2)
 * ============================================================================ */

/* The WHOLE frame's size, (width, height), as every render loop set it at the
 * top of this frame.
 *
 * Distinct from [currentConstraints], which NARROWS during the descent: by
 * the time an <Overlay> nested three panes deep renders, currentConstraints
 * describes that pane, and centring a modal in it would put the dialog in the
 * corner of the pane rather than in the middle of the window. An overlay is a
 * frame-level object, so its box and its position resolve against this.
 *
 * Transient within a frame (like currentConstraints and clipStack), so a
 * module-level ref is enough; resetContainerStack below seeds it, and every
 * render loop calls that. */
let frameSize: ref((int, int)) = ref((80, 24));

/* ============================================================================
 * Container queries (A1)
 * ============================================================================ */

/* The enclosing <Container> boxes, innermost first.
 *
 * Every render loop seeds this with ONE entry - the whole frame - so a query
 * made outside any container answers "the window", which is what a root-level
 * component means by "how much room do I have". An Element.Container case
 * pushes its own constraints while it renders its subtree and pops them
 * afterwards, in BOTH modes, so a measure-time query matches the real one.
 *
 * Transient within a frame (like currentConstraints and clipStack), so a
 * module-level ref is enough. */
let containerStack: ref(list(constraints)) = ref([]);

/* Start a frame's container stack: one entry covering the whole frame.
 *
 * Also records the frame's size (B2). The two always change together - both
 * mean "this is the whole window this frame" - so one call sets both and
 * they cannot drift apart. */
let resetContainerStack = (constraints: constraints): unit => {
  containerStack := [constraints];
  frameSize := (max(0, constraints.availWidth), max(0, constraints.availHeight));
};

/* The nearest enclosing container's box, or the whole frame when there is no
 * <Container> above this point. An empty stack means nothing reset it (only
 * possible outside a render loop, e.g. a detached Element.render), in which
 * case the last constraints installed are the best answer available. */
let getContainerSize = (): constraints =>
  switch (containerStack^) {
  | [top, ..._] => top
  | [] => currentConstraints^
  };

/* ============================================================================
 * Clip stack (B4)
 * ============================================================================ */

/* The rects a component's bounds are clipped against, innermost first.
 *
 * The base entry is the whole frame (pushed by resetClipStack at the start of
 * every render, from that frame's constraints), so a component's recorded rect
 * can never claim space outside the live region. B5's Viewport pushes its own
 * visible rect while rendering scrolled content, which is what makes a
 * scrolled-out child's rect degenerate to zero size - and therefore unhittable
 * - without any special case in the hit test.
 *
 * Transient within a frame (like currentConstraints), so a module-level ref is
 * enough; every render loop resets it before it renders. */
let clipStack: ref(list(Mouse.rect)) = ref([]);

/* Start a frame's clip stack: one entry covering the whole frame. */
let resetClipStack = (constraints: constraints): unit =>
  clipStack :=
    [
      {
        Mouse.rx: 0,
        ry: 0,
        rw: max(0, constraints.availWidth),
        rh: max(0, constraints.availHeight),
      },
    ];

/* The innermost clip rect. An empty stack (nothing reset it - only possible
 * outside a render loop) clips against nothing. */
let clipTop = (): Mouse.rect =>
  switch (clipStack^) {
  | [top, ..._] => top
  | [] => {Mouse.rx: 0, ry: 0, rw: max_int, rh: max_int}
  };

/* ============================================================================
 * Headless Mode Support
 * ============================================================================ */

/* Configuration for headless mode */
type headlessConfig = {
  width: int,
  height: int,
};

/* Default headless configuration */
let defaultHeadlessConfig = {width: 80, height: 24};

/* Handle for controlling a headless app */
type headlessHandle = {
  sendKey: (Key.t, Key.modifiers) => unit,
  /* Simulate a bracketed paste: normalizes CRLF the same way a real
     paste's InputDecoder framing would, then dispatches Key.Paste through
     the normal key-handler path and re-renders if that left the app
     dirty. See B2/S6. */
  sendPaste: string => unit,
  /* Deliver a mouse event (B4) to the app. Coordinates are already
     live-region relative here - a headless frame IS the live region, so
     there is no screen-row mapping to undo - and go straight to
     Hooks.dispatchMouse, which routes them to the innermost component whose
     last painted box contains them. Re-renders if the handlers left the app
     dirty. test/input.re's clickAt wraps this. */
  sendMouse: Mouse.event => unit,
  getOutput: bool => string,        /* stripAnsi parameter */
  /* Everything committed ABOVE the live region so far (A4): <Static> items
     and useStdout writes, in commit order, one per line, newline-terminated
     and accumulated across frames. getOutput/getLines above deliberately do
     NOT include any of it - they are the current live frame, nothing else -
     so a test can assert "this message was emitted exactly once, ever"
     against this, and "the live rows look like that" against those. */
  getStaticOutput: bool => string,  /* stripAnsi parameter */
  getLines: bool => array(string),  /* stripAnsi parameter */
  isRunning: unit => bool,
  render: unit => string,
  resize: (int, int) => unit,
  getSize: unit => (int, int),
  quit: unit => unit,
  /* The currently focused id (B1), or None if nothing is focused - e.g.
     `Some("__focus_root")`/`Some("b")`. Reads focus.focusedId directly
     rather than parsing the inverted-video marker back out of getOutput(),
     since that would be a brittle way to assert focus in tests. */
  getFocusedId: unit => option(string),
  /* Advance the app's virtual clock by `ms` milliseconds, firing timers
     (useInterval/useTimeout) deadline-by-deadline as the clock passes them
     and re-rendering after each one that leaves the app dirty - so a timer
     registered by one callback still participates if its deadline falls
     within the same advance. Unlike the real fireDueTimers loop, missed
     ticks are NOT coalesced: an 800ms advance over a 100ms interval fires
     8 times, not 1. Raises Failure("advanceTime: runaway timer") past
     100_000 iterations (a timer re-registering itself with ms<=~0 would
     otherwise spin forever). */
  advanceTime: int => unit,
  /* Pretend the terminal answered the OSC 11 background-color query with
     this (r, g, b), 0..255 per channel - the testing seam for
     Hooks.useTerminalBackground, which is otherwise None forever in a
     headless run (nothing here sends the query, and there is no terminal to
     answer it). Re-renders once, and only if the value actually changed,
     exactly like the interactive path. */
  setTerminalBackground: ((int, int, int)) => unit,
};

/* Helper to read an int from an environment variable with a default */
let getEnvInt = (name: string, default: int): int =>
  try(int_of_string(Sys.getenv(name))) {
  | _ => default
  };

/* Check if headless mode is enabled via environment variable */
let isHeadless = (): bool =>
  try(Sys.getenv("MATCHA_HEADLESS") == "1") {
  | Not_found => false
  };

/* Get headless config from environment variables */
let getHeadlessConfigFromEnv = (): headlessConfig => {
  width: getEnvInt("MATCHA_WIDTH", 80),
  height: getEnvInt("MATCHA_HEIGHT", 24),
};

/* Extract size hint from an element (looks for Sized wrapper) */
let rec getSizeHint = (el: Element.t): option(Element.size) => {
  switch (el) {
  | Element.Sized(_, size) => Some(size)
  | Element.Lazy(f) => getSizeHint(f())
  /* A <Container> is layout-transparent (A1): a size hint wrapped in one is
   * still this child's size hint, or wrapping something would change the
   * frame. */
  | Element.Container(child) => getSizeHint(child)
  /* NOT recursed into: an <Overlay>'s ovWidth/ovHeight size the FLOATING BOX
   * against the frame and say nothing about the stack slot this node sits in
   * (which is zero-sized - see isInvisibleToLayout below). Falling through to
   * None is therefore right, and is what the wildcard already does; the arm
   * is spelled out so a later edit to the wildcard cannot silently start
   * leaking a modal's width into its parent stack's flex math. */
  | Element.Overlay(_, _) => None
  | _ => None
  };
};

/* Unwrap Sized wrapper to get inner element.
 *
 * Container is deliberately KEPT (its child is unwrapped in place): the
 * wrapper carries no layout meaning, but dropping it here would drop the
 * query boundary it exists to declare. */
let rec unwrapSized = (el: Element.t): Element.t => {
  switch (el) {
  | Element.Sized(child, _) => unwrapSized(child)
  | Element.Lazy(f) => unwrapSized(f())
  | Element.Container(child) => Element.Container(unwrapSized(child))
  /* An <Overlay> is returned WHOLE, never unwrapped into its child: the node
   * itself is what carries the box, the clip, the container push and the
   * membership capture. (This is what the wildcard already does; spelled out
   * beside Container's rebuild so the difference between "transparent
   * wrapper" and "opaque node" is visible at the point of decision.) */
  | Element.Overlay(_, _) => el
  | _ => el
  };
};

/* ============================================================================
 * Static children (A4)
 * ============================================================================ */

/* Is this stack child invisible to layout - a <Static> node or Empty?
 *
 * <Static> occupies no layout space by contract (its output goes above the
 * live region). Empty is React's `null` child: the standard conditional
 * idiom `{cond ? <Foo /> : Element.Empty}` must occupy NOTHING when the
 * condition is off - not a blank line, not a gap slot, not a justify share.
 *
 * Recurses through the wrappers that carry no layout meaning of their own -
 * Sized (a size hint on a zero-size node is vacuous) and Lazy (which is what
 * <Static ... /> actually builds, and what every JSX element builds). It does
 * NOT look through Component: finding out what a component renders means
 * RUNNING it, which cannot happen here (it would run outside the component's
 * hooks context, and twice per stack). A <Static> (or Empty) returned from a
 * component body therefore behaves like any other component child - it
 * renders to "" but occupies a layout slot (one blank line in a VStack). Put
 * the node directly in the stack to get the zero-space behavior.
 */
let rec isInvisibleToLayout = (el: Element.t): bool => {
  switch (el) {
  | Element.Static(_) => true
  /* An <Overlay> floats over the FINISHED frame (B1): it is composited after
   * layout, so it must cost the stack that holds it nothing at all - not a
   * row, not a gap slot, not a justify share. Same contract as <Static>, and
   * the reason <Modal> is Lazy(Overlay(...)) rather than a component: this
   * function does not look through Component (see below), so a
   * component-wrapped modal WOULD take a layout slot. */
  | Element.Overlay(_, _) => true
  | Element.Empty => true
  | Element.Sized(child, _) => isInvisibleToLayout(child)
  | Element.Lazy(f) => isInvisibleToLayout(f())
  /* Layout-transparent (A1): wrapping a <Static> or an Empty in a
   * <Container> must not give it a layout slot it did not have. */
  | Element.Container(child) => isInvisibleToLayout(child)
  | _ => false
  };
};

/* Split a stack's children into (layout-invisible, live), pairing every
 * child with its ORIGINAL index.
 *
 * The index has to survive the split because it is what childPath is built
 * from: a child's identity is its position in the element tree, so filtering
 * invisible children out of the layout must not renumber the ones that
 * remain. Everything downstream - calculateChildSizes, the justify math, the
 * join - sees only the live list, which is what makes a <Static>/Empty node
 * invisible to layout rather than merely zero-sized (a zero-sized child
 * would still consume a gap slot and a justify share). The invisible list is
 * still rendered (measuring, at its own path) so a <Static>'s items commit;
 * for Empty that render is a no-op. */
let partitionStatic =
    (children: list(Element.t))
    : (list((int, Element.t)), list((int, Element.t))) =>
  children
  |> List.mapi((i, child) => (i, child))
  |> List.partition(((_, child)) => isInvisibleToLayout(child));

/* ============================================================================
 * Rendering with Layout
 * ============================================================================ */

/* Render an element tree with layout constraints.
 * Stack components distribute space among children based on size hints.
 *
 * ~measuring switches the renderer into MEASURING MODE. In that mode the
 * output is layout-free and matches Element.render exactly - VStack joins its
 * children with newlines, HStack concatenates them, sizes, gaps, justify and
 * align are all ignored - so it produces the NATURAL content size of a
 * subtree, which is what Auto sizing needs. (Rendering with layout instead
 * would, for example, pad every line to the full available width under
 * AlignStretch and make every Auto child as wide as its parent.)
 *
 * What measuring mode does NOT skip is the component machinery: the tree path,
 * the stable ID, the hooks context, the effect queue. A measured component
 * body therefore runs inside its OWN context - the same context the real pass
 * will use, because both passes walk identical paths - instead of leaking its
 * hooks into whatever context happened to be current.
 *
 * ~origin is the top-left corner of this element's box within the frame (0,0
 * at the top-left of the live region), and drives the mouse bounds registry
 * (B4): the Component case records the box it was allocated so a later click
 * can be routed to it. It is threaded, not measured after the fact, because
 * layout is computed on the way DOWN - a stack knows every child's offset
 * before it renders any of them.
 *
 * ~origin=None means "this pass does not paint, do not record". That covers
 * two cases, and only these two produce a None:
 *   - MEASURING mode (nothing measured is on screen), and
 *   - HStack's FIRST real-mode pass, which renders children at availHeight=0
 *     purely to learn their natural heights and then throws the result away.
 *     Only the second pass, which knows each child's cross-axis shift, carries
 *     real origins.
 * Origin threading is PURELY additive: it changes no output byte, which is
 * what makes the golden suite the verification gate for it.
 */
let rec renderElement =
        (
          ~measuring=false,
          ~origin: option((int, int))=?,
          el: Element.t,
          rootCtx: Hooks.renderContext,
          constraints: constraints,
          ~path: string,
        )
        : string => {
  /* Update current constraints so components can access them */
  let prevConstraints = currentConstraints^;
  currentConstraints := constraints;

  let result =
    switch (el) {
    | Element.Empty => ""
    | Element.Text(s) => s
    | Element.Styled(style, child) =>
      Element.styleToAnsi(style)
      ++ renderElement(~measuring, ~origin?, child, rootCtx, constraints, ~path)
      ++ Element.resetAnsi

    | Element.WrappedText(mode, child) =>
      /* Same computation in both modes: measuring mode needs a wrapped
       * line count for Auto sizing to be accurate, and that requires the
       * same wrap the real pass would produce - so there is no ~measuring
       * branch here, unlike every other case below. The child is rendered
       * layout-free (measuring=true) to get its plain styled text, then
       * StyledText.wrapString wraps/truncates that text to the available
       * width - which is what makes the two passes agree by construction. */
      let inner =
        renderElement(
          ~measuring=true,
          ~origin?,
          child,
          rootCtx,
          constraints,
          ~path,
        );
      StyledText.wrapString(
        ~mode,
        ~width=max(1, constraints.availWidth),
        inner,
      )

    | Element.Viewport(child, options) =>
      /* B5. A scrolling window onto content taller than the box this node
       * was allocated. See lib/ScrollView.re for the component that drives
       * it and lib/Element.re for what the options mean.
       *
       * PASS DISCRIMINATION FIRST - it decides everything below.
       *
       * A frame walks this subtree more than once, and only ONE of those
       * walks is the committed one that paints. Measuring mode is obviously
       * not it. Neither is HStack's first real-mode pass, which renders its
       * children at availHeight=0 purely to learn their natural heights and
       * throws the output away - and which is exactly the pass that would
       * otherwise clip a viewport to zero rows and report a bogus content
       * height to the component. That pass carries NO origin (see the
       * HStack case), and the committed pass always does, so
       * `!measuring && origin != None` is the reliable flag. It is also
       * what makes vpOnViewport fire exactly once per frame.
       *
       * A non-committed pass therefore renders the child in MEASURING mode
       * and returns it UNCLIPPED: a scroller's natural size is its content
       * (the "like a div" rule), so this is what Auto sizing and the
       * HStack pre-pass's line count must both see. No clipping, no
       * scrollbar, no bounds, no vpOnViewport.
       *
       * The child renders at childPath(path, 0) in EVERY pass, committed or
       * not, so a component inside the viewport resolves to one identity
       * and one hooks context however many times the frame visits it.
       *
       * ROWS MODE (options.vpRows = Some) short-circuits all of that: the
       * content is an array of pre-baked, style-self-contained rows, so
       * there is no child to render, measure or clip, and a frame reads
       * only the rows it shows. See Element.viewportOptions for the
       * contract. */
      let childP = childPath(path, 0);
      let committed = !measuring && origin != None;

      /* Shared tail of BOTH content modes: pad the visible lines out to the
       * viewport's box and append the scrollbar column. Factored out so the
       * two modes cannot drift apart - the child path's output must stay
       * byte-identical to what it was before rows mode existed. */
      let composite =
          (~visible: list(string), ~vw: int, ~vh: int, ~showBar: bool,
           ~contentH: int, ~offset: int)
          : string => {
        let padded =
          visible
          @ List.init(max(0, vh - List.length(visible)), _ =>
              String.make(vw, ' ')
            );
        /* Scrollbar column: a thumb sized and placed by
         * ScrollView.scrollbarMetrics (the same pure function its unit
         * tests pin down), or a blank column when the content fits. */
        let rows =
          if (!showBar) {
            padded;
          } else {
            let thumb =
              ScrollView.scrollbarMetrics(
                ~contentH,
                ~viewportH=vh,
                ~offset,
              );
            List.mapi(
              (i, line) =>
                line
                ++ (
                  switch (thumb) {
                  | None => " "
                  | Some((thumbTop, thumbH)) =>
                    i >= thumbTop && i < thumbTop + thumbH ? "█" : "│"
                  }
                ),
              padded,
            );
          };
        String.concat("\n", rows);
      };

      switch (options.vpRows) {
      | Some(rows) when !committed =>
        /* Non-committed = a measurement. A rows viewport's natural height is
         * its row count and its natural width is ZERO: the rows are opaque
         * pre-baked strings, and measuring their width would mean parsing
         * every one of them - the very cost this mode exists to avoid. So an
         * Auto-sized rows-ScrollView is zero columns wide; put it in a sized
         * slot, which the "SIZE IT" rule in ScrollView.re already demands for
         * its height.
         *
         * n empty lines is n-1 newlines and nothing else, so this builds the
         * answer in one allocation rather than a 100_000-element list. */
        String.make(max(0, Array.length(rows) - 1), '\n')

      | Some(rows) =>
        let vh = constraints.availHeight;
        if (vh <= 0) {
          /* Same rule as the child path below: no rows allocated, nothing
             painted. */
          "";
        } else {
          let wantBar = options.vpShowScrollbar;
          let barlessWidth = constraints.availWidth - (wantBar ? 1 : 0);
          let (showBar, vw) =
            barlessWidth <= 0
              ? (false, max(0, constraints.availWidth))
              : (wantBar, barlessWidth);

          let contentH = Array.length(rows);
          let clamped =
            max(0, min(options.vpOffset, max(0, contentH - vh)));
          /* Array.sub with bounds that cannot raise, however stale the
             offset or however short the array. */
          let count = max(0, min(vh, contentH - clamped));
          let slice =
            count <= 0 ? [||] : Array.sub(rows, clamped, count);

          /* Clip each VISIBLE row to the content width. Only viewport-many
             rows are parsed, so this stays O(viewport) - and it is what
             keeps one over-wide row from pushing the scrollbar column off
             the frame. */
          let visible =
            Array.to_list(slice)
            |> List.map(row =>
                 if (vw <= 0) {
                   "";
                 } else {
                   let clippedRow =
                     switch (StyledText.parse(row)) {
                     | [] => ""
                     | [line, ..._] =>
                       StyledText.bake([StyledText.truncateLine(line, vw)])
                     };
                   Element.padToWidth(clippedRow, vw);
                 }
               );

          let out =
            composite(~visible, ~vw, ~vh, ~showBar, ~contentH, ~offset=clamped);

          /* HERE ONLY: see the child path's identical call below. */
          switch (options.vpOnViewport) {
          | Some(f) => f((contentH, vh))
          | None => ()
          };

          out;
        };

      | None =>
      /* CHILD MODE - the original path, left at its original indentation so
         that its diff against the pre-rows-mode version is empty. It ends at
         the closing brace of this arm, just before the switch's own. */
      if (!committed) {
        renderElement(~measuring=true, child, rootCtx, constraints, ~path=childP);
      } else {
        let (ox, oy) =
          switch (origin) {
          | Some(o) => o
          | None => (0, 0) /* unreachable: committed implies origin != None */
          };
        let vh = constraints.availHeight;
        if (vh <= 0) {
          /* A committed viewport with no rows paints NOTHING. Falling back
           * to the unclipped content here (the non-committed answer) would
           * splatter a whole scrolled list across a stack that allocated it
           * zero lines. */
          "";
        } else {
          /* The scrollbar costs a column, but only if a column is left over
           * for content afterwards - a 1-column viewport shows content, not
           * a lone scrollbar. */
          let wantBar = options.vpShowScrollbar;
          let barlessWidth = constraints.availWidth - (wantBar ? 1 : 0);
          let (showBar, vw) =
            barlessWidth <= 0
              ? (false, max(0, constraints.availWidth)) : (wantBar, barlessWidth);

          /* Natural content height, measured at the CONTENT width (so a
           * <Text wrap> inside wraps the same way it will when painted). */
          let measured =
            renderElement(
              ~measuring=true,
              child,
              rootCtx,
              {availWidth: vw, availHeight: vh},
              ~path=childP,
            );
          let naturalH = List.length(Element.splitLines(measured));
          let clamped = max(0, min(options.vpOffset, max(0, naturalH - vh)));

          /* Real render of the child at its FULL height, shifted up by the
           * scroll offset, with the visible rect on the clip stack.
           *
           * The origin shift and the clip together are what make hit
           * testing right for free: a row scrolled off the top gets a
           * negative recorded y, intersects the viewport rect to a zero-size
           * box, and is simply never hit (see the Component case). The clip
           * entry is intersected with the enclosing one so that a ScrollView
           * inside a ScrollView inherits its parent's window.
           *
           * The stack is restored with Fun.protect: an exception escaping a
           * component body must not leave the rest of the frame clipping
           * against a viewport that is no longer being rendered. */
          let savedClip = clipStack^;
          clipStack :=
            [
              Mouse.intersect(
                clipTop(),
                {Mouse.rx: ox, ry: oy, rw: vw, rh: vh},
              ),
              ...savedClip,
            ];
          let rendered =
            Fun.protect(
              ~finally=() => clipStack := savedClip,
              () =>
              renderElement(
                ~origin=(ox, oy - clamped),
                child,
                rootCtx,
                {availWidth: vw, availHeight: naturalH},
                ~path=childP,
              )
            );

          /* Clip to the window. StyledText.sliceLines - not a plain line
           * split - because a style opened above the first visible row has
           * to be re-opened on it, or the whole window renders unstyled. */
          let visible =
            Element.splitLines(
              StyledText.sliceLines(rendered, ~from=clamped, ~count=vh),
            )
            |> List.map(line => vw <= 0 ? "" : Element.padToWidth(line, vw));

          let out =
            composite(
              ~visible,
              ~vw,
              ~vh,
              ~showBar,
              ~contentH=naturalH,
              ~offset=clamped,
            );

          /* HERE ONLY: the committed pass is the one frame-accurate
           * measurement, so this is what ScrollView clamps against. */
          switch (options.vpOnViewport) {
          | Some(f) => f((naturalH, vh))
          | None => ()
          };

          out;
        };
      }
      };

    | Element.Overlay(_, _) when measuring || origin == None =>
      /* NOT the committed pass, so there is nothing to float and nothing to
       * record - and, deliberately, the child is not visited at all.
       *
       * Measuring mode is obviously not it: an overlay contributes no size
       * to anything (isInvisibleToLayout), so a measurement has nothing to
       * compute here. An origin-less REAL pass is the other one, and covers
       * two cases exactly: HStack's natural-height pre-pass (whose output is
       * thrown away), and a <Static> item's render, which happens outside
       * the live region entirely. A layer recorded from either would be
       * painted from a pass that is not the frame - so an <Overlay> inside a
       * <Static> records nothing, by construction rather than by a special
       * case.
       *
       * This is the same `!measuring && origin != None` discrimination the
       * Viewport case uses, and it is why the two stack cases thread
       * ~origin? through their layout-invisible partition (see the VStack
       * case). */
      ""

    | Element.Overlay(child, opts) =>
      /* THE COMMITTED PASS (B2). Render the child into its own box, in FRAME
       * coordinates, and queue the result for compositeOverlays - which
       * splices it over the finished frame after layout is done. This case
       * returns "" either way: an overlay contributes nothing to the string
       * its parent is building.
       *
       * Everything resolves against frameSize, never against `constraints`.
       * By the time this node renders, `constraints` describes whatever slot
       * the stack holding the <Modal> happened to be in; a floating layer is
       * a frame-level object and centring it in a pane would put it in that
       * pane's corner. */
      let (fw, fh) = frameSize^;
      let childP = childPath(path, 0);

      /* The child's NATURAL size, for an Auto dimension.
       *
       * Measured with the dimension being determined set to 0 - the
       * "unconstrained, tell me what you want" convention HStack's first
       * pass already uses - and with those same constraints pushed as the
       * CONTAINER, so the measure pass and the real pass agree about what
       * useContainerSize() reports. Zero for the unknown axis is what breaks
       * the circularity a self-sizing child would otherwise create: <Modal>
       * draws (container height - 2) rows of border, so measuring it under
       * the frame's height would answer "as tall as the frame" instead of
       * "as tall as my content plus two". */
      let measureChild = (~w: int, ~h: int, ~wantWidth: bool): int => {
        let c = {availWidth: w, availHeight: h};
        let saved = containerStack^;
        containerStack := [c, ...saved];
        let out =
          Fun.protect(~finally=() => containerStack := saved, () =>
            renderElement(~measuring=true, child, rootCtx, c, ~path=childP)
          );
        let lines = Element.splitLines(out);
        if (wantWidth) {
          List.fold_left(
            (m, l) => max(m, Element.visibleLength(l)),
            0,
            lines,
          );
        } else {
          List.length(lines);
        };
      };

      /* Width first: an Auto HEIGHT has to be measured at the final width,
       * or a wrapped line inside the dialog would wrap differently in the
       * measurement than it does when painted. */
      let boxW =
        switch (opts.ovWidth) {
        | Element.Chars(n) => max(0, min(n, fw))
        | Element.Percent(p) => max(0, min(fw * p / 100, fw))
        | Element.Flex(_) => max(0, fw)
        | Element.Auto =>
          max(0, min(measureChild(~w=fw, ~h=0, ~wantWidth=true), fw))
        };
      let boxH =
        switch (opts.ovHeight) {
        | Element.Chars(n) => max(0, min(n, fh))
        | Element.Percent(p) => max(0, min(fh * p / 100, fh))
        | Element.Flex(_) => max(0, fh)
        | Element.Auto =>
          max(0, min(measureChild(~w=boxW, ~h=0, ~wantWidth=false), fh))
        };

      /* Horizontally always centred; vertically per ovAlign, clamped so the
       * box cannot start outside the frame. */
      let ox = max(0, (fw - boxW) / 2);
      let oy =
        switch (opts.ovAlign) {
        | Element.OverlayCenter => max(0, (fh - boxH) / 2)
        | Element.OverlayTop(n) => max(0, min(max(0, n), max(0, fh - boxH)))
        | Element.OverlayBottom(n) => max(0, fh - boxH - max(0, n))
        };

      if (boxW <= 0 || boxH <= 0) {
        /* A degenerate box paints nothing and owns nothing. The child is not
           rendered, so its components unmount - which is the honest reading
           of "this layer has no room to exist". */
        "";
      } else {
        let boxConstraints = {availWidth: boxW, availHeight: boxH};
        let savedClip = clipStack^;
        let savedContainer = containerStack^;

        /* The clip REPLACES the enclosing one (it is intersected with the
         * FRAME, not with clipTop()): a modal opened from inside a
         * <ScrollView> floats over the whole window and must not inherit
         * that scroller's visible rect, or half the dialog would be
         * unhittable. Pushing onto the stack is what "replace" means here -
         * clipTop() reads the head. */
        clipStack :=
          [
            Mouse.intersect(
              {Mouse.rx: ox, ry: oy, rw: boxW, rh: boxH},
              {Mouse.rx: 0, ry: 0, rw: max(0, fw), rh: max(0, fh)},
            ),
            ...savedClip,
          ];
        /* An overlay IS a container (A1): dialog content is responsive to
         * the dialog, with no extra API and no escape hatch. */
        containerStack := [boxConstraints, ...savedContainer];

        /* MEMBERSHIP. recordRenderedComponent only ever PREPENDS, so every
         * id the child's render adds sits in front of whatever was there
         * before it, as one contiguous prefix. Save the list's head cell
         * now and walk the new list with === until we reach that exact cell
         * again: the cells walked over are precisely this layer's members.
         * Physical identity, not structural - the list holds ints, so a
         * structural stop test would stop at the first id that merely EQUALS
         * the old head. */
        let idsRef = Hooks.instance().renderedComponentIds;
        let savedIds = idsRef^;

        let rendered =
          Fun.protect(
            ~finally=() => {
              clipStack := savedClip;
              containerStack := savedContainer;
            },
            () =>
            /* ~origin in FRAME coordinates, so every componentBounds rect
               the child records is directly hit-testable against a mouse
               event without any per-layer rebasing. */
            renderElement(
              ~origin=(ox, oy),
              child,
              rootCtx,
              boxConstraints,
              ~path=childP,
            )
          );

        let members: ref(list(Element.componentId)) = ref([]);
        let rec collectMembers = (lst: list(Element.componentId)): unit =>
          if (lst === savedIds) {
            ();
          } else {
            switch (lst) {
            | [] => ()
            | [id, ...rest] =>
              members := [id, ...members^];
              collectMembers(rest);
            };
          };
        collectMembers(idsRef^);

        /* Clip to the box's rows and pad up to exactly boxH of them, so the
         * compositor can splice row-for-row without a bounds check.
         * sliceLines rather than a plain line split: a style opened on an
         * earlier row has to be re-opened on the first row that survives the
         * cut, or the tail of the dialog renders unstyled. Rows are NOT
         * padded to boxW here - the compositor pads every row it splices,
         * which is what makes the layer opaque. */
        let clipped =
          Element.splitLines(
            StyledText.sliceLines(rendered, ~from=0, ~count=boxH),
          );
        let have = List.length(clipped);
        let lines =
          have >= boxH
            ? clipped : clipped @ List.init(boxH - have, _ => "");

        recordOverlay({
          ofPath: path,
          ofLines: lines,
          ofX: ox,
          ofY: oy,
          ofW: boxW,
          ofH: boxH,
          ofShadow: opts.ovShadow,
          ofOnDismiss: opts.ovOnDismiss,
          ofMembers: members^,
        });
        "";
      };

    | Element.Static(items) when measuring =>
      /* A Static node has no size, so a measurement has nothing to compute -
       * and the items must NOT be rendered here. Measuring is not a commit:
       * emitting during a measure pass would either double-emit (measure +
       * real) or emit items for a subtree that the real pass never draws.
       * Returning "" immediately keeps the question from arising at all.
       *
       * The screen-mode guard fires HERE too, not only in the real pass, so
       * a <Static> under a Fullscreen app is rejected on the very first
       * frame no matter where in the tree it sits (a node measured by an
       * Auto parent is visited by this pass first). */
      Hooks.requireStaticAllowed(Hooks.instance(), "<Static>");
      ignore(items);
      ""

    | Element.Static(items) =>
      /* REAL mode: commit the items this node has not committed yet.
       *
       * The watermark (Hooks.instanceState.staticEmitted, keyed by this
       * node's tree path) says how many items are already in the transcript;
       * everything from there on is rendered now, at its NATURAL height -
       * measureContentSize first, then a real render into
       * {availWidth, availHeight: measured} - because committed output is
       * not part of the live region and must not be padded or truncated to
       * fit it.
       *
       * Item i renders at childPath(path, i) with i its index in the FULL
       * list, so an item's components keep one identity for the frame that
       * mounts them (they are unmounted on the next frame, when nothing
       * visits them again).
       *
       * The watermark is NOT advanced here: this walk may run twice in one
       * frame (an HStack measures its children, then renders them for real),
       * and advancing mid-walk would make the second visit see an empty
       * tail. Runtime.drainStaticLines advances it once, after the frame.
       * Recording REPLACES this path's entry for the same reason - the
       * second visit computes the same lines and simply wins.
       *
       * Committing only means something when there IS a transcript above the
       * live region. Under Fullscreen there is not (see Runtime.screenMode),
       * so this raises instead of quietly rendering into a void. */
      let st = Hooks.instance();
      Hooks.requireStaticAllowed(st, "<Static>");
      let watermark =
        switch (Hashtbl.find_opt(st.staticEmitted, path)) {
        | Some(n) => n
        | None => 0
        };
      let collected = ref([]); /* reverse order */
      List.iteri(
        (i, item) =>
          if (i >= watermark) {
            let itemPath = childPath(path, i);
            let naturalHeight =
              measureContentSize(item, false, ~rootCtx, ~path=itemPath);
            let itemConstraints = {
              availWidth: constraints.availWidth,
              availHeight: naturalHeight,
            };
            let rendered =
              renderElement(item, rootCtx, itemConstraints, ~path=itemPath);
            collected := [rendered, ...collected^];
          },
        items,
      );
      let lines =
        collected^ |> List.rev |> List.concat_map(Element.splitLines);
      Hooks.recordPendingStatic(st, path, List.length(items), lines);
      "";

    | Element.VStack(children, _options) when measuring =>
      /* Layout-free: children joined with newlines, sizes ignored. Children
       * are indexed exactly as in the real pass, so their paths match.
       * Static children are dropped from the join - they occupy no layout
       * space, in this mode as in the real one - but are still visited, so
       * that the walk is identical in both passes (a Static visit is a no-op
       * in measuring mode; see the case above). */
      let (statics, live) = partitionStatic(children);
      List.iter(
        ((i, child)) =>
          ignore(
            renderElement(
              ~measuring=true,
              child,
              rootCtx,
              constraints,
              ~path=childPath(path, i),
            ),
          ),
        statics,
      );
      live
      |> List.map(((i, child)) =>
           renderElement(
             ~measuring=true,
             child,
             rootCtx,
             constraints,
             ~path=childPath(path, i),
           )
         )
      |> String.concat("\n");

    | Element.HStack(children, _options) when measuring =>
      /* Layout-free: children joined SIDE BY SIDE, sizes and gaps ignored.
       * Static children are filtered out exactly as in the VStack case.
       *
       * The join is line-wise: each child is padded to its own widest line
       * and children's lines are concatenated row by row, so the stack's
       * natural height is its TALLEST child and its natural width the sum
       * of the children's widths. (A plain string concat measured a
       * multi-line child stack as the SUM of its children's heights - an
       * Auto parent then over-allocated rows and clipped whatever came
       * after. For all-single-line children the two joins agree, which is
       * why the bug hid this long.) */
      let (statics, live) = partitionStatic(children);
      List.iter(
        ((i, child)) =>
          ignore(
            renderElement(
              ~measuring=true,
              child,
              rootCtx,
              constraints,
              ~path=childPath(path, i),
            ),
          ),
        statics,
      );
      let columns =
        live
        |> List.map(((i, child)) => {
             let out =
               renderElement(
                 ~measuring=true,
                 child,
                 rootCtx,
                 constraints,
                 ~path=childPath(path, i),
               );
             let lines = Array.of_list(Element.splitLines(out));
             let width =
               Array.fold_left(
                 (acc, l) => max(acc, Element.visibleLength(l)),
                 0,
                 lines,
               );
             (lines, width);
           });
      let maxLines =
        List.fold_left(
          (acc, (lines, _)) => max(acc, Array.length(lines)),
          1,
          columns,
        );
      List.init(maxLines, row =>
        columns
        |> List.map(((lines, width)) =>
             Element.padToWidth(
               row < Array.length(lines) ? lines[row] : "",
               width,
             )
           )
        |> String.concat("")
      )
      |> String.concat("\n");

    | Element.VStack(children, options) =>
      let {gap, align, justify}: Element.stackOptions = options;

      /* Static children are invisible to ALL of the layout math below, not
       * just to the join: they are split off FIRST, so numChildren, the gap
       * total, every justify computation and calculateChildSizes only ever
       * see the live children. Being size 0 would not be enough - a size-0
       * child would still take a gap slot and a share of the justify
       * spacing. They are rendered (which is what commits their items; see
       * the Static case above) at their original index's path.
       *
       * ~origin IS threaded through this loop (B2), even though nothing in
       * this partition paints into the stack. An <Overlay> is invisible to
       * layout, so it lands in THIS list, and its "am I the committed pass?"
       * test is `!measuring && origin != None` - the same test the Viewport
       * case uses, and the only reliable one (a frame walks a subtree more
       * than once). Without the origin an <Overlay> placed directly in a
       * stack would never see a committed pass and would never record a
       * layer. Inert for the other invisible kinds: Empty ignores it, and
       * the Static case ignores it too - its recursive item renders
       * deliberately pass none. And what the overlay does with it is read a
       * BOOLEAN, not a coordinate: the box resolves against frameSize, never
       * against this stack's position. */
      let (statics, live) = partitionStatic(children);
      List.iter(
        ((i, child)) =>
          ignore(
            renderElement(
              ~origin?,
              child,
              rootCtx,
              constraints,
              ~path=childPath(path, i),
            ),
          ),
        statics,
      );

      /* Calculate height for each child (measureWidth=false for VStack) */
      let childSizes =
        calculateChildSizes(
          live,
          constraints.availHeight,
          gap,
          false,
          ~rootCtx,
          ~path,
        );

      /* Calculate total content height and remaining space for justify */
      let totalContentHeight =
        List.fold_left((acc, (_, _, h)) => acc + h, 0, childSizes);
      let totalGapHeight = gap * max(0, List.length(live) - 1);
      let usedHeight = totalContentHeight + totalGapHeight;
      let remainingSpace = max(0, constraints.availHeight - usedHeight);

      /* Calculate spacing based on justify */
      let numChildren = List.length(live);
      let (spaceBefore, spaceBetween, _spaceAfter) =
        switch (justify) {
        | Element.JustifyStart => (0, gap, 0)
        | Element.JustifyEnd => (remainingSpace, gap, 0)
        | Element.JustifyCenter => (remainingSpace / 2, gap, remainingSpace / 2)
        | Element.JustifySpaceBetween =>
          if (numChildren <= 1) {
            (0, gap, 0);
          } else {
            (0, gap + remainingSpace / (numChildren - 1), 0);
          }
        | Element.JustifySpaceAround =>
          if (numChildren == 0) {
            (0, gap, 0);
          } else {
            let space = remainingSpace / (numChildren * 2);
            (space, gap + space * 2, space);
          }
        | Element.JustifySpaceEvenly =>
          if (numChildren == 0) {
            (0, gap, 0);
          } else {
            let space = remainingSpace / (numChildren + 1);
            (space, gap + space, space);
          }
        };

      /* Origin of each live child (B4), by its POSITION in childSizes: the
       * stack's own origin, plus the leading justify space, plus every
       * earlier child's height, plus one inter-child gap per child before it.
       * Cross-axis alignment does not move a child's BOX here - every VStack
       * child is allocated the full available width - so only y varies.
       *
       * Precomputed into prefix sums rather than accumulated inside the
       * List.map below, so it does not depend on that map's evaluation
       * order. None (no origin to thread) yields None for every child. */
      let childOriginAt =
        switch (origin) {
        | None => (_pos => None)
        | Some((ox, oy)) =>
          let heights = childSizes |> List.map(((_, _, h)) => h) |> Array.of_list;
          let prefix = Array.make(Array.length(heights) + 1, 0);
          for (k in 0 to Array.length(heights) - 1) {
            prefix[k + 1] = prefix[k] + heights[k];
          };
          (
            pos =>
              Some((ox, oy + spaceBefore + prefix[pos] + pos * spaceBetween))
          );
        };

      /* Render each child with its allocated height */
      let renderedChildren =
        childSizes
        |> List.mapi((pos, (i, child, height)) => {
             let childConstraints = {
               availWidth: constraints.availWidth,
               availHeight: height,
             };
             let unwrapped = unwrapSized(child);
             let rendered =
               renderElement(
                 ~origin=?childOriginAt(pos),
                 unwrapped,
                 rootCtx,
                 childConstraints,
                 ~path=childPath(path, i),
               );

             /* Split into lines */
             let lines = Element.splitLines(rendered);

             /* Apply horizontal alignment (cross-axis for VStack) */
             let alignedLines =
               switch (align) {
               | Element.AlignStretch =>
                 /* Pad each line to full width */
                 lines |> List.map(line => Element.padToWidth(line, constraints.availWidth))
               | Element.AlignStart =>
                 /* Left-align (no change needed, just ensure no extra padding) */
                 lines
               | Element.AlignEnd =>
                 /* Right-align each line */
                 lines
                 |> List.map(line => {
                      let lineWidth = Element.visibleLength(line);
                      let padding = max(0, constraints.availWidth - lineWidth);
                      String.make(padding, ' ') ++ line;
                    })
               | Element.AlignCenter =>
                 /* Center each line */
                 lines
                 |> List.map(line => {
                      let lineWidth = Element.visibleLength(line);
                      let padding = max(0, constraints.availWidth - lineWidth);
                      let leftPad = padding / 2;
                      String.make(leftPad, ' ') ++ line;
                    })
               };

             /* Pad or truncate to exact height */
             let paddedLines =
               if (List.length(alignedLines) >= height) {
                 let rec take = (n, lst) =>
                   switch (n, lst) {
                   | (0, _) => []
                   | (_, []) => []
                   | (n, [h, ...t]) => [h, ...take(n - 1, t)]
                   };
                 take(height, alignedLines);
               } else {
                 alignedLines @ List.init(height - List.length(alignedLines), _ => "");
               };
             String.concat("\n", paddedLines);
           });

      /* Build output with justify spacing */
      let beforeStr =
        if (spaceBefore > 0) {
          String.concat("\n", List.init(spaceBefore, _ => ""));
        } else {
          "";
        };
      let betweenStr =
        if (spaceBetween > 0) {
          String.concat("\n", List.init(spaceBetween, _ => ""));
        } else {
          "";
        };

      let content =
        String.concat(
          betweenStr == "" ? "\n" : "\n" ++ betweenStr ++ "\n",
          renderedChildren,
        );

      (spaceBefore > 0 ? beforeStr ++ "\n" : "") ++ content;

    | Element.HStack(children, options) =>
      let {gap, align, justify}: Element.stackOptions = options;

      /* Static children are filtered out of every layout computation, and
       * rendered once (not once per pass) - see the VStack case above for
       * the full reasoning, including why ~origin is threaded (B2: it is how
       * an <Overlay> in this partition recognizes the committed pass). */
      let (statics, live) = partitionStatic(children);
      List.iter(
        ((i, child)) =>
          ignore(
            renderElement(
              ~origin?,
              child,
              rootCtx,
              constraints,
              ~path=childPath(path, i),
            ),
          ),
        statics,
      );

      /* Calculate width for each child (measureWidth=true for HStack) */
      let childSizes =
        calculateChildSizes(
          live,
          constraints.availWidth,
          gap,
          true,
          ~rootCtx,
          ~path,
        );

      /* Calculate total content width and remaining space for justify */
      let totalContentWidth =
        List.fold_left((acc, (_, _, w)) => acc + w, 0, childSizes);
      let totalGapWidth = gap * max(0, List.length(live) - 1);
      let usedWidth = totalContentWidth + totalGapWidth;
      let remainingSpace = max(0, constraints.availWidth - usedWidth);

      /* Calculate spacing based on justify */
      let numChildren = List.length(live);
      let (spaceBefore, spaceBetween, _spaceAfter) =
        switch (justify) {
        | Element.JustifyStart => (0, gap, 0)
        | Element.JustifyEnd => (remainingSpace, gap, 0)
        | Element.JustifyCenter => (remainingSpace / 2, gap, remainingSpace / 2)
        | Element.JustifySpaceBetween =>
          if (numChildren <= 1) {
            (0, gap, 0);
          } else {
            (0, gap + remainingSpace / (numChildren - 1), 0);
          }
        | Element.JustifySpaceAround =>
          if (numChildren == 0) {
            (0, gap, 0);
          } else {
            let space = remainingSpace / (numChildren * 2);
            (space, gap + space * 2, space);
          }
        | Element.JustifySpaceEvenly =>
          if (numChildren == 0) {
            (0, gap, 0);
          } else {
            let space = remainingSpace / (numChildren + 1);
            (space, gap + space, space);
          }
        };

      /* First pass: render children with minimal height constraint to measure natural size */
      /* Both passes index children the same way, so a child sees the same path
       * in the measure pass and in the real pass.
       *
       * NO origin is threaded here (B4): this pass exists only to learn each
       * child's natural height, its output is thrown away, and the child's
       * cross-axis position is not even known yet - it depends on the height
       * this pass is measuring. Recording bounds from it would put every
       * component at the wrong place (and at availHeight=0). The second pass
       * below is the committed one. */
      let measured =
        childSizes
        |> List.map(((i, child, width)) => {
             let childConstraints = {
               availWidth: width,
               availHeight: 0, /* Minimal height - let children use their natural size */
             };
             let unwrapped = unwrapSized(child);
             let rendered =
               renderElement(
                 unwrapped,
                 rootCtx,
                 childConstraints,
                 ~path=childPath(path, i),
               );
             let lines = Element.splitLines(rendered);
             (i, child, width, List.length(lines));
           });

      /* Use the container height for cross-axis alignment */
      let containerHeight = max(0, constraints.availHeight);

      /* Origin of each live child (B4), by its POSITION in the list: the
       * stack's own origin, plus the leading justify space, plus every
       * earlier child's width, plus one inter-child gap per child before it -
       * and, on the cross axis, the shift that vertical alignment gives a
       * child shorter than the container (see paddedChildren below, which
       * adds exactly that many blank lines above it). */
      let widths = measured |> List.map(((_, _, w, _)) => w) |> Array.of_list;
      let widthPrefix = Array.make(Array.length(widths) + 1, 0);
      for (k in 0 to Array.length(widths) - 1) {
        widthPrefix[k + 1] = widthPrefix[k] + widths[k];
      };
      let childOriginAt = (pos: int, naturalHeight: int) =>
        switch (origin) {
        | None => None
        | Some((ox, oy)) =>
          let alignShift =
            switch (align) {
            | Element.AlignStretch
            | Element.AlignStart => 0
            | Element.AlignEnd => max(0, containerHeight - naturalHeight)
            | Element.AlignCenter => max(0, containerHeight - naturalHeight) / 2
            };
          Some((
            ox + spaceBefore + widthPrefix[pos] + pos * spaceBetween,
            oy + alignShift,
          ));
        };

      /* For AlignStretch, re-render with container height; otherwise use measured results */
      /* Re-render children with appropriate height constraint based on alignment */
      let renderedChildren =
        measured
        |> List.mapi((pos, (i, child, width, naturalHeight)) => {
             let childHeight =
               switch (align) {
               | Element.AlignStretch => containerHeight
               | _ => naturalHeight
               };
             let childConstraints = {
               availWidth: width,
               availHeight: childHeight,
             };
             let unwrapped = unwrapSized(child);
             let rendered =
               renderElement(
                 ~origin=?childOriginAt(pos, naturalHeight),
                 unwrapped,
                 rootCtx,
                 childConstraints,
                 ~path=childPath(path, i),
               );
             let lines = Element.splitLines(rendered);
             lines |> List.map(line => Element.padToWidth(line, width));
           });

      let maxLines = containerHeight;

      /* Combine horizontally - zip lines together with gap */
      let gapStr = String.make(spaceBetween, ' ');

      /* Pad all children to same number of lines with vertical alignment */
      let paddedChildren =
        childSizes
        |> List.mapi((pos, (_, _, width)) => {
             /* [pos] indexes the LIVE children (renderedChildren is built
              * from the same filtered list); the child's original tree index
              * only matters for paths, which were resolved above. */
             let lines = List.nth(renderedChildren, pos);
             let numLines = List.length(lines);
             let emptyLine = String.make(width, ' ');

             if (numLines >= maxLines) {
               lines;
             } else {
               let linesToAdd = maxLines - numLines;
               switch (align) {
               | Element.AlignStretch =>
                 /* For stretch, pad at bottom (content should already fill height) */
                 lines @ List.init(linesToAdd, _ => emptyLine)
               | Element.AlignStart =>
                 /* Add empty lines at bottom */
                 lines @ List.init(linesToAdd, _ => emptyLine)
               | Element.AlignEnd =>
                 /* Add empty lines at top */
                 List.init(linesToAdd, _ => emptyLine) @ lines
               | Element.AlignCenter =>
                 /* Add empty lines evenly top and bottom */
                 let topPad = linesToAdd / 2;
                 let bottomPad = linesToAdd - topPad;
                 List.init(topPad, _ => emptyLine)
                 @ lines
                 @ List.init(bottomPad, _ => emptyLine);
               };
             };
           });

      /* Zip lines together */
      let rec zipLines = (lineNum: int, acc: list(string)): list(string) =>
        if (lineNum >= maxLines) {
          List.rev(acc);
        } else {
          let line =
            paddedChildren
            |> List.map(lines => List.nth(lines, lineNum))
            |> String.concat(gapStr);
          zipLines(lineNum + 1, [line, ...acc]);
        };

      /* Add horizontal padding for justify */
      let beforePad = String.make(spaceBefore, ' ');
      let lines = zipLines(0, []);
      let paddedLines =
        if (spaceBefore > 0) {
          lines |> List.map(line => beforePad ++ line);
        } else {
          lines;
        };

      String.concat("\n", paddedLines);

    | Element.Sized(child, _size) =>
      /* Size hint is used by parent Stack; here we just render the child */
      renderElement(~measuring, ~origin?, child, rootCtx, constraints, ~path)

    | Element.Container(child) =>
      /* A container-query boundary (A1), and NOTHING else: the child renders
       * with the same constraints, at the same origin, at the same path, so
       * the output is byte-identical to rendering the child directly and no
       * component's identity shifts. The only effect is that
       * useContainerSize() inside the subtree reports THIS box.
       *
       * Pushed in measuring mode too: a stack measures an Auto child and then
       * renders it for real, and a component whose output depends on the
       * container's width must give the same answer in both passes or its
       * measured size will not match what it paints.
       *
       * Fun.protect, so a raising child cannot leave the stack unbalanced for
       * the rest of the frame. */
      let saved = containerStack^;
      containerStack := [constraints, ...saved];
      Fun.protect(
        ~finally=() => containerStack := saved,
        () =>
        renderElement(~measuring, ~origin?, child, rootCtx, constraints, ~path)
      );

    | Element.Lazy(f) =>
      renderElement(~measuring, ~origin?, f(), rootCtx, constraints, ~path)

    | Element.Component(typeId, key, _props, renderFn) =>
      /* A component is ALWAYS rendered when it is visited, in both modes.
       *
       * Identity is the path through the element tree, plus this component's
       * type ID and key. Components at the same tree path keep the same ID
       * across renders, even if siblings appear or disappear - so the same
       * context (and therefore the same hook array) is found again here.
       *
       * There is no output cache. Every visit must descend into the body, both
       * because element trees are rebuilt each frame (nothing survives to cache
       * into) and because skipping the descent would hide this component's
       * descendants from recordRenderedComponent - cleanupUnmountedComponents
       * would then reap their contexts and destroy their state.
       *
       * The two modes differ only in what the body's own layout looks like
       * (~measuring is threaded through unchanged); the component machinery -
       * path, stable ID, context swap, effect queue - is identical, so a
       * component measured and then rendered in the same frame uses ONE context
       * and commits its effects once. */
      let selfPath = componentPath(path, typeId, key);
      let stableId = generateStableComponentId(selfPath);
      /* Record that this component was visited this render */
      recordRenderedComponent(stableId);

      /* Perf tracing (lib/Perf.re), off by default. Deliberately
       * closure-free: this is the hottest path in the renderer, so an
       * untraced render pays one bool read and nothing else - no `span`
       * wrapper, no allocation. The label is the ppx typeId (a source
       * location) plus the stableId; NEVER selfPath, whose separators are
       * control characters. `measuring` is the in-scope flag, so a stack's
       * measure pass of an Auto child lands in its own summary row. */
      let perfOn = Perf.isEnabled();
      let perfT0 = perfOn ? Perf.nowUs() : 0.0;

      /* Mouse bounds (B4). Recorded only on a committed pass: real mode with
       * a known origin. The rect is the box the parent ALLOCATED (its origin
       * plus this component's constraints), clipped to the innermost clip
       * rect - not the ink the body painted, so a click in the padding an
       * alignment left around a short child still lands on that child.
       *
       * `replace` (not `add`): a component visited twice in one committed
       * pass - which no current layout does, but B5's Viewport measures then
       * renders - keeps the LAST box, which is the one that was painted. */
      switch (origin) {
      | Some((ox, oy)) when !measuring =>
        Hashtbl.replace(
          Hooks.instance().componentBounds,
          stableId,
          Mouse.intersect(
            clipTop(),
            {
              Mouse.rx: ox,
              ry: oy,
              rw: constraints.availWidth,
              rh: constraints.availHeight,
            },
          ),
        )
      | _ => ()
      };

      let componentCtx =
        switch (Hooks.getComponentContext(stableId)) {
        | Some(ctx) => ctx
        | None => Hooks.createComponentContext(stableId, rootCtx.quit)
        };

      /* Set this component's context as current */
      let st = Hooks.instance();
      let previousContext = st.currentContext^;
      let previousComponentId = st.currentComponentId^;
      st.currentContext := Some(componentCtx);
      st.currentComponentId := Some(stableId);
      Hooks.beginRender(componentCtx);

      /* Render the component body. It occupies exactly this component's box,
       * so the origin passes straight through. */
      let result =
        renderElement(
          ~measuring,
          ~origin?,
          renderFn(),
          rootCtx,
          constraints,
          ~path=selfPath,
        );

      /* Queue effects for the commit phase (after the whole tree rendered) */
      Hooks.enqueueEffects(componentCtx);

      /* Restore previous context */
      st.currentContext := previousContext;
      st.currentComponentId := previousComponentId;

      if (perfOn) {
        Perf.recordComponent(
          ~name=
            switch (typeId) {
            | Some(t) => t
            | None => "?"
            },
          ~t0=perfT0,
          ~measuring,
          ~id=stableId,
        );
      };
      result;

    | Element.WithContext(setup, teardown, children) =>
      setup();
      /* teardown must run even if rendering the children raises, otherwise the
         context stays stuck at the provided value for the rest of the frame. */
      Fun.protect(~finally=teardown, () =>
        renderElement(
          ~measuring,
          ~origin?,
          children,
          rootCtx,
          constraints,
          ~path,
        )
      );
    };

  /* Restore previous constraints */
  currentConstraints := prevConstraints;
  result;
}

/* Calculate sizes for Stack children based on available space.
 *
 * Takes the stack's children paired with their ORIGINAL index in the
 * element tree (static children have already been filtered out by the
 * caller, so the indices may have holes), and returns
 * (originalIndex, element, allocatedSize) triples in the same order.
 *
 * Algorithm:
 * 1. Subtract gap space from available
 * 2. Measure Auto-sized children content
 * 3. Allocate absolute (Chars) and percentage sizes
 * 4. Distribute remaining space to flex children by ratio
 *
 * measureWidth: true for HStack (measuring widths), false for VStack (measuring heights)
 *
 * ~rootCtx and ~path are what measuring an Auto child needs: the child is
 * measured through renderElement at the SAME tree path the real pass will give
 * it, so a component child keeps one identity across both visits. That is why
 * the ORIGINAL index travels with each child: it is what childPath is built
 * from, and filtering must never renumber a child.
 */
and calculateChildSizes =
    (
      children: list((int, Element.t)),
      available: int,
      gap: int,
      measureWidth: bool,
      ~rootCtx: Hooks.renderContext,
      ~path: string,
    )
    : list((int, Element.t, int)) => {
  let numChildren = List.length(children);
  if (numChildren == 0) {
    [];
  } else {
    /* Subtract gap space */
    let totalGap = gap * (numChildren - 1);
    let availableForContent = max(0, available - totalGap);

    /* Extract size hints (default to Auto) */
    let childrenWithHints =
      children
      |> List.map(((i, child)) => {
           let hint =
             switch (getSizeHint(child)) {
             | Some(s) => s
             | None => Element.Auto
             };
           (i, child, hint);
         });

    /* First pass: measure Auto children and calculate fixed sizes.
     * The index here is the child's position in the ORIGINAL children list -
     * the same index the real render pass uses to build its path. */
    let childrenWithMeasured =
      childrenWithHints
      |> List.map(((i, child, hint)) => {
           let measured =
             switch (hint) {
             | Element.Auto =>
               let unwrapped = unwrapSized(child);
               Some(
                 measureContentSize(
                   unwrapped,
                   measureWidth,
                   ~rootCtx,
                   ~path=childPath(path, i),
                 ),
               );
             | _ => None
             };
           (i, child, hint, measured);
         });

    /* Calculate totals for each size type */
    let (autoTotal, absTotal, pctTotal, flexTotal) =
      List.fold_left(
        ((auto, abs, pct, flex), (_, _, hint, measured)) =>
          switch (hint) {
          | Element.Auto =>
            switch (measured) {
            | Some(size) => (auto + size, abs, pct, flex)
            | None => (auto, abs, pct, flex)
            }
          | Element.Chars(n) => (auto, abs + n, pct, flex)
          | Element.Percent(p) => (
              auto,
              abs,
              pct + availableForContent * p / 100,
              flex,
            )
          | Element.Flex(f) => (auto, abs, pct, flex + f)
          },
        (0, 0, 0, 0),
        childrenWithMeasured,
      );

    /* Remaining space for flex children */
    let remainingForFlex =
      max(0, availableForContent - autoTotal - absTotal - pctTotal);

    /* Second pass: allocate actual sizes */
    childrenWithMeasured
    |> List.map(((i, child, hint, measured)) => {
         let size =
           switch (hint) {
           | Element.Auto =>
             switch (measured) {
             | Some(s) => s
             | None => 0
             }
           | Element.Chars(n) => n
           | Element.Percent(p) => availableForContent * p / 100
           | Element.Flex(f) =>
             if (flexTotal > 0) {
               remainingForFlex * f / flexTotal;
             } else {
               0;
             }
           };
         (i, child, max(0, size));
       });
  };
}

/* Measure the natural content size of an element (simple heuristic).
 * For width: returns the maximum line length
 * For height: returns the number of lines
 *
 * The element is rendered in MEASURING mode: layout-free output (the natural
 * content size), but through the full runtime machinery, so any component in
 * the subtree runs its body inside its own hooks context.
 */
and measureContentSize =
    (
      el: Element.t,
      measureWidth: bool,
      ~rootCtx: Hooks.renderContext,
      ~path: string,
    )
    : int => {
  /* currentConstraints is the enclosing stack's own constraints here (set by
     renderElement before it called calculateChildSizes), so a measured
     component sees the same constraints it saw before measurement moved into
     the runtime. */
  let content =
    renderElement(~measuring=true, el, rootCtx, currentConstraints^, ~path);
  let lines = Element.splitLines(content);
  if (measureWidth) {
    /* Return max visible width of any line */
    List.fold_left(
      (maxW, line) => max(maxW, Element.visibleLength(line)),
      0,
      lines,
    );
  } else {
    /* Return number of lines */
    List.length(lines);
  };
};

/* ============================================================================
 * Detached rendering (Element.render from inside a component body)
 * ============================================================================ */

/* Path prefix for a subtree that an application renders by hand with
 * Element.render, rather than by returning it from a component body. Such a
 * subtree has no position in the element tree, so its components are given an
 * identity derived from the component that OWNS the Element.render call (the
 * one currently rendering, or 0 for the root context). That is stable across
 * frames, which is what component identity needs.
 *
 * The component's own type ID and key still separate siblings under that
 * prefix, so one owner can render several detached subtrees. Two detached
 * renders by the SAME owner of the SAME component type and key would collide;
 * give them distinct ~key values if that ever comes up. */
let detachedPathRoot = "detached";

/* Render an element that application code passed to Element.render.
 *
 * Installed into Element.componentRenderer below, so Element.render never runs
 * a component body raw. Output is layout-free (measuring mode), which is
 * exactly what Element.render documents and what its callers - SplitView in
 * examples/people-list, for instance - expect. */
let renderDetachedComponent = (el: Element.t): string => {
  let st = Hooks.instance();
  let rootCtx =
    switch (st.rootContext^) {
    | Some(ctx) => ctx
    /* No application running (Element.render called outside a frame): give the
       subtree a throwaway root so its components still get real contexts. */
    | None => Hooks.createContext(_ => ())
    };
  let ownerId =
    switch (st.currentComponentId^) {
    | Some(id) => id
    | None => 0
    };
  let path = detachedPathRoot ++ pathChildSep ++ string_of_int(ownerId);
  renderElement(~measuring=true, el, rootCtx, currentConstraints^, ~path);
};

/* Install the detached renderer. Element cannot do this itself: it is compiled
 * before Hooks and Runtime. */
let () = Element.componentRenderer := Some(renderDetachedComponent);

/* ============================================================================
 * Compositing floating layers (B2/B4)
 * ============================================================================ */

/* Add [Element.Dim] to every cell of [row] in columns [from, from+len).
 *
 * DIM, NOT PAINT. The cells keep their glyphs and their own styles - a log
 * line under a modal's shadow is still readable, just darker - which is the
 * whole reason the backdrop is a shadow rather than a wash. Cells the row
 * does not actually have are not invented: splitAtWidth simply yields an
 * empty middle for a short row, so the shadow dims what is underneath it and
 * nothing else. */
let dimColumns =
    (row: list(StyledText.chunk), ~from: int, ~len: int)
    : list(StyledText.chunk) =>
  if (len <= 0) {
    row;
  } else {
    let (left, rest) = StyledText.splitAtWidth(row, max(0, from));
    let (mid, right) = StyledText.splitAtWidth(rest, len);
    left
    @ List.map(
        (c: StyledText.chunk) => {
          ...c,
          StyledText.styles: StyledText.addStyle(c.StyledText.styles, Element.Dim),
        },
        mid,
      )
    @ right;
  };

/* Splice this frame's floating layers over the finished frame, and publish
 * them to Hooks so input routing knows what is open.
 *
 * Called at the ONE splice point every render loop has, immediately after the
 * render walk and before anything consumes the frame:
 *
 *   render tree -> compositeOverlays -> drain statics -> paint -> commit
 *
 * That position matters twice over. It is after layout, so a layer costs the
 * layout nothing and can be positioned in frame coordinates; and it is before
 * collectKeyHandlers/commitFocus/dispatch, so the layer stack those read is
 * this frame's.
 *
 * NOTHING OPEN IS THE FAST PATH: [base] comes back PHYSICALLY unchanged - not
 * re-parsed, not re-baked, not reallocated - so every frame of every
 * application that has no overlay is byte-for-byte what it was before this
 * function existed. That is what keeps the whole golden suite valid. It is
 * also why the "composite" Perf span is opened only when there is something
 * to composite: an app with no modal shows no such span at all.
 *
 * THE SPLICE, per layer (bottom-most first, so a later sibling floats over an
 * earlier one) and per row y = ofY + r:
 *
 *   left  = pad(fst(split(baseRow, ofX)), ofX)
 *   ov    = pad(fst(split(overlayRow, avail)), avail)
 *   right = snd(split(snd(split(baseRow, ofX)), avail))
 *   row  := left @ ov @ right
 *
 * The two nested splits are the "drop exactly the columns the box covers"
 * operation - see StyledText.splitAtWidth, which also explains what happens
 * to a double-width cell straddling either edge. The two pads are what make
 * the layer OPAQUE: a base row shorter than ofX still gets the box at the
 * right column, and a dialog row shorter than the box still writes every one
 * of its cells.
 */
let compositeOverlays = (base: string): string => {
  let st = Hooks.instance();
  switch (overlayQueue^) {
  | [] =>
    Hooks.setOverlayLayers(st, []);
    base;
  | frames =>
    Perf.span("composite", () => {
      let (fw, fh) = frameSize^;

      /* Publish topmost-first (the queue is bottom-most first). */
      Hooks.setOverlayLayers(
        st,
        List.rev_map(
          (f: overlayFrame) => {
            let members = Hashtbl.create(max(1, List.length(f.ofMembers)));
            List.iter(id => Hashtbl.replace(members, id, ()), f.ofMembers);
            {
              Hooks.olMembers: members,
              olBox: {Mouse.rx: f.ofX, ry: f.ofY, rw: f.ofW, rh: f.ofH},
              olOnDismiss: f.ofOnDismiss,
            };
          },
          frames,
        ),
      );

      /* Parse the base ONCE, into one mutable row array. A layer may reach
         past the last line the application rendered (a short app in a tall
         terminal), so the array is grown to whatever the deepest layer
         needs - clipped to the frame, which is where the box was clamped. */
      let parsed = StyledText.parse(base);
      let baseLen = List.length(parsed);
      let needed =
        List.fold_left(
          (m, f: overlayFrame) => max(m, min(fh, f.ofY + f.ofH)),
          baseLen,
          frames,
        );
      let rows = Array.make(max(1, needed), []);
      List.iteri((i, line) => rows[i] = line, parsed);
      let numRows = Array.length(rows);

      List.iter(
        (f: overlayFrame) => {
          /* The shadow goes down FIRST, so the layer's own content cannot be
             dimmed by it. An L offset by (+1, +1): a two-column strip down
             the right side, and a strip along the bottom. Two columns
             because a terminal cell is roughly twice as tall as it is wide,
             so a one-column shadow reads as a hairline. It never grows the
             frame - a shadow with nothing underneath it is nothing. */
          if (f.ofShadow) {
            for (y in f.ofY + 1 to f.ofY + f.ofH - 1) {
              if (y >= 0 && y < numRows) {
                rows[y] = dimColumns(rows[y], ~from=f.ofX + f.ofW, ~len=2);
              };
            };
            let by = f.ofY + f.ofH;
            if (by >= 0 && by < numRows) {
              rows[by] =
                dimColumns(rows[by], ~from=f.ofX + 1, ~len=f.ofW + 1);
            };
          };

          let avail = max(0, min(f.ofW, fw - f.ofX));
          if (avail > 0) {
            List.iteri(
              (r, line) => {
                let y = f.ofY + r;
                if (y >= 0 && y < numRows) {
                  let baseRow = rows[y];
                  let (leftRaw, rest) =
                    StyledText.splitAtWidth(baseRow, f.ofX);
                  let left = StyledText.padChunksToWidth(leftRaw, f.ofX);
                  let (_covered, right) =
                    StyledText.splitAtWidth(rest, avail);
                  let ovRow =
                    switch (StyledText.parse(line)) {
                    | [l, ..._] => l
                    | [] => []
                    };
                  let (ovCut, _) = StyledText.splitAtWidth(ovRow, avail);
                  let ov = StyledText.padChunksToWidth(ovCut, avail);
                  rows[y] = left @ ov @ right;
                };
              },
              f.ofLines,
            );
          };
        },
        frames,
      );

      StyledText.bake(Array.to_list(rows));
    })
  };
};

/* ============================================================================
 * Static output drain (A4)
 * ============================================================================ */

/* Take everything this frame wants to commit ABOVE the live region, in
 * order, and advance the watermarks that make the commit permanent.
 *
 * Call this ONCE per frame, straight after the tree has rendered and before
 * the frame is painted. Every render loop does (all three of them):
 *
 *   render tree -> drainStaticLines() -> paint (static lines, then frame)
 *
 * Order within the result: raw useStdout writes first, then each <Static>
 * node's new items in first-visit (tree) order. Advancing the watermarks
 * HERE, rather than inside the render walk, is what makes a subtree that is
 * visited twice in one frame (HStack's measure pass, then its real pass)
 * commit its items exactly once: both visits see the same watermark and
 * record the same lines, and the second recording replaces the first.
 *
 * Returns [] when there is nothing to commit, which is every frame of every
 * application that uses neither <Static> nor useStdout - those apps must
 * stay byte-for-byte identical to what they printed before A4.
 */
let drainStaticLines = (): list(string) => {
  let st = Hooks.instance();
  let raw = st.pendingRawOutput^;
  st.pendingRawOutput := [];
  let pending = st.pendingStatic^;
  st.pendingStatic := [];
  let staticLines =
    pending
    |> List.concat_map(((path, count, lines)) => {
         Hashtbl.replace(st.staticEmitted, path, count);
         lines;
       });
  raw @ staticLines;
};

/* ============================================================================
 * OSC 11 background-color replies
 * ============================================================================ */

let hexDigit = (c: char): option(int) =>
  switch (c) {
  | '0' .. '9' => Some(Char.code(c) - Char.code('0'))
  | 'a' .. 'f' => Some(Char.code(c) - Char.code('a') + 10)
  | 'A' .. 'F' => Some(Char.code(c) - Char.code('A') + 10)
  | _ => None
  };

/* One component of an X color specification, scaled to 0..255.
 *
 * THE XTERM CONVENTION: the number of hex DIGITS is the precision, and the
 * value is a fraction of full intensity at that precision - so "f", "ff",
 * "fff" and "ffff" all mean 255, and a 4-digit "1e1e" means 0x1e, not
 * 0x1e1e. The scaling that gets every width right is
 * v * 255 / (16^digits - 1). 1 to 4 digits are accepted; anything else, or
 * a non-hex digit, is None. */
let hexComponent = (s: string): option(int) => {
  let n = String.length(s);
  if (n < 1 || n > 4) {
    None;
  } else {
    let acc = ref(Some(0));
    String.iter(
      c =>
        switch (acc^, hexDigit(c)) {
        | (Some(a), Some(v)) => acc := Some(a * 16 + v)
        | _ => acc := None
        },
      s,
    );
    switch (acc^) {
    | None => None
    | Some(v) =>
      let maxv = ref(1);
      for (_ in 1 to n) {
        maxv := maxv^ * 16;
      };
      Some(v * 255 / (maxv^ - 1));
    };
  };
};

/* Parse an OSC 11 payload into (r, g, b), 0..255 per channel.
 *
 * Accepts the X form every xterm-compatible terminal answers with,
 * "rgb:RRRR/GGGG/BBBB" (1-4 hex digits per component - see hexComponent),
 * and the "#RRGGBB" form some terminals use instead. Anything else - a
 * different color space, the wrong component count, trailing junk - yields
 * None, and the caller ignores the report. */
let parseOscColor = (payload: string): option((int, int, int)) => {
  let p = String.trim(payload);
  let len = String.length(p);
  if (len > 4 && String.sub(p, 0, 4) == "rgb:") {
    switch (String.split_on_char('/', String.sub(p, 4, len - 4))) {
    | [rs, gs, bs] =>
      switch (hexComponent(rs), hexComponent(gs), hexComponent(bs)) {
      | (Some(r), Some(g), Some(b)) => Some((r, g, b))
      | _ => None
      }
    | _ => None
    };
  } else if (len == 7 && p.[0] == '#') {
    let two = i => hexComponent(String.sub(p, i, 2));
    switch (two(1), two(3), two(5)) {
    | (Some(r), Some(g), Some(b)) => Some((r, g, b))
    | _ => None
    };
  } else {
    None;
  };
};

/* Handle one OscReport. Only OSC 11 (background color) means anything to the
 * runtime; every other code, and every payload that does not parse, is
 * ignored outright. Returns true when the value CHANGED and the caller
 * should therefore mark the root dirty - exactly one extra frame, and only
 * when there is genuinely something new to render. */
let applyOscReport = (code: int, payload: string): bool =>
  if (code != 11) {
    false;
  } else {
    switch (parseOscColor(payload)) {
    | Some(rgb) => Hooks.setTerminalBackground(Hooks.instance(), rgb)
    | None => false
    };
  };

/* Deliver every event InputDecoder produced from one read (or one flush)
 * to the application, in order. Shared by both interactive loops (`start`
 * and `startHeadlessLoop`) - the headless handle's sendKey/sendPaste
 * bypass the decoder entirely and call Hooks.dispatchKey directly (they
 * simulate already-parsed input, not raw bytes).
 *
 * - KeyEvent: normal key dispatch.
 * - PasteEvent: surfaces as Key.Paste through the SAME dispatch path
 *   (Ink-style - one chunk through useInput/useKeyDown, no separate paste
 *   hook), per B2.
 * - MouseEvent: mapped from SCREEN coordinates into LIVE-REGION ones and
 *   handed to Hooks.dispatchMouse (B4). ~liveTop reports the screen row
 *   (1-based) of the region's FIRST line; the decoder has already made the
 *   event's coordinates 0-based, so the row shift is just (liveTop - 1).
 *   An event above the region - a click in the scrollback or in committed
 *   <Static> output - maps to a negative row and is DROPPED: it belongs to
 *   the terminal's transcript, not to the application's tree. The default
 *   ~liveTop of 1 is the "region starts at the top of what we paint" case,
 *   which is what the headless loop sees.
 * - CursorReport: DSR cursor-position response, consumed by Runtime's own
 *   cursor tracking (A4/B4 bottomRow bookkeeping) - never dispatched to
 *   application key handlers. `start` passes ~onCursor to feed its bottomRow
 *   ref; loops that don't track the cursor (the headless one) leave it out
 *   and the report is dropped.
 * - OscReport: an OSC string reply - terminal plumbing, like CursorReport,
 *   and never dispatched to application key handlers. OSC 11 (the
 *   background-color probe Runtime.start sends at startup) updates the
 *   instance's terminalBg and marks the root dirty IF the value changed, so
 *   an application using Hooks.useTerminalBackground repaints itself with
 *   the right theme exactly once. Every other OSC code is ignored.
 *
 * ~flushDirty re-renders NOW if a previous event's handler left the app
 * dirty; it is called before every dispatch. Without it, two keystrokes
 * arriving in one read() would both hit handler closures from the SAME
 * render - and a value-based setState from the second would silently clobber
 * the first's update (type "hi" fast enough and only the "i" survives). The
 * handle-based headless path re-renders after every sendKey for the same
 * reason; this keeps the byte-fed loops equivalent, one event per frame.
 */
let deliverAll =
    (
      ~onCursor: option((int, int) => unit)=?,
      ~liveTop: unit => int=() => 1,
      ~flushDirty: unit => unit=() => (),
      ctx: Hooks.renderContext,
      events: list(InputDecoder.event),
    )
    : unit =>
  List.iter(
    (event: InputDecoder.event) => {
      flushDirty();
      switch (event) {
      | InputDecoder.KeyEvent(key, mods) =>
        Perf.span("dispatch-key", () => Hooks.dispatchKey(ctx, key, mods))
      | InputDecoder.PasteEvent(s) =>
        Perf.span("dispatch-paste", () =>
          Hooks.dispatchKey(ctx, Key.Paste(s), Key.noModifiers)
        )
      | InputDecoder.MouseEvent(ev) =>
        let localY = ev.Mouse.y - (liveTop() - 1);
        if (localY >= 0) {
          Perf.span("dispatch-mouse", () =>
            Hooks.dispatchMouse(ctx, {...ev, Mouse.y: localY})
          );
        }
      | InputDecoder.CursorReport(row, col) =>
        switch (onCursor) {
        | Some(f) => f(row, col)
        | None => ()
        }
      | InputDecoder.OscReport(code, payload) =>
        if (applyOscReport(code, payload)) {
          ctx.needsRerender = true;
        }
      };
    },
    events,
  );

/* Internal: Run the headless main loop (when MATCHA_HEADLESS=1) */
let startHeadlessLoop = (module C: HooksComponent, config: headlessConfig) => {
  /* Start from a clean world: no component contexts, IDs or effects from a
     previously started application. */
  Hooks.currentInstance := Hooks.freshInstance();
  currentConstraints := defaultConstraints;
  resetContainerStack(defaultConstraints);

  let running = ref(true);

  let quit = (behavior: Hooks.quitBehavior) => {
    running := false;
    ignore(behavior); /* No screen to clear in headless mode */
  };

  /* Create hooks context for this component */
  let ctx = Hooks.createContext(quit);
  Hooks.instance().rootContext := Some(ctx);

  /* No signal handler for resize in headless - size is fixed */
  /* No wake pipe needed - we process input synchronously */
  /* No terminal setup needed */

  /* Register cleanup for exit. The handler re-installs this loop's instance so
     that it tears down its own world, whatever ran afterwards. */
  let myInstance = Hooks.instance();
  at_exit(() => {
    Hooks.currentInstance := myInstance;
    Hooks.unmountAll();
  });

  /* Set stdin to non-blocking for headless input */
  Unix.set_nonblock(Unix.stdin);

  /* Fresh decoder for this loop (see InputDecoder.re): reassembles raw
     reads into KeyEvents/PasteEvents/MouseEvents/CursorReports across
     however many reads a paste, a UTF-8 codepoint or a mouse report
     happens to span. */
  let decoder = InputDecoder.create();

  /* One full frame - factored out so deliverAll's ~flushDirty can run it
     between two input events decoded from one read (see deliverAll). */
  let renderFrame = () =>
    Perf.frame(() => {
    /* No clearScreen in headless - just output the frame */
    Hooks.instance().currentContext := Some(ctx);
      Hooks.beginRender(ctx);
      resetComponentTracking();

      let constraints = {
        availWidth: config.width,
        availHeight: config.height,
      };

      /* Mouse bounds (B4) are recorded relative to the top-left of the
         frame, and clipped to it. */
      resetClipStack(constraints);

      /* Root body runs before renderElement - install this frame's
         constraints so a root-level useContainerSize() sees them (see the same
         assignment in startHeadless's doRender for the full story). */
      currentConstraints := constraints;
      /* Container queries (A1) start at the frame: a useContainerSize()
         made outside every <Container> answers "the window". */
      resetContainerStack(constraints);

      let rendered =
        Perf.span("render", () => {
          let element = C.make();
          renderElement(~origin=(0, 0), element, ctx, constraints, ~path="");
        });
      /* Floating layers (B2), spliced over the finished frame and published
       * to Hooks for input routing. Returns `rendered` physically unchanged
       * when nothing is open. */
      let output = compositeOverlays(rendered);
      /* Static output (A4) is committed ABOVE the frame, one line each, in
       * exactly the position the interactive loop would put it. An
       * application that uses neither <Static> nor useStdout drains an
       * empty list here and prints byte-for-byte what it printed before A4 -
       * which is what keeps every existing golden valid. */
      let staticLines = Perf.span("static-drain", () => drainStaticLines());
      Perf.span("paint", () => {
        List.iter(
          line => {
            print_string(line);
            print_newline();
          },
          staticLines,
        );
        print_string(output);
        print_newline(); /* Add newline between frames for readability */
        flush(stdout);
      });

      Perf.span("effects", () => Hooks.commitEffects(ctx));
      Perf.span("unmount-sweep", () =>
        Hooks.cleanupUnmountedComponents(renderedComponentIds())
      );
      Perf.span("collect-handlers", () =>
        Hooks.collectKeyHandlers(ctx, List.rev(renderedComponentIds()))
      );
      Perf.span("focus-commit", () =>
        Hooks.commitFocus(Hooks.instance(), ctx)
      );
      Hooks.instance().currentContext := None;
  });

  /* Bring the frame up to date between two events decoded from one read.
     Loops rather than rendering once, and bounded: a frame's COMMIT can
     leave the app dirty - commitFocus assigning autoFocus is the common
     case, and opening a modal hits it every time - and the handlers the NEXT
     event is dispatched to are the ones the last COMPLETED frame collected.
     Render only once and a Ctrl+K and the character after it, arriving in
     the same read(), would deliver that character to handlers collected
     before focus moved into the dialog: silently dropped. The main loop
     below converges on its own (it re-renders every iteration), so this is
     only about events that share a read. */
  let flushDirty = () => {
    let guard = ref(0);
    while (running^ && ctx.needsRerender && guard^ < 8) {
      incr(guard);
      ctx.needsRerender = false;
      renderFrame();
    };
  };

  /* Main loop */
  while (running^) {
    if (ctx.needsRerender) {
      ctx.needsRerender = false;
      renderFrame();
    };

    /* Try to read input from stdin (non-blocking). The timeout is capped at
       0.1s but shortened to 25ms when the decoder is mid-escape-sequence
       (a lone ESC waiting to see if more follows), and further shortened
       when a timer (useInterval/useTimeout) is due sooner, so the loop
       wakes up in time to fire it either way. */
    let cap = InputDecoder.pendingEsc(decoder) ? 0.025 : 0.1;
    let (ready, _, _) =
      try(Unix.select([Unix.stdin], [], [], Hooks.nextTimerTimeout(~cap))) {
      | Unix.Unix_error(Unix.EINTR, _, _) => ([], [], [])
      };

    if (List.mem(Unix.stdin, ready)) {
      switch (Terminal.readBytes()) {
      | Some((buf, n)) =>
        deliverAll(~flushDirty, ctx, InputDecoder.feed(decoder, buf, n))
      | None =>
        /* EOF on stdin - exit gracefully */
        running := false
      };
    } else if (InputDecoder.pendingEsc(decoder)) {
      /* Nothing arrived within the shortened deadline: the held ESC was a
         keypress on its own, not the start of a longer sequence. */
      deliverAll(~flushDirty, ctx, InputDecoder.flush(decoder));
    };

    /* Fire any timers (useInterval/useTimeout) whose deadline has passed.
       Callbacks run like key handlers, outside of render; setState inside
       one just marks needsRerender for the next loop iteration. */
    Perf.span("timers", () => ignore(Hooks.fireDueTimers()));
  };
};

/* Device Status Report: ask the terminal where the cursor is. The answer
 * comes back on stdin as ESC[{row};{col}R and InputDecoder turns it into a
 * CursorReport event, which `start` feeds into its bottomRow tracking.
 *
 * Fire-and-forget by design: a terminal that does not answer (or answers
 * late, or is not a terminal at all) costs nothing, because bottomRow is
 * initialized to the terminal height first - the right answer whenever the
 * application starts at the bottom of the screen, which is where a command
 * run from a shell prompt starts. */
let dsrQuery = "\027[6n";

/* How the interactive loop owns the terminal (headless modes ignore this).
   Inline (default): render at the cursor as a live region; <Static> and
   useStdout commit into normal scrollback above it. Fullscreen: take over
   the whole viewport on the ALTERNATE screen (no scrollback, nothing to
   scroll away to); on quit the previous terminal contents come back.
   <Static>/useStdout have no home on the alt screen, and rather than
   silently absorbing output that can never appear, using either one under
   Fullscreen RAISES Invalid_argument on the first attempt (see
   Hooks.requireStaticAllowed). A fullscreen app renders its transcript
   itself - e.g. in a <ScrollView>; see examples/claude-code. */
type screenMode =
  | Inline
  | Fullscreen;

/* Start the application with the given root component.
 *
 * This function:
 * 1. Sets up raw terminal mode (no echo, no line buffering)
 * 2. Hides the cursor
 * 3. Installs a SIGWINCH handler for terminal resize
 * 4. Runs the main loop (renders, dispatches events, runs effects)
 * 5. Restores terminal on exit (via at_exit handler)
 *
 * RENDERING IS INLINE BY DEFAULT (A4). ~screen=Inline: the screen is never
 * cleared and the alternate screen buffer is never used: the first frame is
 * painted right where the cursor already is, and every later frame is
 * patched in place, relative to it (see lib/LiveRegion.re). Output an
 * application commits with <Static> or useStdout is printed ABOVE that live
 * region and scrolls into the terminal's scrollback, exactly like ordinary
 * command output - which is why quit(ClearScreen) now erases the live region
 * ONLY, leaving the transcript where it belongs.
 *
 * ~screen=Fullscreen switches to the ALTERNATE screen (see [screenMode]):
 * the frame is padded to the full terminal height and painted with absolute
 * addressing through lib/FrameDiff.re, there is no scrollback to scroll away
 * to, and quitting restores whatever the terminal showed before. <Static>
 * and useStdout RAISE there rather than committing into a void. Every
 * fullscreen difference is guarded on the mode: Inline's output is
 * byte-for-byte what it was before this argument existed.
 *
 * The loop continues until quit is called (via useQuit hook).
 *
 * If MATCHA_HEADLESS=1 environment variable is set, runs in headless mode:
 * - No terminal setup (works without TTY)
 * - Uses MATCHA_WIDTH and MATCHA_HEIGHT for dimensions (default 80x24)
 * - Reads key input from stdin, outputs frames to stdout
 */
let start = (~screen: screenMode=Inline, module C: HooksComponent) => {
  /* Check for headless mode */
  if (isHeadless()) {
    /* ~screen is deliberately ignored here: a headless stream has no screen
       to own - it prints frames to stdout and never touches the terminal. */
    startHeadlessLoop((module C), getHeadlessConfigFromEnv());
  } else {
    /* Start from a clean world (see startHeadlessLoop) */
    Hooks.currentInstance := Hooks.freshInstance();
    currentConstraints := defaultConstraints;
    resetContainerStack(defaultConstraints);

    /* One flag, one source of truth: <Static> and useStdout both consult
       instanceState.staticAllowed, and Fullscreen turns it off before the
       first render so the first attempt raises instead of vanishing. */
    switch (screen) {
    | Inline => ()
    | Fullscreen => Hooks.setStaticAllowed(Hooks.instance(), false)
    };

    let running = ref(true);

    /* Previous frame's lines, as PAINTED (normalized and clamped to the
     * terminal height - see LiveRegion.normalize), for the in-place line
     * diff. None means "nothing is painted": the first frame, or a forced
     * full repaint after a resize (see handleResize below, since a resize
     * can change layout on every line). */
    let prevFrame: ref(option(array(string))) = ref(None);

    /* Height of the live region currently on screen, in lines. Kept next to
     * prevFrame because the erase paths (resize, quit) need it after
     * prevFrame has been cleared. */
    let prevHeight = ref(0);

    /* Screen row (1-based) of the LAST line of the live region - i.e. where
     * the cursor sits between frames. Mouse coordinates (B4) are reported in
     * screen rows, so translating them into frame rows needs this. It is
     * initialized to the terminal height (an application started from a shell
     * prompt begins at the bottom of the screen), corrected by the DSR reply
     * when the terminal sends one, and then maintained per frame by the
     * formula at the end of the render block below. */
    let (_, initialHeight) = Terminal.getSize();
    let bottomRow = ref(initialHeight);

    /* Screen row (1-based) of the FIRST line of the live region - what a
     * mouse report's row has to be measured against. bottomRow is never
     * less than the region's own height (see the tracking formula below), so
     * this is always >= 1.
     *
     * Fullscreen owns the whole viewport starting at the top of the alternate
     * screen, so frame row 0 IS screen row 1 - there is nothing to track and
     * bottomRow is meaningless (it is left untouched throughout). */
    let liveTop = () =>
      switch (screen) {
      | Fullscreen => 1
      | Inline => bottomRow^ - prevHeight^ + 1
      };

    /* Whether terminal mouse reporting is currently ON. Mouse mode is
     * interest-driven (B4): after each frame the loop asks whether anything
     * still wants mouse events and flips the terminal only on a transition,
     * so an application that never calls useMouse never enables it - and one
     * that stops wanting events (a clickable panel that closes) turns text
     * selection back over to the terminal. restoreTerminal disables
     * unconditionally, so a crash cannot leave the terminal in mouse mode. */
    let mouseEnabled = ref(false);

    let quit = (behavior: Hooks.quitBehavior) => {
      running := false;
      let (_, termHeight) = Terminal.getSize();
      switch (screen, behavior) {
      /* FULLSCREEN: neither behavior touches the screen. The alternate
         screen is dropped wholesale by restoreTerminal's ESC[?1049l, which
         puts the terminal back exactly as the app found it - that IS the
         correct restore, for both ClearScreen (nothing of ours survives
         anyway) and PreserveScreen (there is no inline frame to park a
         prompt under). Erasing or printing here would only scribble on the
         normal screen after the buffers swap back. */
      | (Fullscreen, _) => ()
      | (Inline, behavior) =>
        switch (behavior) {
        | ClearScreen =>
          /* Erase the LIVE REGION only. Everything committed above it
           * (<Static> items, useStdout writes, and whatever was on screen
           * before the application started) is transcript and stays - that is
           * the whole point of rendering inline. */
          let s = LiveRegion.erase(~prevHeight=prevHeight^, ~termHeight);
          if (s != "") {
            print_string(s);
            flush(stdout);
          };
        | PreserveScreen =>
          /* Park the cursor on a fresh line below the region, so the shell
           * prompt does not land on top of the last frame. */
          print_string("\r\n");
          flush(stdout);
        }
      };
    };

    /* Create hooks context for this component */
    let ctx = Hooks.createContext(quit);

    /* Set root context reference so components can trigger re-renders */
    Hooks.instance().rootContext := Some(ctx);

    /* Signal handler for resize.
     *
     * The old region's line breakdown is meaningless at the new width, and
     * the terminal may have reflowed it, so it is erased outright and the
     * next frame repaints in full (prevFrame := None). The DSR query is
     * re-sent because the erase leaves the cursor at the top of where the
     * region was, which after a reflow is not necessarily where the tracking
     * thinks it is. */
    let handleResize = _ =>
      switch (screen) {
      | Fullscreen =>
        /* Nothing to erase and no cursor to re-locate: dropping prevFrame is
           enough, because FrameDiff.diff(~prev=None) starts its repaint with
           a full ESC[2J clear and then paints every row of the new frame
           absolutely. */
        prevFrame := None;
        prevHeight := 0;
        ctx.needsRerender = true;
      | Inline =>
        let (_, termHeight) = Terminal.getSize();
        let s = LiveRegion.erase(~prevHeight=prevHeight^, ~termHeight);
        if (s != "") {
          print_string(s);
        };
        print_string(dsrQuery);
        flush(stdout);
        prevFrame := None;
        prevHeight := 0;
        bottomRow := termHeight;
        ctx.needsRerender = true;
      };

    Sys.set_signal(Terminal.sigwinch, Sys.Signal_handle(handleResize));

    /* Initialize wake pipe for background thread state updates */
    initWakePipe();

    /* Fresh decoder for this loop (see InputDecoder.re). */
    let decoder = InputDecoder.create();

    /* Set up terminal */
    Terminal.setRawMode();
    Terminal.hideCursor();
    switch (screen) {
    | Fullscreen =>
      /* Take over the whole viewport. No DSR query: on the alternate screen
         the frame always starts at row 1, so there is no cursor position to
         learn and no bottomRow to correct. */
      Terminal.enterAltScreen();
      /* Push the kitty keyboard protocol AGAIN, for the alternate screen.
         kitty-protocol terminals keep the keyboard-flag stack SEPARATELY
         PER SCREEN BUFFER, so setRawMode's push (made on the main screen a
         moment ago) does not apply here: without this, a fullscreen app
         would silently lack the key disambiguation the inline path gets.
         Terminal.restoreTerminal pops BOTH stacks on the way out - see its
         comment for the ordering that makes that work. */
      Terminal.pushKittyKeyboard();
    | Inline =>
      /* Ask the terminal where the cursor is, once, now that raw mode is on
       * (so the reply is not echoed and lands in our own input stream).
       * Fire-and-forget: see [dsrQuery]. */
      print_string(dsrQuery);
      flush(stdout);
    };
    /* Ask the terminal for its BACKGROUND COLOR, once, at startup. This is
     * mode-independent - unlike the DSR query above, it says nothing about
     * where the cursor is - so Fullscreen sends it too. Fire-and-forget: see
     * Terminal.queryBackground. The reply comes back through the same input
     * stream as the DSR one and is routed by deliverAll. */
    Terminal.queryBackground();
    let myInstance = Hooks.instance();
    at_exit(() => {
      /* Run effect cleanups (of this run's own instance) before exit */
      Hooks.currentInstance := myInstance;
      Hooks.unmountAll();
      Terminal.restoreTerminal();
    });

    /* One full frame: render, patch the live region on screen, run the
       commit phase, sync mouse mode. Factored out of the loop so that
       deliverAll's ~flushDirty can run it BETWEEN two input events decoded
       from one read (see deliverAll's doc comment). */
    let renderFrame = () =>
      Perf.frame(() => {
      /* Set current context and render */
      Hooks.instance().currentContext := Some(ctx);
        Hooks.beginRender(ctx);

        /* Reset component tracking for this render */
        resetComponentTracking();

        /* Get terminal dimensions for layout constraints */
        let (termWidth, termHeight) = Terminal.getSize();
        let constraints = {
          availWidth: termWidth,
          availHeight: termHeight,
        };

        /* Mouse bounds (B4) are recorded relative to the top-left of the
           live region, and clipped to it. */
        resetClipStack(constraints);

        /* Root body runs before renderElement - install this frame's
           constraints so a root-level useContainerSize() sees them (see the same
           assignment in startHeadless's doRender for the full story). */
        currentConstraints := constraints;
        /* Container queries (A1) start at the frame: a useContainerSize()
           made outside every <Container> answers "the window". */
        resetContainerStack(constraints);

        let rendered =
          Perf.span("render", () => {
            let element = C.make();
            renderElement(~origin=(0, 0), element, ctx, constraints, ~path="");
          });
        /* Floating layers (B2). Physically unchanged when nothing is open,
         * so the LiveRegion patch below stays the pure in-place frame diff
         * it has always been for an application without a modal. */
        let output = compositeOverlays(rendered);

        /* Everything the frame wants to commit above the live region, in
         * order (see drainStaticLines). Empty for an application that uses
         * neither <Static> nor useStdout, which makes the patch below a
         * pure in-place frame diff, exactly as before A4.
         *
         * Drained unconditionally, in BOTH screen modes, so the queues can
         * never grow across frames. Under Fullscreen the result is always []
         * anyway: <Static> and useStdout raise before anything can be queued
         * there (see screenMode / Hooks.requireStaticAllowed), so the drain
         * is a cheap invariant check rather than a discard. */
        let staticLines = Perf.span("static-drain", () => drainStaticLines());

        Perf.span("paint", () =>
        switch (screen) {
        | Fullscreen =>
          /* The app owns the whole viewport: clamp the frame to the terminal
             height, then PAD it with blank rows up to exactly termHeight.
             The padding is what is load-bearing for clearing stale rows -
             FrameDiff.diff does handle a shrinking frame (it emits ESC[J
             from the new last row when next is shorter than prev), but with
             the padding in place `next` is always exactly termHeight rows,
             so that branch never fires and every previously-occupied row is
             overwritten by an explicit blank instead. Padding also means the
             app visibly owns the whole screen from the very first frame. */
          let clamped =
            LiveRegion.normalize(
              ~next=Array.of_list(Element.splitLines(output)),
              ~termHeight,
            );
          let rows = max(1, termHeight);
          let next =
            Array.init(rows, i =>
              i < Array.length(clamped) ? clamped[i] : ""
            );
          let patch = FrameDiff.diff(~prev=prevFrame^, ~next);
          if (patch != "") {
            print_string(patch);
            flush(stdout);
          };
          /* No bottomRow arithmetic: on the alternate screen the frame's
             first row is screen row 1, always (see liveTop). */
          prevFrame := Some(next);
          prevHeight := Array.length(next);
        | Inline =>
          /* Store what was PAINTED, not what was rendered: the patch works
           * against the clamped frame, so the next diff has to compare
           * against the same thing. */
          let next =
            LiveRegion.normalize(
              ~next=Array.of_list(Element.splitLines(output)),
              ~termHeight,
            );
          let patch =
            LiveRegion.patch(
              ~prev=prevFrame^,
              ~staticLines,
              ~next,
              ~termHeight,
            );
          if (patch != "") {
            print_string(patch);
            flush(stdout);
          };

          /* Track where the cursor now is (the last line of the live
           * region). Committing S static lines pushes the region S rows
           * down, and the region's own growth or shrink moves its last line
           * by the same delta - both capped by the bottom of the screen,
           * since the screen scrolls instead of the cursor going past it,
           * and floored by the region's own height, since its last line
           * cannot be higher up than that. */
          let prevLen = prevHeight^;
          let nextLen = Array.length(next);
          bottomRow :=
            max(
              nextLen,
              min(
                termHeight,
                bottomRow^ + List.length(staticLines) + (nextLen - prevLen),
              ),
            );
          prevFrame := Some(next);
          prevHeight := nextLen;
        }
        );

        /* Commit phase: run effects queued during the render */
        Perf.span("effects", () => Hooks.commitEffects(ctx));

        /* Remove any component contexts that were not rendered this pass */
        Perf.span("unmount-sweep", () =>
          Hooks.cleanupUnmountedComponents(renderedComponentIds())
        );

        /* Collect key handlers from all component contexts into root context */
        Perf.span("collect-handlers", () =>
          Hooks.collectKeyHandlers(ctx, List.rev(renderedComponentIds()))
        );
        Perf.span("focus-commit", () =>
          Hooks.commitFocus(Hooks.instance(), ctx)
        );

        /* Mouse mode follows the committed frame's interest (B4). Done after
           the commit, so it sees this frame's handlers, and only on a change,
           so a steady-state frame emits no escape bytes at all. */
        Perf.span("mouse-sync", () => {
          let wantMouse = Hooks.hasMouseHandlers();
          if (wantMouse != mouseEnabled^) {
            if (wantMouse) {
              Terminal.enableMouse();
            } else {
              Terminal.disableMouse();
            };
            mouseEnabled := wantMouse;
          };
        });

      Hooks.instance().currentContext := None;
      });

    /* Re-render immediately if an event handler left the app dirty - run by
       deliverAll between two events from the same read. Guarded on running:
       a handler that called quit() has already erased/parked the region and
       must not repaint it. */
    let flushDirty = () => {
      let guard = ref(0);
      while (running^ && ctx.needsRerender && guard^ < 8) {
        incr(guard);
        ctx.needsRerender = false;
        renderFrame();
      };
    };

    /* Main loop */
    while (running^) {
      if (ctx.needsRerender) {
        ctx.needsRerender = false;
        renderFrame();
      };

      /* Wait for input on stdin or wake pipe using select */
      let readFds =
        switch (wakePipe^) {
        | Some((readFd, _)) => [Unix.stdin, readFd]
        | None => [Unix.stdin]
        };

      let cap = InputDecoder.pendingEsc(decoder) ? 0.025 : 0.1;
      let (ready, _, _) =
        try(Unix.select(readFds, [], [], Hooks.nextTimerTimeout(~cap))) {
        | Unix.Unix_error(Unix.EINTR, _, _) => ([], [], []) /* Interrupted by signal */
        };

      /* Drain wake pipe if it was signaled */
      let hasWakePipe =
        switch (wakePipe^) {
        | Some((readFd, _)) => List.mem(readFd, ready)
        | None => false
        };
      if (hasWakePipe) {
        drainWakePipe();
      };

      /* A DSR reply tells us exactly which screen row the cursor is on, so
         it overrides the tracked value outright. Fullscreen never asks (and
         tracks nothing), so a stray report there is simply ignored. */
      let onCursor = (row, _col) =>
        switch (screen) {
        | Fullscreen => ignore(row)
        | Inline => bottomRow := row
        };

      /* Handle stdin input if ready */
      if (List.mem(Unix.stdin, ready)) {
        switch (Terminal.readBytes()) {
        | Some((buf, n)) =>
          deliverAll(
            ~onCursor,
            ~liveTop,
            ~flushDirty,
            ctx,
            InputDecoder.feed(decoder, buf, n),
          )
        | None => () /* EOF on stdin - keep running, matches prior behavior */
        };
      } else if (InputDecoder.pendingEsc(decoder)) {
        /* Nothing arrived within the shortened deadline: the held ESC was
           a keypress on its own, not the start of a longer sequence. */
        deliverAll(
          ~onCursor,
          ~liveTop,
          ~flushDirty,
          ctx,
          InputDecoder.flush(decoder),
        );
      };

      /* Fire any timers (useInterval/useTimeout) whose deadline has passed.
         Callbacks run like key handlers, outside of render; setState inside
         one just marks needsRerender for the next loop iteration. */
      Perf.span("timers", () => ignore(Hooks.fireDueTimers()));
    };
  };
};

/* Start the application in headless mode for testing/agent interaction.
 *
 * Unlike start(), this function:
 * - Does NOT set raw terminal mode
 * - Does NOT read from stdin
 * - Does NOT print to stdout
 * - Returns immediately with a handle for programmatic control
 *
 * Example:
 *   let handle = Runtime.startHeadless((module MyApp));
 *   handle.sendKey(Key.Arrow_up, Key.noModifiers);
 *   let output = handle.getOutput();
 *   handle.quit();
 */
let startHeadless =
    (~config=defaultHeadlessConfig, module C: HooksComponent): headlessHandle => {
  /* This handle owns its own instance state: component contexts, IDs, props
     and pending effects are private to it, so two handles created one after
     the other in the same process cannot see each other's hook state.
     Every handle method re-installs this instance before touching runtime
     state, so an older handle used after a newer one still operates on its
     own world. Interleaving handles from several threads is NOT supported. */
  let myInstance = Hooks.freshInstance();
  Hooks.currentInstance := myInstance;
  currentConstraints := defaultConstraints;
  resetContainerStack(defaultConstraints);

  /* Virtual clock: timers (useInterval/useTimeout) are scheduled against
     this instead of the wall clock, so advanceTime below can fire them
     deterministically without a real sleep. Starts at 0.0 and only moves
     when advanceTime is called. */
  let virtualNow = ref(0.0);
  myInstance.now := (() => virtualNow^);

  let running = ref(true);
  let lastOutput = ref("");
  let currentWidth = ref(config.width);
  let currentHeight = ref(config.height);

  /* Everything committed above the live region so far (A4), one line per
     line, in commit order. It ACCUMULATES across frames - that is what makes
     it a transcript - while getOutput/getLines keep meaning "the current
     live frame" and nothing else. */
  let staticBuf = Buffer.create(256);

  let quit = (behavior: Hooks.quitBehavior) => {
    running := false;
    ignore(behavior); /* No screen to clear in headless mode */
  };

  /* Create hooks context */
  let ctx = Hooks.createContext(quit);
  myInstance.rootContext := Some(ctx);

  /* Make this handle's instance the one in force */
  let activate = (): unit => {
    Hooks.currentInstance := myInstance;
  };

  /* No SIGWINCH handler - size is controlled programmatically */
  /* No wake pipe - we control when to process events */
  /* No terminal setup */

  /* Render function */
  let doRender = (): string =>
    Perf.frame(() => {
    activate();
    ctx.needsRerender = false;

    myInstance.currentContext := Some(ctx);
    Hooks.beginRender(ctx);
    resetComponentTracking();

    let constraints = {
      availWidth: currentWidth^,
      availHeight: currentHeight^,
    };

    /* Mouse bounds (B4) are recorded relative to the top-left of the
       frame, and clipped to it. */
    resetClipStack(constraints);

    /* The ROOT component body runs in C.make(), BEFORE renderElement gets a
       chance to install this frame's constraints - so a useContainerSize()
       call in the root body would otherwise read whatever the refs last
       held (the 80x24 default, forever). Install them here, at every root
       render site, so the root sees the same width/height its children do. */
    currentConstraints := constraints;
    /* Container queries (A1) start at the frame: a useContainerSize()
       made outside every <Container> answers "the window". */
    resetContainerStack(constraints);

    let rendered =
      Perf.span("render", () => {
        let element = C.make();
        renderElement(~origin=(0, 0), element, ctx, constraints, ~path="");
      });
    /* Floating layers (B2). getOutput() below therefore shows the modal, and
     * Hooks has this frame's layer stack before collectKeyHandlers runs. */
    let output = compositeOverlays(rendered);

    /* Static commits belong to the frame that produced them, so they are
       drained here, in the same place the real loops drain them. */
    Perf.span("static-drain", () =>
      List.iter(
        line => {
          Buffer.add_string(staticBuf, line);
          Buffer.add_char(staticBuf, '\n');
        },
        drainStaticLines(),
      )
    );

    Perf.span("effects", () => Hooks.commitEffects(ctx));
    Perf.span("unmount-sweep", () =>
      Hooks.cleanupUnmountedComponents(renderedComponentIds())
    );
    Perf.span("collect-handlers", () =>
      Hooks.collectKeyHandlers(ctx, List.rev(renderedComponentIds()))
    );
    Perf.span("focus-commit", () => Hooks.commitFocus(myInstance, ctx));
    myInstance.currentContext := None;

    lastOutput := output;
    output;
  });

  /* Render until the application is QUIET, exactly as the interactive loops
     do, and never leave the handle holding an unsettled frame.

     A frame's COMMIT can leave the app dirty - commitFocus assigning
     autoFocus (B1 step 3) is the common case, and a modal opening mid-session
     hits it every time. That matters beyond cosmetics, because the handlers a
     key is dispatched to are the ones collectKeyHandlers built during the
     LAST COMPLETED frame: render only once after an event and the next key is
     delivered to a frame that had not settled yet. Open a palette whose input
     has ~autoFocus and the first character typed after it lands on handlers
     collected before focus moved in - i.e. nowhere, and the keystroke is
     silently dropped.

     Runtime.start does not have this problem: its `while (running)` body
     re-renders for as long as needsRerender is set before it blocks on input
     again. This is what makes the handle faithful to that. Bounded, so an
     application that dirties itself on every commit cannot hang a test
     instead of failing one. */
  let settle = (): unit => {
    let guard = ref(0);
    while (ctx.needsRerender && guard^ < 8) {
      incr(guard);
      ignore(doRender());
    };
  };

  /* Advance the virtual clock, firing timers deadline-by-deadline (not
     coalesced - see the headlessHandle.advanceTime doc comment above). At
     each fired deadline, re-render if it left the app dirty, so a timer
     newly registered by that callback can still fire within this same
     advance if its own deadline also falls within [now, target]. */
  let advanceTime = (ms: int): unit => {
    activate();
    let target = virtualNow^ +. float_of_int(ms) /. 1000.0;
    let iterations = ref(0);
    let keepGoing = ref(true);
    while (keepGoing^) {
      iterations := iterations^ + 1;
      if (iterations^ > 100_000) {
        failwith("advanceTime: runaway timer");
      };
      let earliestDeadline =
        Hashtbl.fold(
          (_id, t: Hooks.timer, acc) =>
            switch (acc) {
            | None => Some(t.deadline)
            | Some(d) => Some(d < t.deadline ? d : t.deadline)
            },
          myInstance.timers,
          None,
        );
      switch (earliestDeadline) {
      | Some(deadline) when deadline <= target =>
        virtualNow := deadline;
        Perf.span("timers", () => ignore(Hooks.fireDueTimers()));
        settle();
      | _ =>
        virtualNow := target;
        keepGoing := false;
      };
    };
  };

  /* Initial render, then settle - so autoFocus is visible in the very first
     observable frame, one frame earlier than commitFocus alone would make it. */
  let _ = doRender();
  settle();

  /* Build and return the handle */
  {
    sendKey: (key, modifiers) =>
      if (running^) {
        activate();
        Perf.span("dispatch-key", () =>
          Hooks.dispatchKey(ctx, key, modifiers)
        );
        /* Re-render if state changed, then keep going until the app is
           quiet - see [settle] above for why one frame is not enough. */
        settle();
      },

    /* Simulate a bracketed paste, bypassing the byte-level decoder
       entirely (there are no raw bytes to reassemble in a test) - CRLF
       normalization is the only processing applied, matching what
       InputDecoder does to a real paste body, and the result is
       dispatched as Key.Paste through the same path every other key
       goes through (Ink-style: one event, no separate paste hook). */
    sendPaste: text =>
      if (running^) {
        activate();
        Perf.span("dispatch-paste", () =>
          Hooks.dispatchKey(
            ctx,
            Key.Paste(InputDecoder.normalizePasteBody(text)),
            Key.noModifiers,
          )
        );
        settle();
      },

    /* Deliver a mouse event straight to dispatchMouse. No coordinate
       mapping: a headless frame is painted at (0, 0), so what a test passes
       IS the live-region coordinate the interactive loop would have
       computed. */
    sendMouse: ev =>
      if (running^) {
        activate();
        Perf.span("dispatch-mouse", () => Hooks.dispatchMouse(ctx, ev));
        settle();
      },

    getOutput: stripAnsi =>
      if (stripAnsi) {
        Element.stripAnsi(lastOutput^);
      } else {
        lastOutput^;
      },

    /* Everything <Static> and useStdout have committed so far, in order,
       newline-terminated. Separate from getOutput on purpose: static output
       is a growing transcript, the frame is a snapshot. */
    getStaticOutput: stripAnsi =>
      if (stripAnsi) {
        Element.stripAnsi(Buffer.contents(staticBuf));
      } else {
        Buffer.contents(staticBuf);
      },

    getLines: stripAnsi => {
      let output =
        if (stripAnsi) {
          Element.stripAnsi(lastOutput^);
        } else {
          lastOutput^;
        };
      Array.of_list(String.split_on_char('\n', output));
    },

    isRunning: () => running^,

    render: () => {
      activate();
      ctx.needsRerender = true;
      ignore(doRender());
      settle();
      lastOutput^;
    },

    resize: (w, h) => {
      activate();
      currentWidth := w;
      currentHeight := h;
      ctx.needsRerender = true;
      ignore(doRender());
      settle();
    },

    getSize: () => (currentWidth^, currentHeight^),

    /* Stop the app and unmount the tree: every effect cleanup of this
       instance runs, exactly once, even if quit() is called twice. */
    quit: () => {
      activate();
      quit(Hooks.PreserveScreen);
      Hooks.unmountAll();
    },

    advanceTime,

    getFocusedId: () => {
      activate();
      Hooks.instance().focus.focusedId;
    },

    setTerminalBackground: rgb => {
      activate();
      if (Hooks.setTerminalBackground(myInstance, rgb)) {
        ctx.needsRerender = true;
        ignore(doRender());
        settle();
      };
    },
  };
};
