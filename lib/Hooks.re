/*
 * Hooks - React-style hooks for stateful terminal components
 *
 * This module provides hooks that allow components to have state and
 * side effects while remaining functional. Hooks must be called in the
 * same order on every render.
 *
 * Available hooks:
 * - useState: Local component state
 * - useEffect: Side effects with dependency tracking
 * - useEffectAlways: Side effects that run every render
 * - useMemo: Memoize values based on dependencies
 * - useKeyDown: Register keyboard event handlers
 * - useQuit: Get a function to quit the application
 * - useRef: A mutable value that survives re-renders without triggering one
 * - useInterval: Run a callback on a repeating timer
 * - useTimeout: Run a callback once after a delay
 *
 * ============================================================================
 * CONTRACTS THIS MODULE RELIES ON
 * ============================================================================
 *
 * PER-INSTANCE STATE
 * ------------------
 * Every piece of a running application's hook state lives in one
 * [instanceState] record: the component contexts, the path -> ID registry, the
 * ID counter, the root context, the "currently rendering" refs and the effect
 * commit queue. Runtime installs a FRESH instance on every start (Runtime.start
 * / startHeadless), so two applications started one after another in the same
 * process - a test suite creating several headless handles, say - cannot see
 * each other's hooks, IDs or effects.
 *
 * Exactly ONE instance is in force at a time ([currentInstance], read through
 * [instance()]). A headless handle re-installs its own instance before every
 * operation, so an older handle used after a newer one still acts on its own
 * world. Interleaving two instances from different threads is NOT supported.
 *
 * [useState] captures the owning instance at hook time, because setState can
 * fire long after the render - from a key handler, an effect, or a background
 * thread - possibly while another instance is installed.
 *
 * [wakeMainLoopRef] is deliberately NOT part of instanceState: it holds a
 * process-level callback (the wake-pipe writer), and Runtime re-registers it on
 * every start.
 *
 * CURRENT-CONTEXT SWAP DISCIPLINE
 * -------------------------------
 * Hooks read [instance().currentContext] (see [getContext]). Runtime sets it to
 * the component's context before running the body and RESTORES the previous
 * value afterwards - save, set, render, restore - so nesting works and a
 * component's hooks can never land in its parent's hook array. The runtime
 * calls [beginRender] on the context first, which resets the hook index and
 * clears the per-pass key handlers and pending effects.
 *
 * COMMIT-PHASE EFFECT CONTRACT
 * ----------------------------
 * Rendering a body only SCHEDULES effects: [useEffect] compares the hook's
 * stored deps against the new ones and, if they differ, pushes a
 * [pendingEffect] onto the context; the runtime then calls [enqueueEffects] on
 * that context. Nothing runs until the whole tree has rendered and the runtime
 * calls [commitEffects], which drains the queue (children before parents, root
 * last) and calls [runEffects] on each.
 *
 * The dependency slot is written at COMMIT time, inside [runEffects], after the
 * effect actually ran - never at schedule time. A component can render more
 * than once per frame (a stack measures an Auto child, then renders it for
 * real), and each of those passes must schedule the same effect so that the
 * frame commits it exactly once: [runEffects] clears the pending list, so a
 * context queued twice is a no-op on the second visit. Writing deps early would
 * make the second pass believe the effect had already run.
 *
 * Cleanups are per hook slot: the previous cleanup runs before the new effect,
 * and [runCleanups] clears each slot as it fires, so unmount and at_exit
 * teardown cannot double-run one cleanup.
 *
 * THE ==/=== SAFETY RULE
 * ----------------------
 * In Reason, ==/!= are OCaml's STRUCTURAL =/<>; ===/!== are the PHYSICAL ones.
 * Anything compared here may be an [Obj.t] wrapping a closure (a deps array
 * holding a callback, for instance), and OCaml's structural compare raises
 * Invalid_argument("compare: functional value") the moment it descends into a
 * function. So [depsEqual] and every other Obj.t comparison in this module use
 * !== / ===, which compare pointers for heap values and the raw value for
 * immediates and never look inside. DO NOT "fix" this with Obj.magic to
 * nativeint - that segfaults on immediate values under OCaml 5.
 */

/* Internal hook value storage.
 * Each hook slot stores either state or effect data.
 */
type hookValue =
  | StateHook(ref(Obj.t))
  | EffectHook(ref(option(array(Obj.t))), ref(option(unit => unit)))
  | MemoHook(ref(option((Obj.t, array(Obj.t))))); /* cached value + deps */
/* EffectHook stores: (previous deps for comparison, cleanup function) */

/* Pending effect to be run after render (internal).
 *
 * deps holds the dependency array this effect was scheduled with (None for
 * useEffectAlways, which has no dependencies). prevDeps is the hook's stored
 * dependency slot. The slot is written at COMMIT time, after the effect ran,
 * not when the effect is scheduled: a component can render more than once in
 * one frame (an HStack measures its children, then renders them for real), and
 * each of those passes must schedule the same effect so that the frame commits
 * it exactly once.
 */
type pendingEffect = {
  effect: unit => option(unit => unit),
  cleanup: ref(option(unit => unit)),
  deps: option(array(Obj.t)),
  prevDeps: ref(option(array(Obj.t))),
};

/* A registered timer (useInterval/useTimeout - see the TIMERS section below).
 * intervalMs=Some(ms) repeats every ms; None fires once, then is removed.
 * deadline is an absolute point on the instance's `now` clock (seconds).
 * callbackRef is the useRef cell holding the timer's latest closure, so a
 * re-render's fresh callback is picked up without re-registering the timer. */
type timer = {
  timerId: int,
  mutable deadline: float,
  intervalMs: option(int),
  callbackRef: ref(unit => unit),
};

/* Behavior when quitting the application.
 * Controls whether the screen is cleared on exit.
 */
type quitBehavior =
  | ClearScreen /* Clear the terminal before exiting */
  | PreserveScreen; /* Keep terminal content visible after exit */

/* Render context containing all hook state for a component.
 * Managed by the runtime - not for direct use.
 */
type renderContext = {
  mutable hookIndex: int, /* Current hook slot */
  mutable hooks: array(hookValue), /* Stored hook values */
  mutable keyHandlers: list((Key.t, Key.modifiers) => unit), /* Active handlers */
  /* Handlers registered by useInput during THIS render, most recent first.
     Kept SEPARATE from keyHandlers because the two have different capture
     rules once a floating layer is open (B2): useKeyDown always fires,
     useInput only for the topmost layer's members. Before overlays existed
     useInput was literally a conditional useKeyDown, and capture was
     therefore not expressible at all. Like keyHandlers, this is cleared by
     beginRender and collected into the root context by collectKeyHandlers -
     which is where the filtering happens. */
  mutable inputHandlers: list((Key.t, Key.modifiers) => unit),
  /* Mouse handlers registered by useMouse during THIS render, most recent
     first (like keyHandlers). Unlike keyHandlers they are never collected
     into the root context: dispatchMouse routes a click to ONE component by
     its recorded bounds, so it reads each context's own list. */
  mutable mouseHandlers: list(Mouse.event => unit),
  /* Whether any of this context's mouse handlers wants WHEEL events
     (ScrollUp/ScrollDown). A context without wheel interest is transparent
     to the wheel: dispatchMouse skips it when picking a wheel target, so
     the notch falls through to an enclosing ScrollView instead of being
     swallowed - the reason <Clickable> rows inside a list still scroll.
     Reset alongside mouseHandlers every render. */
  mutable wheelInterest: bool,
  /* The mirror of wheelInterest, for everything that is NOT the wheel
     (Down/Up/Move). A context without click interest is transparent to a
     click: dispatchMouse skips it when picking a target, so the click falls
     through to whatever encloses it.

     <ScrollView> declares exactly this. Its useMouse body handles the wheel
     and ignores every other event - yet without this flag it still WON the
     hit test for clicks, because registering any handler made it a target.
     A list rendered in ~rows mode has no child elements at all, so a click
     on it reached the ScrollView, was dropped on the floor, and never
     reached the application. That was a silent failure with no error
     anywhere, and it cost a real debugging session.

     Reset alongside mouseHandlers every render. */
  mutable clickInterest: bool,
  mutable pendingEffects: list(pendingEffect), /* Effects to run post-render */
  /* Whether a new frame is wanted. Only the ROOT context's flag is read: it is
     what the main loops (and headless sendKey) poll to decide whether to draw
     again. setState on a component sets the component's own flag AND the
     root's; the component's copy is informational only, because a frame that
     runs re-renders the whole tree - a visited component is never skipped. */
  mutable needsRerender: bool,
  componentId: option(Element.componentId), /* Component instance ID if this is a component context */
  quit: quitBehavior => unit /* Quit callback */
};

/* ============================================================================
 * Focus (B1)
 *
 * One focusable registration, made by useFocus (below) every time it runs
 * during a render. fOwner mirrors the owning component's ctx.componentId
 * (None for the root context - at most one root exists, so None acts as its
 * own singleton owner key). fid is the string identity applications and
 * focusManager.focus(id) address; fIsActive/fAutoFocus mirror useFocus's
 * ~isActive/~autoFocus props for that render.
 * ============================================================================ */

type focusable = {
  fid: string,
  fOwner: option(Element.componentId),
  fIsActive: bool,
  fAutoFocus: bool,
};

/* focusState lives on instanceState (one per running application), NOT on a
 * per-component renderContext: useFocus prepends onto a single per-frame
 * [registrations] list (mirroring how useKeyDown prepends onto a context's
 * keyHandlers), and commitFocus (near collectKeyHandlers, in the "Internal
 * Runtime Functions" section below) turns that into tree/render order once
 * per frame.
 *
 * prevOwners encoding: keyed by "owner key" = fOwner mapped through
 * focusOwnerKey (-1 for the root's None, otherwise the componentId) - a
 * single Hashtbl rather than a separate rootSeen bool, since -1 can never
 * collide with a real componentId (those start at 0). Its meaning is "was
 * this owner mounted as of the last commit": freshFocusState starts it
 * empty, so on an application's very first commitFocus every owner -
 * including the root - looks newly mounted, which is exactly what makes
 * autoFocus on the root fire only on that first commit (see commitFocus's
 * step 3 and freshInstance below).
 *
 * order is the PREVIOUS frame's committed, deduped, tree-order array at the
 * moment a new commitFocus call begins (it is overwritten during step 1 of
 * that same call) - step 2 reads it before overwriting to find where a
 * just-vanished focused entry used to sit among last frame's active
 * entries, so its successor at the same position inherits focus. */
type focusState = {
  mutable registrations: list(focusable),
  mutable order: array(focusable),
  mutable prevOwners: Hashtbl.t(int, unit),
  mutable focusedId: option(string),
  mutable enabled: bool,
};

let freshFocusState = (): focusState => {
  registrations: [],
  order: [||],
  prevOwners: Hashtbl.create(16),
  focusedId: None,
  enabled: true,
};

/* ============================================================================
 * Overlay layers (B2)
 *
 * One entry per floating layer the last frame painted, as
 * [Runtime.compositeOverlays] published it. This is the whole of what input
 * routing needs to know about overlays - Hooks never sees an Element, a
 * clip rect or a rendered line.
 *
 * - olMembers: the components rendered INSIDE the layer's child. A layer
 *   "owns" exactly these. Runtime captures the set during the render walk
 *   (see its Overlay case) and builds this table once per frame, because
 *   every consumer below is a membership TEST and a list scan per keystroke
 *   would be quadratic in the size of the dialog.
 * - olBox: where the layer was painted, in frame coordinates - what a mouse
 *   Down is tested against to decide "inside or outside".
 * - olOnDismiss: run when a Down lands outside olBox.
 *
 * NEVER COMPARE ONE OF THESE WITH == OR !=. Reason's ==/!= are OCaml's
 * STRUCTURAL =/<>, this record holds a Hashtbl and a closure, and structural
 * compare raises Invalid_argument("compare: functional value") the moment it
 * reaches the closure. Pattern-match on the list instead (see topOverlayLayer
 * below) - which is all any consumer here actually needs.
 * ============================================================================ */

type overlayLayer = {
  olMembers: Hashtbl.t(Element.componentId, unit),
  olBox: Mouse.rect,
  olOnDismiss: option(unit => unit),
};

/* ============================================================================
 * Instance state
 *
 * All state of a running application lives in one instanceState record, so
 * that two applications started one after another (Runtime.startHeadless in a
 * test suite, for example) cannot see each other's hook contexts, component
 * IDs or pending effects. Runtime installs a fresh instance on every start,
 * and a headless handle re-installs its own instance before it touches
 * anything.
 *
 * Only ONE instance is in force at a time (currentInstance). Interleaving two
 * instances from different threads is not supported.
 * ============================================================================ */

type instanceState = {
  /* Component context registry - maps component IDs to their render contexts */
  componentContexts: Hashtbl.t(Element.componentId, renderContext),
  /* Maps a component's tree path to its stable ID (kept across renders) */
  componentIdRegistry: Hashtbl.t(string, Element.componentId),
  /* Component IDs visited in the current render pass, most recent first */
  renderedComponentIds: ref(list(Element.componentId)),
  /* Source of unique component IDs for this instance */
  nextComponentId: ref(int),
  /* Root context (set when Runtime starts the application) */
  rootContext: ref(option(renderContext)),
  /* Context of the component currently rendering */
  currentContext: ref(option(renderContext)),
  /* ID of the component currently rendering */
  currentComponentId: ref(option(Element.componentId)),
  /* Contexts whose render pass finished this frame, most recent first.
     Filled during render, drained by commitEffects. */
  effectCommitQueue: ref(list(renderContext)),
  /* Timer registry for useInterval/useTimeout (A3). Keyed by timerId. */
  timers: Hashtbl.t(int, timer),
  /* Source of unique timer IDs for this instance. */
  nextTimerId: ref(int),
  /* The clock timers are scheduled against. Defaults to Unix.gettimeofday;
     Runtime.startHeadless overrides it with a virtual clock so advanceTime
     can drive timers deterministically without a real sleep. */
  now: ref(unit => float),
  /* Focus registry (B1). See the focusState comment above. */
  focus: focusState,
  /* ---- Static output (A4) --------------------------------------------
   * How many items each <Static> node has already committed above the live
   * region, keyed by the node's TREE PATH. This is the append-only
   * watermark: on a frame, items with index >= staticEmitted[path] are the
   * new ones. It is deliberately keyed by path rather than by component
   * context, so it survives the Static node unmounting and remounting at the
   * same place (which would otherwise re-emit the whole transcript).
   *
   * ONLY Runtime.drainStaticLines advances it, once per frame, AFTER the
   * tree has rendered - never during the render walk, which may visit the
   * same Static node twice (HStack measures its children, then renders
   * them). */
  staticEmitted: Hashtbl.t(string, int),
  /* Static lines produced by THIS frame's render walk, not yet drained:
   * (static node path, item count after this frame, rendered lines).
   *
   * Kept in FIRST-VISIT order (two <Static> nodes commit in tree order), but
   * an entry is REPLACED in place when the same path is visited again within
   * the frame - the second visit of a double-rendered subtree computes the
   * same lines, and replace-on-revisit means it wins rather than duplicating.
   */
  pendingStatic: ref(list((string, int, list(string)))),
  /* Lines queued by useStdout().write, FIFO. Drained BEFORE pendingStatic,
   * so an explicit write and the frame that follows it stay in order. */
  pendingRawOutput: ref(list(string)),
  /* Whether committing output above the live region is meaningful AT ALL for
   * this application. True everywhere except Runtime's Fullscreen screen
   * mode, which paints the alternate screen: there is no scrollback there, so
   * a committed line has nowhere to go and the request is a programming
   * error, not something to absorb silently. Runtime.start flips this off
   * (setStaticAllowed) right after installing the instance; both <Static> and
   * useStdout().write consult it and RAISE. The headless paths never touch
   * it - they are screen-agnostic and Static works normally there. */
  mutable staticAllowed: bool,
  /* ---- Mouse bounds registry (B4) ------------------------------------
   * Where each component was last PAINTED, keyed by its stable component
   * id: the rect it was allocated by its parent (constraint box, not the
   * ink it actually drew - a click in alignment padding still counts),
   * already intersected with whatever clipped it.
   *
   * Filled by Runtime.renderElement's Component case, and only on a
   * committed pass (real mode with a known origin - measuring passes and
   * HStack's natural-height pre-pass carry no origin and record nothing).
   * Cleared by Runtime.resetComponentTracking at the start of every
   * render, exactly like renderedComponentIds, so the table always
   * describes the last frame that was actually painted. */
  componentBounds: Hashtbl.t(Element.componentId, Mouse.rect),
  /* ---- Terminal background color --------------------------------------
   * The terminal's own background, as (r, g, b) with 0..255 per channel,
   * once it has told us - Runtime.start sends an OSC 11 query at startup and
   * fills this in when (and if) the reply arrives. None means "not known",
   * which for a terminal that never answers is permanent, so
   * useTerminalBackground's callers must always have a default. Not a hook
   * slot: it is a property of the running application, read by any number of
   * components. */
  mutable terminalBg: option((int, int, int)),
  /* ---- Overlay layers (B2) --------------------------------------------
   * The floating layers of the last composited frame, TOPMOST FIRST (the
   * queue Runtime builds is bottom-most first, i.e. paint order; this is
   * the reverse, because every consumer here wants the topmost one).
   *
   * Published by Runtime.compositeOverlays, which runs straight after the
   * render walk and therefore BEFORE collectKeyHandlers, commitFocus and
   * any dispatch - so key capture, focus containment and mouse routing all
   * see this frame's layers, never last frame's. [] when nothing is open,
   * which is the fast path every existing application stays on. */
  mutable overlayLayers: list(overlayLayer),
};

/* Create an empty instance state. */
let freshInstance = (): instanceState => {
  componentContexts: Hashtbl.create(100),
  componentIdRegistry: Hashtbl.create(100),
  renderedComponentIds: ref([]),
  nextComponentId: ref(0),
  rootContext: ref(None),
  currentContext: ref(None),
  currentComponentId: ref(None),
  effectCommitQueue: ref([]),
  timers: Hashtbl.create(16),
  nextTimerId: ref(0),
  now: ref(Unix.gettimeofday),
  focus: freshFocusState(),
  staticEmitted: Hashtbl.create(16),
  pendingStatic: ref([]),
  pendingRawOutput: ref([]),
  staticAllowed: true,
  componentBounds: Hashtbl.create(64),
  terminalBg: None,
  overlayLayers: [],
};

/* The instance currently in force. Replaced by Runtime on every start. */
let currentInstance: ref(instanceState) = ref(freshInstance());

/* The instance currently in force (shorthand used everywhere below). */
let instance = (): instanceState => currentInstance^;

/* Generate a new unique component ID for the current instance. */
let generateComponentId = (): Element.componentId => {
  let st = instance();
  let id = st.nextComponentId^;
  st.nextComponentId := id + 1;
  id;
};

/* Publish this frame's floating layers, TOPMOST FIRST. (internal)
 *
 * RUNTIME-ONLY: Runtime.compositeOverlays calls this exactly once per frame,
 * with [] when nothing is open - which is what makes a modal stop owning the
 * keyboard on the very frame it closes, rather than one frame later. */
let setOverlayLayers = (st: instanceState, layers: list(overlayLayer)): unit =>
  st.overlayLayers = layers;

/* The topmost floating layer, or None when nothing is open. (internal)
 *
 * Only the TOP layer captures: stack two modals and the lower one goes as
 * quiet as the base application, which is what "modal" means. */
let topOverlayLayer = (st: instanceState): option(overlayLayer) =>
  switch (st.overlayLayers) {
  | [top, ..._] => Some(top)
  | [] => None
  };

/* Is [id] a member of the topmost layer - i.e. was it rendered INSIDE the
 * overlay's child? True for everything when no layer is open, which is the
 * fast path every application without a modal stays on. (internal) */
let isCapturedBy = (layer: option(overlayLayer), id: Element.componentId): bool =>
  switch (layer) {
  | None => true
  | Some(l) => Hashtbl.mem(l.olMembers, id)
  };

/* Wake main loop function - set by Runtime to interrupt blocking I/O.
 * Deliberately NOT part of instanceState: it registers a process-level
 * callback (the wake pipe writer), and Runtime re-sets it on every start. */
let wakeMainLoopRef: ref(option(unit => unit)) = ref(None);

/* Called by Runtime to register the wake function */
let setWakeMainLoop = (f: unit => unit): unit => {
  wakeMainLoopRef := Some(f);
};

/* Wake the main loop - call after state changes from background threads */
let wakeMainLoop = (): unit => {
  switch (wakeMainLoopRef^) {
  | Some(f) => f()
  | None => ()
  };
};

/* ============================================================================
 * Static output plumbing (A4)
 *
 * The pieces live here because instanceState does, but the policy lives in
 * Runtime: Runtime's Static case calls recordPendingStatic during the render
 * walk, and Runtime.drainStaticLines empties both queues (raw output first)
 * and advances the watermarks, exactly once per frame.
 * ============================================================================ */

/* Turn committing-above-the-live-region on or off for the instance in force.
 * RUNTIME-ONLY (internal): Runtime.start calls this with `false` when it was
 * asked for the Fullscreen screen mode, immediately after installing the
 * fresh instance and before the first render. Nothing else should call it -
 * an application that flips it lies to <Static> about where its output goes.
 */
let setStaticAllowed = (st: instanceState, allowed: bool): unit =>
  st.staticAllowed = allowed;

/* Raise unless committing above the live region makes sense right now.
 * (internal) [what] names the feature in the message, since <Static> and
 * useStdout are two doors into the same dead end. */
let requireStaticAllowed = (st: instanceState, what: string): unit =>
  if (!st.staticAllowed) {
    raise(
      Invalid_argument(
        what
        ++ " cannot be used in Fullscreen screen mode: the alternate screen "
        ++ "has no scrollback to commit to. Render your transcript inside "
        ++ "the app (e.g. a <ScrollView>) - see the screenMode doc in "
        ++ "lib/Runtime.re.",
      ),
    );
  };

/* Queue this frame's lines for the <Static> node at [path], whose item list
 * is now [count] long. (internal)
 *
 * REPLACES any entry already recorded for the same path in this frame,
 * keeping its position in the list, so that a subtree rendered twice within
 * one frame (an HStack measures its children before it renders them) commits
 * its static items once, not twice. New paths append, which makes the drain
 * order the first-visit - that is, tree - order. */
let recordPendingStatic =
    (st: instanceState, path: string, count: int, lines: list(string))
    : unit => {
  let replaced = ref(false);
  let updated =
    List.map(
      ((p, c, l)) =>
        if (p == path) {
          replaced := true;
          (path, count, lines);
        } else {
          (p, c, l);
        },
      st.pendingStatic^,
    );
  st.pendingStatic :=
    (
      if (replaced^) {
        updated;
      } else {
        updated @ [(path, count, lines)];
      }
    );
};

/* Queue raw text to appear above the live region, as useStdout().write does.
 * (internal)
 *
 * The text is split into lines; ONE trailing empty line coming from a
 * trailing newline is dropped, so write("hello\n") and write("hello") both
 * commit exactly one line - a trailing "\n" is a line terminator, not an
 * extra blank line.
 *
 * Callable from anywhere, including a background thread: the queue is a
 * single ref updated with one assignment, and the root context is marked
 * dirty so the main loop actually wakes up and drains it (a wake alone would
 * not, since the loop only drains as part of rendering a frame). */
let queueRawOutput = (st: instanceState, text: string): unit => {
  /* Fullscreen has nowhere to put this - fail loudly rather than swallow it
   * (see requireStaticAllowed / Runtime's screenMode). */
  requireStaticAllowed(st, "useStdout");
  let parts = String.split_on_char('\n', text);
  let lines =
    switch (List.rev(parts)) {
    | ["", ...rest] when rest != [] => List.rev(rest)
    | _ => parts
    };
  st.pendingRawOutput := st.pendingRawOutput^ @ lines;
  switch (st.rootContext^) {
  | Some(rootCtx) => rootCtx.needsRerender = true
  | None => ()
  };
  wakeMainLoop();
};

/* Get the current render context (internal - fails if not in render) */
let getContext = () => {
  switch (instance().currentContext^) {
  | None => failwith("Hook called outside of render context")
  | Some(ctx) => ctx
  };
};

/* Build a setState closure for a state slot (internal).
 *
 * Bails out - no state write, no re-render - when the new value is
 * physically identical (===) to the current one: immediates (ints, bools,
 * None, chars) compare by value, heap values by pointer. This mirrors
 * React's Object.is bail-out and stops same-value writes (common in
 * effects, e.g. setLoading(false) when already false) from scheduling
 * useless frames. Freshly allocated equal values (a new Some(x), a rebuilt
 * string) do NOT bail out - callers who notify state upward every render
 * must do so from useEffect with stable deps, never from the render body,
 * or the app re-renders forever. */
let makeSetState =
    (
      ctx: renderContext,
      st: instanceState,
      stateRef: ref(Obj.t),
      newValue: 'a,
    )
    : unit => {
  let newRepr = Obj.repr(newValue);
  if (newRepr !== stateRef^) {
    stateRef := newRepr;
    /* Only mark this component's context as needing re-render */
    switch (ctx.componentId) {
    | Some(_id) =>
      /* Component context - mark this component and the root (the root
         flag is what the render loops actually poll) */
      ctx.needsRerender = true;
      switch (st.rootContext^) {
      | Some(rootCtx) => rootCtx.needsRerender = true
      | None => ()
      };
    | None =>
      /* Root context - mark for full re-render */
      ctx.needsRerender = true
    };
    /* Wake main loop to handle state change from background thread */
    wakeMainLoop();
  };
};

/* Create local state for a component.
 *
 * Returns a tuple of (currentValue, setValue). Calling setValue will
 * trigger a re-render with the new value. Setting a value physically
 * identical (===) to the current one is a no-op (see makeSetState).
 *
 * Rules:
 * - Must be called in the same order every render
 * - Don't call inside conditionals or loops
 * - The initial value is only used on first render
 * - Never call setValue unconditionally during render (e.g. to notify a
 *   parent) - that re-marks the tree dirty every frame and loops forever;
 *   use useEffect instead
 *
 * Example:
 *   let (count, setCount) = Hooks.useState(0);
 *   setCount(count + 1);  // Triggers re-render
 */
let useState = (initial: 'a): ('a, 'a => unit) => {
  let ctx = getContext();
  /* Capture the instance that owns this context: setState can be called long
     after the render (from a key handler, an effect or a background thread),
     possibly while another instance is installed. */
  let st = instance();
  let idx = ctx.hookIndex;
  ctx.hookIndex = idx + 1;

  if (idx >= Array.length(ctx.hooks)) {
    /* First render - initialize state */
    let stateRef = ref(Obj.repr(initial));
    ctx.hooks = Array.append(ctx.hooks, [|StateHook(stateRef)|]);
    (initial, makeSetState(ctx, st, stateRef));
  } else {
    /* Subsequent render - return existing state */
    switch (ctx.hooks[idx]) {
    | StateHook(stateRef) =>
      (Obj.magic(stateRef^), makeSetState(ctx, st, stateRef))
    | EffectHook(_, _) => failwith("Hook type mismatch: expected StateHook")
    | MemoHook(_) => failwith("Hook type mismatch: expected StateHook")
    };
  };
};

/* Create a value that survives re-renders without ever triggering one.
 *
 * useRef is a StateHook slot like useState, but the ref it returns is
 * allocated ONCE on first render and handed back unchanged on every later
 * render; mutating it (`r := x`) never marks the component dirty, unlike
 * setState. Used to stash "latest closure" values (see useInterval /
 * useTimeout below) and as an escape hatch for mutable state that should
 * not schedule a re-render.
 *
 * Example:
 *   let latest = Hooks.useRef(callback);
 *   latest := callback; // update every render, no re-render triggered
 */
let useRef = (initial: 'a): ref('a) => {
  let ctx = getContext();
  let idx = ctx.hookIndex;
  ctx.hookIndex = idx + 1;

  if (idx >= Array.length(ctx.hooks)) {
    /* First render - allocate the ref once and store it as the slot's value.
       Note: unlike useState, nothing here ever calls the state-setter path -
       the slot's own ref(Obj.t) cell is written exactly once, at creation. */
    let r: ref('a) = ref(initial);
    ctx.hooks = Array.append(ctx.hooks, [|StateHook(ref(Obj.repr(r)))|]);
    r;
  } else {
    switch (ctx.hooks[idx]) {
    | StateHook(stateRef) => (Obj.magic(stateRef^): ref('a))
    | EffectHook(_, _) =>
      failwith("Hook type mismatch: expected StateHook (useRef)")
    | MemoHook(_) =>
      failwith("Hook type mismatch: expected StateHook (useRef)")
    };
  };
};

/* Is this Obj.t a string block? (internal)
 *
 * Obj.string_tag is the runtime tag OCaml gives every string block; an
 * immediate (int/bool/char/constant constructor) is not a block at all, so
 * the is_block guard has to come first - Obj.tag on an immediate is
 * meaningless. */
let isString = (o: Obj.t): bool =>
  Obj.is_block(o) && Obj.tag(o) === Obj.string_tag;

/* Compare one dependency slot. (internal)
 *
 * Two immediates that are equal are already physically equal, so the
 * `===` below covers ints, bools, chars and constant constructors. The one
 * case it misses is a STRING: a fresh block every render, so a string
 * dependency never matched and every memo holding one recomputed forever.
 * Strings are safe to compare structurally - they cannot contain a closure,
 * so `compare` cannot raise, and they cannot be cyclic, so it cannot hang.
 * Nothing else is added for exactly those two reasons.
 *
 * In particular: do NOT wrap `compare` in a `try` to "handle" the rest. A
 * `try` catches the Invalid_argument("compare: functional value") a closure
 * raises, but a CYCLIC structure does not raise - it loops forever. That
 * turns a silently-slow memo into a hang, which is a strictly worse failure
 * mode than the one being fixed here. */
let depEqual = (a: Obj.t, b: Obj.t): bool =>
  a === b
  || isString(a)
  && isString(b)
  && String.equal((Obj.obj(a): string), (Obj.obj(b): string));

/** Compare dependency arrays for changes (internal) */
let depsEqual = (prev: option(array(Obj.t)), curr: array(Obj.t)): bool => {
  switch (prev) {
  | None => false /* First run, deps don't exist yet */
  | Some(prevDeps) =>
    if (Array.length(prevDeps) !== Array.length(curr)) {
      false;
    } else {
      let equal = ref(true);
      for (i in 0 to Array.length(prevDeps) - 1) {
        /* depEqual is physical equality plus the one string case - see its
           comment for why nothing else may be added here. */
        if (!depEqual(prevDeps[i], curr[i])) {
          equal := false;
        };
      };
      equal^;
    }
  };
};

/* Memoize a computed value until dependencies change.
 *
 * The function runs during render when dependencies differ from the
 * previous render. The returned value is cached in the hook state and
 * reused while dependencies are equal (physical equality via Obj.repr).
 *
 * Example:
 *   let expensive = Hooks.useMemo(() => doWork(data), [|data|]);
 */
let useMemo = (compute: unit => 'a, deps: array('b)): 'a => {
  let ctx = getContext();
  let idx = ctx.hookIndex;
  ctx.hookIndex = idx + 1;

  let depsObj = Array.map(Obj.repr, deps);

  let recalc = (memoRef: ref(option((Obj.t, array(Obj.t))))) => {
    let value = compute();
    memoRef := Some((Obj.repr(value), depsObj));
    value;
  };

  if (idx >= Array.length(ctx.hooks)) {
    /* First render - compute and store */
    let memoRef = ref(None);
    let value = recalc(memoRef);
    ctx.hooks = Array.append(ctx.hooks, [|MemoHook(memoRef)|]);
    value;
  } else {
    switch (ctx.hooks[idx]) {
    | MemoHook(memoRef) =>
      switch (memoRef^) {
      | Some((cachedValue, prevDeps)) =>
        if (depsEqual(Some(prevDeps), depsObj)) {
          Obj.magic(cachedValue);
        } else {
          recalc(memoRef);
        }
      | None => recalc(memoRef)
      }
    | StateHook(_) => failwith("Hook type mismatch: expected MemoHook")
    | EffectHook(_, _) => failwith("Hook type mismatch: expected MemoHook")
    };
  };
};

/* Run a side effect when dependencies change.
 *
 * The effect runs after render completes. If it returns a cleanup function,
 * that function is called before the next effect run or on unmount.
 *
 * The effect only runs when one of the dependencies changes (compared by
 * physical equality).
 *
 * Example:
 *   Hooks.useEffect(() => ..., [|id|]);
 *   Return Some(cleanupFn) to run cleanup before next effect.
 */
let useEffect =
    (effect: unit => option(unit => unit), deps: array('a)): unit => {
  let ctx = getContext();
  let idx = ctx.hookIndex;
  ctx.hookIndex = idx + 1;

  let depsObj = Array.map(Obj.repr, deps);

  if (idx >= Array.length(ctx.hooks)) {
    /* First render - create effect hook and schedule effect */
    let prevDepsRef = ref(None);
    let cleanupRef = ref(None);
    ctx.hooks =
      Array.append(ctx.hooks, [|EffectHook(prevDepsRef, cleanupRef)|]);

    ctx.pendingEffects = [
      {
        effect,
        cleanup: cleanupRef,
        deps: Some(depsObj),
        prevDeps: prevDepsRef,
      },
      ...ctx.pendingEffects,
    ];
  } else {
    /* Subsequent render - check if deps changed */
    switch (ctx.hooks[idx]) {
    | EffectHook(prevDepsRef, cleanupRef) =>
      if (!depsEqual(prevDepsRef^, depsObj)) {
        ctx.pendingEffects = [
          {
            effect,
            cleanup: cleanupRef,
            deps: Some(depsObj),
            prevDeps: prevDepsRef,
          },
          ...ctx.pendingEffects,
        ];
      }
    | StateHook(_) => failwith("Hook type mismatch: expected EffectHook")
    | MemoHook(_) => failwith("Hook type mismatch: expected EffectHook")
    };
  };
};

/* Run a side effect on every render.
 *
 * Unlike useEffect, this runs after every render regardless of any
 * dependencies. Use sparingly as it can impact performance.
 */
let useEffectAlways = (effect: unit => option(unit => unit)): unit => {
  let ctx = getContext();
  let idx = ctx.hookIndex;
  ctx.hookIndex = idx + 1;

  if (idx >= Array.length(ctx.hooks)) {
    let prevDepsRef = ref(None);
    let cleanupRef = ref(None);
    ctx.hooks =
      Array.append(ctx.hooks, [|EffectHook(prevDepsRef, cleanupRef)|]);

    ctx.pendingEffects = [
      {
        effect,
        cleanup: cleanupRef,
        deps: None,
        prevDeps: prevDepsRef,
      },
      ...ctx.pendingEffects,
    ];
  } else {
    switch (ctx.hooks[idx]) {
    | EffectHook(prevDepsRef, cleanupRef) =>
      ctx.pendingEffects = [
        {
          effect,
          cleanup: cleanupRef,
          deps: None,
          prevDeps: prevDepsRef,
        },
        ...ctx.pendingEffects,
      ]
    | StateHook(_) => failwith("Hook type mismatch: expected EffectHook")
    | MemoHook(_) => failwith("Hook type mismatch: expected EffectHook")
    };
  };
};

/* Register a keyboard event handler.
 *
 * The handler is called whenever a key is pressed while the app is running.
 * Handlers are cleared and re-registered on each render.
 *
 * Example:
 *   Hooks.useKeyDown((key, modifiers) =>
 *     switch (key) ...
 *   );
 */
let useKeyDown = (handler: (Key.t, Key.modifiers) => unit): unit => {
  let ctx = getContext();
  ctx.keyHandlers = [handler, ...ctx.keyHandlers];
};

/* Register a mouse event handler for the component that is rendering (B4).
 *
 * REGISTRATION-STYLE, like useKeyDown: it takes no hook slot, so it is safe
 * to call conditionally or several times in one render. Handlers are cleared
 * and re-registered every render.
 *
 * Only ONE component receives a given event - the innermost one whose last
 * painted box contains the pointer (see dispatchMouse below) - and it
 * receives coordinates REBASED to its own box: (0, 0) is the component's
 * top-left corner, whatever its position on screen. There is no bubbling in
 * this version, so an outer handler does not see a click that landed on an
 * inner one.
 *
 * A handler registered by the ROOT application component is the exception:
 * it always runs, for every event, with ABSOLUTE (live-region) coordinates -
 * the global escape hatch for applications that want to route events
 * themselves.
 *
 * Mouse reporting is enabled automatically while any handler is registered,
 * and turned off again when the last one goes away, so an application that
 * never calls this never puts the terminal into mouse mode.
 *
 * ~wheel (default true) declares whether this component is a wheel target:
 * pass ~wheel=false and ScrollUp/ScrollDown events pass THROUGH it to the
 * innermost enclosing component that does want them (a <ScrollView>, say)
 * instead of stopping here. <Clickable> without ~onMouseDown does exactly
 * that, which is why a list of clickable rows still wheel-scrolls.
 *
 * ~click (default true) is the mirror, for Down/Up/Move. Pass ~click=false
 * to receive the wheel without claiming clicks, which is what <ScrollView>
 * does: it scrolls on a notch and is transparent to everything else, so a
 * click on a list lands on whatever the application put there - or, in
 * ~rows mode where there are no child elements at all, on the application
 * itself. Declaring interest you do not act on is how a component swallows
 * events silently.
 *
 * Example:
 *   Hooks.useMouse(ev =>
 *     switch (ev.Mouse.kind, ev.Mouse.button) {
 *     | (Mouse.Down, Mouse.Left) => select(ev.Mouse.y)
 *     | _ => ()
 *     }
 *   );
 */
let useMouse =
    (~wheel: bool=true, ~click: bool=true, handler: Mouse.event => unit): unit => {
  let ctx = getContext();
  ctx.mouseHandlers = [handler, ...ctx.mouseHandlers];
  if (wheel) {
    ctx.wheelInterest = true;
  };
  if (click) {
    ctx.clickInterest = true;
  };
};

/* ============================================================================
 * Focus (B1): useFocus, useFocusManager, useInput
 *
 * useFocus/useInput are REGISTRATION-STYLE hooks, like useKeyDown: they push
 * onto a plain list every render rather than occupying a numbered hook slot
 * (StateHook/EffectHook/MemoHook), so - unlike useState/useEffect/useMemo -
 * they are safe to call conditionally or a variable number of times per
 * render without desyncing later hooks in the same component.
 * ============================================================================ */

/* Result of useFocus: whether THIS call's id is the currently focused one. */
type focusInfo = {isFocused: bool};

/* Imperative handle returned by useFocusManager - see its doc comment
 * below for what each function does and when it marks a re-render. */
type focusManager = {
  enableFocus: unit => unit,
  disableFocus: unit => unit,
  focusNext: unit => unit,
  focusPrevious: unit => unit,
  focus: string => unit,
  /* Like [focus], but able to say "nothing" (B2). <Modal> restores whatever
     held focus before it opened, and that may legitimately have been None -
     an application where nothing was focused yet must come back to nothing
     focused, not to the first entry of the ring. Same no-op-is-free gating
     as [focus]: assigning the value it already has costs no frame. */
  setFocused: option(string) => unit,
};

/* Map a focusable's owner to a Hashtbl-friendly key: -1 for the root (None -
 * at most one root, so it needs no real componentId), otherwise the owning
 * component's ID. Real component IDs start at 0 (see generateComponentId),
 * so -1 can never collide with one. */
let focusOwnerKey = (fOwner: option(Element.componentId)): int =>
  switch (fOwner) {
  | None => (-1)
  | Some(id) => id
  };

/* Pick the next (dir=1) or previous (dir=-1) active focusable's id, relative
 * to focus.focusedId, wrapping around focus.order (the last COMMITTED tree
 * order - see commitFocus). No active focusables -> no-op (returns the
 * current focusedId unchanged, per B1's "focusNext with zero focusables is a
 * no-op"). Shared by dispatchKey's Tab handling and useFocusManager's
 * focusNext/focusPrevious (internal). */
let cycleFocus = (focus: focusState, ~dir: int): option(string) => {
  let actives =
    focus.order |> Array.to_list |> List.filter(f => f.fIsActive) |> Array.of_list;
  let n = Array.length(actives);
  if (n == 0) {
    focus.focusedId;
  } else {
    let curIdx =
      switch (focus.focusedId) {
      | None => (-1)
      | Some(fid) =>
        let found = ref(-1);
        Array.iteri(
          (i, f) =>
            if (found^ == (-1) && f.fid == fid) {
              found := i;
            },
          actives,
        );
        found^;
      };
    let nextIdx =
      if (curIdx == (-1)) {
        dir >= 0 ? 0 : n - 1;
      } else {
        ((curIdx + dir) mod n + n) mod n;
      };
    Some(actives[nextIdx].fid);
  };
};

/* Register this component as focusable for the current frame.
 *
 * ~id defaults to a per-component id derived from ctx.componentId, so it is
 * stable across re-renders without the caller having to invent one:
 * "__focus_" ++ string_of_int(id) for a normal component, or the fixed
 * string "__focus_root" for the root (ctx.componentId is None there, and at
 * most one root exists). Passing an explicit ~id lets several call sites
 * that want to be treated as ONE focusable (a controlled multi-part widget)
 * share one; ids must be unique across owners in that case - there is no
 * runtime check.
 *
 * Calling useFocus more than once in the same component's render - or
 * across the double visit an HStack gives an Auto-sized child (measure pass
 * + real pass) - registers more than one entry for the SAME owner;
 * commitFocus's dedupe (step 1) keeps only the first, in render order.
 *
 * Safe to call conditionally (see the section comment above) - it does not
 * consume a hook slot.
 */
let useFocus =
    (~autoFocus=false, ~isActive=true, ~id: option(string)=?, ())
    : focusInfo => {
  let ctx = getContext();
  let st = instance();
  let fOwner = ctx.componentId;
  let fid =
    switch (id) {
    | Some(explicit) => explicit
    | None =>
      switch (fOwner) {
      | Some(cid) => "__focus_" ++ string_of_int(cid)
      | None => "__focus_root"
      }
    };
  st.focus.registrations = [
    {fid, fOwner, fIsActive: isActive, fAutoFocus: autoFocus},
    ...st.focus.registrations,
  ];
  {isFocused: st.focus.focusedId == Some(fid)};
};

/* Imperative focus control: enable/disable Tab handling, and move focus
 * programmatically. Every function here captures `st = instance()` at the
 * moment useFocusManager() is called (the makeSetState pattern - see its
 * doc comment above), because these functions are typically invoked later,
 * from a key handler, not from inside the render that created them.
 *
 * focusedId is only reassigned - and the root marked dirty - when the new
 * value actually differs from the current one, exactly like makeSetState's
 * physical-identity bail-out: harmless no-op calls (focusNext with a single
 * focusable already focused, disableFocus called twice) must not schedule a
 * useless frame.
 */
let useFocusManager = (): focusManager => {
  /* getContext() is called only to enforce "hooks run during render",
     consistent with every other hook - none of the returned functions closes
     over the render context itself, only over the instance. */
  ignore(getContext());
  let st = instance();
  let setFocused = (newId: option(string)): unit =>
    if (st.focus.focusedId != newId) {
      st.focus.focusedId = newId;
      switch (st.rootContext^) {
      | Some(rootCtx) => rootCtx.needsRerender = true
      | None => ()
      };
      wakeMainLoop();
    };
  {
    enableFocus: () => st.focus.enabled = true,
    disableFocus: () => st.focus.enabled = false,
    focusNext: () => setFocused(cycleFocus(st.focus, ~dir=1)),
    focusPrevious: () => setFocused(cycleFocus(st.focus, ~dir=-1)),
    /* Not validated against the registered focusables (matches Ink):
       focusing an id that does not exist, or that belongs to an inactive
       focusable, is allowed - the id simply is not visibly focused (no
       inverted marker) until a matching active useFocus shows up. */
    focus: id => setFocused(Some(id)),
    setFocused,
  };
};

/* Register a key handler that only fires while ~isActive is true, AND only
 * while this component is not suppressed by a floating layer.
 *
 * `Hooks.useInput(~isActive=isFocused, handler)` is the idiom pairing this
 * with useFocus: only the currently-focused item's handler runs. ~isActive
 * defaults to true.
 *
 * CAPTURE (B2) is the difference between this and useKeyDown, and it is the
 * reason the two are separate lists:
 *
 * - useInput fires only for the members of the TOPMOST open overlay - the
 *   components rendered inside it. With nothing open, everything is a
 *   member and this behaves exactly as it always did. So a <ScrollView>
 *   under a modal goes quiet for free (it is just useFocus + useInput), and
 *   a dialog's Esc binding closes the top dialog rather than all of them.
 * - useKeyDown ALWAYS fires. That is the deliberate escape hatch, and it is
 *   what keeps an application QUITTABLE: raw mode disables ISIG, so Ctrl+C
 *   is an ordinary keypress, and an app that cannot receive it while a modal
 *   is open cannot be exited at all. Bind Ctrl+C (and any other global) with
 *   useKeyDown, never with useInput.
 *
 * The filtering itself happens once per frame in collectKeyHandlers, not
 * here: whether a layer is open is not known until the whole tree has
 * rendered.
 */
let useInput = (~isActive=true, handler: (Key.t, Key.modifiers) => unit): unit =>
  if (isActive) {
    let ctx = getContext();
    ctx.inputHandlers = [handler, ...ctx.inputHandlers];
  };

/* Dispatch a key event.
 *
 * Tab is CONSUMED (never reaches keyHandlers) when focus.enabled and at
 * least one registered focusable is active, and neither Ctrl nor Alt is
 * held (Shift selects direction, not consumption - Shift+Tab still cycles).
 * Consuming means focusNext/focusPrevious runs and the root is marked dirty
 * INSTEAD OF broadcasting - otherwise a focused TextArea would both insert a
 * literal tab character and lose focus on the same keypress. Back-compat:
 * an application that never calls useFocus has an empty focus.order, so
 * Tab always falls through to the broadcast branch, unchanged from before
 * B1.
 *
 * Every other key (including Tab when there is nothing focusable, or focus
 * is disabled, or Ctrl/Alt is held) broadcasts to every handler in
 * collectKeyHandlers order (root handlers first, then each component's, in
 * tree order) exactly as before - this is a pure refactor of what used to
 * be three copy-pasted `List.iter(handler => handler(key, modifiers),
 * ctx.keyHandlers)` call sites in Runtime (the two main loops and the
 * headless handle's sendKey).
 */
let dispatchKey =
    (rootCtx: renderContext, key: Key.t, mods: Key.modifiers): unit => {
  let st = instance();
  let focus = st.focus;
  let consumesTab =
    switch (key) {
    | Key.Tab =>
      focus.enabled
      && !mods.ctrl
      && !mods.alt
      && Array.exists(f => f.fIsActive, focus.order)
    | _ => false
    };
  if (consumesTab) {
    let dir = mods.shift ? (-1) : 1;
    focus.focusedId = cycleFocus(focus, ~dir);
    rootCtx.needsRerender = true;
  } else {
    /* Both collected lists, useKeyDown's first (B2). Recorded ordering
       change: every useKeyDown in the tree now runs before every useInput,
       where previously the two interleaved in tree order. Harmless here -
       there is no stopPropagation, so a handler cannot prevent a later one
       from running, and no application in this repo depends on a useInput
       running before a useKeyDown. */
    List.iter(handler => handler(key, mods), rootCtx.keyHandlers);
    List.iter(handler => handler(key, mods), rootCtx.inputHandlers);
  };
};

/* Get a function to quit the application.
 *
 * Returns a function that, when called, will stop the main loop.
 * The quitBehavior argument controls whether the terminal is cleared.
 *
 * Example:
 *   let quit = Hooks.useQuit();
 *   quit(ClearScreen);    // Exit and clear terminal
 *   quit(PreserveScreen); // Exit but keep output visible
 */
let useQuit = (): (quitBehavior => unit) => {
  let ctx = getContext();
  ctx.quit;
};

/* ============================================================================
 * Terminal background color (OSC 11)
 * ============================================================================ */

/* Record a terminal background reply on [st]. Returns true only when the
 * value actually CHANGED, so the caller can mark the root dirty on that and
 * nothing else - a terminal that re-answers with the color we already knew
 * must not cost a frame. Same discipline as commitFocus's gated re-render. */
let setTerminalBackground =
    (st: instanceState, rgb: (int, int, int)): bool =>
  if (st.terminalBg == Some(rgb)) {
    false;
  } else {
    st.terminalBg = Some(rgb);
    true;
  };

/* The terminal's background color as (r, g, b), 0..255 per channel, or None.
 *
 * Registration-style: no hook slot, no dependency array - it just reads the
 * running application's state, so it is safe to call conditionally and from
 * any depth.
 *
 * None until the terminal answers Runtime's startup OSC 11 query - AND
 * POSSIBLY FOREVER: plenty of terminals (and every pipe, CI job and headless
 * run) never reply. Applications must therefore have a None branch; the
 * usual choice is "assume dark", which is what most terminals are.
 *
 * When a reply does arrive mid-session the runtime marks the root dirty, so
 * the application re-renders exactly once with the new value.
 *
 * Example:
 *   let isLight =
 *     switch (Hooks.useTerminalBackground()) {
 *     | Some((r, g, b)) => r + g + b > 382
 *     | None => false
 *     };
 */
let useTerminalBackground = (): option((int, int, int)) =>
  instance().terminalBg;

/* Handle returned by useStdout: an escape hatch for writing plain text above
 * the live region, without going through <Static>. */
type stdoutHandle = {write: string => unit};

/* Write text above the live region, Ink-style.
 *
 * `write(text)` queues text to be committed above the live region on the
 * next frame - the same place <Static> items land, and through the same
 * drain, so a write and a Static commit made in the same frame keep their
 * relative order (raw writes first). The live frame itself is untouched: it
 * is repainted below whatever was committed.
 *
 * The handle is stable across renders, and `write` may be called from
 * anywhere: the render body, an effect, a key handler, or a background
 * thread. It marks the application dirty and wakes the main loop, so the
 * text appears on the very next frame even if nothing else changed.
 *
 * This is the escape hatch, not the main road: prefer <Static> for anything
 * that is a list of rendered items, since it gets Matcha's layout, styling
 * and its append-only bookkeeping. Reach for useStdout when you have text
 * (a log line, a subprocess's output) rather than elements.
 *
 * INLINE ONLY. `write` RAISES Invalid_argument under Runtime's Fullscreen
 * screen mode: the alternate screen has no scrollback, so "above the live
 * region" does not exist there. Getting the handle is always fine; writing
 * with it is what fails, and it fails on the first attempt rather than
 * dropping the text.
 *
 * Example:
 *   let stdout = Hooks.useStdout();
 *   Hooks.useEffect(() => { stdout.write("started"); None }, [||]);
 */
let useStdout = (): stdoutHandle => {
  let st = instance();
  useMemo(() => {write: text => queueRawOutput(st, text)}, [|st|]);
};

/* ============================================================================
 * Timers: useInterval, useTimeout (A3)
 *
 * Decision: event-handler semantics. Timer callbacks run from the main loop
 * like key handlers - outside of render - so setState inside a callback just
 * marks needsRerender and the loop renders once, same as any other input
 * event. There is no new hook-slot kind: useInterval/useTimeout are built
 * entirely out of useRef + useEffect.
 * ============================================================================ */

/* Register a timer (internal - useInterval/useTimeout are the public API).
 * `ms` is milliseconds from now; intervalMs=Some(ms) repeats every ms, None
 * fires once. Returns the timer id, used to cancel it. */
let registerTimer =
    (~intervalMs: option(int), ~ms: int, callbackRef: ref(unit => unit))
    : int => {
  let st = instance();
  let id = st.nextTimerId^;
  st.nextTimerId := id + 1;
  let deadline = st.now^() +. float_of_int(ms) /. 1000.0;
  Hashtbl.replace(
    st.timers,
    id,
    {timerId: id, deadline, intervalMs, callbackRef},
  );
  id;
};

/* Cancel a timer (internal). Resilient to the timer already being gone -
 * fired one-shot, already cancelled, or the instance torn down - since
 * Hashtbl.remove is a no-op when the key is absent. */
let cancelTimer = (id: int): unit => Hashtbl.remove(instance().timers, id);

/* Seconds until the earliest timer deadline, clamped to [0, cap]; cap itself
 * when there are no timers. Runtime.re feeds this straight into Unix.select's
 * timeout so the loop wakes up exactly when a timer is due, without busy
 * waiting and without delaying keyboard input beyond `cap` seconds. */
let nextTimerTimeout = (~cap: float): float => {
  let st = instance();
  let now = st.now^();
  Hashtbl.fold(
    (_id, t: timer, acc) => {
      let remaining = t.deadline -. now;
      let clamped =
        if (remaining < 0.0) {
          0.0;
        } else if (remaining > cap) {
          cap;
        } else {
          remaining;
        };
      if (clamped < acc) {clamped} else {acc};
    },
    st.timers,
    cap,
  );
};

/* Fire every timer whose deadline is due (<= now). Called once per loop
 * iteration, after input handling.
 *
 * - Snapshots the due set BEFORE invoking any callback, so a callback that
 *   registers a new timer does not fire within this same batch.
 * - One-shot timers are removed from the table BEFORE their callback runs.
 * - Repeating timers advance their deadline past `now` in one step (missed
 *   ticks coalesce into a single callback per timer per call) - the deadline
 *   catches up, but the callback fires at most once here.
 * - Timers due at the same deadline fire in registration order (timerId
 *   order).
 * - A timer cancelled by an earlier callback in this same batch (including
 *   cancelling itself, or a sibling due at the same instant) is skipped.
 *
 * Returns whether any timer fired. */
let fireDueTimers = (): bool => {
  let st = instance();
  let now = st.now^();
  let due =
    Hashtbl.fold(
      (_id, t: timer, acc) => t.deadline <= now ? [t, ...acc] : acc,
      st.timers,
      [],
    )
    |> List.sort((a: timer, b: timer) => compare(a.timerId, b.timerId));
  List.iter(
    (t: timer) =>
      if (Hashtbl.mem(st.timers, t.timerId)) {
        switch (t.intervalMs) {
        | None =>
          Hashtbl.remove(st.timers, t.timerId);
          t.callbackRef^();
        | Some(intervalMs) =>
          let intervalSec = float_of_int(intervalMs) /. 1000.0;
          while (t.deadline <= now) {
            t.deadline = t.deadline +. intervalSec;
          };
          t.callbackRef^();
        };
      },
    due,
  );
  due != [];
};

/* Run `callback` every `ms` milliseconds while mounted.
 *
 * ms<=0 disables the timer (the React `delay=null` idiom - matches Ink's
 * useInterval). Changing `ms` resets the cadence: the old timer is cancelled
 * and a fresh one registered `ms` milliseconds from that moment. The latest
 * `callback` is always the one invoked, even though the timer itself is only
 * re-registered when `ms` changes - see the useRef "latest closure" pattern.
 *
 * Example:
 *   Hooks.useInterval(() => setCount(count + 1), ~ms=1000);
 */
let useInterval = (callback: unit => unit, ~ms: int): unit => {
  let cbRef = useRef(callback);
  cbRef := callback;
  useEffect(
    () =>
      if (ms <= 0) {
        None;
      } else {
        let id = registerTimer(~intervalMs=Some(ms), ~ms, cbRef);
        wakeMainLoop();
        Some(() => cancelTimer(id));
      },
    [|ms|],
  );
};

/* Run `callback` once, `ms` milliseconds after mount (or after `ms`
 * changes). ms<=0 disables the timer.
 *
 * Example:
 *   Hooks.useTimeout(() => setVisible(false), ~ms=3000);
 */
let useTimeout = (callback: unit => unit, ~ms: int): unit => {
  let cbRef = useRef(callback);
  cbRef := callback;
  useEffect(
    () =>
      if (ms <= 0) {
        None;
      } else {
        let id = registerTimer(~intervalMs=None, ~ms, cbRef);
        wakeMainLoop();
        Some(() => cancelTimer(id));
      },
    [|ms|],
  );
};

/* ============================================================================
 * Internal Runtime Functions
 * These are used by Runtime.re to manage the render lifecycle.
 * ============================================================================ */

/* Create a fresh render context for a component.
 * Called once when the app starts. (internal)
 */
let createContext = (quit: quitBehavior => unit): renderContext => {
  {
    hookIndex: 0,
    hooks: [||],
    keyHandlers: [],
    inputHandlers: [],
    mouseHandlers: [],
    wheelInterest: false,
    clickInterest: false,
    pendingEffects: [],
    needsRerender: true,
    componentId: None, /* Root context has no component ID */
    quit,
  };
};

/* Create a render context for a component instance.
 * Called when a component is first rendered. (internal)
 */
let createComponentContext =
    (componentId: Element.componentId, quit: quitBehavior => unit)
    : renderContext => {
  let ctx = {
    hookIndex: 0,
    hooks: [||],
    keyHandlers: [],
    inputHandlers: [],
    mouseHandlers: [],
    wheelInterest: false,
    clickInterest: false,
    pendingEffects: [],
    needsRerender: true,
    componentId: Some(componentId),
    quit,
  };
  Hashtbl.add(instance().componentContexts, componentId, ctx);
  ctx;
};

/* Get the render context for a component instance. (internal) */
let getComponentContext =
    (componentId: Element.componentId): option(renderContext) =>
  Hashtbl.find_opt(instance().componentContexts, componentId);

/* Prepare context for a new render pass.
 * Resets hook index and clears transient state. (internal)
 */
let beginRender = (ctx: renderContext): unit => {
  ctx.hookIndex = 0;
  ctx.keyHandlers = [];
  ctx.inputHandlers = [];
  ctx.mouseHandlers = [];
  ctx.wheelInterest = false;
  ctx.clickInterest = false;
  ctx.pendingEffects = [];
};

/* Collect key handlers from all component contexts into the root context.
 * Handlers dispatch in deterministic order: the root's own handlers first
 * (in registration order), then each component's handlers in tree
 * (traversal) order. orderedComponentIds is the render-pass traversal
 * order; components visited more than once in a pass (e.g. HStack
 * measure + render passes) are deduplicated to their first occurrence.
 *
 * TWO LISTS, TWO RULES (B2). useKeyDown's handlers are collected from every
 * component, unconditionally - that is the global escape hatch, and what
 * keeps Ctrl+C working under a modal. useInput's are collected only from the
 * topmost open layer's MEMBERS, and the root context's own useInput is
 * dropped entirely while a layer is open (the root is base, not a member -
 * see the Overlay case in lib/Runtime.re for what membership means). With no
 * layer open both lists are collected from everything, which is exactly the
 * behaviour that existed before overlays.
 */
let collectKeyHandlers =
    (rootCtx: renderContext, orderedComponentIds: list(Element.componentId))
    : unit => {
  let layer = topOverlayLayer(instance());
  /* Handlers are prepended at registration time - reverse to restore order */
  let rootHandlers = List.rev(rootCtx.keyHandlers);
  let rootInput =
    switch (layer) {
    | None => List.rev(rootCtx.inputHandlers)
    | Some(_) => []
    };
  let seen: Hashtbl.t(Element.componentId, unit) = Hashtbl.create(16);
  let ordered =
    orderedComponentIds
    |> List.filter(id =>
         if (Hashtbl.mem(seen, id)) {
           false;
         } else {
           Hashtbl.add(seen, id, ());
           true;
         }
       );
  let componentHandlers =
    ordered
    |> List.concat_map(id =>
         switch (getComponentContext(id)) {
         | Some(componentCtx) => List.rev(componentCtx.keyHandlers)
         | None => []
         }
       );
  let componentInput =
    ordered
    |> List.filter(id => isCapturedBy(layer, id))
    |> List.concat_map(id =>
         switch (getComponentContext(id)) {
         | Some(componentCtx) => List.rev(componentCtx.inputHandlers)
         | None => []
         }
       );
  rootCtx.keyHandlers = rootHandlers @ componentHandlers;
  rootCtx.inputHandlers = rootInput @ componentInput;
};

/* ============================================================================
 * Mouse dispatch (B4)
 * ============================================================================ */

/* Does anything currently want mouse events?
 *
 * True when the root context has a useMouse handler, or when any component
 * rendered by the last frame does. The interactive loop calls this after
 * every commit and enables or disables terminal mouse reporting on the
 * transition, so mouse mode follows what the UI actually needs and an
 * application that never calls useMouse never turns it on. (internal)
 */
let hasMouseHandlers = (): bool => {
  let st = instance();
  let nonEmpty = (ctx: renderContext) =>
    switch (ctx.mouseHandlers) {
    | [] => false
    | _ => true
    };
  let rootWants =
    switch (st.rootContext^) {
    | Some(rootCtx) => nonEmpty(rootCtx)
    | None => false
    };
  rootWants
  || List.exists(
       id =>
         switch (getComponentContext(id)) {
         | Some(ctx) => nonEmpty(ctx)
         | None => false
         },
       st.renderedComponentIds^,
     );
};

/* Dispatch one mouse event, in live-region coordinates (0-based, relative to
 * the top-left of the frame - Runtime maps terminal rows into this space).
 *
 * SINGLE TARGET, INNERMOST WINS. The rendered components are walked in tree
 * order and the LAST one that both has handlers and whose last painted box
 * contains the pointer is picked: a descendant is always visited after its
 * ancestor, so "last containing" is "innermost". Overlapping siblings resolve
 * the same way - last in tree order wins. There is no bubbling and no
 * stopPropagation in this version (both stay compatible future extensions:
 * dispatch would walk the containing chain outward instead of picking one).
 *
 * The chosen component's handlers get coordinates REBASED to its own box, so
 * a component can reason in local coordinates without knowing where it was
 * laid out. The root context's handlers then always run, with the ABSOLUTE
 * event - a global escape hatch, and the only thing a click outside every
 * handler-bearing component reaches.
 *
 * A component whose handlers were only registered during a MEASURING pass has
 * no recorded box (measuring records none) and is therefore unreachable -
 * which is correct: it is not on screen. Boxes come from the last painted
 * frame, so they stay right across resizes without any invalidation.
 * (internal)
 */
let dispatchMouse = (rootCtx: renderContext, ev: Mouse.event): unit => {
  let st = instance();
  let layer = topOverlayLayer(st);
  /* OUTSIDE CLICK DISMISS (B2). While a layer is open, a Down that lands
     outside its box runs ovOnDismiss and is SWALLOWED WHOLE - including the
     root fan-out, which is the one deliberate exception to "root handlers
     always run". A click that dismisses a dialog must not also press the
     button it happened to land on underneath. Only Down: a wheel notch or a
     drag outside the dialog is not a dismissal gesture, so those fall
     through to the normal routing below (where the member filter still
     applies, so they simply find no target inside the layer). */
  let dismissed =
    switch (layer, ev.Mouse.kind) {
    | (Some(l), Mouse.Down) when !Mouse.contains(l.olBox, ev.Mouse.x, ev.Mouse.y) =>
      switch (l.olOnDismiss) {
      | Some(f) => f()
      | None => ()
      };
      true;
    | _ => false
    };
  if (dismissed) {
    ();
  } else {
  /* A wheel event only targets a context that declared wheel interest
     (useMouse's ~wheel, true by default; <Clickable> without ~onMouseDown
     opts out) - anything else is transparent to it, so a notch over a
     clickable row inside a <ScrollView> still scrolls the list. */
  let isWheel =
    switch (ev.Mouse.kind) {
    | Mouse.ScrollUp
    | Mouse.ScrollDown => true
    | _ => false
    };
  /* renderedComponentIds is most-recent-first; reverse for tree order, and
     dedupe (a component visited twice in one frame appears twice) keeping the
     first occurrence, exactly like collectKeyHandlers. */
  let seen: Hashtbl.t(Element.componentId, unit) = Hashtbl.create(16);
  let target: ref(option((renderContext, Mouse.rect))) = ref(None);
  List.iter(
    id =>
      /* While a layer is open, only its members are hit-testable (B2), so a
         click inside the dialog can never reach the component that happens
         to be painted at the same coordinates underneath it. */
      if (!Hashtbl.mem(seen, id) && isCapturedBy(layer, id)) {
        Hashtbl.add(seen, id, ());
        switch (getComponentContext(id), Hashtbl.find_opt(st.componentBounds, id)) {
        | (Some(ctx), Some(rect)) =>
          switch (ctx.mouseHandlers) {
          | [] => ()
          | _ =>
            if ((isWheel ? ctx.wheelInterest : ctx.clickInterest)
                && Mouse.contains(rect, ev.Mouse.x, ev.Mouse.y)) {
              target := Some((ctx, rect));
            }
          }
        | _ => ()
        };
      },
    List.rev(st.renderedComponentIds^),
  );
  switch (target^) {
  | Some((ctx, rect)) =>
    let local = {...ev, Mouse.x: ev.Mouse.x - rect.Mouse.rx, y: ev.Mouse.y - rect.Mouse.ry};
    /* Handlers are prepended at registration time - reverse for order. */
    List.iter(handler => handler(local), List.rev(ctx.mouseHandlers));
  | None => ()
  };
  List.iter(handler => handler(ev), List.rev(rootCtx.mouseHandlers));
  };
};

/* Clear this frame's focus registrations, ready for a fresh render pass.
 * Called by Runtime.resetComponentTracking at the START of every render (like
 * renderedComponentIds), so useFocus calls during THIS render begin from an
 * empty list; commitFocus (below) consumes them into `order` right after the
 * render finishes, and leaves the (by-then-stale) list alone until the next
 * reset. (internal)
 */
let resetFocusRegistrations = (): unit => {
  instance().focus.registrations = [];
};

/* Commit this frame's focus registrations into committed state. Called by
 * all three render loops (the two main loops and startHeadless's doRender)
 * immediately after collectKeyHandlers.
 *
 * 1. Reverse registrations (prepended, like keyHandlers) to render order,
 *    then dedupe by owner - keeping the FIRST occurrence - into `order`.
 *    This is also what makes a component visited twice in one frame (an
 *    HStack's Auto-measure pass, then its real pass) register only once.
 * 2. If the focused id is gone (deregistered) or now inactive, focus the
 *    active entry that now sits at the same position the old focused entry
 *    held among LAST FRAME's active entries (clamped to the new active
 *    count) - a predictable "successor at the same index", rather than
 *    Ink's drop-to-none. Needs last frame's order, so this reads the OLD
 *    `focus.order` (captured as `prevOrder` before step 1 overwrites it).
 * 3. Only if focus is still None after step 2: autoFocus picks the first
 *    tree-order entry with fAutoFocus whose owner was NOT in prevOwners
 *    (i.e. newly mounted this frame).
 * 4. If focusedId changed (steps 2/3, or a focusManager call queued between
 *    commits), mark the root dirty so focus shows on the NEXT frame
 *    (React-like). Gated on an actual change: commitFocus runs every frame
 *    regardless of input, so an unconditional flag here would re-render
 *    forever.
 * 5. Refresh prevOwners to this frame's owner set, for step 3 next time.
 *    (Registrations themselves are cleared by resetFocusRegistrations, at
 *    the start of the NEXT render, not here.)
 * (internal)
 */
let commitFocus = (st: instanceState, rootCtx: renderContext): unit => {
  let focus = st.focus;
  let prevOrder = focus.order;
  let prevFocusedId = focus.focusedId;

  /* Step 1 - membership filter first (B2), then the dedupe.
   *
   * While a layer is open, `order` holds ONLY that layer's focusables, and
   * containment then falls out of machinery that already exists: cycleFocus
   * walks `order`, dispatchKey's Tab guard reads `order`, and step 2's
   * min(oldIdx, numActive - 1) clamp lands focus inside the dialog even
   * without ~autoFocus. The root context's own useFocus (fOwner = None) is
   * base, not a member, so it drops out too.
   *
   * What does NOT fall out is RESTORE: when the layer closes, `order` is the
   * full ring again and step 2 would hand focus to whatever now sits at the
   * old index, not to whatever had it before the dialog opened. That is
   * <Modal>'s job - it records focusedId on mount and puts it back in its
   * unmount cleanup, which runs strictly before this function. */
  let layer = topOverlayLayer(st);
  let seenOwners: Hashtbl.t(int, unit) = Hashtbl.create(16);
  let deduped =
    focus.registrations
    |> List.rev
    |> List.filter(f =>
         switch (layer) {
         | None => true
         | Some(l) =>
           switch (f.fOwner) {
           | None => false
           | Some(cid) => Hashtbl.mem(l.olMembers, cid)
           }
         }
       )
    |> List.filter(f => {
         let key = focusOwnerKey(f.fOwner);
         if (Hashtbl.mem(seenOwners, key)) {
           false;
         } else {
           Hashtbl.add(seenOwners, key, ());
           true;
         };
       });
  let newOrder = Array.of_list(deduped);
  focus.order = newOrder;
  let newActives =
    newOrder |> Array.to_list |> List.filter(f => f.fIsActive) |> Array.of_list;

  /* Step 2 */
  let stillActive =
    switch (focus.focusedId) {
    | None => false
    | Some(fid) => Array.exists(f => f.fid == fid, newActives)
    };
  if (!stillActive) {
    let prevActives =
      prevOrder
      |> Array.to_list
      |> List.filter(f => f.fIsActive)
      |> Array.of_list;
    let oldIdx =
      switch (focus.focusedId) {
      | None => 0
      | Some(fid) =>
        let found = ref(-1);
        Array.iteri(
          (i, f) =>
            if (found^ == (-1) && f.fid == fid) {
              found := i;
            },
          prevActives,
        );
        found^ == (-1) ? 0 : found^;
      };
    let numActive = Array.length(newActives);
    focus.focusedId =
      numActive == 0
        ? None : Some(newActives[min(oldIdx, numActive - 1)].fid);
  };

  /* Step 3 */
  switch (focus.focusedId) {
  | Some(_) => ()
  | None =>
    let candidate =
      newOrder
      |> Array.to_list
      |> List.find_opt(f =>
           f.fAutoFocus
           && !Hashtbl.mem(focus.prevOwners, focusOwnerKey(f.fOwner))
         );
    switch (candidate) {
    | Some(f) => focus.focusedId = Some(f.fid)
    | None => ()
    };
  };

  /* Step 4 */
  if (focus.focusedId != prevFocusedId) {
    rootCtx.needsRerender = true;
  };

  /* Step 5 */
  Hashtbl.reset(focus.prevOwners);
  Array.iter(
    f => Hashtbl.replace(focus.prevOwners, focusOwnerKey(f.fOwner), ()),
    newOrder,
  );
};

/* Run pending effects after render completes.
 * Effects run in declaration order. Cleanup from previous
 * render is called before running the new effect. (internal)
 */
let runEffects = (ctx: renderContext): unit => {
  let effects = List.rev(ctx.pendingEffects);
  /* Clear first: an effect that renders (or schedules) must not see, or lose,
     the effects of this pass. */
  ctx.pendingEffects = [];
  List.iter(
    ({effect, cleanup, deps, prevDeps}) => {
      switch (cleanup^) {
      | Some(cleanupFn) => cleanupFn()
      | None => ()
      };
      cleanup := effect();
      /* Commit the dependencies only now that the effect actually ran. */
      switch (deps) {
      | Some(_) => prevDeps := deps
      | None => ()
      };
    },
    effects,
  );
};

/* Queue a component context for the commit phase.
 * Called by the runtime once the component body has rendered. (internal)
 */
let enqueueEffects = (ctx: renderContext): unit => {
  let queue = instance().effectCommitQueue;
  queue := [ctx, ...queue^];
};

/* Commit phase: run every effect scheduled during this frame, once.
 *
 * Contexts run in the order their render finished - children before parents -
 * and the root context runs last. A context queued more than once in a frame
 * (HStack measure pass + real pass) runs on its first visit; runEffects clears
 * the pending list, so later visits are no-ops. (internal)
 */
let commitEffects = (rootCtx: renderContext): unit => {
  let queue = instance().effectCommitQueue;
  let queued = List.rev(queue^);
  queue := [];
  List.iter(ctx => runEffects(ctx), queued);
  runEffects(rootCtx);
};

/* Run all cleanup functions for component unmount.
 * Called when the app is exiting. (internal)
 *
 * Each cleanup slot is cleared as it runs, so calling this twice on the same
 * context (quit() then at_exit, for example) runs every cleanup exactly once.
 */
let runCleanups = (ctx: renderContext): unit => {
  Array.iter(
    hook => {
      switch (hook) {
      | EffectHook(_, cleanupRef) =>
        switch (cleanupRef^) {
        | Some(cleanup) =>
          cleanupRef := None;
          cleanup();
        | None => ()
        }
      | StateHook(_) => ()
      | MemoHook(_) => ()
      }
    },
    ctx.hooks,
  );
};

/* Remove component contexts that were not rendered in the latest pass.
 * Runs cleanups for those contexts and drops them, so key handlers and effects
 * cannot leak after unmount.
 *
 * This is why every visited component must be recorded (and therefore actually
 * visited): a component whose render is skipped looks unmounted here, and its
 * state is destroyed.
 */
let cleanupUnmountedComponents =
    (activeComponentIds: list(Element.componentId)): unit => {
  let st = instance();
  /* Track currently active IDs for quick membership checks */
  let activeSet: Hashtbl.t(Element.componentId, unit) =
    Hashtbl.create(Hashtbl.length(st.componentContexts) + 10);
  List.iter(id => Hashtbl.replace(activeSet, id, ()), activeComponentIds);

  Hashtbl.iter(
    (componentId, ctx) =>
      if (!Hashtbl.mem(activeSet, componentId)) {
        runCleanups(ctx);
        Hashtbl.remove(st.componentContexts, componentId);
      },
    st.componentContexts,
  );
};

/* Unmount the whole tree of the current instance.
 *
 * Runs the cleanup of every live component context and of the root context,
 * then drops all per-component state. Idempotent: runCleanups clears each
 * cleanup slot as it runs and the tables are emptied here, so a second call
 * (quit() twice, or quit() followed by at_exit) does nothing. (internal)
 */
let unmountAll = (): unit => {
  let st = instance();
  Hashtbl.iter((_id, ctx) => runCleanups(ctx), st.componentContexts);
  Hashtbl.reset(st.componentContexts);
  switch (st.rootContext^) {
  | Some(rootCtx) =>
    runCleanups(rootCtx);
    rootCtx.keyHandlers = [];
    rootCtx.inputHandlers = [];
    rootCtx.mouseHandlers = [];
    rootCtx.wheelInterest = false;
    rootCtx.clickInterest = false;
    rootCtx.pendingEffects = [];
  | None => ()
  };
  st.effectCommitQueue := [];
  st.renderedComponentIds := [];
  /* Defensive: timers should already be cancelled by their effect cleanups
     (just run above), but a timer whose owning component's cleanup never
     ran for some reason must not survive unmount. */
  Hashtbl.reset(st.timers);
  /* Undrained static/raw output belongs to a frame that will never be
     painted now - drop it rather than let it surface if this instance is
     somehow rendered again. The watermarks stay: they are per tree path and
     an unmounted tree that comes back must not re-emit (see staticEmitted). */
  st.pendingStatic := [];
  st.pendingRawOutput := [];
  /* No frame is painted any more, so no layer owns input. Left set, this
     would keep suppressing every useInput in an application that is being
     torn down and restarted on the same instance. */
  st.overlayLayers = [];
};
