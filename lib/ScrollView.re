/*
 * ScrollView - a scrolling window onto content taller than its box (B5).
 *
 *   <Sized size={Flex(1)}>
 *     <ScrollView>
 *       <VStack> ...thirty rows... </VStack>
 *     </ScrollView>
 *   </Sized>
 *
 * SIZE IT, OR IT WILL NOT SCROLL. A ScrollView's natural size is its
 * CONTENT's size - the "like a div" rule - so an Auto-sized one is exactly
 * as tall as what is inside it and has nothing to scroll. Put it in a
 * <Sized size={Flex(n)}> or <Sized size={Chars(n)}> slot (or under any
 * parent that caps its height) to give it a window smaller than its content.
 *
 * TWO CONTENT MODES.
 * - CHILDREN (the default): the content is the element you nest inside. The
 *   child is rendered whole, then clipped to the window - which means a
 *   frame costs O(TOTAL content), because clipping a styled string has to
 *   parse all of it to know which SGR state is open at the cut. Right for
 *   anything from a handful of rows to a few hundred.
 * - ~rows: an array of already-rendered rows, one string per row. The child
 *   is then IGNORED (write <ScrollView rows />, self-closing) and only the
 *   visible rows are ever touched, so a frame costs O(VIEWPORT) however long
 *   the content is. Reach for it when the application already holds its
 *   content pre-rendered and there is a lot of it - a log, a syntax-
 *   highlighted diff, a table of tens of thousands of lines. Giving both
 *   ~rows and children is not an error: ~rows wins.
 *
 * THE ROW CONTRACT, for ~rows only, and nothing checks it: each row must be
 * SELF-CONTAINED - it opens the styles it needs and assumes nothing is left
 * open by the row above. That is precisely what lets the runtime start
 * painting at row N without reading rows 0..N-1; a row that inherits its
 * colour from its predecessor will render unstyled as soon as the window
 * starts below that predecessor. The array is re-read every frame, so
 * mutating rows in place (filling in highlighting lazily, appending log
 * lines) shows up on the next frame with no rebuild.
 *
 * CONTROLLED AND UNCONTROLLED, React-input style:
 * - No ~offset: the ScrollView owns the scroll position. Keys and the wheel
 *   move it; ~onScroll, if given, is notified.
 * - ~offset given: the APPLICATION owns the position. The prop is what is
 *   displayed, internal state is ignored, and every gesture reports the
 *   value it WOULD have scrolled to (already clamped) through ~onScroll. An
 *   app that ignores onScroll gets a ScrollView that does not move, which is
 *   the same contract a controlled <input value> has in React.
 *
 * INPUT. While focused (Tab-reachable unless ~focusable=false): Up/Down move
 * by one row, PageUp/PageDown by a viewport minus one row, Home/End jump to
 * the extremes. The wheel scrolls by three rows without focus (unless
 * ~mouse=false), and hits the INNERMOST ScrollView under the pointer, so
 * nested scrollers behave the way they look.
 *
 * The clipping itself is not implemented here - it belongs to layout, which
 * is Runtime's. This component only tracks a position and hands it to an
 * [Element.Viewport] node; see Runtime.renderElement's Viewport case for the
 * clip/scrollbar algorithm and for how [vpOnViewport] reports the two
 * heights this component needs to clamp.
 *
 * This is a hand-written component rather than a [@component] one because
 * lib/ is compiled without the ppx; the shape below - props record, a
 * componentTypeId, a make that takes the record, and a createElement that
 * calls Element.createComponent - is exactly what the ppx emits.
 */

type props = {
  children: Element.t,
  rows: option(array(string)),
  offset: option(int),
  onScroll: option(int => unit),
  showScrollbar: option(bool),
  focusable: option(bool),
  id: option(string),
  mouse: option(bool),
};

/* Component type ID: part of a component's identity (path + type + key), so
 * it must be stable across renders and distinct from every other component
 * type. */
let componentTypeId = "Matcha.ScrollView";

/* Where the scrollbar thumb goes, as (thumbTop, thumbHeight) in viewport
 * rows - or None when there is nothing to scroll (content fits, or there is
 * no viewport to draw in), in which case the scrollbar column is blank.
 *
 * PURE, and exported, so the geometry can be unit-tested without rendering
 * anything: Runtime's Viewport case calls exactly this function to paint the
 * column, so the tests below it and the pixels on screen cannot drift apart.
 *
 * - thumbHeight is the viewport's share of the content, at least one row, so
 *   a thumb never disappears on very long content.
 * - thumbTop moves the thumb through the (viewportH - thumbH) rows it has
 *   available, in proportion to how far through the scrollable range the
 *   offset is. At offset 0 it is flush with the top; at the maximum offset
 *   it is flush with the bottom.
 * - [offset] is clamped here too, so a caller that has not clamped yet still
 *   gets a thumb inside the track.
 */
let scrollbarMetrics =
    (~contentH: int, ~viewportH: int, ~offset: int): option((int, int)) =>
  if (viewportH <= 0 || contentH <= viewportH) {
    None;
  } else {
    let thumbH = max(1, viewportH * viewportH / contentH);
    let maxOffset = contentH - viewportH; /* > 0, per the guard above */
    let clamped = max(0, min(offset, maxOffset));
    Some(((viewportH - thumbH) * clamped / maxOffset, thumbH));
  };

let make = (props: props): Element.t => {
  /* The two heights the last committed frame measured: (contentH,
   * viewportH). A REF behind useMemo, deliberately not state: Runtime calls
   * vpOnViewport DURING the render that paints this subtree, and writing
   * state from inside a render is forbidden (it would schedule a frame from
   * within a frame, forever). A ref just records what was measured, and the
   * next gesture reads it. */
  let metrics = Hooks.useMemo(() => ref((0, 0)), [||]);

  /* Uncontrolled position. Ignored entirely while ~offset is given - see
   * the controlled/uncontrolled note at the top of this file. */
  let (internal, setInternal) = Hooks.useState(0);
  let controlled =
    switch (props.offset) {
    | Some(_) => true
    | None => false
    };
  let current =
    switch (props.offset) {
    | Some(o) => o
    | None => internal
    };

  let maxOffset = () => {
    let (contentH, viewportH) = metrics^;
    max(0, contentH - viewportH);
  };

  /* Move to an absolute row, clamped to what the last frame measured. */
  let scrollTo = (target: int): unit => {
    let next = max(0, min(target, maxOffset()));
    if (!controlled) {
      setInternal(next);
    };
    switch (props.onScroll) {
    | Some(f) => f(next)
    | None => ()
    };
  };

  /* Move by a delta FROM THE CLAMPED current position. Clamping the current
   * position first is what makes shrinking content behave: an offset of 40
   * left over from a 50-row list is already snapped to the end of a 10-row
   * one before the delta is applied, so one press of Up moves one row up
   * from what is on screen - not thirty presses of nothing. */
  let scrollBy = (delta: int): unit =>
    scrollTo(min(current, maxOffset()) + delta);

  /* Focus (B1). useFocus is registration-style, so calling it conditionally
   * is safe - it takes no hook slot. ~focusable=false opts out of the Tab
   * ring entirely (a ScrollView driven only by the wheel, or by its parent
   * through ~offset). */
  let focusable =
    switch (props.focusable) {
    | Some(b) => b
    | None => true
    };
  let isFocused =
    if (focusable) {
      Hooks.useFocus(~id=?props.id, ()).Hooks.isFocused;
    } else {
      false;
    };

  Hooks.useInput(~isActive=isFocused, (key, _modifiers) =>
    switch (key) {
    | Key.Arrow_up => scrollBy(-1)
    | Key.Arrow_down => scrollBy(1)
    | Key.Page_up =>
      let (_, viewportH) = metrics^;
      scrollBy(- max(1, viewportH - 1));
    | Key.Page_down =>
      let (_, viewportH) = metrics^;
      scrollBy(max(1, viewportH - 1));
    | Key.Home => scrollTo(0)
    | Key.End => scrollTo(maxOffset())
    | _ => ()
    }
  );

  /* Wheel (B4). Single-target dispatch already picks the innermost
   * component under the pointer, so nested ScrollViews need no special
   * case: the inner one scrolls and the outer one never sees the event. */
  let wantsMouse =
    switch (props.mouse) {
    | Some(b) => b
    | None => true
    };
  if (wantsMouse) {
    /* ~click=false: this body acts on the wheel and nothing else, so
       claiming clicks would only swallow them. It is the mirror of what
       <Clickable> declares with ~wheel=false, and it is what lets a click on
       a ~rows list - which has no child elements to hit - reach the
       application that rendered it. */
    Hooks.useMouse(~click=false, ev =>
      switch (ev.Mouse.kind) {
      | Mouse.ScrollUp => scrollBy(-3)
      | Mouse.ScrollDown => scrollBy(3)
      | _ => ()
      }
    );
  };

  /* Nothing above this line knows which content mode is in force: focus,
   * keys, the wheel and the clamping all work off the (contentH, viewportH)
   * pair vpOnViewport reports, and the runtime reports it the same way in
   * both modes. The mode shows up only here, in what is handed to the
   * Viewport - and ~rows wins over children when both are given. */
  Element.Viewport(
    switch (props.rows) {
    | Some(_) => Element.Empty
    | None => props.children
    },
    {
      vpOffset: current,
      vpShowScrollbar:
        switch (props.showScrollbar) {
        | Some(b) => b
        | None => true
        },
      vpOnViewport: Some(m => metrics := m),
      vpRows: props.rows,
    },
  );
};

let createElement =
    (
      ~key: option(string)=?,
      ~rows: option(array(string))=?,
      ~offset: option(int)=?,
      ~onScroll: option(int => unit)=?,
      ~showScrollbar: option(bool)=?,
      ~focusable: option(bool)=?,
      ~id: option(string)=?,
      ~mouse: option(bool)=?,
      /* Optional, so that a rows-mode ScrollView can be written
       * self-closing - <ScrollView rows /> - exactly as <Static> is. */
      ~children: Element.t=Element.Empty,
      (),
    )
    : Element.t => {
  let props = {children, rows, offset, onScroll, showScrollbar, focusable, id, mouse};
  Element.createComponent(~key?, ~typeId=componentTypeId, props, () =>
    make(props)
  );
};
