/*
 * Element - Terminal UI element tree and rendering
 *
 * This module provides the core element types for building terminal UIs.
 * Elements form a tree structure that gets rendered to ANSI-escaped strings.
 *
 * Element Types:
 * - Text(string): Plain text content
 * - Styled(style, t): Apply ANSI styling to an element
 * - VStack(list(t), options): Stack children vertically with flex layout
 * - HStack(list(t), options): Stack children horizontally with flex layout
 * - Sized(t, size): Wrapper to specify size in parent Stack
 * - Lazy(unit => t): Deferred element (used by components)
 * - Component(typeId, key, props, renderFn): A component call site
 * - WithContext(setup, teardown, t): Context boundary for providers
 * - Static(list(t)): Append-only output committed above the live region
 * - Empty: No content
 *
 * Components: the element is a plain DESCRIPTION of a call site - a type ID, an
 * optional key, the erased props record and the body thunk. It holds no
 * per-instance mutable state and no cached output. Instance identity and hook
 * state live in the runtime, keyed by the component's path through the tree.
 *
 * This module never runs a component body itself. [render] delegates every
 * Component to the function Runtime installs in the [componentRenderer] ref, so
 * the body always executes inside a real hooks context. See the comments on the
 * Component variant and on [componentRenderer] below.
 *
 * Size Types (for Sized wrapper):
 * - Auto: Size to content (default for children without Sized wrapper)
 * - Flex(int): Flex units like CSS flex-grow
 * - Percent(int): Percentage of parent container
 * - Chars(int): Absolute character count
 *
 * Alignment (for HStack/VStack):
 * - align: Cross-axis alignment (AlignStart, AlignEnd, AlignCenter, AlignStretch)
 * - justify: Main-axis distribution (JustifyStart, JustifyEnd, JustifyCenter,
 *            JustifySpaceBetween, JustifySpaceAround, JustifySpaceEvenly)
 *
 * JSX Components:
 * This module also exports JSX-compatible component modules:
 * Text, VStack, HStack, Sized, Fragment
 *
 * Text Styling:
 * The Text component supports optional styling props:
 * bold, dim, italic, underline, inverted (booleans)
 * color, bgColor (color type with named colors + Rgb)
 */

/* Color type for terminal colors.
 * Named colors map to the 16 standard ANSI colors (0-15).
 * Rgb maps to the 216-color cube (16-231).
 * RgbFull is 24-bit direct color (truecolor), emitted as SGR 38;2/48;2.
 */
type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | BrightBlack
  | BrightRed
  | BrightGreen
  | BrightYellow
  | BrightBlue
  | BrightMagenta
  | BrightCyan
  | BrightWhite
  | Rgb(int, int, int) /* RGB values 0-5 each, maps to 216-color cube */
  | RgbFull(int, int, int); /* 24-bit truecolor, 0-255 each (clamped at emission) */

/* Text styling options.
 * These map to ANSI escape codes for terminal formatting.
 */
type style =
  | Bold /* Bold/bright text */
  | Dim /* Dimmed/faint text */
  | Italic /* Italic text (terminal support varies) */
  | Underline /* Underlined text */
  | Inverted /* Inverted foreground/background colors */
  | FgColor(color) /* Foreground color */
  | BgColor(color); /* Background color */

/* Size type for flex layout.
 * Used to specify how children should be sized within a Stack.
 */
type size =
  | Auto /* Size to content (default) */
  | Flex(int) /* Flex units - like CSS flex-grow */
  | Percent(int) /* Percentage of parent container */
  | Chars(int); /* Absolute character count */

/* Alignment along the cross axis (perpendicular to main axis).
 * For HStack: controls vertical alignment of items
 * For VStack: controls horizontal alignment of items
 */
type align =
  | AlignStart /* Align to start (top for HStack, left for VStack) */
  | AlignEnd /* Align to end (bottom for HStack, right for VStack) */
  | AlignCenter /* Center along cross axis */
  | AlignStretch; /* Stretch to fill cross axis (default) */

/* Justification along the main axis.
 * For HStack: controls horizontal distribution of items
 * For VStack: controls vertical distribution of items
 */
type justify =
  | JustifyStart /* Pack items at start */
  | JustifyEnd /* Pack items at end */
  | JustifyCenter /* Pack items at center */
  | JustifySpaceBetween /* Distribute with space between items */
  | JustifySpaceAround /* Distribute with space around items */
  | JustifySpaceEvenly; /* Distribute with equal space everywhere */

/* Stack layout options */
type stackOptions = {
  gap: int,
  align,
  justify,
};

/* Options of a [Viewport] node (see [t] below) - the scroll primitive
 * <ScrollView> builds on. Every field is filled in by ScrollView; an
 * application never constructs this record by hand.
 *
 * - vpOffset: the FIRST content line to show, in rows. It is a request, not
 *   a promise: the runtime clamps it into [0, contentHeight - viewportHeight]
 *   before it clips, so an offset left over from taller content simply snaps
 *   back to the end of the shorter content.
 * - vpShowScrollbar: reserve the rightmost column for a scrollbar. The
 *   column is dropped (and the width given back to the content) when there
 *   would be nothing left to show content in - see Runtime's Viewport case.
 * - vpOnViewport: called with (contentHeight, viewportHeight) on the
 *   COMMITTED pass only, exactly once per painted frame. This is how
 *   ScrollView learns how far it may scroll; it must therefore write to a
 *   ref, never to state - it fires DURING the render.
 * - vpRows: the VIRTUALIZED content mode, and the reason it exists.
 *
 *   None (the default) is the ordinary mode: the content is the Viewport's
 *   CHILD, rendered whole and then clipped. Clipping an already-rendered
 *   multi-line ANSI string means PARSING all of it cell by cell (a style
 *   opened above the window has to be re-opened on the first visible row),
 *   so a frame costs O(total content), not O(viewport) - fine for tens of
 *   rows, ruinous for tens of thousands.
 *
 *   Some(rows) says "I already hold my content as one pre-baked styled
 *   string per row". The CHILD IS THEN IGNORED (pass [Empty] by
 *   convention) and content height is [Array.length(rows)]. Only the
 *   visible rows are ever touched, so the frame costs O(viewport).
 *
 *   THE CONTRACT, and it is not checked: every row must be SELF-CONTAINED -
 *   it opens whatever SGR state it needs and does not rely on styling left
 *   open by the row above it. That independence is exactly what lets the
 *   runtime jump straight to row N without reading rows 0..N-1, and a row
 *   that breaks it renders unstyled whenever the window starts below it.
 *   The array is read fresh every frame, so an application may mutate rows
 *   in place (a lazily-highlighted diff, say) without rebuilding it. */
type viewportOptions = {
  vpOffset: int,
  vpShowScrollbar: bool,
  vpOnViewport: option(((int, int)) => unit),
  vpRows: option(array(string)),
};

/* Component instance identifier - defined here for Element, but also used by Hooks.
 * IDs are generated by Hooks (Hooks.generateComponentId), from a counter that
 * belongs to the running instance, so that two applications started in the
 * same process do not share an ID space. */
type componentId = int;

/* Wrapping/truncation mode for a [WrappedText] node (see below) and the
 * [Text] component's [~wrap] prop. Applied by [Runtime.renderElement] once a
 * layout width is known, via [StyledText.wrapString]:
 * - Wrap: greedy word wrap onto multiple lines.
 * - Truncate: cut the end, replacing it with an ellipsis.
 * - TruncateStart: cut the start, prefixed with an ellipsis.
 * - TruncateMiddle: cut the middle, with an ellipsis between the two halves.
 */
type wrap =
  | Wrap
  | Truncate
  | TruncateMiddle
  | TruncateStart;

/* Element tree type.
 * Represents the structure of a terminal UI before rendering.
 */
type t =
  | Text(string) /* Plain text content */
  | Styled(style, t) /* Apply styling to child element */
  | VStack(list(t), stackOptions) /* VStack(children, options) - vertical flex layout */
  | HStack(list(t), stackOptions) /* HStack(children, options) - horizontal flex layout */
  | Sized(t, size) /* Wrapper to specify size in parent Stack */
  | Empty /* Empty element (renders to "") */
  | Lazy(unit => t) /* Deferred element - thunk is called during render */
  | Component(option(string), option(string), Obj.t, unit => t)
  /* Component instance: (typeId, key, props, renderFn).
   *
   * - typeId: emitted by the [@component] ppx, identifies the component TYPE
   *   (source location of its definition).
   * - key: optional, React-style; forces a distinct instance among siblings.
   * - props: the ppx-generated props record, erased to Obj.t. Nothing reads it
   *   today - it is carried so a future memoization pass has something to
   *   compare - but the ppx passes it, so the field stays.
   * - renderFn: the component body, a thunk. It is called by the RUNTIME, once
   *   per visit, inside the component's own hooks context. Never call it here.
   *
   * The element carries NO per-instance mutable state. Identity lives entirely
   * in the runtime (tree path -> stable ID registry), and a component's output
   * is never cached on the element: element trees are rebuilt every frame, so
   * anything stored here would be thrown away, and serving a stale string would
   * skip the descendants' visits (losing their contexts to the unmount sweep). */
  | WithContext(unit => unit, unit => unit, t) /* Context boundary: (setup, teardown, children) */
  | Static(list(t))
  /* Static(items): output that is COMMITTED once, above the live region, and
   * never repainted (see lib/LiveRegion.re and the [Static] module below).
   *
   * A Static node occupies ZERO layout space: it renders to "" wherever it
   * sits, and Runtime's stacks filter it out of every layout computation
   * before they run (it consumes no gap slot and no justify share). What it
   * actually does happens as a side effect of the render: items the runtime
   * has not emitted yet are rendered at their natural height and queued for
   * the frame's static drain, which prints them above the live region.
   *
   * Emission is watermarked per tree path, so items are append-only: item i
   * is emitted the first frame the list is at least i+1 long, and never
   * again. See Runtime's Static case for the full contract. */
  | WrappedText(wrap, t)
  /* WrappedText(mode, child): wrap or truncate the child's rendered text to
   * the layout width, once one is known. Produced by <Text wrap=...>; see
   * [Runtime.renderElement]'s WrappedText case and [StyledText.wrapString].
   * Outside layout (Element.render), this is a no-op passthrough to the
   * child - wrapping needs a width, which detached rendering doesn't have. */
  | Viewport(t, viewportOptions);
  /* Viewport(child, options): a scrolling window onto [child] (B5). The
   * child is rendered at its NATURAL height and the window keeps only the
   * rows [vpOffset, vpOffset + viewportHeight), padded to the viewport's
   * width, with an optional scrollbar column on the right.
   *
   * When [options.vpRows] is Some, the child is ignored entirely and the
   * content is that array of pre-baked rows instead - see [viewportOptions]
   * for the mode and the row-independence contract it rests on.
   *
   * Its natural size is its CONTENT's size - the "like a div" rule - so a
   * Viewport only ever scrolls when something above it caps its height:
   * give it a <Sized size={Flex(1)}> or <Sized size={Chars(n)}> parent
   * slot. An Auto-sized ScrollView is exactly as tall as its content and
   * therefore never scrolls.
   *
   * Only [Runtime.renderElement] can clip: clipping needs the committed
   * layout height, which detached rendering does not have. Outside layout
   * (Element.render), this is a passthrough to the child, matching the
   * natural-size rule above. */

/* ============================================================================
 * Element Constructors
 * ============================================================================ */

/* Create a text element */
let text = (s: string): t => Text(s);

/* Create a styled element */
let styled = (style: style, el: t): t => Styled(style, el);

/* Create a vertical stack of elements */
let vstack =
    (
      ~gap=0,
      ~align=AlignStretch,
      ~justify=JustifyStart,
      children: list(t),
    )
    : t =>
  VStack(children, {gap, align, justify});

/* Create a horizontal stack of elements */
let hstack =
    (
      ~gap=0,
      ~align=AlignStretch,
      ~justify=JustifyStart,
      children: list(t),
    )
    : t =>
  HStack(children, {gap, align, justify});

/* Wrap an element with a size hint for parent Stack */
let sized = (size: size, el: t): t => Sized(el, size);

/* Empty element constant */
let empty = Empty;

/* Create a lazy element wrapping a render function.
 * This is used by the component system to defer rendering.
 * The thunk is called during Element.render().
 */
let createElement = (render: unit => t): t => Lazy(render);

/* Create a component element.
 * This is what the [@component] ppx emits for a component call site.
 * The stable instance ID is assigned by Runtime from the component's position
 * in the tree, the optional key, and the component type ID.
 * The typeId is emitted by the ppx and identifies the component type (source
 * location of its definition). Two different component types at the same tree
 * position therefore get different instance IDs.
 * An optional key can be provided to force instance identity changes,
 * mirroring React's key semantics.
 * props is stored (erased) for future memoization; see the Component variant.
 */
let createComponent = (~key=?, ~typeId=?, props: 'a, renderFn: unit => t): t =>
  Component(typeId, key, Obj.repr(props), renderFn);

/* ============================================================================
 * Style Helpers
 * ============================================================================ */

/* Apply bold styling */
let bold = (el: t): t => Styled(Bold, el);

/* Apply dim styling */
let dim = (el: t): t => Styled(Dim, el);

/* Apply italic styling */
let italic = (el: t): t => Styled(Italic, el);

/* Apply underline styling */
let underline = (el: t): t => Styled(Underline, el);

/* Apply inverted (reverse video) styling */
let inverted = (el: t): t => Styled(Inverted, el);

/* ============================================================================
 * ANSI Escape Code Utilities
 * ============================================================================ */

/* Clamp a truecolor channel into 0..255. */
let clampChannel = (v: int): int => max(0, min(255, v));

/* Convert a color to its 256-color code.
 *
 * [RgbFull] has no 256-color code at all - it is emitted as a DIRECT-color
 * (24-bit) SGR by [styleToAnsi] and never reaches this function on the
 * emission path. For callers that need a palette index anyway, it is
 * down-sampled into the nearest cell of the 216-color cube. That is lossy on
 * purpose: use [styleToAnsi] if you want the exact color.
 */
let colorToCode = (c: color): int => {
  switch (c) {
  | Black => 0
  | Red => 1
  | Green => 2
  | Yellow => 3
  | Blue => 4
  | Magenta => 5
  | Cyan => 6
  | White => 7
  | BrightBlack => 8
  | BrightRed => 9
  | BrightGreen => 10
  | BrightYellow => 11
  | BrightBlue => 12
  | BrightMagenta => 13
  | BrightCyan => 14
  | BrightWhite => 15
  | Rgb(r, g, b) =>
    /* Clamp values to 0-5 range and convert to 216-color cube index */
    let clamp = v => max(0, min(5, v));
    16 + 36 * clamp(r) + 6 * clamp(g) + clamp(b);
  | RgbFull(r, g, b) =>
    /* Lossy fallback (see the doc comment): 0..255 -> the nearest 0..5 cube
     * axis, rounded rather than truncated. */
    let cube = v => (clampChannel(v) * 5 + 127) / 255;
    16 + 36 * cube(r) + 6 * cube(g) + cube(b);
  };
};

/* The SGR parameters that select [c], WITHOUT the leading 38 (foreground) or
 * 48 (background) that says which slot it goes in:
 *   "5;<n>"         - a 256-color palette index (named colors and [Rgb])
 *   "2;<r>;<g>;<b>" - 24-bit direct color ([RgbFull]), channels clamped 0..255
 */
let colorParams = (c: color): string =>
  switch (c) {
  | RgbFull(r, g, b) =>
    Printf.sprintf(
      "2;%d;%d;%d",
      clampChannel(r),
      clampChannel(g),
      clampChannel(b),
    )
  | _ => Printf.sprintf("5;%d", colorToCode(c))
  };

/* Convert a style to its ANSI escape code. */
let styleToAnsi = (style: style): string => {
  switch (style) {
  | Bold => "\027[1m"
  | Dim => "\027[2m"
  | Italic => "\027[3m"
  | Underline => "\027[4m"
  | Inverted => "\027[7m"
  | FgColor(c) => "\027[38;" ++ colorParams(c) ++ "m"
  | BgColor(c) => "\027[48;" ++ colorParams(c) ++ "m"
  };
};

/* ANSI reset code - clears all styling */
let resetAnsi = "\027[0m";

/* ============================================================================
 * String Layout Utilities
 * ============================================================================ */

/* Split a string into lines. */
let splitLines = (s: string): list(string) => {
  String.split_on_char('\n', s);
};

/* Calculate the visible length of a string, in terminal COLUMNS.
 * Ignores ANSI escape codes and correctly handles multi-byte UTF-8:
 * CJK and emoji count as 2, combining marks as 0.
 * This is essential for proper terminal layout calculations.
 */
let visibleLength = (s: string): int => TextWidth.stringWidth(s);

/* Pad or truncate a string to an exact visible width.
 * Handles ANSI escape codes and UTF-8 correctly.
 * If truncating, appends a reset code to prevent style leaking.
 * A double-width character is never split: if only one column is left,
 * a single space fills it.
 * If padding, uses spaces.
 */
let padToWidth = (s: string, width: int): string => {
  let visible = visibleLength(s);
  if (visible >= width) {
    /* Truncate - need to be careful with ANSI codes and UTF-8 */
    let len = String.length(s);
    let buf = Buffer.create(len);
    /* Copy the escape sequence starting at [i] (ESC up to and including the
     * final letter) and return the offset just past it. */
    let rec copyEscape = i =>
      if (i >= len) {
        i;
      } else {
        let c = Char.code(s.[i]);
        Buffer.add_char(buf, s.[i]);
        if (c >= 65 && c <= 90 || c >= 97 && c <= 122) {
          i + 1;
        } else {
          copyEscape(i + 1);
        };
      };
    let rec loop = (i, w) =>
      if (i >= len || w >= width) {
        ();
      } else if (Char.code(s.[i]) == 27) {
        /* Escape sequences cost no columns - copy them through */
        Buffer.add_char(buf, s.[i]);
        loop(copyEscape(i + 1), w);
      } else {
        let (cp, consumed) = TextWidth.decodeUtf8(s, i);
        let cw = TextWidth.charWidth(cp);
        if (w + cw > width) {
          /* A double-width char would straddle the limit: pad the last
           * column with a space instead of splitting the character. */
          Buffer.add_char(buf, ' ');
        } else {
          Buffer.add_substring(buf, s, i, min(consumed, len - i));
          loop(i + consumed, w + cw);
        };
      };
    loop(0, 0);
    Buffer.add_string(buf, resetAnsi);
    Buffer.contents(buf);
  } else {
    /* Pad with spaces - reset styles first to prevent leaking */
    s ++ resetAnsi ++ String.make(width - visible, ' ');
  };
};

/* Repeat a string n times.
 * Works correctly with multi-byte UTF-8 strings.
 */
let repeatString = (s: string, n: int): string => {
  let buf = Buffer.create(String.length(s) * n);
  for (_ in 1 to n) {
    Buffer.add_string(buf, s);
  };
  Buffer.contents(buf);
};

/* Strip all ANSI escape codes from a string.
 * Useful for testing output without dealing with formatting codes.
 */
let stripAnsi = (s: string): string => {
  let len = String.length(s);
  let buf = Buffer.create(len);
  let rec loop = (i, inEscape) =>
    if (i >= len) {
      ();
    } else {
      let c = Char.code(s.[i]);
      if (inEscape) {
        /* End of escape sequence when we hit a letter (A-Z or a-z) */
        if (c >= 65 && c <= 90 || c >= 97 && c <= 122) {
          loop(i + 1, false);
        } else {
          loop(i + 1, true);
        };
      } else if (c == 27) {
        /* ESC (0x1B) - start of escape sequence */
        loop(i + 1, true);
      } else {
        Buffer.add_char(buf, s.[i]);
        loop(i + 1, false);
      };
    };
  loop(0, false);
  Buffer.contents(buf);
};

/* Box drawing characters for terminal UIs.
 * These are UTF-8 characters that render as solid lines.
 */
module BoxChars = {
  let topLeft = "┌";
  let topRight = "┐";
  let bottomLeft = "└";
  let bottomRight = "┘";
  let horizontal = "─";
  let vertical = "│";
  let teeDown = "┬";
  let teeUp = "┴";
  let teeRight = "├";
  let teeLeft = "┤";
  let cross = "┼";
};

/* ============================================================================
 * Rendering
 * ============================================================================ */

/* Renderer for Component nodes met by [render] below.
 *
 * A component body must never run without a hooks context around it, but
 * Element cannot set one up: the hook machinery lives in Runtime/Hooks, which
 * depend on this module. So Runtime installs its own component renderer here
 * at module initialization time (see the bottom of lib/Runtime.re). The
 * installed function renders the component through the runtime's measuring
 * mode, which produces exactly the layout-free output documented for [render]
 * while giving the component a proper context, a stable identity and a place
 * in the effect commit phase.
 *
 * The fallback used when nothing is installed (calling the render function
 * raw) only applies if Runtime is not linked into the program at all. Every
 * real application links it, because there is no other way to start one.
 *
 * The contract, in short:
 * - Element OWNS the ref and the Component case; it decides when to delegate.
 * - Runtime OWNS the function and installs it exactly once, unconditionally, at
 *   module-initialization time. It is not per-application state - the installed
 *   renderer reads whatever Hooks instance is in force when it is called.
 * - The installed function is given the WHOLE Component element (not just the
 *   thunk), because it needs the type ID and key to derive the instance
 *   identity, and it must be the one to call the thunk, after it has swapped in
 *   the component's context.
 */
let componentRenderer: ref(option(t => string)) = ref(None);

/* Render an element tree to a string.
 *
 * NOTE: This is a simple render function without layout calculation.
 * For proper flex layout, use Runtime.renderElement which handles
 * size constraints and flex distribution.
 *
 * Rendering behavior by element type:
 * - Text(s) -> the string as-is
 * - Styled(style, child) -> ANSI code + render(child) + reset
 * - VStack(children) -> children joined with newlines (ignores sizes)
 * - HStack(children) -> children concatenated (ignores sizes)
 * - Lazy(f) -> render(f()) - forces the thunk
 * - Component(...) -> delegated to Runtime via [componentRenderer], which
 *   renders the body layout-free inside a proper hooks context
 * - WithContext(setup, teardown, children) -> setup(); render(children); teardown()
 * - Empty -> empty string
 */
let rec render = (el: t): string => {
  switch (el) {
  | Empty => ""
  | Text(s) => s
  | Styled(style, child) => styleToAnsi(style) ++ render(child) ++ resetAnsi
  | VStack(children, _options) =>
    children |> List.map(render) |> String.concat("\n")
  | HStack(children, _options) =>
    children |> List.map(render) |> String.concat("")
  | Sized(child, _size) =>
    /* Size is handled by Runtime layout; here we just render the child */
    render(child)
  | Lazy(f) => render(f())
  | Component(_typeId, _key, _props, renderFn) =>
    /* Component rendering is delegated to Runtime (see [componentRenderer]),
     * so that the body runs inside a real hooks context. */
    switch (componentRenderer^) {
    | Some(renderComponent) => renderComponent(el)
    | None =>
      /* Runtime is not linked - fall back to the raw render function. Hooks
       * called by the body will fail or land in the wrong context; this only
       * exists so that Element stays usable on its own. */
      render(renderFn())
    }
  | WithContext(setup, teardown, children) =>
    setup();
    let result = render(children);
    teardown();
    result;
  | WrappedText(_mode, child) =>
    /* No layout width here (see [t]'s WrappedText doc) - pass through. */
    render(child)
  | Viewport(child, options) =>
    /* No layout height here (see [t]'s Viewport doc), and a scroller's
     * natural size IS its content, so the UNCLIPPED content is the right
     * answer for a detached render - the rows, joined, when the viewport is
     * in rows mode, and otherwise the child. */
    switch (options.vpRows) {
    | Some(rows) => String.concat("\n", Array.to_list(rows))
    | None => render(child)
    }
  | Static(_items) =>
    /* Nothing. A Static node's output does not belong to the string its
     * parent is building - it is committed above the live region by the
     * frame's static drain, which needs the runtime's per-path watermarks
     * (Hooks.instanceState.staticEmitted) to know which items are new.
     * Element.render is detached from all of that - no frame, no path, no
     * watermark - so rendering the items here would either duplicate them
     * (once detached, once in the real tree) or silently consume them.
     * Returning "" keeps detached rendering a pure, side-effect-free
     * measurement, exactly as this function documents. */
    ""
  };
};

/* ============================================================================
 * JSX-Compatible Component Modules
 *
 * These modules provide the interface expected by ReasonML's JSX syntax.
 * Each has: type props, let make, let createElement
 * ============================================================================ */

/* Text component - renders plain text with optional inline styling.
 *
 * Usage:
 *   <Text> "plain" </Text>
 *   <Text bold=true> "bold text" </Text>
 *   <Text bold=true dim=true> "bold and dim" </Text>
 *   <Text color=Red> "red text" </Text>
 *   <Text color=Rgb(5, 0, 0) bgColor=White> "custom colors" </Text>
 */
module Text = {
  type props = {
    children: string,
    bold: option(bool),
    dim: option(bool),
    italic: option(bool),
    underline: option(bool),
    inverted: option(bool),
    color: option(color),
    bgColor: option(color),
    wrap: option(wrap),
  };

  /* Default props for simple <Text> usage */
  let defaultProps = {
    children: "",
    bold: None,
    dim: None,
    italic: None,
    underline: None,
    inverted: None,
    color: None,
    bgColor: None,
    wrap: None,
  };

  let make = props => {
    let el = ref(Text(props.children));

    /* Apply styles in reverse order so they nest correctly */
    switch (props.bgColor) {
    | Some(c) => el := Styled(BgColor(c), el^)
    | None => ()
    };
    switch (props.color) {
    | Some(c) => el := Styled(FgColor(c), el^)
    | None => ()
    };
    switch (props.inverted) {
    | Some(true) => el := Styled(Inverted, el^)
    | _ => ()
    };
    switch (props.underline) {
    | Some(true) => el := Styled(Underline, el^)
    | _ => ()
    };
    switch (props.italic) {
    | Some(true) => el := Styled(Italic, el^)
    | _ => ()
    };
    switch (props.dim) {
    | Some(true) => el := Styled(Dim, el^)
    | _ => ()
    };
    switch (props.bold) {
    | Some(true) => el := Styled(Bold, el^)
    | _ => ()
    };

    /* ~wrap is applied last (outermost): Runtime's WrappedText case measures
     * its child - the already-styled tree built above - in measuring mode
     * and wraps the resulting ANSI-styled text, so the color/bold escapes
     * survive into StyledText.parse. Default None leaves the element
     * exactly as before this prop existed - zero behavior change. */
    switch (props.wrap) {
    | Some(mode) => el := WrappedText(mode, el^)
    | None => ()
    };

    el^;
  };

  /* createElement using labeled args with defaults for JSX compatibility */
  let createElement =
      (
        ~bold=?,
        ~dim=?,
        ~italic=?,
        ~underline=?,
        ~inverted=?,
        ~color=?,
        ~bgColor=?,
        ~wrap=?,
        ~children,
        (),
      ) =>
    Lazy(
      () =>
        make({
          children,
          bold,
          dim,
          italic,
          underline,
          inverted,
          color,
          bgColor,
          wrap,
        }),
    );
};

/* VStack component - stacks children vertically with flex layout.
 *
 * Usage:
 *   <VStack> child1 child2 </VStack>
 *   <VStack gap=1> child1 child2 </VStack>
 *   <VStack align=AlignCenter justify=JustifySpaceBetween> ... </VStack>
 *
 * Props:
 *   gap: int - Space between children (default: 0)
 *   align: align - Cross-axis alignment (default: AlignStretch)
 *     AlignStart, AlignEnd, AlignCenter, AlignStretch
 *   justify: justify - Main-axis distribution (default: JustifyStart)
 *     JustifyStart, JustifyEnd, JustifyCenter,
 *     JustifySpaceBetween, JustifySpaceAround, JustifySpaceEvenly
 *
 * Children can be wrapped with <Sized> to specify their size:
 *   <VStack>
 *     <Sized size={Flex(2)}> child1 </Sized>
 *     <Sized size={Percent(30)}> child2 </Sized>
 *     child3  /* defaults to Flex(1) */
 *   </VStack>
 */
module VStack = {
  let make =
      (
        ~children: list(t),
        ~gap=0,
        ~align=AlignStretch,
        ~justify=JustifyStart,
        (),
      ) =>
    VStack(children, {gap, align, justify});
  let createElement =
      (
        ~children: list(t),
        ~gap=0,
        ~align=AlignStretch,
        ~justify=JustifyStart,
        (),
      ) =>
    Lazy(() => make(~children, ~gap, ~align, ~justify, ()));
};

/* HStack component - stacks children horizontally with flex layout.
 *
 * Usage:
 *   <HStack> child1 child2 </HStack>
 *   <HStack gap=2> child1 child2 </HStack>
 *   <HStack align=AlignCenter justify=JustifySpaceBetween> ... </HStack>
 *
 * Props:
 *   gap: int - Space between children (default: 0)
 *   align: align - Cross-axis alignment (default: AlignStretch)
 *     AlignStart, AlignEnd, AlignCenter, AlignStretch
 *   justify: justify - Main-axis distribution (default: JustifyStart)
 *     JustifyStart, JustifyEnd, JustifyCenter,
 *     JustifySpaceBetween, JustifySpaceAround, JustifySpaceEvenly
 *
 * Children can be wrapped with <Sized> to specify their size:
 *   <HStack>
 *     <Sized size={Chars(20)}> sidebar </Sized>
 *     <Sized size={Flex(1)}> content </Sized>
 *   </HStack>
 */
module HStack = {
  let make =
      (
        ~children: list(t),
        ~gap=0,
        ~align=AlignStretch,
        ~justify=JustifyStart,
        (),
      ) =>
    HStack(children, {gap, align, justify});
  let createElement =
      (
        ~children: list(t),
        ~gap=0,
        ~align=AlignStretch,
        ~justify=JustifyStart,
        (),
      ) =>
    Lazy(() => make(~children, ~gap, ~align, ~justify, ()));
};

/* Sized component - wraps a child with a size hint for parent Stack.
 *
 * Usage:
 *   <Sized size={Flex(2)}> child </Sized>
 *   <Sized size={Percent(50)}> child </Sized>
 *   <Sized size={Chars(30)}> child </Sized>
 */
module Sized = {
  let make = (~children: t, ~size: size, ()) => Sized(children, size);
  let createElement = (~children: t, ~size: size, ()) =>
    Lazy(() => make(~children, ~size, ()));
};

/* Static component - commit output above the live region, once per item.
 *
 * Usage is ALWAYS self-closing, with the items and a render function:
 *
 *   <Static
 *     items=messages
 *     renderItem={(msg, _i) =>
 *       <VStack>
 *         <Text bold=true color=Green> {"> " ++ msg.prompt} </Text>
 *         <Text wrap=Wrap> msg.reply </Text>
 *       </VStack>}
 *   />
 *
 * Put it above the live rows (spinner, input, status) of the frame. Every
 * item is rendered EXACTLY ONCE, printed above the live region, and then
 * belongs to the terminal's scrollback: it is never repainted, never
 * re-measured, and scrolls away naturally like ordinary command output. That
 * is what keeps a long transcript cheap - the live region stays a handful of
 * lines no matter how much has been committed.
 *
 * CONTRACT (all of it enforced by the append-only watermark in Runtime):
 * - `items` is APPEND-ONLY. Appending emits the new tail. Mutating an item
 *   that was already emitted has no effect (it is already in the
 *   transcript). PREPENDING or reordering duplicates output, because
 *   emission is tracked by COUNT, not by identity.
 * - An item's components mount on the frame that emits them and are
 *   unmounted on the next frame (they are no longer visited, so
 *   cleanupUnmountedComponents reaps them). A mount effect therefore runs
 *   exactly once; anything ongoing (a timer, a subscription) belongs in the
 *   live part of the tree, not in a static item.
 * - The watermark is keyed by the Static node's TREE PATH and survives
 *   unmount/remount at that path: a Static that disappears and comes back
 *   does not re-emit what it already emitted.
 * - Static inside Static is NOT supported.
 *
 * `renderItem` receives the item and its index in `items`. ~children exists
 * only because Reason's JSX passes it on self-closing elements; it is
 * ignored. Ink's function-as-children form is deliberately not supported.
 */
module Static = {
  let make = (~items: list('a), ~renderItem: ('a, int) => t, ()) =>
    Static(List.mapi((i, item) => renderItem(item, i), items));
  let createElement =
      (
        ~items: list('a),
        ~renderItem: ('a, int) => t,
        ~children: list(t)=[],
        (),
      ) => {
    ignore(children);
    Lazy(() => make(~items, ~renderItem, ()));
  };
};

/* Fragment component - groups children without adding structure */
module Fragment = {
  let make = (~children: list(t), ()) =>
    VStack(children, {gap: 0, align: AlignStretch, justify: JustifyStart});
  let createElement = (~children: list(t), ()) =>
    Lazy(() => make(~children, ()));
};

/* ============================================================================
 * TextArea Component - Multi-line text editor
 *
 * A stateful text input component with full keyboard navigation, selection,
 * and auto-grow behavior.
 *
 * Usage:
 *   <TextArea value=text onChange=setText />
 *   <TextArea value=text onChange=setText onSubmit=handleSubmit placeholder="Enter text..." />
 * ============================================================================ */
module TextArea = {
  /* Selection range: (startRow, startCol, endRow, endCol) */
  type selection = (int, int, int, int);

  /* Props for TextArea component */
  type props = {
    value: string,
    onChange: string => unit,
    onSubmit: option(unit => unit),
    placeholder: option(string),
    maxWidth: option(int),
    maxHeight: option(int),
    minHeight: option(int),
    cursorColor: option(color),
    selectionColor: option(color),
  };

  /* ============================================================================
   * Text manipulation utilities
   * ============================================================================ */

  /* Split text into array of lines */
  let textToLines = (text: string): array(string) => {
    let lines = String.split_on_char('\n', text);
    Array.of_list(lines);
  };

  /* Join lines back into text */
  let linesToText = (lines: array(string)): string => {
    String.concat("\n", Array.to_list(lines));
  };

  /* ----------------------------------------------------------------------
   * Cells
   *
   * Every column index in TextArea (cursorCol, the two column components of
   * a selection, the results of the word-motion helpers) is a CELL index,
   * not a byte offset. A cell is one user-perceived character: a base
   * codepoint plus the zero-width marks fused onto it by TextWidth.toCells.
   * For pure ASCII a cell is exactly one byte, so all of this collapses to
   * the byte arithmetic it replaced.
   *
   * Text is still stored as a plain string; these helpers convert a cell
   * column into the byte offset that String.sub needs.
   * -------------------------------------------------------------------- */

  /* Split one line into cells.
   *
   * TextWidth.toCells fuses every zero-width codepoint onto the cell before
   * it. That is what combining marks need, but ASCII control bytes (a stray
   * tab in the value, say) are zero-width too and must not vanish into the
   * character before them - the editor keeps one cell per byte for ASCII
   * text, so a control byte stays editable as a cell of its own. The line is
   * therefore split around control bytes and toCells is applied in between.
   */
  let lineCells = (line: string): array(TextWidth.cell) => {
    let len = String.length(line);
    let hasControl = ref(false);
    for (i in 0 to len - 1) {
      if (Char.code(line.[i]) < 0x20) {
        hasControl := true;
      };
    };
    if (! hasControl^) {
      TextWidth.toCells(line);
    } else {
      /* Reversed list of cell arrays, concatenated at the end */
      let parts = ref([]);
      let segStart = ref(0);
      let flush = i =>
        if (i > segStart^) {
          let seg = String.sub(line, segStart^, i - segStart^);
          parts := [TextWidth.toCells(seg), ...parts^];
        };
      for (i in 0 to len - 1) {
        if (Char.code(line.[i]) < 0x20) {
          flush(i);
          let c = {TextWidth.bytes: String.make(1, line.[i]), width: 0};
          parts := [[|c|], ...parts^];
          segStart := i + 1;
        };
      };
      flush(len);
      Array.concat(List.rev(parts^));
    };
  };

  /* Number of cells in a line (its length in cell columns). */
  let cellCount = (line: string): int => Array.length(lineCells(line));

  /* Byte offset of cell column [col] in [line], clamped to the line. */
  let cellToByte = (line: string, col: int): int => {
    let cells = lineCells(line);
    let stop = max(0, min(col, Array.length(cells)));
    let rec loop = (i, off) =>
      if (i >= stop) {
        off;
      } else {
        loop(i + 1, off + String.length(cells[i].TextWidth.bytes));
      };
    loop(0, 0);
  };

  /* The part of [line] before cell column [col]. */
  let cellsBefore = (line: string, col: int): string =>
    String.sub(line, 0, cellToByte(line, col));

  /* The part of [line] from cell column [col] to the end. */
  let cellsFrom = (line: string, col: int): string => {
    let start = cellToByte(line, col);
    String.sub(line, start, String.length(line) - start);
  };

  /* Get the byte index for a (row, cellCol) position in text */
  let positionToIndex = (text: string, row: int, col: int): int => {
    let lines = textToLines(text);
    let rec countBytes = (r, idx) =>
      if (r >= row) {
        idx + cellToByte(lines[r], col);
      } else {
        countBytes(r + 1, idx + String.length(lines[r]) + 1);
      };
    if (row >= Array.length(lines)) {
      String.length(text);
    } else {
      countBytes(0, 0);
    };
  };

  /* Character classes for word boundary detection */
  type charClass =
    | WordChar /* alphanumeric + underscore */
    | Whitespace /* spaces, tabs */
    | Punctuation; /* everything else (symbols, punctuation) */

  /* Classify a character */
  let classifyChar = (c: char): charClass =>
    if (c >= 'a'
        && c <= 'z'
        || c >= 'A'
        && c <= 'Z'
        || c >= '0'
        && c <= '9'
        || c == '_') {
      WordChar;
    } else if (c == ' ' || c == '\t') {
      Whitespace;
    } else {
      Punctuation;
    };

  /* Classify a cell by its first codepoint.
   *
   * ASCII cells keep exactly the classes above. Everything else (accented
   * letters, ideographs, emoji, a stray combining mark) counts as a word
   * character, so word motion treats a run of non-ASCII text as one word.
   */
  let classifyCell = (cell: TextWidth.cell): charClass => {
    let (cp, _) = TextWidth.decodeUtf8(cell.TextWidth.bytes, 0);
    if (cp < 128) {
      classifyChar(Char.chr(cp));
    } else {
      WordChar;
    };
  };

  /* Check if character is whitespace */
  let isWhitespace = (c: char): bool => c == ' ' || c == '\t';

  /* Check if a cell is whitespace */
  let isWhitespaceCell = (cell: TextWidth.cell): bool =>
    classifyCell(cell) == Whitespace;

  /* Find the start of the previous word from a cell column in a line.
   * Skips backwards over whitespace, then skips the previous "word"
   * (where a word is a sequence of same-class cells). Returns a cell column.
   */
  let findPrevWordStart = (line: string, col: int): int => {
    let cells = lineCells(line);
    let len = Array.length(cells);
    if (col <= 0 || len == 0) {
      0;
    } else {
      let pos = ref(min(col - 1, len - 1));
      /* Skip whitespace backwards */
      while (pos^ > 0 && isWhitespaceCell(cells[pos^])) {
        pos := pos^ - 1;
      };
      /* If we're at start after skipping whitespace, done */
      if (pos^ <= 0) {
        0;
      } else {
        /* Get the class of the cell we're on */
        let cellClass = classifyCell(cells[pos^]);
        /* Skip all cells of the same class */
        while (pos^ > 0 && classifyCell(cells[pos^]) == cellClass) {
          pos := pos^ - 1;
        };
        /* If we stopped on a different class, move forward one */
        if (pos^ > 0 || classifyCell(cells[0]) != cellClass) {
          pos^ + 1;
        } else {
          0;
        };
      };
    };
  };

  /* Find the end of the next word from a cell column in a line.
   * Skips forward over whitespace, then skips the next "word"
   * (where a word is a sequence of same-class cells). Returns a cell column.
   */
  let findNextWordEnd = (line: string, col: int): int => {
    let cells = lineCells(line);
    let len = Array.length(cells);
    if (col >= len) {
      len;
    } else {
      let pos = ref(col);
      /* Skip whitespace forward */
      while (pos^ < len && isWhitespaceCell(cells[pos^])) {
        pos := pos^ + 1;
      };
      /* If we're at end after skipping whitespace, done */
      if (pos^ >= len) {
        len;
      } else {
        /* Get the class of the cell we're on */
        let cellClass = classifyCell(cells[pos^]);
        /* Skip all cells of the same class */
        while (pos^ < len && classifyCell(cells[pos^]) == cellClass) {
          pos := pos^ + 1;
        };
        pos^;
      };
    };
  };

  /* Normalize selection so start <= end */
  let normalizeSelection = ((sr, sc, er, ec): selection): selection =>
    if (sr < er || sr == er && sc <= ec) {
      (sr, sc, er, ec);
    } else {
      (er, ec, sr, sc);
    };

  /* Delete text in a selection range */
  let deleteSelection = (text: string, sel: selection): (string, int, int) => {
    let (sr, sc, er, ec) = normalizeSelection(sel);
    let lines = textToLines(text);
    let numLines = Array.length(lines);

    if (sr >= numLines) {
      (text, sr, sc);
    } else {
      let startLine = lines[sr];
      let endLine =
        if (er < numLines) {
          lines[er];
        } else {
          "";
        };
      let beforeSel = cellsBefore(startLine, sc);
      let afterSel =
        if (er < numLines && ec < cellCount(endLine)) {
          cellsFrom(endLine, ec);
        } else {
          "";
        };

      let newLines = Array.make(sr + 1 + (numLines - er - 1), "");
      for (i in 0 to sr - 1) {
        newLines[i] = lines[i];
      };
      newLines[sr] = beforeSel ++ afterSel;
      for (i in er + 1 to numLines - 1) {
        newLines[sr + 1 + (i - er - 1)] = lines[i];
      };

      /* Rebuild lines array correctly */
      let resultLines =
        if (sr == er) {
          /* Single line deletion */
          let result = Array.copy(lines);
          result[sr] = beforeSel ++ afterSel;
          result;
        } else {
          /* Multi-line deletion */
          let beforeLines = Array.sub(lines, 0, sr);
          let afterLines =
            if (er + 1 < numLines) {
              Array.sub(lines, er + 1, numLines - er - 1);
            } else {
              [||];
            };
          Array.concat([beforeLines, [|beforeSel ++ afterSel|], afterLines]);
        };

      (linesToText(resultLines), sr, sc);
    };
  };

  /* Insert text at a position */
  let insertAt =
      (text: string, row: int, col: int, toInsert: string)
      : (string, int, int) => {
    let lines = textToLines(text);
    let numLines = Array.length(lines);
    let safeRow = max(0, min(row, numLines - 1));
    let line =
      if (safeRow < numLines) {
        lines[safeRow];
      } else {
        "";
      };
    let safeCol = max(0, min(col, cellCount(line)));

    let before = cellsBefore(line, safeCol);
    let after = cellsFrom(line, safeCol);

    let insertedLines = textToLines(toInsert);
    let numInserted = Array.length(insertedLines);

    if (numInserted == 1) {
      /* Single line insert */
      lines[safeRow] = before ++ insertedLines[0] ++ after;
      (linesToText(lines), safeRow, safeCol + cellCount(insertedLines[0]));
    } else {
      /* Multi-line insert */
      let beforeLines = Array.sub(lines, 0, safeRow);
      let afterLines =
        if (safeRow + 1 < numLines) {
          Array.sub(lines, safeRow + 1, numLines - safeRow - 1);
        } else {
          [||];
        };

      let firstLine = before ++ insertedLines[0];
      let lastLine = insertedLines[numInserted - 1] ++ after;
      let middleLines =
        if (numInserted > 2) {
          Array.sub(insertedLines, 1, numInserted - 2);
        } else {
          [||];
        };

      let resultLines =
        Array.concat([
          beforeLines,
          [|firstLine|],
          middleLines,
          [|lastLine|],
          afterLines,
        ]);

      let newRow = safeRow + numInserted - 1;
      let newCol = cellCount(insertedLines[numInserted - 1]);
      (linesToText(resultLines), newRow, newCol);
    };
  };

  /* ============================================================================
   * Soft wrapping - the display mapping
   *
   * Editing stays LOGICAL: handleKeyDown, the cursor and both ends of a
   * selection address a logical row and a CELL column of that row, and none
   * of that changes here. Only the DISPLAY wraps. A logical line too wide for
   * the box is cut into consecutive display rows, each one a slice of that
   * line's cells, and the box grows downward (between minHeight and
   * maxHeight) instead of overflowing its layout slot.
   * ============================================================================ */

  /* Greedy hard wrap of one logical line into (startCell, cellCount) segments
   * by DISPLAY columns.
   *
   * Cells are accumulated while their widths fit in [width] columns; a cell
   * that would overflow starts the next segment, so a width-2 ideograph never
   * straddles a boundary. A single cell wider than the whole box still gets a
   * segment of its own - making progress beats the rule. [width <= 0] reads
   * as 1.
   *
   * An empty line yields one empty segment [(0, 0)]: it is still a row.
   *
   * EXACT MULTIPLE: when the cells fill the last segment with no spare
   * column, one extra empty segment [(len, 0)] is appended. That row is where
   * an end-of-line cursor lives - type the character that fills a row and the
   * cursor shows at the start of the next one, the way readline behaves.
   */
  let wrapSegments =
      (cells: array(TextWidth.cell), width: int): list((int, int)) => {
    let w = width <= 0 ? 1 : width;
    let n = Array.length(cells);
    if (n == 0) {
      [(0, 0)];
    } else {
      let segs = ref([]);
      let start = ref(0);
      let used = ref(0);
      for (i in 0 to n - 1) {
        let cw = cells[i].TextWidth.width;
        if (used^ + cw > w && i > start^) {
          segs := [(start^, i - start^), ...segs^];
          start := i;
          used := cw;
        } else {
          used := used^ + cw;
        };
      };
      segs := [(start^, n - start^), ...segs^];
      /* No spare column left on the last segment - give the end-of-line
       * cursor a row of its own. */
      if (used^ >= w) {
        segs := [(n, 0), ...segs^];
      };
      List.rev(segs^);
    };
  };

  /* Every display row of [text] at [width], top to bottom, as
   * (logicalRow, startCell, cellCount). */
  let displayRows = (text: string, width: int): list((int, int, int)) => {
    let lines = textToLines(text);
    List.init(Array.length(lines), i => i)
    |> List.concat_map(i =>
         wrapSegments(lineCells(lines[i]), width)
         |> List.map(((s, c)) => (i, s, c))
       );
  };

  /* Index into [rows] of the display row the cursor sits on: the segment of
   * [cursorRow] whose cells contain [cursorCol], or - when the cursor is at
   * or past the end of its line - that line's LAST segment, where it sits in
   * the padding exactly as it does unwrapped. */
  let cursorDisplayRow =
      (rows: list((int, int, int)), cursorRow: int, cursorCol: int): int => {
    let rec loop = (i, best, rest) =>
      switch (rest) {
      | [] => best
      | [(lr, s, c), ...tl] =>
        if (lr != cursorRow) {
          loop(i + 1, best, tl);
        } else if (cursorCol >= s && cursorCol < s + c) {
          i;
        } else {
          loop(i + 1, i, tl);
        }
      };
    loop(0, 0, rows);
  };

  /* The text of one segment - the bytes of cells [startCell ..
   * startCell + count - 1]. */
  let segmentText =
      (cells: array(TextWidth.cell), startCell: int, count: int): string => {
    let buf = Buffer.create(count * 2 + 1);
    for (k in 0 to count - 1) {
      Buffer.add_string(buf, cells[startCell + k].TextWidth.bytes);
    };
    Buffer.contents(buf);
  };

  /* ============================================================================
   * Rendering helpers
   * ============================================================================ */

  /* Check if a position is within a selection */
  let isInSelection = (row: int, col: int, sel: option(selection)): bool => {
    switch (sel) {
    | None => false
    | Some(s) =>
      let (sr, sc, er, ec) = normalizeSelection(s);
      if (row < sr || row > er) {
        false;
      } else if (row == sr && row == er) {
        col >= sc && col < ec;
      } else if (row == sr) {
        col >= sc;
      } else if (row == er) {
        col < ec;
      } else {
        true;
      };
    };
  };

  /* Render ONE DISPLAY ROW - the slice [startCell .. startCell + count - 1]
   * of the cells of logical row [row] - with cursor and selection
   * highlighting.
   *
   * The slice is walked CELL by cell and every cell is emitted with its
   * ABSOLUTE cell index, so the cursor test and isInSelection are asked the
   * same (logical row, cell column) question they were asked before wrapping
   * existed: a selection that spans a wrap boundary simply highlights cells
   * on both display rows, with no special case. The cursor and the selection
   * highlight always wrap a whole cell, so a width-2 ideograph is highlighted
   * across both of its columns and a combining mark stays inside the
   * highlight of the character it belongs to.
   *
   * After the last cell the row is padded with spaces up to [width] display
   * columns. With [~padAsCells=true] the padding continues the absolute cell
   * numbering, so a cursor or a selection reaching past the end of the line
   * is still drawn there - that is what the LAST segment of a line wants. A
   * segment with a segment after it must not do that (its padding columns
   * belong to the next row's first cell, which would then be drawn twice), so
   * it pads with plain spaces.
   *
   * [~cursorVisible=false] paints no cursor cell at all - the character
   * under the cursor renders as ordinary text. That is the "off" half of
   * the blink owned by the TextArea COMPONENT (lib/TextArea.re); this
   * module is pure and has no clock of its own. Selection highlighting is
   * unaffected by the flag. */
  let renderSegment =
      (
        ~cursorVisible: bool=true,
        ~padAsCells: bool=true,
        cells: array(TextWidth.cell),
        startCell: int,
        count: int,
        row: int,
        cursorRow: int,
        cursorCol: int,
        sel: option(selection),
        width: int,
        cursorBgColor: color,
        selBgColor: color,
      )
      : string => {
    let buf = Buffer.create(count * 2 + width);

    /* Emit one cell (or one trailing pad space) at cell column [col]. */
    let emit = (col: int, c: string) => {
      let isCursor = cursorVisible && row == cursorRow && col == cursorCol;
      let isSelected = isInSelection(row, col, sel);

      if (isCursor) {
        /* Cursor - show with inverted colors */
        Buffer.add_string(buf, styleToAnsi(BgColor(cursorBgColor)));
        Buffer.add_string(buf, styleToAnsi(FgColor(Black)));
        Buffer.add_string(buf, c);
        Buffer.add_string(buf, resetAnsi);
      } else if (isSelected) {
        /* Selection - show with background color */
        Buffer.add_string(buf, styleToAnsi(BgColor(selBgColor)));
        Buffer.add_string(buf, c);
        Buffer.add_string(buf, resetAnsi);
      } else {
        Buffer.add_string(buf, c);
      };
    };

    /* Process each cell of the slice, tracking the columns it consumed */
    let used = ref(0);
    for (k in 0 to count - 1) {
      let cell = cells[startCell + k];
      emit(startCell + k, cell.TextWidth.bytes);
      used := used^ + cell.TextWidth.width;
    };

    /* Pad out to [width] columns. On a line's last segment the padding keeps
     * counting cells, so the cursor (or a selection running past the line
     * end) is drawn there. */
    for (k in 0 to width - used^ - 1) {
      if (padAsCells) {
        emit(startCell + count + k, " ");
      } else {
        Buffer.add_string(buf, " ");
      };
    };

    Buffer.contents(buf);
  };

  /* Render a whole logical line as one row - the unwrapped case, and the
   * shape this renderer had before wrapping existed. */
  let renderLine =
      (
        ~cursorVisible: bool=true,
        line: string,
        row: int,
        cursorRow: int,
        cursorCol: int,
        sel: option(selection),
        width: int,
        cursorBgColor: color,
        selBgColor: color,
      )
      : string => {
    let cells = lineCells(line);
    renderSegment(
      ~cursorVisible,
      cells,
      0,
      Array.length(cells),
      row,
      cursorRow,
      cursorCol,
      sel,
      width,
      cursorBgColor,
      selBgColor,
    );
  };

  /* The first placeholder line, with the cursor block sitting on its first
   * display cell.
   *
   * An empty value has no text to put a cursor in, so the cursor borrows the
   * placeholder's first cell: that cell keeps its own character and only
   * gains a background, and the rest of the line stays dim. The frame
   * therefore reads the same after stripAnsi as a plain dim placeholder -
   * only the styling differs. A placeholder line with no cells at all (an
   * empty or blank placeholder) gets a single space to put the block on.
   *
   * padToWidth is applied to the whole composed string because it is
   * ANSI-aware; padding each piece separately would count escape bytes as
   * columns. */
  let placeholderCursorLine =
      (line: string, width: int, cursorBgColor: color): string => {
    let cells = TextWidth.toCells(line);
    let (firstCell, rest) =
      if (Array.length(cells) == 0) {
        (" ", "");
      } else {
        let bytes = cells[0].TextWidth.bytes;
        let n = String.length(bytes);
        (bytes, String.sub(line, n, String.length(line) - n));
      };
    padToWidth(
      styleToAnsi(BgColor(cursorBgColor))
      ++ firstCell
      ++ resetAnsi
      ++ styleToAnsi(Dim)
      ++ rest
      ++ resetAnsi,
      width,
    );
  };

  /* ============================================================================
   * Main make function - creates the component
   * ============================================================================ */

  /* Default height bounds and width. [make] and [measure] share them: a
   * container that measured with one set of defaults and a renderer that
   * painted with another would disagree about how many rows the box has. */
  let defaultMinHeight = 2;
  let defaultMaxHeight = 100;
  let defaultMaxWidth = 80;

  /* The height in rows that <TextArea> will render for this value at this
   * width - what a container needs to size itself around a growing input.
   * Pure; same wrapping and clamping as the renderer.
   *
   * The empty value measures like the empty value RENDERS - one row, clamped
   * to [minHeight] - whatever the placeholder is: the placeholder only ever
   * fills rows the value already earned. */
  let measure =
      (
        ~value: string,
        ~maxWidth: int,
        ~minHeight: int=defaultMinHeight,
        ~maxHeight: int=defaultMaxHeight,
        (),
      )
      : int => {
    let totalRows = max(1, List.length(displayRows(value, maxWidth)));
    max(minHeight, min(maxHeight, totalRows));
  };

  let make =
      (
        ~cursorVisible: bool=true,
        ~value: string,
        ~onChange as _onChange: string => unit,
        ~onSubmit as _onSubmit: option(unit => unit)=?,
        ~placeholder: option(string)=?,
        ~maxWidth: option(int)=?,
        ~maxHeight: option(int)=?,
        ~minHeight: option(int)=?,
        ~cursorColor: option(color)=?,
        ~selectionColor: option(color)=?,
        ~cursorRow: int,
        ~cursorCol: int,
        ~setCursor as _setCursor: ((int, int)) => unit,
        ~selection: option(selection),
        ~setSelection as _setSelection: option(selection) => unit,
        (),
      )
      : t => {
    /* Default colors */
    let cursorBg =
      switch (cursorColor) {
      | Some(c) => c
      | None => White
      };
    let selBg =
      switch (selectionColor) {
      | Some(c) => c
      | None => BrightBlack
      };

    /* Calculate dimensions */
    let minH =
      switch (minHeight) {
      | Some(h) => h
      | None => defaultMinHeight
      };
    let maxH =
      switch (maxHeight) {
      | Some(h) => h
      | None => defaultMaxHeight
      };
    let maxW =
      switch (maxWidth) {
      | Some(w) => w
      | None => defaultMaxWidth
      };

    /* The box is as tall as the wrapped VALUE, clamped - which is exactly
     * what [measure] reports, so a container can size itself around this. */
    let rows = displayRows(value, maxW);
    let totalRows = max(1, List.length(rows));
    let height = max(minH, min(maxH, totalRows));

    let isEmpty = String.length(value) == 0;

    /* Render each visible display row */
    let renderedLines =
      if (isEmpty) {
        /* The placeholder goes through the same wrapping, so a long one
         * cannot overflow either - but it only fills rows the (empty) value
         * already earned, which is what keeps [measure] honest. */
        let ph =
          switch (placeholder) {
          | Some(p) => p
          | None => ""
          };
        let phLines = textToLines(ph);
        let phRows = Array.of_list(displayRows(ph, maxW));
        Array.init(min(height, Array.length(phRows)), i => {
          let (lr, s, c) = phRows[i];
          let text = segmentText(lineCells(phLines[lr]), s, c);
          if (cursorVisible && i == 0) {
            /* Placeholder, first display row - dimmed behind a cursor block */
            placeholderCursorLine(text, maxW, cursorBg);
          } else {
            /* Placeholder - render dimmed */
            styleToAnsi(Dim) ++ padToWidth(text, maxW) ++ resetAnsi;
          };
        });
      } else {
        let lines = textToLines(value);
        let rowsArr = Array.of_list(rows);
        /* Taller than the box: show the window that ENDS on the cursor's
         * display row. Deterministic and stateless - this renderer is pure,
         * so there is no scroll position to keep - and the cursor is always
         * visible, pinned to the last row once the content no longer fits. */
        let cursorDR = cursorDisplayRow(rows, cursorRow, cursorCol);
        let offset =
          totalRows > height ? max(0, cursorDR - height + 1) : 0;
        Array.init(min(height, totalRows - offset), k => {
          let (lr, s, c) = rowsArr[offset + k];
          let cells = lineCells(lines[lr]);
          /* Only a line's LAST segment owns the columns past its content;
           * see renderSegment's ~padAsCells. */
          let isLastSegment = s + c >= Array.length(cells);
          renderSegment(
            ~cursorVisible,
            ~padAsCells=isLastSegment,
            cells,
            s,
            c,
            lr,
            cursorRow,
            cursorCol,
            selection,
            maxW,
            cursorBg,
            selBg,
          );
        });
      };

    /* Pad to minimum height */
    let paddedLines =
      if (Array.length(renderedLines) < minH) {
        let extra =
          Array.make(
            minH - Array.length(renderedLines),
            String.make(maxW, ' '),
          );
        Array.append(renderedLines, extra);
      } else {
        renderedLines;
      };

    /* Join lines */
    let content = String.concat("\n", Array.to_list(paddedLines));
    Text(content);
  };

  /* ============================================================================
   * Keyboard event handler - processes input and updates state
   * ============================================================================ */

  let handleKeyDown =
      (
        key: Key.t,
        modifiers: Key.modifiers,
        value: string,
        onChange: string => unit,
        onSubmit: option(unit => unit),
        cursorRow: int,
        cursorCol: int,
        setCursor: ((int, int)) => unit,
        selection: option(selection),
        setSelection: option(selection) => unit,
      )
      : unit => {
    let lines = textToLines(value);
    let numLines = max(1, Array.length(lines));
    let currentLine =
      if (cursorRow < numLines) {
        lines[cursorRow];
      } else {
        "";
      };
    /* Line length in CELLS - every column below is a cell column */
    let lineLen = cellCount(currentLine);

    /* Helper to extend or start selection */
    let extendSelection = (newRow: int, newCol: int) =>
      switch (selection) {
      | None => Some((cursorRow, cursorCol, newRow, newCol))
      | Some((sr, sc, _, _)) => Some((sr, sc, newRow, newCol))
      };

    /* Helper to clear selection and move cursor */
    let moveCursor = (newRow: int, newCol: int) => {
      setSelection(None);
      setCursor((newRow, newCol));
    };

    /* Helper to move cursor, optionally extending selection */
    let moveWithSelection = (newRow: int, newCol: int) =>
      if (modifiers.shift) {
        setSelection(extendSelection(newRow, newCol));
        setCursor((newRow, newCol));
      } else {
        moveCursor(newRow, newCol);
      };

    switch (key, modifiers) {
    /* Submit: Cmd+Enter */
    | (Key.Enter, mods) when mods.meta =>
      switch (onSubmit) {
      | Some(submit) => submit()
      | None => ()
      }

    /* Insert newline: Enter */
    | (Key.Enter, _) =>
      let (newText, newRow, newCol) =
        switch (selection) {
        | Some(sel) =>
          let (text', r, col) = deleteSelection(value, sel);
          insertAt(text', r, col, "\n");
        | None => insertAt(value, cursorRow, cursorCol, "\n")
        };
      setSelection(None);
      onChange(newText);
      setCursor((newRow, newCol));

    /* Move to line start: Cmd+Left */
    | (Key.Arrow_left, mods) when mods.meta =>
      if (mods.shift) {
        setSelection(extendSelection(cursorRow, 0));
        setCursor((cursorRow, 0));
      } else {
        moveCursor(cursorRow, 0);
      }

    /* Move to line start: Home */
    | (Key.Home, mods) =>
      if (mods.shift) {
        setSelection(extendSelection(cursorRow, 0));
        setCursor((cursorRow, 0));
      } else {
        moveCursor(cursorRow, 0);
      }

    /* Move to line end: Cmd+Right */
    | (Key.Arrow_right, mods) when mods.meta =>
      if (mods.shift) {
        setSelection(extendSelection(cursorRow, lineLen));
        setCursor((cursorRow, lineLen));
      } else {
        moveCursor(cursorRow, lineLen);
      }

    /* Move to line end: End */
    | (Key.End, mods) =>
      if (mods.shift) {
        setSelection(extendSelection(cursorRow, lineLen));
        setCursor((cursorRow, lineLen));
      } else {
        moveCursor(cursorRow, lineLen);
      }

    /* Move to document start: Cmd+Up */
    | (Key.Arrow_up, mods) when mods.meta =>
      if (mods.shift) {
        setSelection(extendSelection(0, 0));
        setCursor((0, 0));
      } else {
        moveCursor(0, 0);
      }

    /* Move to document end: Cmd+Down */
    | (Key.Arrow_down, mods) when mods.meta =>
      let lastRow = numLines - 1;
      let lastCol =
        cellCount(
          if (lastRow < numLines) {
            lines[lastRow];
          } else {
            "";
          },
        );
      if (mods.shift) {
        setSelection(extendSelection(lastRow, lastCol));
        setCursor((lastRow, lastCol));
      } else {
        moveCursor(lastRow, lastCol);
      };

    /* Move by word left: Alt+Left */
    | (Key.Arrow_left, mods) when mods.alt =>
      let newCol = findPrevWordStart(currentLine, cursorCol);
      if (mods.shift) {
        setSelection(extendSelection(cursorRow, newCol));
        setCursor((cursorRow, newCol));
      } else {
        moveCursor(cursorRow, newCol);
      };

    /* Move by word right: Alt+Right */
    | (Key.Arrow_right, mods) when mods.alt =>
      let newCol = findNextWordEnd(currentLine, cursorCol);
      if (mods.shift) {
        setSelection(extendSelection(cursorRow, newCol));
        setCursor((cursorRow, newCol));
      } else {
        moveCursor(cursorRow, newCol);
      };

    /* Move left */
    | (Key.Arrow_left, _) =>
      let (newRow, newCol) =
        if (cursorCol > 0) {
          (cursorRow, cursorCol - 1);
        } else if (cursorRow > 0) {
          let prevLine = lines[cursorRow - 1];
          (cursorRow - 1, cellCount(prevLine));
        } else {
          (cursorRow, cursorCol);
        };
      moveWithSelection(newRow, newCol);

    /* Move right */
    | (Key.Arrow_right, _) =>
      let (newRow, newCol) =
        if (cursorCol < lineLen) {
          (cursorRow, cursorCol + 1);
        } else if (cursorRow < numLines - 1) {
          (cursorRow + 1, 0);
        } else {
          (cursorRow, cursorCol);
        };
      moveWithSelection(newRow, newCol);

    /* Move up */
    | (Key.Arrow_up, _) =>
      if (cursorRow > 0) {
        let prevLine = lines[cursorRow - 1];
        let newCol = min(cursorCol, cellCount(prevLine));
        moveWithSelection(cursorRow - 1, newCol);
      }

    /* Move down */
    | (Key.Arrow_down, _) =>
      if (cursorRow < numLines - 1) {
        let nextLine = lines[cursorRow + 1];
        let newCol = min(cursorCol, cellCount(nextLine));
        moveWithSelection(cursorRow + 1, newCol);
      }

    /* Delete to line start: Cmd+Backspace */
    | (Key.Backspace, mods) when mods.meta =>
      let after = cellsFrom(currentLine, cursorCol);
      lines[cursorRow] = after;
      onChange(linesToText(lines));
      setCursor((cursorRow, 0));

    /* Delete previous word: Alt+Backspace */
    | (Key.Backspace, mods) when mods.alt =>
      let wordStart = findPrevWordStart(currentLine, cursorCol);
      let before = cellsBefore(currentLine, wordStart);
      let after = cellsFrom(currentLine, cursorCol);
      lines[cursorRow] = before ++ after;
      onChange(linesToText(lines));
      setCursor((cursorRow, wordStart));

    /* Backspace */
    | (Key.Backspace, _) =>
      switch (selection) {
      | Some(sel) =>
        let (newText, newRow, newCol) = deleteSelection(value, sel);
        setSelection(None);
        onChange(newText);
        setCursor((newRow, newCol));
      | None =>
        if (cursorCol > 0) {
          /* Deletes one whole cell: a codepoint plus its combining marks */
          let before = cellsBefore(currentLine, cursorCol - 1);
          let after = cellsFrom(currentLine, cursorCol);
          lines[cursorRow] = before ++ after;
          onChange(linesToText(lines));
          setCursor((cursorRow, cursorCol - 1));
        } else if (cursorRow > 0) {
          /* Join with previous line */
          let prevLine = lines[cursorRow - 1];
          let prevLen = cellCount(prevLine);
          let newLines =
            Array.concat([
              Array.sub(lines, 0, cursorRow - 1),
              [|prevLine ++ currentLine|],
              if (cursorRow + 1 < numLines) {
                Array.sub(lines, cursorRow + 1, numLines - cursorRow - 1);
              } else {
                [||];
              },
            ]);
          onChange(linesToText(newLines));
          setCursor((cursorRow - 1, prevLen));
        }
      }

    /* Delete forward */
    | (Key.Delete, _) =>
      switch (selection) {
      | Some(sel) =>
        let (newText, newRow, newCol) = deleteSelection(value, sel);
        setSelection(None);
        onChange(newText);
        setCursor((newRow, newCol));
      | None =>
        if (cursorCol < lineLen) {
          let before = cellsBefore(currentLine, cursorCol);
          let after = cellsFrom(currentLine, cursorCol + 1);
          lines[cursorRow] = before ++ after;
          onChange(linesToText(lines));
        } else if (cursorRow < numLines - 1) {
          /* Join with next line */
          let nextLine = lines[cursorRow + 1];
          let newLines =
            Array.concat([
              Array.sub(lines, 0, cursorRow),
              [|currentLine ++ nextLine|],
              if (cursorRow + 2 < numLines) {
                Array.sub(lines, cursorRow + 2, numLines - cursorRow - 2);
              } else {
                [||];
              },
            ]);
          onChange(linesToText(newLines));
        }
      }

    /* Kill line: Ctrl+U */
    | (Key.KillLine, _) =>
      lines[cursorRow] = "";
      onChange(linesToText(lines));
      setCursor((cursorRow, 0));

    /* Kill word: Ctrl+W */
    | (Key.KillWord, _) =>
      let wordStart = findPrevWordStart(currentLine, cursorCol);
      let before = cellsBefore(currentLine, wordStart);
      let after = cellsFrom(currentLine, cursorCol);
      lines[cursorRow] = before ++ after;
      onChange(linesToText(lines));
      setCursor((cursorRow, wordStart));

    /* Multi-byte character input: one complete UTF-8 codepoint */
    | (Key.Text(s), mods) when !mods.ctrl && !mods.alt && !mods.meta =>
      let (newText, newRow, newCol) =
        switch (selection) {
        | Some(sel) =>
          let (text', r, col) = deleteSelection(value, sel);
          insertAt(text', r, col, s);
        | None => insertAt(value, cursorRow, cursorCol, s)
        };
      setSelection(None);
      onChange(newText);
      setCursor((newRow, newCol));

    /* Character input */
    | (Key.Char(ch), mods) when !mods.ctrl && !mods.alt && !mods.meta =>
      let charStr = String.make(1, ch);
      let (newText, newRow, newCol) =
        switch (selection) {
        | Some(sel) =>
          let (text', r, col) = deleteSelection(value, sel);
          insertAt(text', r, col, charStr);
        | None => insertAt(value, cursorRow, cursorCol, charStr)
        };
      setSelection(None);
      onChange(newText);
      setCursor((newRow, newCol));

    /* Tab - insert spaces */
    | (Key.Tab, _) =>
      let (newText, newRow, newCol) =
        switch (selection) {
        | Some(sel) =>
          let (text', r, col) = deleteSelection(value, sel);
          insertAt(text', r, col, "  ");
        | None => insertAt(value, cursorRow, cursorCol, "  ")
        };
      setSelection(None);
      onChange(newText);
      setCursor((newRow, newCol));

    | _ => ()
    };
  };

  /* JSX-compatible createElement */
  let createElement =
      (
        ~value: string,
        ~onChange: string => unit,
        ~onSubmit: option(unit => unit)=?,
        ~placeholder: option(string)=?,
        ~maxWidth: option(int)=?,
        ~maxHeight: option(int)=?,
        ~minHeight: option(int)=?,
        ~cursorColor: option(color)=?,
        ~selectionColor: option(color)=?,
        ~cursorRow: int,
        ~cursorCol: int,
        ~setCursor: ((int, int)) => unit,
        ~selection: option(selection),
        ~setSelection: option(selection) => unit,
        (),
      ) =>
    Lazy(
      () =>
        make(
          ~value,
          ~onChange,
          ~onSubmit?,
          ~placeholder?,
          ~maxWidth?,
          ~maxHeight?,
          ~minHeight?,
          ~cursorColor?,
          ~selectionColor?,
          ~cursorRow,
          ~cursorCol,
          ~setCursor,
          ~selection,
          ~setSelection,
          (),
        ),
    );
};
