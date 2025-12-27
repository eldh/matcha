/*
 * Element - Terminal UI element tree and rendering
 *
 * This module provides the core element types for building terminal UIs.
 * Elements form a tree structure that gets rendered to ANSI-escaped strings.
 *
 * Element Types:
 * - Text(string): Plain text content
 * - Styled(style, t): Apply ANSI styling to an element
 * - VStack(list(t), gap): Stack children vertically with flex layout
 * - HStack(list(t), gap): Stack children horizontally with flex layout
 * - Sized(t, size): Wrapper to specify size in parent Stack
 * - Lazy(unit => t): Deferred element (used by components)
 * - WithContext(setup, teardown, t): Context boundary for providers
 * - Empty: No content
 *
 * Size Types (for Sized wrapper):
 * - Flex(int): Flex units like CSS flex-grow
 * - Percent(int): Percentage of parent container
 * - Chars(int): Absolute character count
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
  | Rgb(int, int, int); /* RGB values 0-5 each, maps to 216-color cube */

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
  | Flex(int) /* Flex units - like CSS flex-grow */
  | Percent(int) /* Percentage of parent container */
  | Chars(int); /* Absolute character count */

/* Component instance identifier - defined here for Element, but also used by Hooks */
type componentId = int;

/* Counter for generating unique component IDs */
let nextComponentId = ref(0);

/* Generate a new unique component ID */
let generateComponentId = (): componentId => {
  let id = nextComponentId^;
  nextComponentId := id + 1;
  id;
};

/* Element tree type.
 * Represents the structure of a terminal UI before rendering.
 */
type t =
  | Text(string) /* Plain text content */
  | Styled(style, t) /* Apply styling to child element */
  | VStack(list(t), int) /* VStack(children, gap) - vertical flex layout */
  | HStack(list(t), int) /* HStack(children, gap) - horizontal flex layout */
  | Sized(t, size) /* Wrapper to specify size in parent Stack */
  | Empty /* Empty element (renders to "") */
  | Lazy(unit => t) /* Deferred element - thunk is called during render */
  | Component(
      componentId,
      option(string),
      Obj.t,
      unit => t,
      ref(option(string)),
      ref(option(componentId)),
    ) /* Component instance: (id, key, props, renderFn, cachedOutput, stableIdRef) */
  | WithContext(unit => unit, unit => unit, t); /* Context boundary: (setup, teardown, children) */

/* ============================================================================
 * Element Constructors
 * ============================================================================ */

/* Create a text element */
let text = (s: string): t => Text(s);

/* Create a styled element */
let styled = (style: style, el: t): t => Styled(style, el);

/* Create a vertical stack of elements */
let vstack = (~gap=0, children: list(t)): t => VStack(children, gap);

/* Create a horizontal stack of elements */
let hstack = (~gap=0, children: list(t)): t => HStack(children, gap);

/* Wrap an element with a size hint for parent Stack */
let sized = (size: size, el: t): t => Sized(el, size);

/* Empty element constant */
let empty = Empty;

/* Create a lazy element wrapping a render function.
 * This is used by the component system to defer rendering.
 * The thunk is called during Element.render().
 */
let createElement = (render: unit => t): t => Lazy(render);

/* Create a component element with props for selective re-rendering.
 * This is used by the component system to track component instances
 * and enable prop-based memoization.
 * The component ID will be assigned by Runtime based on position in tree.
 * An optional key can be provided to force instance identity changes,
 * mirroring React's key semantics.
 */
let createComponent = (~key=?, props: 'a, renderFn: unit => t): t => {
  let propsObj = Obj.repr(props);
  /* ID will be assigned by Runtime based on position in tree */
  let id = (-1); /* Temporary ID, will be replaced during render */
  let cachedOutput = ref(None);
  let stableIdRef = ref(None); /* Store stable ID across renders */
  Component(id, key, propsObj, renderFn, cachedOutput, stableIdRef);
};

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

/* Convert a color to its 256-color code. */
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
  };
};

/* Convert a style to its ANSI escape code. */
let styleToAnsi = (style: style): string => {
  switch (style) {
  | Bold => "\027[1m"
  | Dim => "\027[2m"
  | Italic => "\027[3m"
  | Underline => "\027[4m"
  | Inverted => "\027[7m"
  | FgColor(c) => Printf.sprintf("\027[38;5;%dm", colorToCode(c))
  | BgColor(c) => Printf.sprintf("\027[48;5;%dm", colorToCode(c))
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

/* Calculate the visible length of a string.
 * Ignores ANSI escape codes and correctly handles multi-byte UTF-8 characters.
 * This is essential for proper terminal layout calculations.
 */
let visibleLength = (s: string): int => {
  let len = String.length(s);
  let rec loop = (i, visible, inEscape) =>
    if (i >= len) {
      visible;
    } else {
      let c = Char.code(s.[i]);
      if (inEscape) {
        /* End of escape sequence when we hit a letter */
        if (c >= 65 && c <= 90 || c >= 97 && c <= 122) {
          loop(i + 1, visible, false);
        } else {
          loop(i + 1, visible, true);
        };
      } else if (c == 27) {
        /* ESC - start of escape sequence */
        loop(i + 1, visible, true);
      } else if (c >= 0x80 && c <= 0xBF) {
        /* UTF-8 continuation byte - don't count */
        loop(
          i + 1,
          visible,
          false,
        );
      } else {
        /* Regular character or UTF-8 start byte */
        loop(
          i + 1,
          visible + 1,
          false,
        );
      };
    };
  loop(0, 0, false);
};

/* Pad or truncate a string to an exact visible width.
 * Handles ANSI escape codes and UTF-8 correctly.
 * If truncating, appends a reset code to prevent style leaking.
 * If padding, uses spaces.
 */
let padToWidth = (s: string, width: int): string => {
  let visible = visibleLength(s);
  if (visible >= width) {
    /* Truncate - need to be careful with ANSI codes and UTF-8 */
    let len = String.length(s);
    let buf = Buffer.create(len);
    let rec loop = (i, visible, inEscape) =>
      if (i >= len || visible >= width) {
        Buffer.add_string(buf, resetAnsi);
      } else {
        let c = Char.code(s.[i]);
        Buffer.add_char(buf, s.[i]);
        if (inEscape) {
          if (c >= 65 && c <= 90 || c >= 97 && c <= 122) {
            loop(i + 1, visible, false);
          } else {
            loop(i + 1, visible, true);
          };
        } else if (c == 27) {
          loop(i + 1, visible, true);
        } else if (c >= 0x80 && c <= 0xBF) {
          loop(i + 1, visible, false);
        } else {
          loop(i + 1, visible + 1, false);
        };
      };
    loop(0, 0, false);
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
 * - Component(id, f, cache) -> uses cached output or renders
 * - WithContext(setup, teardown, children) -> setup(); render(children); teardown()
 * - Empty -> empty string
 */
let rec render = (el: t): string => {
  switch (el) {
  | Empty => ""
  | Text(s) => s
  | Styled(style, child) => styleToAnsi(style) ++ render(child) ++ resetAnsi
  | VStack(children, _gap) =>
    children |> List.map(render) |> String.concat("\n")
  | HStack(children, _gap) =>
    children |> List.map(render) |> String.concat("")
  | Sized(child, _size) =>
    /* Size is handled by Runtime layout; here we just render the child */
    render(child)
  | Lazy(f) => render(f())
  | Component(_id, _key, _props, renderFn, cachedOutput, _stableIdRef) =>
    /* Component rendering is handled by Runtime to avoid circular dependency.
     * For now, always render (Runtime will handle caching and selective re-rendering) */
    switch (cachedOutput^) {
    | Some(cached) => cached
    | None =>
      let result = render(renderFn());
      cachedOutput := Some(result);
      result;
    }
  | WithContext(setup, teardown, children) =>
    setup();
    let result = render(children);
    teardown();
    result;
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
        }),
    );
};

/* VStack component - stacks children vertically with flex layout.
 *
 * Usage:
 *   <VStack> child1 child2 </VStack>
 *   <VStack gap=1> child1 child2 </VStack>
 *
 * Children can be wrapped with <Sized> to specify their size:
 *   <VStack>
 *     <Sized size={Flex(2)}> child1 </Sized>
 *     <Sized size={Percent(30)}> child2 </Sized>
 *     child3  /* defaults to Flex(1) */
 *   </VStack>
 */
module VStack = {
  let make = (~children: list(t), ~gap=0, ()) => VStack(children, gap);
  let createElement = (~children: list(t), ~gap=0, ()) =>
    Lazy(() => make(~children, ~gap, ()));
};

/* HStack component - stacks children horizontally with flex layout.
 *
 * Usage:
 *   <HStack> child1 child2 </HStack>
 *   <HStack gap=2> child1 child2 </HStack>
 *
 * Children can be wrapped with <Sized> to specify their size:
 *   <HStack>
 *     <Sized size={Chars(20)}> sidebar </Sized>
 *     <Sized size={Flex(1)}> content </Sized>
 *   </HStack>
 */
module HStack = {
  let make = (~children: list(t), ~gap=0, ()) => HStack(children, gap);
  let createElement = (~children: list(t), ~gap=0, ()) =>
    Lazy(() => make(~children, ~gap, ()));
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

/* Fragment component - groups children without adding structure */
module Fragment = {
  let make = (~children: list(t), ()) => VStack(children, 0);
  let createElement = (~children: list(t), ()) =>
    Lazy(() => make(~children, ()));
};
