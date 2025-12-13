/*
 * Element - Terminal UI element tree and rendering
 *
 * This module provides the core element types for building terminal UIs.
 * Elements form a tree structure that gets rendered to ANSI-escaped strings.
 *
 * Element Types:
 * - Text(string): Plain text content
 * - Styled(style, t): Apply ANSI styling to an element
 * - Column(list(t)): Stack children vertically (newline separated)
 * - Row(list(t)): Stack children horizontally (concatenated)
 * - Box(t, width, height, padding): Fixed-size container with padding
 * - Lazy(unit => t): Deferred element (used by components)
 * - WithContext(setup, teardown, t): Context boundary for providers
 * - Empty: No content
 *
 * JSX Components:
 * This module also exports JSX-compatible component modules:
 * Text, Bold, Dim, Italic, Underline, Inverted, Column, Row, Box, Fragment
 */

/* Text styling options.
 * These map to ANSI escape codes for terminal formatting.
 */
type style =
  | Bold /* Bold/bright text */
  | Dim /* Dimmed/faint text */
  | Italic /* Italic text (terminal support varies) */
  | Underline /* Underlined text */
  | Inverted /* Inverted foreground/background colors */
  | FgColor(int) /* 256-color foreground (0-255) */
  | BgColor(int); /* 256-color background (0-255) */

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
  | Column(list(t)) /* Vertical stack (newline-separated) */
  | Row(list(t)) /* Horizontal stack (concatenated) */
  | Empty /* Empty element (renders to "") */
  | Lazy(unit => t) /* Deferred element - thunk is called during render */
  | Component(
      componentId,
      Obj.t,
      unit => t,
      ref(option(string)),
      ref(option(componentId)),
    ) /* Component instance: (id, props, renderFn, cachedOutput, stableIdRef) */
  | Box(t, int, int, int) /* Box(content, width, height, padding) */
  | WithContext(unit => unit, unit => unit, t); /* Context boundary: (setup, teardown, children) */

/* ============================================================================
 * Element Constructors
 * ============================================================================ */

/* Create a text element */
let text = (s: string): t => Text(s);

/* Create a styled element */
let styled = (style: style, el: t): t => Styled(style, el);

/* Create a vertical column of elements */
let column = (children: list(t)): t => Column(children);

/* Create a horizontal row of elements */
let row = (children: list(t)): t => Row(children);

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
 */
let createComponent = (props: 'a, renderFn: unit => t): t => {
  let propsObj = Obj.repr(props);
  /* ID will be assigned by Runtime based on position in tree */
  let id = (-1); /* Temporary ID, will be replaced during render */
  let cachedOutput = ref(None);
  let stableIdRef = ref(None); /* Store stable ID across renders */
  Component(id, propsObj, renderFn, cachedOutput, stableIdRef);
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

/* Convert a style to its ANSI escape code. */
let styleToAnsi = (style: style): string => {
  switch (style) {
  | Bold => "\027[1m"
  | Dim => "\027[2m"
  | Italic => "\027[3m"
  | Underline => "\027[4m"
  | Inverted => "\027[7m"
  | FgColor(c) => Printf.sprintf("\027[38;5;%dm", c)
  | BgColor(c) => Printf.sprintf("\027[48;5;%dm", c)
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
 * This is the main rendering function that converts the element tree
 * into an ANSI-escaped string suitable for terminal output.
 *
 * Rendering behavior by element type:
 * - Text(s) -> the string as-is
 * - Styled(style, child) -> ANSI code + render(child) + reset
 * - Column(children) -> children joined with newlines
 * - Row(children) -> children concatenated
 * - Lazy(f) -> render(f()) - forces the thunk
 * - Component(id, f, cache) -> checks if component needs re-render, uses cache if not
 * - WithContext(setup, teardown, children) -> setup(); render(children); teardown()
 * - Box(...) -> formatted box with padding and size constraints
 * - Empty -> empty string
 */
let rec render = (el: t): string => {
  switch (el) {
  | Empty => ""
  | Text(s) => s
  | Styled(style, child) => styleToAnsi(style) ++ render(child) ++ resetAnsi
  | Column(children) => children |> List.map(render) |> String.concat("\n")
  | Row(children) => children |> List.map(render) |> String.concat("")
  | Lazy(f) => render(f())
  | Component(_id, _props, renderFn, cachedOutput, _stableIdRef) =>
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
  | Box(content, width, height, padding) =>
    /* Calculate inner dimensions */
    let innerWidth = width - padding * 2;
    let innerHeight = height - padding * 2;

    /* Render content and split into lines */
    let contentLines = splitLines(render(content));

    /* Pad lines list to fill height */
    let padLines = (lines, targetHeight) => {
      let len = List.length(lines);
      if (len >= targetHeight) {
        let rec take = (n, lst) =>
          switch (n, lst) {
          | (0, _) => []
          | (_, []) => []
          | (n, [h, ...t]) => [h, ...take(n - 1, t)]
          };
        take(targetHeight, lines);
      } else {
        lines @ List.init(targetHeight - len, _ => "");
      };
    };

    let paddedLines = padLines(contentLines, innerHeight);

    /* Add horizontal padding and constrain width */
    let hPad = String.make(padding, ' ');
    let formattedLines =
      paddedLines
      |> List.map(line =>
           hPad ++ padToWidth(line, innerWidth) ++ resetAnsi ++ hPad
         );

    /* Add vertical padding */
    let emptyLine = String.make(width, ' ');
    let vPadLines = List.init(padding, _ => emptyLine);

    String.concat("\n", vPadLines @ formattedLines @ vPadLines);
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
 *   <Text fgColor=196> "red text" </Text>
 */
module Text = {
  type props = {
    children: string,
    bold: bool,
    dim: bool,
    italic: bool,
    underline: bool,
    inverted: bool,
    fgColor: option(int),
    bgColor: option(int),
  };

  /* Default props for simple <Text> usage */
  let defaultProps = {
    children: "",
    bold: false,
    dim: false,
    italic: false,
    underline: false,
    inverted: false,
    fgColor: None,
    bgColor: None,
  };

  let make = props => {
    let el = ref(Text(props.children));

    /* Apply styles in reverse order so they nest correctly */
    switch (props.bgColor) {
    | Some(c) => el := Styled(BgColor(c), el^)
    | None => ()
    };
    switch (props.fgColor) {
    | Some(c) => el := Styled(FgColor(c), el^)
    | None => ()
    };
    if (props.inverted) {
      el := Styled(Inverted, el^);
    };
    if (props.underline) {
      el := Styled(Underline, el^);
    };
    if (props.italic) {
      el := Styled(Italic, el^);
    };
    if (props.dim) {
      el := Styled(Dim, el^);
    };
    if (props.bold) {
      el := Styled(Bold, el^);
    };

    el^;
  };

  /* createElement using labeled args with defaults for JSX compatibility */
  let createElement =
      (
        ~bold=false,
        ~dim=false,
        ~italic=false,
        ~underline=false,
        ~inverted=false,
        ~fgColor=?,
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
          fgColor,
          bgColor,
        }),
    );
};

/* Bold component - renders children in bold */
module Bold = {
  let make = (~children, ()) => Styled(Bold, children);
  let createElement = (~children, ()) => Lazy(() => make(~children, ()));
};

/* Dim component - renders children dimmed */
module Dim = {
  let make = (~children, ()) => Styled(Dim, children);
  let createElement = (~children, ()) => Lazy(() => make(~children, ()));
};

/* Italic component - renders children italic */
module Italic = {
  let make = (~children, ()) => Styled(Italic, children);
  let createElement = (~children, ()) => Lazy(() => make(~children, ()));
};

/* Underline component - renders children underlined */
module Underline = {
  let make = (~children, ()) => Styled(Underline, children);
  let createElement = (~children, ()) => Lazy(() => make(~children, ()));
};

/* Inverted component - renders children with inverted colors */
module Inverted = {
  let make = (~children, ()) => Styled(Inverted, children);
  let createElement = (~children, ()) => Lazy(() => make(~children, ()));
};

/* Column component - stacks children vertically */
module Column = {
  let make = (~children, ()) => Column(children);
  let createElement = (~children, ()) => Lazy(() => make(~children, ()));
};

/* Row component - stacks children horizontally */
module Row = {
  let make = (~children, ()) => Row(children);
  let createElement = (~children, ()) => Lazy(() => make(~children, ()));
};

/* Box component - fixed-size container with optional padding */
module Box = {
  let make = (~children, ~width, ~height, ~padding=0, ()) => {
    Box(children, width, height, padding);
  };
  let createElement = (~children, ~width, ~height, ~padding=0, ()) =>
    Lazy(() => make(~children, ~width, ~height, ~padding, ()));
};

/* Fragment component - groups children without adding structure (renders as Column) */
module Fragment = {
  let make = (~children, ()) => Column(children);
  let createElement = (~children, ()) => Lazy(() => make(~children, ()));
};
