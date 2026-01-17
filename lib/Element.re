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
 * - WithContext(setup, teardown, t): Context boundary for providers
 * - Empty: No content
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
  | VStack(list(t), stackOptions) /* VStack(children, options) - vertical flex layout */
  | HStack(list(t), stackOptions) /* HStack(children, options) - horizontal flex layout */
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
  | VStack(children, _options) =>
    children |> List.map(render) |> String.concat("\n")
  | HStack(children, _options) =>
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

  /* Get the byte index for a (row, col) position in text */
  let positionToIndex = (text: string, row: int, col: int): int => {
    let lines = textToLines(text);
    let rec countBytes = (r, idx) =>
      if (r >= row) {
        idx + min(col, String.length(lines[r]));
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

  /* Check if character is whitespace */
  let isWhitespace = (c: char): bool => c == ' ' || c == '\t';

  /* Find the start of the previous word from a position in a line.
   * Skips backwards over whitespace, then skips the previous "word"
   * (where a word is a sequence of same-class characters).
   */
  let findPrevWordStart = (line: string, col: int): int => {
    let len = String.length(line);
    if (col <= 0 || len == 0) {
      0;
    } else {
      let pos = ref(min(col - 1, len - 1));
      /* Skip whitespace backwards */
      while (pos^ > 0 && isWhitespace(line.[pos^])) {
        pos := pos^ - 1;
      };
      /* If we're at start after skipping whitespace, done */
      if (pos^ <= 0) {
        0;
      } else {
        /* Get the class of the character we're on */
        let charClass = classifyChar(line.[pos^]);
        /* Skip all characters of the same class */
        while (pos^ > 0 && classifyChar(line.[pos^]) == charClass) {
          pos := pos^ - 1;
        };
        /* If we stopped on a different class, move forward one */
        if (pos^ > 0 || classifyChar(line.[0]) != charClass) {
          pos^ + 1;
        } else {
          0;
        };
      };
    };
  };

  /* Find the end of the next word from a position in a line.
   * Skips forward over whitespace, then skips the next "word"
   * (where a word is a sequence of same-class characters).
   */
  let findNextWordEnd = (line: string, col: int): int => {
    let len = String.length(line);
    if (col >= len) {
      len;
    } else {
      let pos = ref(col);
      /* Skip whitespace forward */
      while (pos^ < len && isWhitespace(line.[pos^])) {
        pos := pos^ + 1;
      };
      /* If we're at end after skipping whitespace, done */
      if (pos^ >= len) {
        len;
      } else {
        /* Get the class of the character we're on */
        let charClass = classifyChar(line.[pos^]);
        /* Skip all characters of the same class */
        while (pos^ < len && classifyChar(line.[pos^]) == charClass) {
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
      let beforeSel =
        String.sub(startLine, 0, min(sc, String.length(startLine)));
      let afterSel =
        if (er < numLines && ec < String.length(endLine)) {
          String.sub(endLine, ec, String.length(endLine) - ec);
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
    let safeCol = max(0, min(col, String.length(line)));

    let before = String.sub(line, 0, safeCol);
    let after = String.sub(line, safeCol, String.length(line) - safeCol);

    let insertedLines = textToLines(toInsert);
    let numInserted = Array.length(insertedLines);

    if (numInserted == 1) {
      /* Single line insert */
      lines[safeRow] = before ++ insertedLines[0] ++ after;
      (
        linesToText(lines),
        safeRow,
        safeCol + String.length(insertedLines[0]),
      );
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
      let newCol = String.length(insertedLines[numInserted - 1]);
      (linesToText(resultLines), newRow, newCol);
    };
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

  /* Render a line with cursor and selection highlighting */
  let renderLine =
      (
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
    let len = String.length(line);
    let buf = Buffer.create(len * 2);

    /* Process each character */
    for (col in 0 to max(len, width) - 1) {
      let c =
        if (col < len) {
          String.make(1, line.[col]);
        } else {
          " ";
        };

      let isCursor = row == cursorRow && col == cursorCol;
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

    /* If cursor is at end of line, show it */
    if (row == cursorRow && cursorCol >= len && cursorCol < width) {
      ();
        /* Already handled in loop above when col == cursorCol */
    };

    Buffer.contents(buf);
  };

  /* ============================================================================
   * Main make function - creates the component
   * ============================================================================ */

  let make =
      (
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
    let lines = textToLines(value);
    let numLines = max(1, Array.length(lines));

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
      | None => 2
      };
    let maxH =
      switch (maxHeight) {
      | Some(h) => h
      | None => 100
      };
    let height = max(minH, min(maxH, numLines));

    let maxW =
      switch (maxWidth) {
      | Some(w) => w
      | None => 80
      };

    /* Show placeholder if empty */
    let displayLines =
      if (String.length(value) == 0) {
        switch (placeholder) {
        | Some(p) => textToLines(p)
        | None => [|""|]
        };
      } else {
        lines;
      };

    let isEmpty = String.length(value) == 0;

    /* Render each visible line */
    let renderedLines =
      Array.mapi(
        (i, line) =>
          if (isEmpty) {
            /* Placeholder - render dimmed */
            styleToAnsi(Dim) ++ padToWidth(line, maxW) ++ resetAnsi;
          } else {
            renderLine(
              line,
              i,
              cursorRow,
              cursorCol,
              selection,
              maxW,
              cursorBg,
              selBg,
            );
          },
        Array.sub(displayLines, 0, min(height, Array.length(displayLines))),
      );

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
    let lineLen = String.length(currentLine);

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

    /* Move to line end: Cmd+Right */
    | (Key.Arrow_right, mods) when mods.meta =>
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
        String.length(
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
          (cursorRow - 1, String.length(prevLine));
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
        let newCol = min(cursorCol, String.length(prevLine));
        moveWithSelection(cursorRow - 1, newCol);
      }

    /* Move down */
    | (Key.Arrow_down, _) =>
      if (cursorRow < numLines - 1) {
        let nextLine = lines[cursorRow + 1];
        let newCol = min(cursorCol, String.length(nextLine));
        moveWithSelection(cursorRow + 1, newCol);
      }

    /* Delete to line start: Cmd+Backspace */
    | (Key.Backspace, mods) when mods.meta =>
      let after = String.sub(currentLine, cursorCol, lineLen - cursorCol);
      lines[cursorRow] = after;
      onChange(linesToText(lines));
      setCursor((cursorRow, 0));

    /* Delete previous word: Alt+Backspace */
    | (Key.Backspace, mods) when mods.alt =>
      let wordStart = findPrevWordStart(currentLine, cursorCol);
      let before = String.sub(currentLine, 0, wordStart);
      let after = String.sub(currentLine, cursorCol, lineLen - cursorCol);
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
          let before = String.sub(currentLine, 0, cursorCol - 1);
          let after = String.sub(currentLine, cursorCol, lineLen - cursorCol);
          lines[cursorRow] = before ++ after;
          onChange(linesToText(lines));
          setCursor((cursorRow, cursorCol - 1));
        } else if (cursorRow > 0) {
          /* Join with previous line */
          let prevLine = lines[cursorRow - 1];
          let prevLen = String.length(prevLine);
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
          let before = String.sub(currentLine, 0, cursorCol);
          let after =
            String.sub(currentLine, cursorCol + 1, lineLen - cursorCol - 1);
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
      let before = String.sub(currentLine, 0, wordStart);
      let after = String.sub(currentLine, cursorCol, lineLen - cursorCol);
      lines[cursorRow] = before ++ after;
      onChange(linesToText(lines));
      setCursor((cursorRow, wordStart));

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
