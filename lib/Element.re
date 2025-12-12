type style =
  | Bold
  | Dim
  | Italic
  | Underline
  | Inverted
  | FgColor(int)
  | BgColor(int);

type t =
  | Text(string)
  | Styled(style, t)
  | Column(list(t))
  | Row(list(t))
  | Empty
  | Lazy(unit => t)
  | Box(t, int, int, int) /* content, width, height, padding */
  | WithContext(unit => unit, unit => unit, t); /* setup, teardown, children */

/* Constructors */
let text = (s: string): t => Text(s);
let styled = (style: style, el: t): t => Styled(style, el);
let column = (children: list(t)): t => Column(children);
let row = (children: list(t)): t => Row(children);
let empty = Empty;
let createElement = (render: unit => t): t => Lazy(render);

/* Style helpers */
let bold = (el: t): t => Styled(Bold, el);
let dim = (el: t): t => Styled(Dim, el);
let italic = (el: t): t => Styled(Italic, el);
let underline = (el: t): t => Styled(Underline, el);
let inverted = (el: t): t => Styled(Inverted, el);

/* ANSI codes */
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

let resetAnsi = "\027[0m";

/* String helpers for layout */
let splitLines = (s: string): list(string) => {
  String.split_on_char('\n', s);
};

/* Calculate visible length (ignoring ANSI escape codes and handling UTF-8) */
let visibleLength = (s: string): int => {
  let len = String.length(s);
  let rec loop = (i, visible, inEscape) =>
    if (i >= len) {
      visible;
    } else {
      let c = Char.code(s.[i]);
      if (inEscape) {
        /* End of escape sequence */
        if (c >= 65 && c <= 90 || c >= 97 && c <= 122) {
          /* A-Z or a-z */
          loop(i + 1, visible, false);
        } else {
          loop(i + 1, visible, true);
        };
      } else if (c == 27) {
        /* ESC */
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

/* Pad or truncate string to exact visible width */
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
            /* A-Z or a-z - end of escape */
            loop(i + 1, visible, false);
          } else {
            loop(i + 1, visible, true);
          };
        } else if (c == 27) {
          /* ESC */
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
    Buffer.contents(buf);
  } else {
    /* Pad with spaces - reset styles first to prevent leaking */
    s ++ resetAnsi ++ String.make(width - visible, ' ');
  };
};

/* Repeat a string n times (works with multi-byte UTF-8) */
let repeatString = (s: string, n: int): string => {
  let buf = Buffer.create(String.length(s) * n);
  for (_ in 1 to n) {
    Buffer.add_string(buf, s);
  };
  Buffer.contents(buf);
};

/* Box drawing characters - solid lines (UTF-8) */
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

/* Render element to string */
let rec render = (el: t): string => {
  switch (el) {
  | Empty => ""
  | Text(s) => s
  | Styled(style, child) => styleToAnsi(style) ++ render(child) ++ resetAnsi
  | Column(children) => children |> List.map(render) |> String.concat("\n")
  | Row(children) => children |> List.map(render) |> String.concat("")
  | Lazy(f) => render(f())
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
        /* Take only what fits */
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

/* JSX-compatible component modules */

module Text = {
  type props = {children: string};
  let make = ({children}) => Text(children);
  let createElement = props => Lazy(() => make(props));
};

module Bold = {
  type props = {children: t};
  let make = ({children}) => Styled(Bold, children);
  let createElement = props => Lazy(() => make(props));
};

module Dim = {
  type props = {children: t};
  let make = ({children}) => Styled(Dim, children);
  let createElement = props => Lazy(() => make(props));
};

module Italic = {
  type props = {children: t};
  let make = ({children}) => Styled(Italic, children);
  let createElement = props => Lazy(() => make(props));
};

module Underline = {
  type props = {children: t};
  let make = ({children}) => Styled(Underline, children);
  let createElement = props => Lazy(() => make(props));
};

module Inverted = {
  type props = {children: t};
  let make = ({children}) => Styled(Inverted, children);
  let createElement = props => Lazy(() => make(props));
};

module Column = {
  type props = {children: list(t)};
  let make = ({children}) => Column(children);
  let createElement = props => Lazy(() => make(props));
};

module Row = {
  type props = {children: list(t)};
  let make = ({children}) => Row(children);
  let createElement = props => Lazy(() => make(props));
};

module Box = {
  type props = {
    children: t,
    width: int,
    height: int,
    padding: option(int),
  };
  let make = ({children, width, height, padding}) => {
    let pad =
      switch (padding) {
      | Some(p) => p
      | None => 0
      };
    Box(children, width, height, pad);
  };
  let createElement = props => Lazy(() => make(props));
};

/* Fragment - groups children without adding structure (renders as Column) */
module Fragment = {
  type props = {children: list(t)};
  let make = ({children}) => Column(children);
  let createElement = props => Lazy(() => make(props));
};
