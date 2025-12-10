type style =
  | Bold
  | Dim
  | Italic
  | Underline
  | Inverted
  | FgColor(int)
  | BgColor(int);

type splitWidth =
  | Chars(int)
  | Percent(int)
  | Half;

type t =
  | Text(string)
  | Styled(style, t)
  | Column(list(t))
  | Row(list(t))
  | Empty
  | Lazy(unit => t)
  | SplitView(t, t, splitWidth, int, int); /* left, right, leftWidth, totalWidth, height */

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

/* Box drawing characters (ASCII for compatibility) */
let boxTopLeft = "+";
let boxTopRight = "+";
let boxBottomLeft = "+";
let boxBottomRight = "+";
let boxHorizontal = "-";
let boxVertical = "|";
let boxTeeDown = "+";
let boxTeeUp = "+";

/* Render element to string */
let rec render = (el: t): string => {
  switch (el) {
  | Empty => ""
  | Text(s) => s
  | Styled(style, child) => styleToAnsi(style) ++ render(child) ++ resetAnsi
  | Column(children) => children |> List.map(render) |> String.concat("\n")
  | Row(children) => children |> List.map(render) |> String.concat("")
  | Lazy(f) => render(f())
  | SplitView(left, right, leftWidthSpec, totalWidth, height) =>
    /* Padding: 1 char on each side of content */
    let padding = 1;

    /* Calculate widths (subtract 3 for borders: │ │ │, and 4 for padding) */
    let innerWidth = totalWidth - 3 - padding * 4;
    let leftWidth =
      switch (leftWidthSpec) {
      | Chars(n) => min(n, innerWidth - 1)
      | Percent(p) => innerWidth * p / 100
      | Half => innerWidth / 2
      };
    let rightWidth = innerWidth - leftWidth;

    /* Total cell widths including padding */
    let leftCellWidth = leftWidth + padding * 2;
    let rightCellWidth = rightWidth + padding * 2;

    /* Calculate inner height (subtract 2 for borders, 2 for vertical padding) */
    let innerHeight = height - 2 - padding * 2;

    /* Render children and split into lines */
    let leftLines = splitLines(render(left));
    let rightLines = splitLines(render(right));

    /* Use terminal height, but at least fit the content */
    let contentHeight =
      max(
        innerHeight,
        max(List.length(leftLines), List.length(rightLines)),
      );

    /* Pad lines lists to same height */
    let padLines = (lines, targetHeight) => {
      let len = List.length(lines);
      if (len >= targetHeight) {
        lines;
      } else {
        lines @ List.init(targetHeight - len, _ => "");
      };
    };

    let leftPadded = padLines(leftLines, contentHeight);
    let rightPadded = padLines(rightLines, contentHeight);

    /* Add vertical padding (empty lines at top and bottom) */
    let emptyLine = "";
    let leftWithVPad = [emptyLine, ...leftPadded] @ [emptyLine];
    let rightWithVPad = [emptyLine, ...rightPadded] @ [emptyLine];

    /* Build output */
    let topBorder =
      boxTopLeft
      ++ String.make(leftCellWidth, boxHorizontal.[0])
      ++ boxTeeDown
      ++ String.make(rightCellWidth, boxHorizontal.[0])
      ++ boxTopRight;

    let pad = String.make(padding, ' ');

    let contentLines =
      List.map2(
        (l, r) => {
          boxVertical
          ++ resetAnsi
          ++ pad
          ++ padToWidth(l, leftWidth)
          ++ pad
          ++ resetAnsi
          ++ boxVertical
          ++ resetAnsi
          ++ pad
          ++ padToWidth(r, rightWidth)
          ++ pad
          ++ resetAnsi
          ++ boxVertical
        },
        leftWithVPad,
        rightWithVPad,
      );

    let bottomBorder =
      boxBottomLeft
      ++ String.make(leftCellWidth, boxHorizontal.[0])
      ++ boxTeeUp
      ++ String.make(rightCellWidth, boxHorizontal.[0])
      ++ boxBottomRight;

    String.concat("\n", [topBorder, ...contentLines] @ [bottomBorder]);
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

module SplitView = {
  type props = {
    left: t,
    right: t,
    leftWidth: option(splitWidth),
    width: int,
    height: int,
  };
  let make = ({left, right, leftWidth, width, height}) => {
    let widthSpec =
      switch (leftWidth) {
      | Some(w) => w
      | None => Half
      };
    SplitView(left, right, widthSpec, width, height);
  };
  let createElement = props => Lazy(() => make(props));
};
