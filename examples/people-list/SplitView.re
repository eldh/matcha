open Rere;

type splitWidth =
  | Chars(int)
  | Percent(int)
  | Half;

[@component]
let make =
    (
      ~left: Element.t,
      ~right: Element.t,
      ~leftWidth: option(splitWidth),
      ~width: int,
      ~height: int,
      ~padding: option(int),
    ) => {
  let leftWidth =
    switch (leftWidth) {
    | Some(w) => w
    | None => Half
    };
  let padding =
    switch (padding) {
    | Some(p) => p
    | None => 1
    };
  /* Calculate widths (subtract 3 for borders: │ │ │, and padding on each side) */
  let innerWidth = width - 3 - padding * 4;
  let leftContentWidth =
    switch (leftWidth) {
    | Chars(n) => min(n, innerWidth - 1)
    | Percent(p) => innerWidth * p / 100
    | Half => innerWidth / 2
    };
  let rightContentWidth = innerWidth - leftContentWidth;

  /* Total cell widths including padding */
  let leftCellWidth = leftContentWidth + padding * 2;
  let rightCellWidth = rightContentWidth + padding * 2;

  /* Calculate inner height (subtract 2 for borders, 2 for vertical padding) */
  let innerHeight = height - 2 - padding * 2;

  /* Render children and split into lines */
  let leftLines = splitLines(Element.render(left));
  let rightLines = splitLines(Element.render(right));

  /* Use terminal height, but at least fit the content */
  let contentHeight =
    max(innerHeight, max(List.length(leftLines), List.length(rightLines)));

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
  let vPadLines = List.init(padding, _ => emptyLine);
  let leftWithVPad = vPadLines @ leftPadded @ vPadLines;
  let rightWithVPad = vPadLines @ rightPadded @ vPadLines;

  /* Build output using solid UTF-8 box characters */
  let topBorder =
    BoxChars.topLeft
    ++ repeatString(BoxChars.horizontal, leftCellWidth)
    ++ BoxChars.teeDown
    ++ repeatString(BoxChars.horizontal, rightCellWidth)
    ++ BoxChars.topRight;

  let hPad = String.make(padding, ' ');
  let resetAnsi = "\027[0m";

  let contentLines =
    List.map2(
      (l, r) => {
        BoxChars.vertical
        ++ resetAnsi
        ++ hPad
        ++ padToWidth(l, leftContentWidth)
        ++ hPad
        ++ resetAnsi
        ++ BoxChars.vertical
        ++ resetAnsi
        ++ hPad
        ++ padToWidth(r, rightContentWidth)
        ++ hPad
        ++ resetAnsi
        ++ BoxChars.vertical
      },
      leftWithVPad,
      rightWithVPad,
    );

  let bottomBorder =
    BoxChars.bottomLeft
    ++ repeatString(BoxChars.horizontal, leftCellWidth)
    ++ BoxChars.teeUp
    ++ repeatString(BoxChars.horizontal, rightCellWidth)
    ++ BoxChars.bottomRight;

  let allLines = [topBorder, ...contentLines] @ [bottomBorder];

  <Text> {String.concat("\n", allLines)} </Text>;
};
