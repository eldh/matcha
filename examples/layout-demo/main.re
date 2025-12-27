open Matcha;

/* A bordered section that fills its allocated space */
module Section = {
  [@component]
  let make = (~label: string, ~color: Element.color) => {
    let { Runtime.availWidth: width, availHeight: height } = useLayout();

    /* Build box that fills available space */
    let innerWidth = max(0, width - 2); /* Account for side borders */
    let innerHeight = max(0, height - 2); /* Account for top/bottom borders */

    /* Top border with label */
    let labelText = " " ++ label ++ " ";
    let labelLen = visibleLength(labelText);
    let remainingWidth = max(0, innerWidth - labelLen - 1);
    let topBorder =
      BoxChars.topLeft
      ++ labelText
      ++ repeatString(BoxChars.horizontal, remainingWidth)
      ++ BoxChars.topRight;

    /* Bottom border */
    let bottomBorder =
      BoxChars.bottomLeft
      ++ repeatString(BoxChars.horizontal, innerWidth)
      ++ BoxChars.bottomRight;

    /* Middle rows (empty with side borders) - pad content manually to avoid reset codes */
    let padContent = (s: string, targetWidth: int): string => {
      let visible = visibleLength(s);
      if (visible >= targetWidth) {
        s;
      } else {
        s ++ String.make(targetWidth - visible, ' ');
      };
    };

    let emptyRow =
      BoxChars.vertical ++ String.make(innerWidth, ' ') ++ BoxChars.vertical;

    /* Size info in first content row */
    let sizeInfo = Printf.sprintf("  %dx%d allocated", width, height);
    let sizeRow =
      BoxChars.vertical
      ++ padContent(sizeInfo, innerWidth)
      ++ BoxChars.vertical;

    /* Build all lines */
    let middleLines =
      if (innerHeight <= 1) {
        [sizeRow];
      } else {
        [sizeRow] @ List.init(max(0, innerHeight - 1), _ => emptyRow);
      };

    let allLines = [topBorder] @ middleLines @ [bottomBorder];
    let boxStr = String.concat("\n", allLines);

    <Text color> boxStr </Text>;
  };
};

[@component]
let make = () => {
  let quit = Event.useQuit();
  let (termWidth, termHeight) = Terminal.getSize();

  Event.useKeyDown((key, _modifiers) => {
    switch (key) {
    | Key.Char('q')
    | Key.Char('Q') => quit(ClearScreen)
    | _ => ()
    }
  });

  <VStack>
    <Sized size={Chars(5)}>
      <Section
        label={
          "Layout Demo - Terminal: "
          ++ string_of_int(termWidth)
          ++ "x"
          ++ string_of_int(termHeight)
          ++ " - Press Q to quit"
        }
        color=Element.Cyan
      />
    </Sized>
    /* 30% of parent height */
    <Sized size={Percent(30)}>
      <Section label="Percent(30) - 30% of parent" color=Element.Magenta />
    </Sized>
    /* 2x flex (takes 2/3 of remaining) */
    <Sized size={Flex(2)}>
      <Section label="Flex(2) - 2x flex share" color=Element.Green />
    </Sized>
    /* 1x flex (takes 1/3 of remaining) */
    <Sized size={Flex(1)}>
      <Section label="Flex(1) - 1x flex share" color=Element.Yellow />
    </Sized>
  </VStack>;
  /* Fixed 5 rows - header */
};

module App = {
  let make = make;
};

let () = Runtime.start((module App));
