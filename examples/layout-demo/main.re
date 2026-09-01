open Matcha;

/* A bordered section that fills its container.
 *
 * Every <Section> below sits inside a <Container> of its own, so
 * useContainerSize() reports the pane the <Sized> wrapper allocated. Without
 * that wrapper it would report the whole frame - responsive queries are
 * container-relative by default, and <Sized> is deliberately NOT a boundary
 * (nudging layout must not silently re-target the descendants' queries). */
module Section = {
  [@component]
  let make = (~label: string, ~color: Element.color) => {
    let { Runtime.availWidth: width, availHeight: height } =
      useContainerSize();

    /* Build box that fills available space */
    let innerWidth = max(0, width - 2); /* Account for side borders */
    let innerHeight = max(0, height - 2); /* Account for top/bottom borders */

    /* Top border with label */
    let labelText = " " ++ label ++ " ";
    let labelLen = visibleLength(labelText);
    /* corner + label + horizontals + corner must sum to the full width:
       horizontals = innerWidth - labelLen (no extra -1, which used to leave
       every titled top border one column short of the other rows). */
    let remainingWidth = max(0, innerWidth - labelLen);
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
  /* This is the ROOT component and nothing wraps it in a <Container>, so the
     query answers with the whole frame - which is exactly the "terminal
     size" the header reports. It also honours MATCHA_WIDTH/MATCHA_HEIGHT,
     which the raw Terminal.getSize() this replaced did not. */
  let {Runtime.availWidth: termWidth, availHeight: termHeight} =
    useContainerSize();

  Event.useKeyDown((key, _modifiers) => {
    switch (key) {
    | Key.Char('q')
    | Key.Char('Q') => quit(ClearScreen)
    | _ => ()
    }
  });

  <VStack>
    <Sized size={Chars(5)}>
      <Container>
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
      </Container>
    </Sized>
    /* 30% of parent height */
    <Sized size={Percent(30)}>
      <Container>
        <Section label="Percent(30) - 30% of parent" color=Element.Magenta />
      </Container>
    </Sized>
    /* 2x flex (takes 2/3 of remaining) */
    <Sized size={Flex(2)}>
      <Container>
        <Section label="Flex(2) - 2x flex share" color=Element.Green />
      </Container>
    </Sized>
    /* 1x flex (takes 1/3 of remaining) */
    <Sized size={Flex(1)}>
      <Container>
        <Section label="Flex(1) - 1x flex share" color=Element.Yellow />
      </Container>
    </Sized>
  </VStack>;
  /* Fixed 5 rows - header */
};

module App = {
  let make = make;
};

let () = Runtime.start((module App));
