/*
 * Layout Alignment Example
 *
 * Demonstrates the align and justify props for HStack and VStack.
 * These work similarly to CSS flexbox:
 *
 * - justify: Controls distribution along the main axis
 *   - JustifyStart, JustifyEnd, JustifyCenter
 *   - JustifySpaceBetween, JustifySpaceAround, JustifySpaceEvenly
 *
 * - align: Controls alignment along the cross axis
 *   - AlignStart, AlignEnd, AlignCenter, AlignStretch
 *
 * Press 1-6 to change justify mode, a-d to change align mode.
 */

open Matcha;

/* Box component - renders a bordered box with content.
 * Uses layout constraints to stretch to available height when parent uses AlignStretch.
 * When availHeight <= minHeight, uses minHeight (natural size).
 * When availHeight > minHeight, stretches to availHeight.
 */
module Box = {
  [@component]
  let make = (~label: string, ~width: int, ~minHeight: int, ()) => {
    /* Get available height from layout constraints */
    let {Runtime.availHeight, _} = useLayout();

    let w = max(width, String.length(label) + 4);
    /* Only stretch if availHeight is explicitly larger than our minimum */
    let h =
      if (availHeight > minHeight) {
        availHeight; /* Stretch mode */
      } else {
        minHeight; /* Natural size mode */
      };
    let h = max(h, 3); /* Ensure at least 3 lines for border */

    let topBottom = "+" ++ String.make(w - 2, '-') ++ "+";
    let labelPadLeft = (w - 2 - String.length(label)) / 2;
    let labelPadRight = w - 2 - String.length(label) - labelPadLeft;
    let middle =
      "|"
      ++ String.make(labelPadLeft, ' ')
      ++ label
      ++ String.make(labelPadRight, ' ')
      ++ "|";
    let emptyMiddle = "|" ++ String.make(w - 2, ' ') ++ "|";

    let middleRows = h - 2;
    let middleIndex = middleRows / 2;
    let lines =
      [topBottom]
      @ List.init(
          middleRows,
          i =>
            if (i == middleIndex) {
              middle;
            } else {
              emptyMiddle;
            },
        )
      @ [topBottom];

    <Text> {String.concat("\n", lines)} </Text>;
  };
};

/* Main justification values for cycling */
let justifyModes = [|
  (Element.JustifyStart, "JustifyStart"),
  (Element.JustifyEnd, "JustifyEnd"),
  (Element.JustifyCenter, "JustifyCenter"),
  (Element.JustifySpaceBetween, "JustifySpaceBetween"),
  (Element.JustifySpaceAround, "JustifySpaceAround"),
  (Element.JustifySpaceEvenly, "JustifySpaceEvenly"),
|];

let alignModes = [|
  (Element.AlignStart, "AlignStart"),
  (Element.AlignEnd, "AlignEnd"),
  (Element.AlignCenter, "AlignCenter"),
  (Element.AlignStretch, "AlignStretch"),
|];

[@component]
let make = () => {
  let quit = Event.useQuit();
  let (justifyIndex, setJustifyIndex) = Component.useState(0);
  let (alignIndex, setAlignIndex) = Component.useState(0);
  let (showHStack, setShowHStack) = Component.useState(true);

  let (justify, justifyName) = justifyModes[justifyIndex];
  let (align, alignName) = alignModes[alignIndex];

  Event.useKeyDown((key, modifiers) => {
    switch (key, modifiers) {
    | (Key.Char('q'), _)
    | (Key.Char('Q'), _) => quit(ClearScreen)
    | (Key.Char('c'), {Key.ctrl: true, _}) => quit(ClearScreen)
    /* Justify modes: 1-6 */
    | (Key.Char('1'), _) => setJustifyIndex(0)
    | (Key.Char('2'), _) => setJustifyIndex(1)
    | (Key.Char('3'), _) => setJustifyIndex(2)
    | (Key.Char('4'), _) => setJustifyIndex(3)
    | (Key.Char('5'), _) => setJustifyIndex(4)
    | (Key.Char('6'), _) => setJustifyIndex(5)
    /* Align modes: a-d */
    | (Key.Char('a'), _) => setAlignIndex(0)
    | (Key.Char('s'), _) => setAlignIndex(1)
    | (Key.Char('d'), _) => setAlignIndex(2)
    | (Key.Char('f'), _) => setAlignIndex(3)
    /* Toggle HStack/VStack */
    | (Key.Tab, _) => setShowHStack(!showHStack)
    | _ => ()
    }
  });

  let header =
    <VStack>
      <Text bold=true color=Cyan> "Layout Alignment Example" </Text>
      <Text dim=true>
        "Demonstrates align and justify props for HStack/VStack"
      </Text>
      <Text> "" </Text>
    </VStack>;

  let controls =
    <VStack>
      <Text> "" </Text>
      <Text dim=true> "─────────────────────────────────────────────────" </Text>
      <Text> "" </Text>
      <HStack gap=2>
        <Text dim=true> "Mode:" </Text>
        <Text bold=true color={showHStack ? Green : White}>
          {showHStack ? "HStack" : "VStack"}
        </Text>
        <Text dim=true> "(Tab to toggle)" </Text>
      </HStack>
      <Text> "" </Text>
      <HStack gap=2>
        <Text dim=true> "Justify:" </Text>
        <Text bold=true color=Yellow> justifyName </Text>
      </HStack>
      <Text dim=true>
        "  1: Start  2: End  3: Center  4: SpaceBetween  5: SpaceAround  6: SpaceEvenly"
      </Text>
      <Text> "" </Text>
      <HStack gap=2>
        <Text dim=true> "Align:" </Text>
        <Text bold=true color=Magenta> alignName </Text>
      </HStack>
      <Text dim=true> "  a: Start  s: End  d: Center  f: Stretch" </Text>
      <Text> "" </Text>
      <Text dim=true> "q: Quit" </Text>
    </VStack>;

  /* Demo area with border */
  let demoContent =
    if (showHStack) {
      /* HStack demo - horizontal layout */
      <Sized size={Chars(12)}>
        <HStack gap=1 align justify>
          <Box label="A" width=8 minHeight=3 />
          <Box label="B" width=8 minHeight=5 />
          <Box label="C" width=8 minHeight=4 />
        </HStack>
      </Sized>;
    } else {
      /* VStack demo - vertical layout */
      <Sized size={Chars(15)}>
        <VStack gap=0 align justify>
          <Box label="A" width=12 minHeight=3 />
          <Box label="BB" width=18 minHeight=3 />
          <Box label="CCC" width=24 minHeight=3 />
        </VStack>
      </Sized>;
    };

  <VStack>
    header
    <Text bold=true> "Demo:" </Text>
    <Text dim=true>
      "┌────────────────────────────────────────────────────────────────────┐"
    </Text>
    demoContent
    <Text dim=true>
      "└────────────────────────────────────────────────────────────────────┘"
    </Text>
    controls
  </VStack>;
};

module App = {
  let make = make;
};

let () = Runtime.start((module App));
