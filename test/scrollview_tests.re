/*
 * Tests for B5: <ScrollView> and the Element.Viewport node under it.
 *
 * Almost everything here drives a real application through
 * Runtime.startHeadless at 20x6, because what is being tested is a LAYOUT
 * behavior: which content rows land in the window, what the scrollbar column
 * says about them, and which components are still reachable by a click once
 * the content has moved. A viewport that is not laid out has nothing to clip.
 *
 * The exceptions are the two pure pieces: ScrollView.scrollbarMetrics (a
 * plain geometry table) and the raw Element.Viewport fixture, which exists to
 * pin down PASS DISCRIMINATION - that vpOnViewport fires exactly once per
 * frame, from the committed pass, even inside an HStack, whose first
 * real-mode pass renders every child at availHeight=0.
 */
open Matcha;

/* A VStack of n numbered rows, e.g. "L00", "L01", ... - short enough that a
 * row is recognizable from the first three characters of a frame line. */
let rowsEl = (prefix: string, n: int): Element.t =>
  Element.vstack(
    List.init(n, i => Element.text(Printf.sprintf("%s%02d", prefix, i))),
  );

let startsWith = (s: string, prefix: string): bool =>
  String.length(s) >= String.length(prefix)
  && String.sub(s, 0, String.length(prefix)) == prefix;

/* The scrollbar cell of frame line [i]: the last (multi-byte) character of
 * the line. "█" is the thumb, "│" the track, " " means "nothing to scroll". */
let barCell = (lines: array(string), i: int): string => {
  let l = lines[i];
  let n = String.length(l);
  n >= 3 ? String.sub(l, n - 3, 3) : "";
};

let thumb = "█";
let track = "│";

let assertRow =
    (lines: array(string), i: int, expected: string, msg: string): unit =>
  Test.assertTrue(
    startsWith(lines[i], expected),
    msg ++ " (line " ++ string_of_int(i) ++ " = " ++ lines[i] ++ ")",
  );

let config20x6: Runtime.headlessConfig = {width: 20, height: 6};

/* ============================================================================
 * 1. The basic fixture: a title row plus a 12-row list in a 5-row window
 *
 * At 20x6 the title takes one line and the Flex(1) slot takes the other
 * five, so the viewport is 5 rows tall and (minus the scrollbar column) 19
 * columns wide. 12 rows of content therefore scroll through 8 positions,
 * offset 0 through 7.
 * ========================================================================== */

module BasicApp = {
  [@component]
  let make = () =>
    <VStack>
      <Sized size={Chars(1)}> <Text> "title" </Text> </Sized>
      <Sized size={Flex(1)}> <ScrollView> {rowsEl("L", 12)} </ScrollView> </Sized>
    </VStack>;
};

/* ============================================================================
 * 2. Focus variants: two lists to Tab between, and one that opts out
 * ========================================================================== */

/* Two lists side by side, so that Tab has somewhere to move focus TO. (With
 * a single focusable, B1 hands it the focus on the first frame and Tab
 * cycles it straight back to itself.) */
module TwoListsApp = {
  [@component]
  let make = () =>
    <HStack>
      <Sized size={Chars(10)}>
        <ScrollView id="left" showScrollbar=false> {rowsEl("A", 12)} </ScrollView>
      </Sized>
      <Sized size={Chars(10)}>
        <ScrollView id="right" showScrollbar=false> {rowsEl("B", 12)} </ScrollView>
      </Sized>
    </HStack>;
};

/* A list that opts out of the focus ring entirely. */
module UnfocusableApp = {
  [@component]
  let make = () =>
    <VStack>
      <Sized size={Chars(1)}> <Text> "title" </Text> </Sized>
      <Sized size={Flex(1)}>
        <ScrollView focusable=false> {rowsEl("L", 12)} </ScrollView>
      </Sized>
    </VStack>;
};

/* ============================================================================
 * 3. Content that shrinks under a scrolled-away offset
 * ========================================================================== */

module ShrinkApp = {
  [@component]
  let make = () => {
    let (n, setN) = Hooks.useState(12);
    Hooks.useKeyDown((key, _modifiers) =>
      switch (key) {
      | Key.Char('s') => setN(6)
      | _ => ()
      }
    );
    <VStack>
      <Sized size={Chars(1)}> <Text> "title" </Text> </Sized>
      <Sized size={Flex(1)}> <ScrollView> {rowsEl("S", n)} </ScrollView> </Sized>
    </VStack>;
  };
};

/* ============================================================================
 * 3. Controlled mode: the offset comes from outside and never moves on its own
 *
 * A plain ref, not state, so that a gesture cannot repaint anything: the
 * whole point is that a controlled ScrollView shows what it was TOLD to show,
 * and reports where it would have gone through onScroll.
 * ========================================================================== */

let controlledOffset = ref(0);
let controlledScrolls: ref(list(int)) = ref([]);

module ControlledApp = {
  [@component]
  let make = () =>
    <VStack>
      <Sized size={Chars(1)}> <Text> "title" </Text> </Sized>
      <Sized size={Flex(1)}>
        <ScrollView
          offset={controlledOffset^}
          onScroll={v => controlledScrolls := [v, ...controlledScrolls^]}>
          {rowsEl("C", 12)}
        </ScrollView>
      </Sized>
    </VStack>;
};

/* ============================================================================
 * 4. A zero-height committed viewport
 *
 * The title eats all three rows, so the Flex(1) slot is allocated none. A
 * committed viewport with no rows must paint NOTHING - falling back to the
 * unclipped content (which is the right answer for a MEASURING pass) would
 * splatter twelve rows across a stack that allocated it zero lines.
 * ========================================================================== */

module ZeroHeightApp = {
  [@component]
  let make = () =>
    <VStack>
      <Sized size={Chars(3)}> <Text> "t1\nt2\nt3" </Text> </Sized>
      <Sized size={Flex(1)}> <ScrollView> {rowsEl("N", 12)} </ScrollView> </Sized>
    </VStack>;
};

/* ============================================================================
 * 5. A raw Element.Viewport inside an HStack (pass discrimination)
 *
 * HStack renders every child TWICE in real mode: once at availHeight=0 with
 * no origin, purely to learn its natural height, and once for real. Only the
 * second one paints, and vpOnViewport must fire on that one alone.
 * ========================================================================== */

let vpCalls: ref(list((int, int))) = ref([]);

module RawViewportApp = {
  [@component]
  let make = () =>
    <HStack>
      <Sized size={Chars(10)}>
        {Element.Viewport(
           rowsEl("V", 8),
           {
             Element.vpOffset: 2,
             vpShowScrollbar: false,
             vpOnViewport: Some(m => vpCalls := [m, ...vpCalls^]),
             vpRows: None,
           },
         )}
      </Sized>
      <Sized size={Flex(1)}> <Text> "side" </Text> </Sized>
    </HStack>;
};

/* ============================================================================
 * 6. Nested ScrollViews (wheel routing)
 *
 * The inner one occupies the top four rows of the outer one's window, so a
 * wheel event at y=1 is over both and one at y=5 is over the outer only.
 * ========================================================================== */

let outerScrolls = ref(0);
let innerScrolls = ref(0);

module NestedApp = {
  [@component]
  let make = () =>
    <Sized size={Chars(6)}>
      <ScrollView onScroll={_ => outerScrolls := outerScrolls^ + 1}>
        <VStack>
          <Sized size={Chars(4)}>
            <ScrollView onScroll={_ => innerScrolls := innerScrolls^ + 1}>
              {rowsEl("I", 10)}
            </ScrollView>
          </Sized>
          {rowsEl("O", 6)}
        </VStack>
      </ScrollView>
    </Sized>;
};

/* ============================================================================
 * 7. A Clickable inside a ScrollView
 *
 * No title row here, so the viewport starts at y=0: content row r sits on
 * frame line (r - offset) whenever that is inside [0, 5).
 * ========================================================================== */

let rowClicks = ref(0);

module ClickableInScrollApp = {
  [@component]
  let make = () =>
    <ScrollView>
      <VStack>
        ...{List.init(12, i =>
              if (i == 8) {
                <Clickable onClick={() => rowClicks := rowClicks^ + 1}>
                  <Text> "K08" </Text>
                </Clickable>;
              } else {
                <Text> {Printf.sprintf("K%02d", i)} </Text>;
              }
            )}
      </VStack>
    </ScrollView>;
};

/* ============================================================================
 * 8. ROWS MODE (the virtualized content mode)
 *
 * <ScrollView rows /> takes content the application has already rendered,
 * one self-contained styled string per row, and never looks at the rows it
 * is not showing. The group below pins four things:
 *   - PARITY: the same twelve lines, once as a child VStack and once as
 *     ~rows, must produce the SAME frame at every offset. That is the whole
 *     claim of the mode - it is a faster route to an identical picture.
 *   - MECHANICS: scrollbar, wheel, controlled mode, short-window padding and
 *     over-wide-row clipping all behave as they do for a child.
 *   - SELF-CONTAINMENT: a row that carries its own SGR keeps its colour at
 *     every scroll position, which is what the contract buys.
 *   - SCALE: 100_000 rows is a normal frame, not a stall.
 * ========================================================================== */

/* The same twelve lines both fixtures below render. Baked by hand for the
   rows fixture, so that "the same lines" means the same BYTES on both
   sides - not the same intent. */
let parityLines: array(string) =
  Array.init(12, i => Printf.sprintf("P%02d", i));

module ParityChildApp = {
  [@component]
  let make = () =>
    <VStack>
      <Sized size={Chars(1)}> <Text> "title" </Text> </Sized>
      <Sized size={Flex(1)}>
        <ScrollView>
          <VStack>
            ...{Array.to_list(Array.map(l => <Text> l </Text>, parityLines))}
          </VStack>
        </ScrollView>
      </Sized>
    </VStack>;
};

module ParityRowsApp = {
  [@component]
  let make = () =>
    <VStack>
      <Sized size={Chars(1)}> <Text> "title" </Text> </Sized>
      <Sized size={Flex(1)}> <ScrollView rows=parityLines /> </Sized>
    </VStack>;
};

/* Self-closing JSX, no ~children at all - the shape the docs promise. Also
   the fixture for the padding case: 7 rows in a 6-row window scroll to an
   offset where only one row is left. */
let shortRows: array(string) = Array.init(7, i => Printf.sprintf("S%02d", i));

module ShortRowsApp = {
  [@component]
  let make = () => <ScrollView rows=shortRows />;
};

/* One row far wider than the frame, and ANSI in the middle of it. The frame
   must never grow past its width, and the escape must survive the clip. */
let wideRow =
  "W"
  ++ Element.styleToAnsi(Element.FgColor(Element.Red))
  ++ "RED"
  ++ Element.resetAnsi
  ++ String.make(200, 'x');

let wideRows: array(string) = [|
  wideRow,
  "second row",
  "third row",
  "fourth row",
  "fifth row",
  "sixth row",
  "seventh row",
  "eighth row",
|];

module WideRowsApp = {
  [@component]
  let make = () => <ScrollView rows=wideRows />;
};

/* Style self-containment: EVERY row opens its own colour and closes it. Row
   7 is the one the assertions follow - it is off-screen at offset 0 and on
   screen after a scroll, and its escape must appear exactly when it does. */
let green = Element.styleToAnsi(Element.FgColor(Element.Green));
let magenta = Element.styleToAnsi(Element.FgColor(Element.Magenta));

let styledRows: array(string) =
  Array.init(12, i =>
    Printf.sprintf("R%02d ", i)
    ++ (i == 7 ? magenta : green)
    ++ "word"
    ++ Element.resetAnsi
  );

module StyledRowsApp = {
  [@component]
  let make = () => <ScrollView rows=styledRows />;
};

/* Controlled rows mode: same contract as ControlledApp above, rows content. */
let ctlRowsOffset = ref(0);
let ctlRowsScrolls: ref(list(int)) = ref([]);
let ctlRows: array(string) = Array.init(12, i => Printf.sprintf("D%02d", i));

module ControlledRowsApp = {
  [@component]
  let make = () =>
    <VStack>
      <Sized size={Chars(1)}> <Text> "title" </Text> </Sized>
      <Sized size={Flex(1)}>
        <ScrollView
          rows=ctlRows
          offset={ctlRowsOffset^}
          onScroll={v => ctlRowsScrolls := [v, ...ctlRowsScrolls^]}
        />
      </Sized>
    </VStack>;
};

/* The scale guard. A hundred thousand rows, each styled, in a 20x6 frame:
   the child path would parse all of it once per pass, per frame. */
let hugeRows: array(string) =
  Array.init(100_000, i =>
    green ++ Printf.sprintf("H%06d", i) ++ Element.resetAnsi
  );

module HugeRowsApp = {
  [@component]
  let make = () => <ScrollView rows=hugeRows />;
};

/* A wheel event, which Input.clickAt has no equivalent for. */
let wheelAt = (~kind: Mouse.kind, ~x: int, ~y: int): Mouse.event => {
  Mouse.kind: kind,
  button: Mouse.NoButton,
  x,
  y,
  shift: false,
  alt: false,
  ctrl: false,
};

let run = () => {
  Test.group("ScrollView: initial window and scrollbar", () => {
    Test.run("shows the first viewport-worth of rows, thumb at the top", () => {
      let handle = Runtime.startHeadless(~config=config20x6, (module BasicApp));
      let lines = handle.getLines(true);
      Test.assertEqual(Array.length(lines), 6, "one title row plus a 5-row window");
      assertRow(lines, 0, "title", "the title keeps its own row");
      assertRow(lines, 1, "L00", "the window starts at the first content row");
      assertRow(lines, 2, "L01", "");
      assertRow(lines, 3, "L02", "");
      assertRow(lines, 4, "L03", "");
      assertRow(lines, 5, "L04", "and stops after five rows - L05.. are clipped");
      Test.assertFalse(
        Test.contains(handle.getOutput(true), "L05"),
        "nothing below the window leaks into the frame",
      );
      handle.quit();
    });

    Test.run("draws a proportional thumb at the top of the track", () => {
      let handle = Runtime.startHeadless(~config=config20x6, (module BasicApp));
      let lines = handle.getLines(true);
      /* 5 rows of a 12-row list: thumbH = max(1, 5*5/12) = 2, at the top. */
      Test.assertEqualStr(barCell(lines, 1), thumb, "viewport row 0 is thumb");
      Test.assertEqualStr(barCell(lines, 2), thumb, "viewport row 1 is thumb");
      Test.assertEqualStr(barCell(lines, 3), track, "viewport row 2 is track");
      Test.assertEqualStr(barCell(lines, 4), track, "viewport row 3 is track");
      Test.assertEqualStr(barCell(lines, 5), track, "viewport row 4 is track");
      handle.quit();
    });

    Test.run("the scrollbar column costs the content one column", () => {
      let handle = Runtime.startHeadless(~config=config20x6, (module BasicApp));
      let lines = handle.getLines(true);
      Test.assertEqual(
        Element.visibleLength(lines[1]),
        20,
        "19 content columns plus the scrollbar fill the 20-column frame",
      );
      handle.quit();
    });
  });

  Test.group("ScrollView: keyboard scrolling (focused)", () => {
    Test.run("the only focusable takes focus, and Arrow_down shifts one row", () => {
      let handle = Runtime.startHeadless(~config=config20x6, (module BasicApp));
      Test.assertTrue(
        handle.getFocusedId() != None,
        "B1 hands the focus to the one focusable on the first frame",
      );
      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      let lines = handle.getLines(true);
      assertRow(lines, 1, "L01", "the window moved down one row");
      assertRow(lines, 5, "L05", "and so did its last row");
      handle.quit();
    });

    Test.run("~focusable=false stays out of the ring and ignores the arrows", () => {
      let handle =
        Runtime.startHeadless(~config=config20x6, (module UnfocusableApp));
      Test.assertEqual(handle.getFocusedId(), None, "it registered no focusable");
      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      assertRow(handle.getLines(true), 1, "L00", "no focus, no scroll");
      handle.sendKey(Key.End, Key.noModifiers);
      assertRow(handle.getLines(true), 1, "L00", "not even End");
      handle.quit();
    });

    Test.run("Tab moves focus, and only the focused list scrolls", () => {
      let handle =
        Runtime.startHeadless(~config=config20x6, (module TwoListsApp));
      Test.assertEqual(handle.getFocusedId(), Some("left"), "the first list has focus");

      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      let lines = handle.getLines(true);
      Test.assertTrue(startsWith(lines[0], "A01"), "the left list moved: " ++ lines[0]);
      Test.assertContains(lines[0], "B00", "the right one did not");

      Input.pressTab(handle);
      Test.assertEqual(handle.getFocusedId(), Some("right"), "Tab moved focus right");
      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      let lines = handle.getLines(true);
      Test.assertTrue(startsWith(lines[0], "A01"), "the left list stayed put: " ++ lines[0]);
      Test.assertContains(lines[0], "B01", "and now the right one moves");

      Input.pressShiftTab(handle);
      Test.assertEqual(handle.getFocusedId(), Some("left"), "Shift+Tab moves back");
      handle.quit();
    });

    Test.run("Page_down jumps a window minus one row of overlap", () => {
      let handle = Runtime.startHeadless(~config=config20x6, (module BasicApp));
      handle.sendKey(Key.Page_down, Key.noModifiers);
      let lines = handle.getLines(true);
      assertRow(lines, 1, "L04", "5-row window, so PgDn moves 4 rows");
      Input.feedKeys(handle, [(Key.Page_up, Key.noModifiers)]);
      assertRow(handle.getLines(true), 1, "L00", "and PgUp moves back the same 4");
      handle.quit();
    });

    Test.run("End clamps to the last full window, Home returns to the top", () => {
      let handle = Runtime.startHeadless(~config=config20x6, (module BasicApp));
      handle.sendKey(Key.End, Key.noModifiers);
      let lines = handle.getLines(true);
      assertRow(lines, 1, "L07", "12 rows in a 5-row window stop at offset 7");
      assertRow(lines, 5, "L11", "the last content row is the last frame row");
      /* Thumb flush with the bottom: thumbTop = (5-2)*7/7 = 3. */
      Test.assertEqualStr(barCell(lines, 3), track, "viewport row 2 is track");
      Test.assertEqualStr(barCell(lines, 4), thumb, "viewport row 3 is thumb");
      Test.assertEqualStr(barCell(lines, 5), thumb, "viewport row 4 is thumb");

      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      assertRow(handle.getLines(true), 1, "L07", "Arrow_down at the end is a no-op");

      handle.sendKey(Key.Home, Key.noModifiers);
      assertRow(handle.getLines(true), 1, "L00", "Home goes back to the very top");
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      assertRow(handle.getLines(true), 1, "L00", "Arrow_up at the top is a no-op");
      handle.quit();
    });
  });

  Test.group("ScrollView: wheel", () => {
    Test.run("the wheel scrolls three rows, without focus", () => {
      let handle = Runtime.startHeadless(~config=config20x6, (module BasicApp));
      handle.sendMouse(wheelAt(~kind=Mouse.ScrollDown, ~x=2, ~y=3));
      assertRow(handle.getLines(true), 1, "L03", "one wheel notch is three rows");
      handle.sendMouse(wheelAt(~kind=Mouse.ScrollUp, ~x=2, ~y=3));
      assertRow(handle.getLines(true), 1, "L00", "and back up again");
      handle.quit();
    });

    Test.run("a wheel event outside the ScrollView's box is ignored", () => {
      let handle = Runtime.startHeadless(~config=config20x6, (module BasicApp));
      /* y=0 is the title row, above the viewport. */
      handle.sendMouse(wheelAt(~kind=Mouse.ScrollDown, ~x=2, ~y=0));
      assertRow(handle.getLines(true), 1, "L00", "the title row does not scroll the list");
      handle.quit();
    });
  });

  Test.group("ScrollView: content that shrinks under the offset", () => {
    Test.run("the window re-clamps, and the next keypress moves from there", () => {
      let handle = Runtime.startHeadless(~config=config20x6, (module ShrinkApp));
      handle.sendKey(Key.End, Key.noModifiers);
      assertRow(handle.getLines(true), 1, "S07", "12 rows: the end is offset 7");

      handle.sendKey(Key.Char('s'), Key.noModifiers);
      assertRow(
        handle.getLines(true),
        1,
        "S01",
        "6 rows in a 5-row window: offset 7 re-clamps to 1",
      );
      assertRow(handle.getLines(true), 5, "S05", "the shorter list still fills the window");

      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      assertRow(
        handle.getLines(true),
        1,
        "S00",
        "one press moves one row from what is ON SCREEN, not from the stale offset",
      );
      handle.quit();
    });
  });

  Test.group("ScrollView: controlled mode", () => {
    Test.run("the offset prop drives the view and gestures only report", () => {
      controlledOffset := 0;
      controlledScrolls := [];
      let handle =
        Runtime.startHeadless(~config=config20x6, (module ControlledApp));
      assertRow(handle.getLines(true), 1, "C00", "the prop decides what is shown");

      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      Test.assertEqual(
        controlledScrolls^,
        [1],
        "onScroll reported where it would have gone",
      );
      assertRow(handle.getLines(true), 1, "C00", "but nothing moved");
      ignore(handle.render());
      assertRow(
        handle.getLines(true),
        1,
        "C00",
        "and re-rendering shows no hidden internal state either",
      );

      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      Test.assertEqual(
        controlledScrolls^,
        [1, 1],
        "a second press reports 1 again - it moves from the PROP, not from itself",
      );
      handle.quit();
    });

    Test.run("onScroll always receives a clamped value", () => {
      controlledOffset := 0;
      controlledScrolls := [];
      let handle =
        Runtime.startHeadless(~config=config20x6, (module ControlledApp));
      handle.sendKey(Key.End, Key.noModifiers);
      Test.assertEqual(controlledScrolls^, [7], "End reports the maximum offset, 7");

      /* An out-of-range prop is clamped for display AND for the next gesture. */
      controlledOffset := 20;
      controlledScrolls := [];
      ignore(handle.render());
      assertRow(handle.getLines(true), 1, "C07", "offset 20 displays as offset 7");
      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      Test.assertEqual(
        controlledScrolls^,
        [7],
        "scrolling down from a clamped 7 reports 7, never 8 or 21",
      );
      handle.quit();
    });
  });

  Test.group("ScrollView: pass discrimination", () => {
    Test.run("vpOnViewport fires once per frame, from the committed pass", () => {
      vpCalls := [];
      let handle =
        Runtime.startHeadless(~config=config20x6, (module RawViewportApp));
      Test.assertEqual(
        List.length(vpCalls^),
        1,
        "one call, even though HStack renders every child twice in real mode",
      );
      Test.assertEqual(
        vpCalls^,
        [(8, 6)],
        "with the natural content height and the committed viewport height",
      );
      let lines = handle.getLines(true);
      assertRow(lines, 0, "V02", "the committed pass clipped from offset 2");
      Test.assertFalse(
        Test.contains(handle.getOutput(true), "V01"),
        "the pre-pass's unclipped rows are not what got painted",
      );
      Test.assertTrue(
        Test.contains(lines[0], "side"),
        "the HStack's other child still sits beside it",
      );

      vpCalls := [];
      ignore(handle.render());
      Test.assertEqual(List.length(vpCalls^), 1, "and once more on the next frame");
      handle.quit();
    });

    Test.run("a committed viewport with no rows paints nothing", () => {
      let config: Runtime.headlessConfig = {width: 20, height: 3};
      let handle = Runtime.startHeadless(~config, (module ZeroHeightApp));
      let output = handle.getOutput(true);
      Test.assertContains(output, "t1", "the title is still there");
      Test.assertFalse(
        Test.contains(output, "N00"),
        "a zero-height viewport does NOT fall back to unclipped content",
      );
      handle.quit();
    });
  });

  Test.group("ScrollView: nesting", () => {
    Test.run("the wheel hits the innermost ScrollView under the pointer", () => {
      outerScrolls := 0;
      innerScrolls := 0;
      let config: Runtime.headlessConfig = {width: 20, height: 6};
      let handle = Runtime.startHeadless(~config, (module NestedApp));

      handle.sendMouse(wheelAt(~kind=Mouse.ScrollDown, ~x=2, ~y=1));
      Test.assertEqual(innerScrolls^, 1, "a notch over the inner list scrolled it");
      Test.assertEqual(outerScrolls^, 0, "and the outer one never saw the event");

      handle.sendMouse(wheelAt(~kind=Mouse.ScrollDown, ~x=2, ~y=5));
      Test.assertEqual(outerScrolls^, 1, "a notch below the inner list scrolled the outer one");
      Test.assertEqual(innerScrolls^, 1, "the inner one did not move");
      handle.quit();
    });
  });

  Test.group("ScrollView: clipping and hit testing", () => {
    Test.run("a Clickable is hittable on screen and unhittable scrolled out", () => {
      rowClicks := 0;
      let handle =
        Runtime.startHeadless(~config=config20x6, (module ClickableInScrollApp));
      assertRow(handle.getLines(true), 0, "K00", "the window starts at the top");

      /* K08 is seven rows below the bottom of the window. Nothing on screen
       * can reach it: its recorded box clips to zero size. */
      for (y in 0 to 5) {
        Input.clickAt(handle, ~x=1, ~y);
      };
      Test.assertEqual(rowClicks^, 0, "no click anywhere in the window reaches a clipped row");

      handle.sendKey(Key.End, Key.noModifiers);
      assertRow(handle.getLines(true), 0, "K06", "scrolled to the end, K08 is on line 2");
      Input.clickAt(handle, ~x=1, ~y=2);
      Test.assertEqual(rowClicks^, 1, "now the very same row is clickable, at its NEW position");

      Input.clickAt(handle, ~x=1, ~y=0);
      Test.assertEqual(rowClicks^, 1, "and its old position is not");
      handle.quit();
    });

    Test.run("the wheel passes through a Clickable row to the list", () => {
      let handle =
        Runtime.startHeadless(~config=config20x6, (module ClickableInScrollApp));
      handle.sendKey(Key.End, Key.noModifiers);
      assertRow(handle.getLines(true), 0, "K06", "scrolled to the end");
      /* y=2 is the Clickable row K08. A Clickable without ~onMouseDown has
       * no wheel interest, so the notch must reach the ScrollView UNDER it
       * rather than dying on the row. */
      handle.sendMouse(wheelAt(~kind=Mouse.ScrollUp, ~x=1, ~y=2));
      assertRow(
        handle.getLines(true),
        0,
        "K03",
        "the notch over the clickable row scrolled the list",
      );
      handle.quit();
    });
  });

  Test.group("ScrollView rows: parity with a child of the same content", () => {
    Test.run("the same twelve lines paint the same frame in both modes", () => {
      let childH =
        Runtime.startHeadless(~config=config20x6, (module ParityChildApp));
      let rowsH =
        Runtime.startHeadless(~config=config20x6, (module ParityRowsApp));

      let compare = (what: string) =>
        Test.assertEqualStr(
          String.concat("\n", Array.to_list(rowsH.getLines(true))),
          String.concat("\n", Array.to_list(childH.getLines(true))),
          "rows and child frames agree " ++ what,
        );

      compare("at offset 0");

      childH.sendKey(Key.Arrow_down, Key.noModifiers);
      rowsH.sendKey(Key.Arrow_down, Key.noModifiers);
      compare("after Arrow_down");

      childH.sendKey(Key.End, Key.noModifiers);
      rowsH.sendKey(Key.End, Key.noModifiers);
      compare("after End");
      /* Not a vacuous comparison: End really did move both of them. */
      assertRow(rowsH.getLines(true), 1, "P07", "End reached the last window");

      childH.quit();
      rowsH.quit();
    });
  });

  Test.group("ScrollView rows: mechanics", () => {
    Test.run("<ScrollView rows /> compiles and renders self-closing", () => {
      /* The assertion that matters here is that this file COMPILES: every
         rows fixture above is written <ScrollView rows=... /> with no
         children at all, which only type-checks because ~children is
         optional. The frame check just proves the element is real. */
      let handle =
        Runtime.startHeadless(~config=config20x6, (module ShortRowsApp));
      assertRow(handle.getLines(true), 0, "S00", "a self-closing rows ScrollView paints");
      handle.quit();
    });

    Test.run("the thumb sits where scrollbarMetrics says it should", () => {
      let handle =
        Runtime.startHeadless(~config=config20x6, (module ParityRowsApp));
      /* 12 rows, 5-row window: thumbH = 2, at the top. */
      Test.assertEqual(
        ScrollView.scrollbarMetrics(~contentH=12, ~viewportH=5, ~offset=0),
        Some((0, 2)),
        "the geometry this frame should be drawing",
      );
      let lines = handle.getLines(true);
      Test.assertEqualStr(barCell(lines, 1), thumb, "viewport row 0 is thumb");
      Test.assertEqualStr(barCell(lines, 2), thumb, "viewport row 1 is thumb");
      Test.assertEqualStr(barCell(lines, 3), track, "viewport row 2 is track");

      handle.sendKey(Key.End, Key.noModifiers);
      Test.assertEqual(
        ScrollView.scrollbarMetrics(~contentH=12, ~viewportH=5, ~offset=7),
        Some((3, 2)),
        "at the end the thumb is flush with the bottom",
      );
      let lines = handle.getLines(true);
      Test.assertEqualStr(barCell(lines, 3), track, "viewport row 2 is track");
      Test.assertEqualStr(barCell(lines, 4), thumb, "viewport row 3 is thumb");
      Test.assertEqualStr(barCell(lines, 5), thumb, "viewport row 4 is thumb");
      handle.quit();
    });

    Test.run("the wheel scrolls three rows, without focus", () => {
      let handle =
        Runtime.startHeadless(~config=config20x6, (module ShortRowsApp));
      assertRow(handle.getLines(true), 0, "S00", "the window starts at the top");
      handle.sendMouse(wheelAt(~kind=Mouse.ScrollDown, ~x=2, ~y=3));
      assertRow(handle.getLines(true), 0, "S01", "7 rows in 6: a notch clamps to offset 1");
      handle.sendMouse(wheelAt(~kind=Mouse.ScrollUp, ~x=2, ~y=3));
      assertRow(handle.getLines(true), 0, "S00", "and back up again");
      handle.quit();
    });

    Test.run("controlled mode shows the prop and only reports gestures", () => {
      ctlRowsOffset := 0;
      ctlRowsScrolls := [];
      let handle =
        Runtime.startHeadless(~config=config20x6, (module ControlledRowsApp));
      assertRow(handle.getLines(true), 1, "D00", "the prop decides what is shown");

      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      Test.assertEqual(ctlRowsScrolls^, [1], "onScroll reported where it would go");
      assertRow(handle.getLines(true), 1, "D00", "but nothing moved");

      ctlRowsOffset := 20;
      ctlRowsScrolls := [];
      ignore(handle.render());
      assertRow(handle.getLines(true), 1, "D07", "an out-of-range prop clamps to 7");
      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      Test.assertEqual(
        ctlRowsScrolls^,
        [7],
        "and the next gesture moves from the CLAMPED value",
      );
      handle.quit();
    });

    Test.run("a short final window is padded with blank rows", () => {
      let handle =
        Runtime.startHeadless(~config=config20x6, (module ShortRowsApp));
      handle.sendKey(Key.End, Key.noModifiers);
      let lines = handle.getLines(true);
      Test.assertEqual(Array.length(lines), 6, "the window keeps all six rows");
      assertRow(lines, 0, "S01", "7 rows in a 6-row window end at offset 1");
      assertRow(lines, 5, "S06", "the last content row is the last frame row");

      /* Now a genuinely short slice: three rows of content in a six-row
         window leaves three blanks under them. */
      let handle2 =
        Runtime.startHeadless(
          ~config=config20x6,
          (module
           {
             let make = () =>
               ScrollView.createElement(
                 ~rows=[|"one", "two", "three"|],
                 (),
               );
           }),
        );
      let lines2 = handle2.getLines(true);
      Test.assertEqual(Array.length(lines2), 6, "still a six-row frame");
      assertRow(lines2, 2, "three", "the last content row");
      Test.assertEqualStr(
        String.trim(lines2[3]),
        "",
        "and the rows below it are blank, not missing",
      );
      Test.assertEqualStr(String.trim(lines2[5]), "", "all the way down");
      handle.quit();
      handle2.quit();
    });

    Test.run("an over-wide row is clipped to the content width", () => {
      let handle =
        Runtime.startHeadless(~config=config20x6, (module WideRowsApp));
      let lines = handle.getLines(true);
      Array.iteri(
        (i, l) =>
          Test.assertTrue(
            Element.visibleLength(l) <= 20,
            "frame line "
            ++ string_of_int(i)
            ++ " is "
            ++ string_of_int(Element.visibleLength(l))
            ++ " columns wide, past the 20-column frame",
          ),
        lines,
      );
      assertRow(lines, 0, "WRED", "the row's own text is still at the front");
      Test.assertContains(
        handle.getOutput(false),
        Element.styleToAnsi(Element.FgColor(Element.Red)),
        "and the ANSI inside the surviving part of the row is kept",
      );
      handle.quit();
    });
  });

  Test.group("ScrollView rows: style self-containment", () => {
    Test.run("a row keeps its own colour at every scroll position", () => {
      let handle =
        Runtime.startHeadless(~config=config20x6, (module StyledRowsApp));
      /* Row 7 is magenta; rows 0..5 (the first window) are green. */
      Test.assertFalse(
        Test.contains(handle.getOutput(false), magenta),
        "row 7 is below the window, so its colour is nowhere in the frame",
      );
      Test.assertContains(
        handle.getOutput(false),
        green,
        "the visible rows carry theirs",
      );

      handle.sendKey(Key.End, Key.noModifiers);
      assertRow(handle.getLines(true), 0, "R06", "the last window starts at row 6");
      Test.assertContains(
        handle.getOutput(false),
        magenta,
        "row 7 is on screen now, and brought its own SGR with it - no style "
        ++ "had to be carried across the rows above it",
      );

      handle.sendKey(Key.Home, Key.noModifiers);
      Test.assertFalse(
        Test.contains(handle.getOutput(false), magenta),
        "and scrolling back off screen takes the colour with it",
      );
      handle.quit();
    });
  });

  Test.group("ScrollView rows: scale", () => {
    Test.run("100_000 rows render and scroll in well under a second", () => {
      let t0 = Unix.gettimeofday();
      let handle =
        Runtime.startHeadless(~config=config20x6, (module HugeRowsApp));
      assertRow(handle.getLines(true), 0, "H000000", "the top of a very long list");
      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      handle.sendKey(Key.Page_down, Key.noModifiers);
      handle.sendKey(Key.End, Key.noModifiers);
      let elapsed = Unix.gettimeofday() -. t0;
      assertRow(handle.getLines(true), 0, "H099994", "and End reaches the bottom");
      /* Deliberately generous. The point is not a millisecond budget, it is
         catching a regression to O(content) parsing - which at this size
         costs tens of seconds, not tenths. */
      Test.assertTrue(
        elapsed < 1.0,
        "first frame plus three scrolls took "
        ++ string_of_float(elapsed)
        ++ "s - rows mode must be O(viewport), not O(content)",
      );
      handle.quit();
    });
  });

  Test.group("ScrollView: scrollbarMetrics (pure)", () => {
    Test.run("content that fits has no thumb at all", () => {
      Test.assertEqual(
        ScrollView.scrollbarMetrics(~contentH=5, ~viewportH=5, ~offset=0),
        None,
        "exactly filling the viewport is not scrollable",
      );
      Test.assertEqual(
        ScrollView.scrollbarMetrics(~contentH=3, ~viewportH=5, ~offset=0),
        None,
        "shorter than the viewport is not scrollable",
      );
      Test.assertEqual(
        ScrollView.scrollbarMetrics(~contentH=50, ~viewportH=0, ~offset=0),
        None,
        "a viewport with no rows has no track to draw in",
      );
    });

    Test.run("the thumb is the viewport's share of the content, at least one row", () => {
      Test.assertEqual(
        ScrollView.scrollbarMetrics(~contentH=12, ~viewportH=5, ~offset=0),
        Some((0, 2)),
        "5 of 12 rows: 5*5/12 = 2 rows of thumb, flush with the top",
      );
      Test.assertEqual(
        ScrollView.scrollbarMetrics(~contentH=1000, ~viewportH=5, ~offset=0),
        Some((0, 1)),
        "very long content still shows a one-row thumb",
      );
    });

    Test.run("the thumb travels the track in proportion to the offset", () => {
      Test.assertEqual(
        ScrollView.scrollbarMetrics(~contentH=12, ~viewportH=5, ~offset=4),
        Some((1, 2)),
        "4/7 of the way through a 3-row track",
      );
      Test.assertEqual(
        ScrollView.scrollbarMetrics(~contentH=12, ~viewportH=5, ~offset=7),
        Some((3, 2)),
        "the maximum offset puts the thumb flush with the bottom",
      );
    });

    Test.run("an unclamped offset still lands inside the track", () => {
      Test.assertEqual(
        ScrollView.scrollbarMetrics(~contentH=12, ~viewportH=5, ~offset=999),
        Some((3, 2)),
        "past the end clamps to the bottom of the track",
      );
      Test.assertEqual(
        ScrollView.scrollbarMetrics(~contentH=12, ~viewportH=5, ~offset=-9),
        Some((0, 2)),
        "before the start clamps to the top",
      );
    });
  });
};
