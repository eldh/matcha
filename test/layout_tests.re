/*
 * Tests for the layout engine.
 *
 * Part A exercises Runtime.calculateChildSizes directly (lib/Runtime.re:245)
 * - the pure function that turns a list of size hints (Auto/Chars/Percent/
 * Flex) plus available space into concrete allocated sizes.
 *
 * Part B renders real element trees through Runtime.startHeadless with a
 * small fixed terminal size and checks the resulting lines. Expectations
 * here were derived by first running each scenario and inspecting
 * handle.getLines(true) (per examples/layout-alignment/main.re as the
 * behavioral spec for align/justify), not guessed in advance. Lines
 * commonly carry trailing padding spaces, so assertions below trim
 * trailing whitespace before comparing (`rtrim`) unless the padding itself
 * is what's being verified.
 */
open Matcha;

let rtrim = (s: string): string => {
  let n = String.length(s);
  let rec lastNonSpace = i =>
    if (i < 0) {
      (-1);
    } else if (s.[i] == ' ') {
      lastNonSpace(i - 1);
    } else {
      i;
    };
  let last = lastNonSpace(n - 1);
  String.sub(s, 0, last + 1);
};

let rtrimAll = (lines: array(string)): array(string) => Array.map(rtrim, lines);

/* calculateChildSizes takes children paired with their original index in the
 * stack (static children are filtered out by the caller, so the indices can
 * have holes) and returns (index, child, size) triples. These cases all pass
 * a full, gap-free list, so the indices are simply 0, 1, 2... */
let indexed = (children: list(Element.t)): list((int, Element.t)) =>
  List.mapi((i, child) => (i, child), children);

let sizesOf = (result: list((int, Element.t, int))): list(int) =>
  List.map(((_i, _child, size)) => size, result);

/* calculateChildSizes measures its Auto children through the runtime renderer
 * (in measuring mode), so it needs a root context to hand to any component it
 * meets and the parent's tree path to build child paths from. The Part A cases
 * below only use plain text and fixed size hints, so a throwaway root context
 * and the empty path are enough - no component is ever instantiated. */
let measureRootCtx = Hooks.createContext(_ => ());

/* ============================================================================
 * Part B fixtures - small components rendered through startHeadless
 * ============================================================================ */

module VJustifyStart = {
  let make = () =>
    Element.vstack(
      ~justify=Element.JustifyStart,
      [
        Element.sized(Element.Chars(1), Element.text("A")),
        Element.sized(Element.Chars(1), Element.text("B")),
      ],
    );
};

/* The React `cond ? x : null` idiom: an Element.Empty child must occupy
 * NOTHING - no line, no gap slot - so these two stacks must render
 * identically. (Before the fix, the Empty cost a blank line plus a gap.) */
module VWithEmptyChild = {
  let make = () =>
    Element.vstack(
      ~gap=1,
      [Element.text("A"), Element.Empty, Element.text("B")],
    );
};

module VWithoutEmptyChild = {
  let make = () =>
    Element.vstack(~gap=1, [Element.text("A"), Element.text("B")]);
};

module HWithEmptyChild = {
  let make = () =>
    Element.hstack(
      ~gap=2,
      [Element.text("AA"), Element.Empty, Element.text("BB")],
    );
};

module HWithoutEmptyChild = {
  let make = () =>
    Element.hstack(~gap=2, [Element.text("AA"), Element.text("BB")]);
};

/* An Auto (unsized) HStack of MULTI-LINE children followed by a marker row.
 * The HStack's natural height is its tallest child (3 rows), so "end" must
 * land on frame line 3 - regression: the measuring pass used to concatenate
 * child outputs end-to-end and measure this HStack 5 rows tall (2+3),
 * pushing "end" down and clipping Auto parents. */
module AutoHStackTallChild = {
  let make = () =>
    Element.vstack([
      Element.hstack([
        Element.text("L1\nL2"),
        Element.text("R1\nR2\nR3"),
      ]),
      Element.text("end"),
    ]);
};

/* A root component that renders whatever useLayout() reports. The ROOT body
 * runs in C.make(), BEFORE renderElement installs the frame's constraints,
 * so the runtime has to install them at each root render site itself -
 * regression: a root-level useLayout() used to read the 80x24 default
 * forever, whatever the real terminal size (examples/claude-code drew its
 * input-box borders 80 wide inside a 100-column terminal). */
module RootLayoutEcho = {
  let make = () => {
    let c = Matcha.useLayout();
    Element.text(
      "root:"
      ++ string_of_int(c.Runtime.availWidth)
      ++ "x"
      ++ string_of_int(c.Runtime.availHeight),
    );
  };
};

module VJustifyEnd = {
  let make = () =>
    Element.vstack(
      ~justify=Element.JustifyEnd,
      [
        Element.sized(Element.Chars(1), Element.text("A")),
        Element.sized(Element.Chars(1), Element.text("B")),
      ],
    );
};

module VJustifyCenter = {
  let make = () =>
    Element.vstack(
      ~justify=Element.JustifyCenter,
      [
        Element.sized(Element.Chars(1), Element.text("A")),
        Element.sized(Element.Chars(1), Element.text("B")),
      ],
    );
};

module VJustifySpaceBetween = {
  let make = () =>
    Element.vstack(
      ~justify=Element.JustifySpaceBetween,
      [
        Element.sized(Element.Chars(1), Element.text("A")),
        Element.sized(Element.Chars(1), Element.text("B")),
      ],
    );
};

/* Inner VStack whose single child is pinned to the bottom via JustifyEnd.
 * Wrapping it in an HStack with AlignStart vs AlignStretch shows the real
 * difference between the two: AlignStretch actually hands the inner stack
 * the full container height (letting its own justify take effect), while
 * AlignStart only gives it its natural (unstretched) height and pads the
 * leftover space at the bottom instead. */
module HAlignStartNested = {
  let make = () =>
    Element.hstack(
      ~align=Element.AlignStart,
      [
        Element.sized(
          Element.Chars(4),
          Element.vstack(
            ~justify=Element.JustifyEnd,
            [Element.sized(Element.Chars(1), Element.text("X"))],
          ),
        ),
      ],
    );
};

module HAlignStretchNested = {
  let make = () =>
    Element.hstack(
      ~align=Element.AlignStretch,
      [
        Element.sized(
          Element.Chars(4),
          Element.vstack(
            ~justify=Element.JustifyEnd,
            [Element.sized(Element.Chars(1), Element.text("X"))],
          ),
        ),
      ],
    );
};

module GapTest = {
  let make = () =>
    Element.hstack(
      ~gap=2,
      [
        Element.sized(Element.Chars(2), Element.text("AA")),
        Element.sized(Element.Chars(2), Element.text("BB")),
      ],
    );
};

module SizedTruncate = {
  let make = () =>
    Element.hstack([
      Element.sized(Element.Chars(3), Element.text("HelloWorld")),
    ]);
};

module FlexFill = {
  let make = () =>
    Element.hstack([
      Element.sized(Element.Flex(1), Element.text("X")),
      Element.sized(Element.Flex(1), Element.text("Y")),
    ]);
};

let run = () => {
  Test.group("calculateChildSizes", () => {
    Test.run("zero children returns empty list", () => {
      let result = Runtime.calculateChildSizes([], 20, 1, true, ~rootCtx=measureRootCtx, ~path="");
      Test.assertEqual(result, [], "no children -> []");
    });

    Test.run("Chars sizes are used directly", () => {
      let children = [
        Element.sized(Element.Chars(3), Element.text("x")),
        Element.sized(Element.Chars(4), Element.text("x")),
        Element.sized(Element.Chars(5), Element.text("x")),
      ];
      let result = Runtime.calculateChildSizes(indexed(children), 20, 0, true, ~rootCtx=measureRootCtx, ~path="");
      Test.assertEqual(sizesOf(result), [3, 4, 5], "Chars sizes pass through unchanged");
    });

    Test.run("gap is subtracted before distributing space, but doesn't shrink fixed Chars sizes", () => {
      let children = [
        Element.sized(Element.Chars(3), Element.text("x")),
        Element.sized(Element.Chars(3), Element.text("x")),
      ];
      /* available=10, gap=2 -> availableForContent=8, but Chars(3) still
       * yields exactly 3 regardless - only Percent/Flex read
       * availableForContent. */
      let result = Runtime.calculateChildSizes(indexed(children), 10, 2, true, ~rootCtx=measureRootCtx, ~path="");
      Test.assertEqual(sizesOf(result), [3, 3], "gap doesn't affect Chars sizes");
    });

    Test.run("Percent sizes split proportionally", () => {
      let children = [
        Element.sized(Element.Percent(25), Element.text("x")),
        Element.sized(Element.Percent(75), Element.text("x")),
      ];
      let result = Runtime.calculateChildSizes(indexed(children), 100, 0, true, ~rootCtx=measureRootCtx, ~path="");
      Test.assertEqual(sizesOf(result), [25, 75], "25% and 75% of 100");
    });

    Test.run("Percent rounds down (integer division) per child independently", () => {
      let children = [
        Element.sized(Element.Percent(33), Element.text("x")),
        Element.sized(Element.Percent(33), Element.text("x")),
        Element.sized(Element.Percent(33), Element.text("x")),
      ];
      /* Each child computes availableForContent * 33 / 100 = 10*33/100 = 3
       * independently; the three together only sum to 9, not 10 - there's
       * no remainder redistribution. Documenting current behavior. */
      let result = Runtime.calculateChildSizes(indexed(children), 10, 0, true, ~rootCtx=measureRootCtx, ~path="");
      Test.assertEqual(sizesOf(result), [3, 3, 3], "33% of 10 rounds down to 3, three times");
    });

    Test.run("Flex splits remaining space evenly when ratios are equal", () => {
      let children = [
        Element.sized(Element.Flex(1), Element.text("x")),
        Element.sized(Element.Flex(1), Element.text("x")),
      ];
      let result = Runtime.calculateChildSizes(indexed(children), 10, 0, true, ~rootCtx=measureRootCtx, ~path="");
      Test.assertEqual(sizesOf(result), [5, 5], "equal Flex ratios split 10 evenly");
    });

    Test.run("Flex splits remaining space by ratio (with rounding loss)", () => {
      let children = [
        Element.sized(Element.Flex(1), Element.text("x")),
        Element.sized(Element.Flex(3), Element.text("x")),
      ];
      /* remainingForFlex=10, flexTotal=4 -> 10*1/4=2 (2.5 truncated),
       * 10*3/4=7 (7.5 truncated). Sums to 9, not 10 - same rounding-loss
       * pattern as Percent above, not redistributed either. */
      let result = Runtime.calculateChildSizes(indexed(children), 10, 0, true, ~rootCtx=measureRootCtx, ~path="");
      Test.assertEqual(sizesOf(result), [2, 7], "1:3 ratio of 10 -> 2 and 7");
    });

    Test.run("Flex with flexTotal 0 allocates zero", () => {
      let children = [Element.sized(Element.Flex(0), Element.text("x"))];
      let result = Runtime.calculateChildSizes(indexed(children), 10, 0, true, ~rootCtx=measureRootCtx, ~path="");
      Test.assertEqual(sizesOf(result), [0], "Flex(0) alone -> flexTotal=0 -> size 0");
    });

    Test.run("Auto measures content width", () => {
      let children = [Element.text("hello")];
      let result = Runtime.calculateChildSizes(indexed(children), 20, 0, true, ~rootCtx=measureRootCtx, ~path="");
      Test.assertEqual(sizesOf(result), [5], "Auto width measured from content ('hello' = 5 chars)");
    });

    Test.run("Auto measures content height (line count)", () => {
      let children = [Element.text("line1\nline2\nline3")];
      let result = Runtime.calculateChildSizes(indexed(children), 20, 0, false, ~rootCtx=measureRootCtx, ~path="");
      Test.assertEqual(sizesOf(result), [3], "Auto height measured as line count");
    });

    Test.run("over-constrained: Chars sizes are not clamped to available space", () => {
      /* Chars is an absolute request; the function doesn't clip individual
       * children to fit - it's on the caller to keep totals sane. Not a
       * bug, just documenting there's no clamping here. */
      let children = [Element.sized(Element.Chars(100), Element.text("x"))];
      let result = Runtime.calculateChildSizes(indexed(children), 10, 0, true, ~rootCtx=measureRootCtx, ~path="");
      Test.assertEqual(sizesOf(result), [100], "Chars(100) stays 100 even though available=10");
    });

    Test.run("mixed Chars + Percent + Flex", () => {
      let children = [
        Element.sized(Element.Chars(2), Element.text("x")),
        Element.sized(Element.Percent(50), Element.text("x")),
        Element.sized(Element.Flex(1), Element.text("x")),
      ];
      /* available=20, gap=0: Chars=2, Percent(50) of 20=10, remaining for
       * flex = 20-2-10 = 8, single Flex(1) child gets all of it. */
      let result = Runtime.calculateChildSizes(indexed(children), 20, 0, true, ~rootCtx=measureRootCtx, ~path="");
      Test.assertEqual(sizesOf(result), [2, 10, 8], "Chars, Percent and Flex combine correctly");
    });
  });

  Test.group("Layout Rendering", () => {
    Test.run("VStack JustifyStart packs children at the top, no trailing fill", () => {
      let handle = Runtime.startHeadless(~config={width: 10, height: 6}, (module VJustifyStart));
      let lines = rtrimAll(handle.getLines(true));
      /* JustifyStart doesn't pad the bottom - the output is exactly as
       * tall as its content (2 lines), not the full 6-row container. */
      Test.assertEqual(lines, [|"A", "B"|], "JustifyStart: only 2 lines, content at top");
      handle.quit();
    });

    Test.run("VStack JustifyEnd pads the top to push content to the bottom", () => {
      let handle = Runtime.startHeadless(~config={width: 10, height: 6}, (module VJustifyEnd));
      let lines = rtrimAll(handle.getLines(true));
      Test.assertEqual(
        lines,
        [|"", "", "", "", "A", "B"|],
        "JustifyEnd: 4 blank lines then content, filling all 6 rows",
      );
      handle.quit();
    });

    Test.run("VStack JustifyCenter centers content (extra space rounds to top half)", () => {
      let handle = Runtime.startHeadless(~config={width: 10, height: 6}, (module VJustifyCenter));
      let lines = rtrimAll(handle.getLines(true));
      /* remainingSpace=4, spaceBefore=4/2=2; no space is added after the
       * content, so the output is 4 lines tall, not the full 6. */
      Test.assertEqual(lines, [|"", "", "A", "B"|], "JustifyCenter: 2 blank lines then content");
      handle.quit();
    });

    Test.run("VStack JustifySpaceBetween puts all extra space between children", () => {
      let handle = Runtime.startHeadless(~config={width: 10, height: 6}, (module VJustifySpaceBetween));
      let lines = rtrimAll(handle.getLines(true));
      Test.assertEqual(
        lines,
        [|"A", "", "", "", "", "B"|],
        "JustifySpaceBetween: all 4 spare rows between A and B",
      );
      handle.quit();
    });

    Test.run("an Auto HStack measures as tall as its TALLEST child", () => {
      let config: Runtime.headlessConfig = {width: 20, height: 8};
      let handle =
        Runtime.startHeadless(~config, (module AutoHStackTallChild));
      let lines = rtrimAll(handle.getLines(true));
      Test.assertTrue(
        Array.length(lines) >= 4,
        "the frame has the three stack rows plus the marker",
      );
      Test.assertEqual(
        lines[3],
        "end",
        "the marker sits directly under the 3-row HStack - max height, "
        ++ "not the 5-row sum of its children",
      );
      Test.assertContains(lines[0], "L1", "row 0 holds both first lines");
      Test.assertContains(lines[0], "R1", "side by side");
      Test.assertContains(lines[2], "R3", "the tall child's last line is row 2");
      handle.quit();
    });

    Test.run("a root-level useLayout sees the real terminal size", () => {
      /* Deliberately NOT 80x24: the default config masks this bug - the
         stale ref happens to hold the same values. */
      let config: Runtime.headlessConfig = {width: 57, height: 9};
      let handle = Runtime.startHeadless(~config, (module RootLayoutEcho));
      Test.assertContains(
        handle.getOutput(true),
        "root:57x9",
        "the root body sees the same constraints its children get",
      );
      handle.resize(64, 12);
      Test.assertContains(
        handle.getOutput(true),
        "root:64x12",
        "and a resize reaches the next root render too",
      );
      handle.quit();
    });

    Test.run("an Empty child is invisible to VStack layout", () => {
      let config: Runtime.headlessConfig = {width: 10, height: 6};
      let withEmpty = Runtime.startHeadless(~config, (module VWithEmptyChild));
      let without =
        Runtime.startHeadless(~config, (module VWithoutEmptyChild));
      Test.assertEqualStr(
        withEmpty.getOutput(false),
        without.getOutput(false),
        "Element.Empty takes no line and no gap slot - the null-child idiom",
      );
      withEmpty.quit();
      without.quit();
    });

    Test.run("an Empty child is invisible to HStack layout", () => {
      let config: Runtime.headlessConfig = {width: 20, height: 3};
      let withEmpty = Runtime.startHeadless(~config, (module HWithEmptyChild));
      let without =
        Runtime.startHeadless(~config, (module HWithoutEmptyChild));
      Test.assertEqualStr(
        withEmpty.getOutput(false),
        without.getOutput(false),
        "no column and no gap slot in an HStack either",
      );
      withEmpty.quit();
      without.quit();
    });

    Test.run("HStack AlignStart gives the child its natural height, padding after", () => {
      let handle =
        Runtime.startHeadless(~config={width: 10, height: 3}, (module HAlignStartNested));
      let lines = rtrimAll(handle.getLines(true));
      Test.assertEqual(
        lines,
        [|"X", "", ""|],
        "AlignStart: inner VStack only gets its natural height (1), padded at bottom",
      );
      handle.quit();
    });

    Test.run("HStack AlignStretch gives the child the full container height", () => {
      let handle =
        Runtime.startHeadless(~config={width: 10, height: 3}, (module HAlignStretchNested));
      let lines = rtrimAll(handle.getLines(true));
      Test.assertEqual(
        lines,
        [|"", "", "X"|],
        "AlignStretch: inner VStack gets height 3, so its own JustifyEnd pushes X to the bottom",
      );
      handle.quit();
    });

    Test.run("HStack gap adds space between children, not around them", () => {
      let handle = Runtime.startHeadless(~config={width: 10, height: 1}, (module GapTest));
      let lines = rtrimAll(handle.getLines(true));
      /* padToWidth always resets styles before padding/truncating (see
       * lib/Element.re:354), so strip ANSI to match getLines(true). */
      let expected =
        Element.stripAnsi(
          Element.padToWidth("AA", 2) ++ String.make(2, ' ') ++ Element.padToWidth("BB", 2),
        );
      Test.assertEqual(lines, [|expected|], "gap=2 inserts exactly 2 spaces between columns");
      handle.quit();
    });

    Test.run("Sized(Chars(n)) truncates content wider than its allocated column", () => {
      let handle = Runtime.startHeadless(~config={width: 10, height: 1}, (module SizedTruncate));
      let lines = rtrimAll(handle.getLines(true));
      Test.assertEqual(lines, [|"Hel"|], "Chars(3) truncates 'HelloWorld' to 'Hel'");
      handle.quit();
    });

    Test.run("HStack Flex(1)/Flex(1) splits available width evenly", () => {
      let handle = Runtime.startHeadless(~config={width: 20, height: 1}, (module FlexFill));
      let lines = handle.getLines(true);
      let expected = Element.stripAnsi(Element.padToWidth("X", 10) ++ Element.padToWidth("Y", 10));
      Test.assertEqualStr(lines[0], expected, "two equal Flex children each take half of 20");
      handle.quit();
    });
  });
};
