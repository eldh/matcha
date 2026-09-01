/*
 * Tests for StyledText - parse/bake of ANSI-styled text into per-cell
 * chunks, and wrapString's Wrap/Truncate/TruncateStart/TruncateMiddle
 * transforms, plus the <Text wrap> integration through Runtime.
 */
open Matcha;

let bold = "\027[1m";
let reset = Element.resetAnsi;
let ellipsis = "\xE2\x80\xA6"; /* U+2026, as StyledText.ellipsisChunk emits it */

/* A chunk carrying no style - what plain, unstyled text parses to. */
let plainChunk = (s: string): StyledText.chunk => {
  bytes: s,
  width: TextWidth.charWidth(fst(TextWidth.decodeUtf8(s, 0))),
  styles: [],
};

/* Compare two chunk-lines structurally, with a readable failure message. */
let assertLine =
    (actual: list(StyledText.chunk), expected: list(StyledText.chunk), msg: string)
    : unit =>
  Test.assertEqual(actual, expected, msg);

/* <Sized size> sets the SIZED AXIS of its parent stack (height for a
 * VStack, width for an HStack) - see Runtime's calculateChildSizes. Inside
 * this VStack, Sized(Chars(10)) therefore fixes the Text row's HEIGHT to 10
 * lines, not its width; the wrap width instead comes from the VStack's own
 * availWidth, which the headless config below pins at 10 columns. That is
 * what makes "hello wonderful world" wrap at 10 here. */
module WrapHeadlessDemo = {
  let make = () =>
    <VStack>
      <Sized size={Element.Chars(10)}>
        <Text wrap=Element.Wrap> "hello wonderful world" </Text>
      </Sized>
      <Text> "below" </Text>
    </VStack>;
};

let run = () => {
  Test.group("StyledText: parse/bake round-trip", () => {
    Test.run("single-style Text round-trips byte-for-byte", () => {
      let el = <Text bold=true> "hi" </Text>;
      let rendered = Element.render(el);
      Test.assertEqualStr(rendered, bold ++ "hi" ++ reset, "sanity: rendered bytes");
      let baked = StyledText.bake(StyledText.parse(rendered));
      Test.assertEqualStr(baked, rendered, "bake(parse(x)) == x for a single style layer");
    });

    Test.run("unstyled Text round-trips byte-for-byte", () => {
      let rendered = Element.render(<Text> "plain text" </Text>);
      Test.assertEqualStr(rendered, "plain text", "sanity: no ANSI at all");
      let baked = StyledText.bake(StyledText.parse(rendered));
      Test.assertEqualStr(baked, rendered, "bake(parse(x)) == x with no styling");
    });

    Test.run(
      "multi-style Text (nested Styled wrappers) round-trips at the chunk level",
      () => {
      /* <Text bold=true color=Red> nests two Styled wrappers, so
       * Element.render emits a doubled trailing reset ("...\027[0m\027[0m" -
       * one per wrapper). parse() has no way to represent "an extra reset
       * that touches no character", so bake() only re-emits ONE trailing
       * reset per line. That is a deliberate consequence of the documented
       * bake algorithm (reset-if-emitted, once per line), not a bug: the
       * chunk-level content - which characters carry which styles - still
       * round-trips exactly, which is what the second assertion checks. */
      let rendered = Element.render(<Text bold=true color=Red> "hi" </Text>);
      Test.assertEqualStr(
        rendered,
        bold ++ "\027[38;5;1m" ++ "hi" ++ reset ++ reset,
        "sanity: nested Styled wrappers double the trailing reset",
      );
      let parsed = StyledText.parse(rendered);
      let baked = StyledText.bake(parsed);
      Test.assertEqualStr(
        baked,
        bold ++ "\027[38;5;1m" ++ "hi" ++ reset,
        "bake collapses the redundant second reset",
      );
      Test.assertEqual(
        StyledText.parse(baked),
        parsed,
        "re-parsing the baked string yields the identical chunks (idempotent)",
      );
    });
  });

  /* Truecolor has to survive this module intact, because every clipped or
   * wrapped row goes through parse -> transform -> bake. A parser that did
   * not know "38;2;r;g;b" would silently DROP the style on exactly those
   * rows, so a tinted diff would lose its background wherever it was cut. */
  Test.group("StyledText: 24-bit truecolor round-trip", () => {
    Test.run("a truecolor fg+bg line round-trips byte-for-byte", () => {
      let fg = Element.styleToAnsi(Element.FgColor(Element.RgbFull(12, 200, 255)));
      let bg = Element.styleToAnsi(Element.BgColor(Element.RgbFull(0, 40, 8)));
      Test.assertEqualStr(fg, "\027[38;2;12;200;255m", "sanity: fg bytes");
      Test.assertEqualStr(bg, "\027[48;2;0;40;8m", "sanity: bg bytes");
      let rendered = fg ++ bg ++ "hi" ++ reset;
      Test.assertEqualStr(
        StyledText.bake(StyledText.parse(rendered)),
        rendered,
        "bake(parse(x)) == x for a truecolor fg+bg span",
      );
    });

    Test.run("parse decodes truecolor into RgbFull chunks", () => {
      let fg = Element.styleToAnsi(Element.FgColor(Element.RgbFull(1, 2, 3)));
      let bg = Element.styleToAnsi(Element.BgColor(Element.RgbFull(4, 5, 6)));
      switch (StyledText.parse(fg ++ bg ++ "a" ++ reset)) {
      | [[c]] =>
        assertLine(
          [c],
          [
            {
              bytes: "a",
              width: 1,
              styles: [
                Element.FgColor(Element.RgbFull(1, 2, 3)),
                Element.BgColor(Element.RgbFull(4, 5, 6)),
              ],
            },
          ],
          "both direct-color styles are active on the cell",
        )
      | other =>
        Test.assertEqual(List.length(other), 1, "expected exactly one chunk line")
      };
    });

    Test.run("truncateLine keeps the truecolor styles at the cut", () => {
      let bg = Element.styleToAnsi(Element.BgColor(Element.RgbFull(0, 40, 8)));
      let rendered = bg ++ "abcdefgh" ++ reset;
      switch (StyledText.parse(rendered)) {
      | [line] =>
        let clipped = StyledText.bake([StyledText.truncateLine(line, 4)]);
        Test.assertEqualStr(
          clipped,
          bg ++ "abc" ++ ellipsis ++ reset,
          "the clipped row re-opens the exact truecolor background, "
          ++ "ellipsis included",
        );
      | _ => Test.assertTrue(false, "expected one parsed line")
      };
    });

    Test.run("a wrap across a truecolor span re-opens it on the next line", () => {
      let bg = Element.styleToAnsi(Element.BgColor(Element.RgbFull(72, 8, 8)));
      let wrapped =
        StyledText.wrapString(
          ~mode=Element.Wrap,
          ~width=6,
          bg ++ "hello world" ++ reset,
        );
      Test.assertEqualStr(
        wrapped,
        bg ++ "hello" ++ reset ++ "\n" ++ bg ++ "world" ++ reset,
        "the continuation line carries the same 48;2 escape, not a lost tint",
      );
    });

    Test.run("a malformed direct-color introducer drops the style, no crash", () => {
      /* Three params where five are needed: the sequence is consumed and the
         style dropped, rather than an RgbFull built from what is not there. */
      Test.assertEqualStr(
        StyledText.bake(StyledText.parse("\027[38;2;12m" ++ "hi")),
        "hi",
        "truncated 38;2 leaves the text unstyled",
      );
      Test.assertEqualStr(
        StyledText.bake(StyledText.parse("\027[48;2m" ++ "hi")),
        "hi",
        "a bare 48;2 with no channels at all is dropped too",
      );
    });
  });

  Test.group("StyledText: wrapString(Wrap)", () => {
    Test.run("plain greedy word wrap", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.Wrap, ~width=10, "hello wonderful world"),
        "hello\nwonderful\nworld",
        "wraps at word boundaries, dropping the boundary spaces",
      )
    });

    Test.run("styled continuation line re-opens the active style", () => {
      let input = bold ++ "hello wonderful world" ++ reset;
      let out = StyledText.wrapString(~mode=Element.Wrap, ~width=10, input);
      let lines = String.split_on_char('\n', out);
      Test.assertEqual(List.length(lines), 3, "still three lines");
      Test.assertContains(
        List.nth(lines, 1),
        bold,
        "line 2 (\"wonderful\") re-opens the bold escape",
      );
      Test.assertContains(
        List.nth(lines, 1),
        "wonderful",
        "line 2 still carries the right text",
      );
    });

    Test.run("a wide cell is never split across a wrap boundary", () => {
      /* "abc" (width 3) + a CJK char (width 2) = width 5, one unbreakable
       * word (no spaces) at width 4: the wide char can't fit after "abc"
       * (3+2 > 4), so it must move to its own line - leaving line 1 one
       * column short rather than splitting the wide cell. */
      let wide = "\xE6\x97\xA5"; /* 日, width 2 */
      let out = StyledText.wrapString(~mode=Element.Wrap, ~width=4, "abc" ++ wide);
      Test.assertEqualStr(out, "abc\n" ++ wide, "line 1 is short, the wide char moves whole");
    });

    Test.run("an unbreakable word hard-breaks at cell granularity", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.Wrap, ~width=5, "abcdefghijkl"),
        "abcde\nfghij\nkl",
        "12 chars, no spaces, packed 5/5/2 at width 5",
      )
    });

    Test.run("a leading space is dropped rather than starting a line", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.Wrap, ~width=10, " hello"),
        "hello",
        "the space before the first word never appears",
      )
    });

    Test.run("a space that doesn't fit is dropped, not carried to the next line", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.Wrap, ~width=5, "abcde fghij"),
        "abcde\nfghij",
        "the separating space fits nowhere, so it just disappears",
      )
    });

    Test.run("an empty line wraps to one empty line, not zero", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.Wrap, ~width=10, ""),
        "",
        "wrapping empty text yields empty text",
      );
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.Wrap, ~width=10, "a\n\nb"),
        "a\n\nb",
        "an embedded blank line survives wrapping",
      );
    });

    Test.run("width <= 0 is clamped to 1", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.Wrap, ~width=0, "ab"),
        "a\nb",
        "width 0 behaves like width 1",
      )
    });
  });

  Test.group("StyledText: wrapString(Truncate)", () => {
    Test.run("text that already fits is unchanged - no ellipsis", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.Truncate, ~width=5, "abcde"),
        "abcde",
        "exact-width fit stays intact",
      );
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.TruncateStart, ~width=10, "abc"),
        "abc",
        "TruncateStart on fitting text is a no-op",
      );
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.TruncateMiddle, ~width=3, "abc"),
        "abc",
        "TruncateMiddle on fitting text is a no-op",
      );
    });

    Test.run("cuts the end and appends an ellipsis", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.Truncate, ~width=5, "abcdefghij"),
        "abcd" ++ ellipsis,
        "longest prefix <= w-1, plus the ellipsis",
      )
    });

    Test.run("the ellipsis is styled like the last kept cell", () => {
      let input = bold ++ "abcdefghij" ++ reset;
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.Truncate, ~width=5, input),
        bold ++ "abcd" ++ ellipsis ++ reset,
        "the bold that covered 'd' keeps covering the ellipsis",
      )
    });

    Test.run("w=1 keeps nothing: the ellipsis alone", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.Truncate, ~width=1, "abcdefghij"),
        ellipsis,
        "w-1 == 0, so the kept prefix is empty",
      )
    });

    Test.run("w=0 is empty", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.Truncate, ~width=0, "abcdefghij"),
        "",
        "no width at all leaves nothing, not even the ellipsis",
      )
    });
  });

  Test.group("StyledText: wrapString(TruncateStart)", () => {
    Test.run("cuts the start and prepends an ellipsis", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.TruncateStart, ~width=5, "abcdefghij"),
        ellipsis ++ "ghij",
        "the ellipsis, plus the longest suffix <= w-1",
      )
    });

    Test.run("w=1 keeps nothing: the ellipsis alone", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.TruncateStart, ~width=1, "abcdefghij"),
        ellipsis,
        "w-1 == 0, so the kept suffix is empty",
      )
    });

    Test.run("w=0 is empty", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.TruncateStart, ~width=0, "abcdefghij"),
        "",
        "no width at all leaves nothing",
      )
    });
  });

  Test.group("StyledText: wrapString(TruncateMiddle)", () => {
    Test.run("cuts the middle, ellipsis between the two halves", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.TruncateMiddle, ~width=6, "abcdefghij"),
        "abc" ++ ellipsis ++ "ij",
        "headW = ceil((w-1)/2) = 3, tailW = (w-1)-headW = 2",
      )
    });

    Test.run("an odd width still splits head/tail per the ceil formula", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.TruncateMiddle, ~width=7, "abcdefghij"),
        "abc" ++ ellipsis ++ "hij",
        "headW = ceil(6/2) = 3, tailW = 6-3 = 3",
      )
    });

    Test.run("w=1 keeps nothing: the ellipsis alone", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.TruncateMiddle, ~width=1, "abcdefghij"),
        ellipsis,
        "headW = tailW = 0",
      )
    });

    Test.run("w=0 is empty", () => {
      Test.assertEqualStr(
        StyledText.wrapString(~mode=Element.TruncateMiddle, ~width=0, "abcdefghij"),
        "",
        "no width at all leaves nothing",
      )
    });
  });

  Test.group("StyledText: parse building blocks", () => {
    Test.run("parse tracks a color change mid-line (second FgColor replaces first)", () => {
      let input = "\027[38;5;1ma\027[38;5;2mb\027[0m";
      let lines = StyledText.parse(input);
      Test.assertEqual(List.length(lines), 1, "one line");
      assertLine(
        List.hd(lines),
        [
          {StyledText.bytes: "a", width: 1, styles: [Element.FgColor(Element.Red)]},
          {StyledText.bytes: "b", width: 1, styles: [Element.FgColor(Element.Green)]},
        ],
        "the second FgColor replaces the first, not stacks alongside it",
      );
    });

    Test.run("parse fuses a combining mark onto the previous chunk", () => {
      let lines = StyledText.parse("e\xCC\x81x");
      assertLine(
        List.hd(lines),
        [
          {StyledText.bytes: "e\xCC\x81", width: 1, styles: []},
          plainChunk("x"),
        ],
        "combining acute fuses onto 'e'",
      );
    });

    Test.run("parse drops unknown escape sequences without touching style", () => {
      /* Cursor-forward (ESC[5C) is not a style code Matcha emits; it must
       * be skipped to its final byte and leave the active style set alone,
       * and it must not itself become part of any chunk's bytes. */
      let lines = StyledText.parse(bold ++ "\027[5Ca" ++ reset);
      assertLine(
        List.hd(lines),
        [{StyledText.bytes: "a", width: 1, styles: [Element.Bold]}],
        "'a' is still bold, and the unknown escape produced no chunk",
      );
    });
  });

  Test.group("StyledText: sliceLines", () => {
    Test.run("keeps exactly the requested line range", () => {
      let s = "a\nb\nc\nd\ne";
      Test.assertEqualStr(
        StyledText.sliceLines(s, ~from=1, ~count=3),
        "b\nc\nd",
        "lines 1..3 of five",
      );
      Test.assertEqualStr(
        StyledText.sliceLines(s, ~from=0, ~count=1),
        "a",
        "a single line off the front",
      );
      Test.assertEqualStr(
        StyledText.sliceLines(s, ~from=4, ~count=1),
        "e",
        "the last line",
      );
    });

    Test.run("from=0 over the whole range is identity, modulo re-baking", () => {
      /* <Text bold> nests one Styled wrapper per prop, so the rendered
       * bytes carry a doubled trailing reset that bake collapses (see the
       * round-trip group above). The two things a full-range slice must
       * preserve are therefore: every visible character, and every style
       * on it. */
      let rendered =
        Element.render(
          <VStack>
            <Text bold=true color=Red> "one" </Text>
            <Text> "two" </Text>
            <Text underline=true> "three" </Text>
          </VStack>,
        );
      let sliced = StyledText.sliceLines(rendered, ~from=0, ~count=3);
      Test.assertEqualStr(
        Element.stripAnsi(sliced),
        Element.stripAnsi(rendered),
        "every visible character survives a full-range slice",
      );
      Test.assertEqual(
        StyledText.parse(sliced),
        StyledText.parse(rendered),
        "and so does every style on every cell",
      );
    });

    Test.run("a slice inside a styled block re-opens the style", () => {
      /* One Styled wrapper around a three-line Text: Element.render emits
       * the bold escape ONCE, before line 1, and the reset after line 3.
       * Slicing lines 1..2 with plain string surgery would therefore lose
       * the escape entirely and render the window unstyled. */
      let rendered = Element.render(<Text bold=true> "one\ntwo\nthree" </Text>);
      Test.assertEqualStr(
        rendered,
        bold ++ "one\ntwo\nthree" ++ reset,
        "sanity: the style is opened once, on the first line only",
      );
      Test.assertEqualStr(
        StyledText.sliceLines(rendered, ~from=1, ~count=2),
        bold ++ "two" ++ reset ++ "\n" ++ bold ++ "three" ++ reset,
        "the slice re-opens bold on its first line (and on every line it writes)",
      );
      Test.assertEqualStr(
        StyledText.sliceLines(rendered, ~from=2, ~count=1),
        bold ++ "three" ++ reset,
        "a one-line slice from the tail of the block is still bold",
      );
    });

    Test.run("a range past the end yields fewer lines - it never pads", () => {
      let s = "a\nb\nc";
      Test.assertEqualStr(
        StyledText.sliceLines(s, ~from=2, ~count=10),
        "c",
        "only the lines that exist come back",
      );
      Test.assertEqualStr(
        StyledText.sliceLines(s, ~from=3, ~count=2),
        "",
        "a range starting past the end is empty",
      );
      Test.assertEqualStr(
        StyledText.sliceLines(s, ~from=99, ~count=2),
        "",
        "so is one starting far past the end",
      );
    });

    Test.run("count<=0 is empty, from<0 clamps to the first line", () => {
      let s = "a\nb\nc";
      Test.assertEqualStr(StyledText.sliceLines(s, ~from=1, ~count=0), "", "count=0");
      Test.assertEqualStr(StyledText.sliceLines(s, ~from=1, ~count=-3), "", "count<0");
      Test.assertEqualStr(
        StyledText.sliceLines(s, ~from=-2, ~count=2),
        "a\nb",
        "a negative start begins at line 0 (and still takes count lines)",
      );
    });

    Test.run("wide and zero-width cells are not split by a slice", () => {
      let s = "ab\n\xE6\x97\xA5\xE6\x9C\xAC\ne\xCC\x81x";
      Test.assertEqualStr(
        StyledText.sliceLines(s, ~from=1, ~count=1),
        "\xE6\x97\xA5\xE6\x9C\xAC",
        "a line of double-width characters comes back whole",
      );
      Test.assertEqualStr(
        StyledText.sliceLines(s, ~from=2, ~count=1),
        "e\xCC\x81x",
        "a combining mark stays fused to the character it belongs to",
      );
    });
  });

  /* ---------------------------------------------------------------------
   * splitAtWidth / padChunksToWidth (B3) - the splice primitives the
   * overlay compositor is built from. Their two invariants are what stop a
   * spliced-in box shifting the columns to its right:
   *   width(fst) == min(w, total)     and     width(fst)+width(snd) == total
   * ------------------------------------------------------------------- */
  Test.group("StyledText: splitAtWidth", () => {
    let widthOf = StyledText.chunkListWidth;
    let parseOne = (s: string): list(StyledText.chunk) =>
      switch (StyledText.parse(s)) {
      | [line, ..._] => line
      | [] => []
      };
    /* The two invariants, asserted together, for one (input, w) pair. */
    let checkInvariants = (s: string, w: int, msg: string): unit => {
      let line = parseOne(s);
      let total = widthOf(line);
      let (a, b) = StyledText.splitAtWidth(line, w);
      Test.assertEqual(
        widthOf(a),
        min(max(0, w), total),
        msg ++ ": width(fst) == min(w, total)",
      );
      Test.assertEqual(
        widthOf(a) + widthOf(b),
        total,
        msg ++ ": the two halves still add up to the whole",
      );
    };

    Test.run("widths and round-trip on plain text", () => {
      let line = parseOne("abcdef");
      let (a, b) = StyledText.splitAtWidth(line, 2);
      Test.assertEqualStr(StyledText.bake([a]), "ab", "left half");
      Test.assertEqualStr(StyledText.bake([b]), "cdef", "right half");
      Test.assertEqualStr(
        StyledText.bake([a @ b]),
        "abcdef",
        "concatenating the halves reproduces the line",
      );
      checkInvariants("abcdef", 2, "cut in the middle");
    });

    Test.run("w <= 0 and w >= total are the degenerate ends", () => {
      let line = parseOne("abc");
      let (a0, b0) = StyledText.splitAtWidth(line, 0);
      Test.assertEqual(List.length(a0), 0, "w=0: nothing on the left");
      Test.assertEqualStr(StyledText.bake([b0]), "abc", "w=0: everything on the right");
      let (an, bn) = StyledText.splitAtWidth(line, -5);
      Test.assertEqual(List.length(an), 0, "a negative w behaves like 0");
      Test.assertEqualStr(StyledText.bake([bn]), "abc", "negative w: all on the right");
      let (a9, b9) = StyledText.splitAtWidth(line, 9);
      Test.assertEqualStr(StyledText.bake([a9]), "abc", "w past the end keeps everything");
      Test.assertEqual(List.length(b9), 0, "w past the end leaves no remainder");
      checkInvariants("abc", 0, "w=0");
      checkInvariants("abc", 9, "w past the end");
      checkInvariants("", 3, "an empty line");
    });

    Test.run("a double-width cell straddling the cut blanks BOTH sides", () => {
      /* "a" + CJK(2 cols) + "b" = 4 columns. Cutting at 2 lands inside the
         wide cell: neither half may contain half a codepoint, and the total
         column count must not change. */
      let s = "a\xE6\x97\xA5b";
      let line = parseOne(s);
      Test.assertEqual(widthOf(line), 4, "the fixture is 4 columns wide");
      let (a, b) = StyledText.splitAtWidth(line, 2);
      Test.assertEqualStr(StyledText.bake([a]), "a ", "left half pads the straddle with a blank");
      Test.assertEqualStr(StyledText.bake([b]), " b", "right half does too");
      checkInvariants(s, 2, "straddle from the left");
      /* And from the other side: cutting at 3 leaves the wide cell whole on
         the left, so nothing is blanked. */
      let (c, d) = StyledText.splitAtWidth(line, 3);
      Test.assertEqualStr(StyledText.bake([c]), "a\xE6\x97\xA5", "cut after the wide cell keeps it intact");
      Test.assertEqualStr(StyledText.bake([d]), "b", "and the tail is untouched");
      checkInvariants(s, 3, "cut after the wide cell");
      checkInvariants(s, 1, "cut before the wide cell");
    });

    Test.run("a styled cut leaves the right half re-opening the style", () => {
      /* "ab" plain, then bold "cd". Cutting at 3 puts the bold "c" on the
         left and the bold "d" on the right; baking the right half alone has
         to re-emit the bold escape, or the spliced row renders unstyled. */
      let line = parseOne("ab" ++ bold ++ "cd" ++ reset);
      let (a, b) = StyledText.splitAtWidth(line, 3);
      Test.assertEqualStr(StyledText.bake([a]), "ab" ++ bold ++ "c" ++ reset, "left half closes its style");
      Test.assertEqualStr(StyledText.bake([b]), bold ++ "d" ++ reset, "right half re-opens it");
      /* The straddle blank inherits the style of the cell it replaced. */
      let wide = parseOne(bold ++ "\xE6\x97\xA5" ++ reset);
      let (wa, wb) = StyledText.splitAtWidth(wide, 1);
      Test.assertEqualStr(StyledText.bake([wa]), bold ++ " " ++ reset, "the left blank keeps the wide cell's style");
      Test.assertEqualStr(StyledText.bake([wb]), bold ++ " " ++ reset, "so does the right one");
    });

    Test.run("the middle-of-a-row splice a compositor actually performs", () => {
      /* left = the columns before the box, right = the columns after it -
         the "drop exactly what the box covers" operation that neither
         takeWidthPrefix nor takeWidthSuffix can express. */
      let line = parseOne("0123456789");
      let (left, rest) = StyledText.splitAtWidth(line, 3);
      let (covered, right) = StyledText.splitAtWidth(rest, 4);
      Test.assertEqualStr(StyledText.bake([left]), "012", "columns left of the box");
      Test.assertEqualStr(StyledText.bake([covered]), "3456", "the columns the box covers");
      Test.assertEqualStr(StyledText.bake([right]), "789", "columns right of the box");
      Test.assertEqual(
        widthOf(left) + widthOf(covered) + widthOf(right),
        10,
        "the three pieces still tile the row exactly",
      );
    });
  });

  Test.group("StyledText: padChunksToWidth", () => {
    Test.run("pads short rows with UNSTYLED blanks and never truncates", () => {
      let line =
        switch (StyledText.parse(bold ++ "ab" ++ reset)) {
        | [l, ..._] => l
        | [] => []
        };
      let padded = StyledText.padChunksToWidth(line, 5);
      Test.assertEqual(StyledText.chunkListWidth(padded), 5, "padded to exactly 5 columns");
      Test.assertEqualStr(
        StyledText.bake([padded]),
        bold ++ "ab" ++ reset ++ "   ",
        "the padding is unstyled - it must not inherit the row's open style",
      );
      let short = StyledText.padChunksToWidth(line, 1);
      Test.assertEqual(
        StyledText.chunkListWidth(short),
        2,
        "already wider than the target: returned unchanged, not truncated",
      );
      Test.assertEqual(
        StyledText.chunkListWidth(StyledText.padChunksToWidth([], 4)),
        4,
        "an empty row pads to a full run of blanks - what makes an overlay opaque",
      );
      Test.assertEqual(
        StyledText.chunkListWidth(StyledText.padChunksToWidth([], 0)),
        0,
        "a zero-width target adds nothing",
      );
    });
  });

  Test.group("StyledText: <Text wrap> headless integration", () => {
    Test.run(
      "wraps at the layout width and agrees with the Auto sibling below it (measuring)",
      () => {
      let handle =
        Runtime.startHeadless(~config={width: 10, height: 12}, (module WrapHeadlessDemo));
      let lines = handle.getLines(true);
      Test.assertEqual(Array.length(lines), 11, "10 rows for the Sized(10) block, plus 1 for the sibling");
      Test.assertEqualStr(String.trim(lines[0]), "hello", "line 1 of the wrap");
      Test.assertEqualStr(String.trim(lines[1]), "wonderful", "line 2 of the wrap");
      Test.assertEqualStr(String.trim(lines[2]), "world", "line 3 of the wrap");
      for (i in 3 to 9) {
        Test.assertEqualStr(
          String.trim(lines[i]),
          "",
          "the rest of the Sized(10) block is blank padding",
        );
      };
      Test.assertEqualStr(
        String.trim(lines[10]),
        "below",
        "the Auto sibling renders right after the wrapped block - same line " ++
        "count the real pass and the measuring pass (used for Auto sizing " ++
        "elsewhere) both see",
      );
      handle.quit();
    });
  });
};
