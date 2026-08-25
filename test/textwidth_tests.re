/*
 * Tests for TextWidth - UTF-8 decoding and terminal display width,
 * plus the two Element helpers that now measure in columns
 * (visibleLength / padToWidth).
 *
 * Byte-exact expectations are written with explicit \xHH escapes wherever
 * the exact encoding is the point of the test.
 */
open Matcha;

let reset = Element.resetAnsi;

let assertPair =
    (actual: (int, int), expected: (int, int), msg: string): unit => {
  let (a1, a2) = actual;
  let (e1, e2) = expected;
  Test.assertTrue(
    a1 == e1 && a2 == e2,
    Printf.sprintf(
      "%s (expected (0x%X, %d), got (0x%X, %d))",
      msg,
      e1,
      e2,
      a1,
      a2,
    ),
  );
};

let assertCells =
    (actual: array(TextWidth.cell), expected: list((string, int)), msg: string)
    : unit => {
  let got =
    Array.to_list(actual)
    |> List.map((c: TextWidth.cell) => (c.bytes, c.width));
  Test.assertEqual(List.length(got), List.length(expected), msg ++ " (cell count)");
  List.iteri(
    (i, (bytes, width)) => {
      let (gotBytes, gotWidth) = List.nth(got, i);
      Test.assertEqualStr(
        gotBytes,
        bytes,
        Printf.sprintf("%s (cell %d bytes)", msg, i),
      );
      Test.assertEqual(
        gotWidth,
        width,
        Printf.sprintf("%s (cell %d width)", msg, i),
      );
    },
    expected,
  );
};

let run = () => {
  Test.group("TextWidth: utf8ExpectedLen", () => {
    Test.run("lead byte announces its sequence length", () => {
      Test.assertEqual(TextWidth.utf8ExpectedLen('a'), 1, "ASCII -> 1");
      Test.assertEqual(TextWidth.utf8ExpectedLen('\x00'), 1, "NUL -> 1");
      Test.assertEqual(TextWidth.utf8ExpectedLen('\x7F'), 1, "0x7F -> 1");
      Test.assertEqual(TextWidth.utf8ExpectedLen('\xC3'), 2, "0xC3 -> 2");
      Test.assertEqual(TextWidth.utf8ExpectedLen('\xE6'), 3, "0xE6 -> 3");
      Test.assertEqual(TextWidth.utf8ExpectedLen('\xF0'), 4, "0xF0 -> 4");
    });

    Test.run("invalid lead bytes report 1 so scanning advances", () => {
      Test.assertEqual(TextWidth.utf8ExpectedLen('\x80'), 1, "continuation -> 1");
      Test.assertEqual(TextWidth.utf8ExpectedLen('\xC0'), 1, "overlong lead -> 1");
      Test.assertEqual(TextWidth.utf8ExpectedLen('\xC1'), 1, "overlong lead -> 1");
      Test.assertEqual(TextWidth.utf8ExpectedLen('\xF5'), 1, "out of range -> 1");
      Test.assertEqual(TextWidth.utf8ExpectedLen('\xFF'), 1, "0xFF -> 1");
    });
  });

  Test.group("TextWidth: decodeUtf8", () => {
    Test.run("decodes 1/2/3/4-byte sequences", () => {
      assertPair(TextWidth.decodeUtf8("a", 0), (0x61, 1), "'a'");
      assertPair(TextWidth.decodeUtf8("\xC3\xA9", 0), (0xE9, 2), "'e-acute'");
      assertPair(TextWidth.decodeUtf8("\xE6\x97\xA5", 0), (0x65E5, 3), "'ri'");
      assertPair(
        TextWidth.decodeUtf8("\xF0\x9F\x98\x80", 0),
        (0x1F600, 4),
        "grinning face",
      );
    });

    Test.run("decodes at a non-zero offset", () => {
      assertPair(
        TextWidth.decodeUtf8("a\xE6\x97\xA5b", 1),
        (0x65E5, 3),
        "offset 1 is the 3-byte char",
      );
      assertPair(TextWidth.decodeUtf8("a\xE6\x97\xA5b", 4), (0x62, 1), "'b'");
    });

    Test.run("malformed input yields U+FFFD and consumes one byte", () => {
      assertPair(TextWidth.decodeUtf8("\xFF", 0), (0xFFFD, 1), "0xFF");
      assertPair(TextWidth.decodeUtf8("\xA9", 0), (0xFFFD, 1), "lone continuation");
      assertPair(TextWidth.decodeUtf8("\xC3", 0), (0xFFFD, 1), "truncated 2-byte");
      assertPair(
        TextWidth.decodeUtf8("\xC3\x41", 0),
        (0xFFFD, 1),
        "bad continuation byte",
      );
      assertPair(
        TextWidth.decodeUtf8("\xE6\x97", 0),
        (0xFFFD, 1),
        "truncated 3-byte",
      );
      assertPair(
        TextWidth.decodeUtf8("\xED\xA0\x80", 0),
        (0xFFFD, 1),
        "surrogate is rejected",
      );
      assertPair(
        TextWidth.decodeUtf8("\xC0\x80", 0),
        (0xFFFD, 1),
        "overlong NUL is rejected",
      );
    });

    Test.run("out-of-bounds offset never raises", () => {
      assertPair(TextWidth.decodeUtf8("", 0), (0xFFFD, 1), "empty string");
      assertPair(TextWidth.decodeUtf8("ab", 5), (0xFFFD, 1), "past the end");
      assertPair(TextWidth.decodeUtf8("ab", -1), (0xFFFD, 1), "negative offset");
    });
  });

  Test.group("TextWidth: charWidth", () => {
    Test.run("ASCII and Latin are one column", () => {
      Test.assertEqual(TextWidth.charWidth(Char.code('a')), 1, "'a' = 1");
      Test.assertEqual(TextWidth.charWidth(0xE9), 1, "e-acute = 1");
      Test.assertEqual(TextWidth.charWidth(0x20), 1, "space = 1");
    });

    Test.run("CJK and emoji are two columns", () => {
      Test.assertEqual(TextWidth.charWidth(0x4E2D), 2, "U+4E2D = 2");
      Test.assertEqual(TextWidth.charWidth(0x65E5), 2, "U+65E5 = 2");
      Test.assertEqual(TextWidth.charWidth(0xAC00), 2, "Hangul = 2");
      Test.assertEqual(TextWidth.charWidth(0xFF21), 2, "fullwidth A = 2");
      Test.assertEqual(TextWidth.charWidth(0x1F600), 2, "grinning face = 2");
      Test.assertEqual(TextWidth.charWidth(0x1F680), 2, "rocket = 2");
    });

    Test.run("combining marks and controls are zero columns", () => {
      Test.assertEqual(TextWidth.charWidth(0x0301), 0, "combining acute = 0");
      Test.assertEqual(TextWidth.charWidth(0x0000), 0, "NUL = 0");
      Test.assertEqual(TextWidth.charWidth(0x001B), 0, "ESC = 0");
      Test.assertEqual(TextWidth.charWidth(0xFE0F), 0, "variation selector = 0");
      Test.assertEqual(TextWidth.charWidth(0xFEFF), 0, "BOM = 0");
      Test.assertEqual(TextWidth.charWidth(0x200B), 0, "zero-width space = 0");
    });

    Test.run("box drawing stays one column (goldens depend on it)", () => {
      Test.assertEqual(TextWidth.charWidth(0x2500), 1, "U+2500 = 1");
      Test.assertEqual(TextWidth.charWidth(0x2502), 1, "U+2502 = 1");
      Test.assertEqual(TextWidth.charWidth(0x250C), 1, "U+250C = 1");
      Test.assertEqual(TextWidth.charWidth(0x257F), 1, "U+257F = 1");
      Test.assertEqual(TextWidth.charWidth(0x2588), 1, "block element = 1");
    });

    Test.run("range boundaries are inclusive", () => {
      Test.assertEqual(TextWidth.charWidth(0x10FF), 1, "just below U+1100");
      Test.assertEqual(TextWidth.charWidth(0x1100), 2, "first wide Hangul jamo");
      Test.assertEqual(TextWidth.charWidth(0x115F), 2, "last wide Hangul jamo");
      Test.assertEqual(TextWidth.charWidth(0x1160), 1, "just above U+115F");
      Test.assertEqual(TextWidth.charWidth(0x02FF), 1, "just below combining");
      Test.assertEqual(TextWidth.charWidth(0x036F), 0, "last combining mark");
      Test.assertEqual(TextWidth.charWidth(0x0370), 1, "just above combining");
      Test.assertEqual(TextWidth.charWidth(0xFFFD), 1, "replacement char = 1");
    });
  });

  Test.group("TextWidth: stringWidth", () => {
    Test.run("counts columns, not bytes", () => {
      Test.assertEqual(TextWidth.stringWidth(""), 0, "empty = 0");
      Test.assertEqual(TextWidth.stringWidth("hello"), 5, "ASCII = 5");
      Test.assertEqual(
        TextWidth.stringWidth("\xE6\x97\xA5\xE6\x9C\xAC"),
        4,
        "two CJK chars = 4",
      );
      Test.assertEqual(
        TextWidth.stringWidth("a\xE6\x97\xA5b"),
        4,
        "mixed ASCII + CJK = 4",
      );
      Test.assertEqual(
        TextWidth.stringWidth("\xF0\x9F\x98\x80"),
        2,
        "emoji = 2",
      );
    });

    Test.run("combining marks add nothing", () => {
      Test.assertEqual(
        TextWidth.stringWidth("e\xCC\x81"),
        1,
        "decomposed e-acute = 1",
      );
      Test.assertEqual(
        TextWidth.stringWidth("e\xCC\x81e\xCC\x81"),
        2,
        "two decomposed chars = 2",
      );
    });

    Test.run("ANSI escape sequences cost nothing", () => {
      Test.assertEqual(
        TextWidth.stringWidth("\027[1mhi\027[0m"),
        2,
        "bold hi = 2",
      );
      Test.assertEqual(
        TextWidth.stringWidth("\027[38;5;196m\xE6\x97\xA5\027[0m"),
        2,
        "colored CJK char = 2",
      );
      Test.assertEqual(
        TextWidth.stringWidth("\027[0m"),
        0,
        "escape only = 0",
      );
    });

    Test.run("malformed bytes count as one column each", () => {
      Test.assertEqual(TextWidth.stringWidth("\xFF\xFE"), 2, "two bad bytes = 2");
      Test.assertEqual(
        TextWidth.stringWidth("a\xC3"),
        2,
        "truncated tail still counts",
      );
    });

    Test.run("box-drawing row measures as its character count", () => {
      /* "|-----|" in box characters: 3 bytes each, 1 column each */
      let row = "\xE2\x94\x8C\xE2\x94\x80\xE2\x94\x80\xE2\x94\x90";
      Test.assertEqual(TextWidth.stringWidth(row), 4, "4 box chars = 4");
    });
  });

  Test.group("TextWidth: toCells", () => {
    Test.run("one cell per visible character", () => {
      assertCells(
        TextWidth.toCells("ab"),
        [("a", 1), ("b", 1)],
        "two ASCII cells",
      );
      assertCells(
        TextWidth.toCells("\xE6\x97\xA5a"),
        [("\xE6\x97\xA5", 2), ("a", 1)],
        "CJK cell then ASCII cell",
      );
      assertCells(TextWidth.toCells(""), [], "empty string = no cells");
    });

    Test.run("zero-width codepoints fuse onto the previous cell", () => {
      assertCells(
        TextWidth.toCells("e\xCC\x81x"),
        [("e\xCC\x81", 1), ("x", 1)],
        "combining mark fuses",
      );
      assertCells(
        TextWidth.toCells("\xF0\x9F\x98\x80\xEF\xB8\x8F"),
        [("\xF0\x9F\x98\x80\xEF\xB8\x8F", 2)],
        "variation selector fuses onto the emoji",
      );
    });

    Test.run("a leading combining mark becomes its own width-0 cell", () => {
      assertCells(
        TextWidth.toCells("\xCC\x81a"),
        [("\xCC\x81", 0), ("a", 1)],
        "no base character to fuse onto",
      );
    });

    Test.run("malformed bytes become single-byte width-1 cells", () => {
      assertCells(
        TextWidth.toCells("\xFFa"),
        [("\xFF", 1), ("a", 1)],
        "bad byte is its own cell",
      );
    });
  });

  Test.group("Element width helpers (UTF-8 aware)", () => {
    Test.run("visibleLength measures columns", () => {
      Test.assertEqual(Element.visibleLength("hello"), 5, "ASCII = 5");
      Test.assertEqual(
        Element.visibleLength("\xE6\x97\xA5\xE6\x9C\xAC"),
        4,
        "two CJK chars = 4",
      );
      Test.assertEqual(
        Element.visibleLength("e\xCC\x81"),
        1,
        "combining mark adds nothing",
      );
      Test.assertEqual(
        Element.visibleLength("\027[1m\xE6\x97\xA5\027[0m"),
        2,
        "styled CJK char = 2",
      );
    });

    Test.run("padToWidth pads with spaces after a reset", () => {
      Test.assertEqualStr(
        Element.padToWidth("abc", 5),
        "abc" ++ reset ++ "  ",
        "ASCII padding",
      );
      Test.assertEqualStr(
        Element.padToWidth("\xE6\x97\xA5", 4),
        "\xE6\x97\xA5" ++ reset ++ "  ",
        "a wide char leaves 2 columns to pad",
      );
    });

    Test.run("padToWidth truncates on a character boundary", () => {
      Test.assertEqualStr(
        Element.padToWidth("abcd", 2),
        "ab" ++ reset,
        "ASCII truncation",
      );
      Test.assertEqualStr(
        Element.padToWidth("\xE6\x97\xA5\xE6\x9C\xAC", 4),
        "\xE6\x97\xA5\xE6\x9C\xAC" ++ reset,
        "exact fit keeps both wide chars whole",
      );
      Test.assertEqualStr(
        Element.padToWidth("\xE6\x97\xA5\xE6\x9C\xAC", 2),
        "\xE6\x97\xA5" ++ reset,
        "one wide char fills 2 columns",
      );
    });

    Test.run("padToWidth never splits a double-width character", () => {
      Test.assertEqualStr(
        Element.padToWidth("\xE6\x97\xA5\xE6\x9C\xAC", 3),
        "\xE6\x97\xA5 " ++ reset,
        "the straddling wide char becomes one space",
      );
      Test.assertEqualStr(
        Element.padToWidth("\xE6\x97\xA5", 1),
        " " ++ reset,
        "a lone wide char in 1 column is one space",
      );
      Test.assertEqualStr(
        Element.padToWidth("a\xE6\x97\xA5", 2),
        "a " ++ reset,
        "ASCII then a straddling wide char",
      );
    });

    Test.run("padToWidth keeps escape sequences it truncates through", () => {
      Test.assertEqualStr(
        Element.padToWidth("\027[1mabcd\027[0m", 2),
        "\027[1mab" ++ reset,
        "style is opened, then truncated and reset",
      );
      Test.assertEqualStr(
        Element.padToWidth("\027[1m\xE6\x97\xA5\xE6\x9C\xAC", 3),
        "\027[1m\xE6\x97\xA5 " ++ reset,
        "styled wide char with a straddle space",
      );
    });

    Test.run("padToWidth on box-drawing rows is unchanged", () => {
      let row = "\xE2\x94\x8C\xE2\x94\x80\xE2\x94\x80\xE2\x94\x90";
      Test.assertEqualStr(
        Element.padToWidth(row, 4),
        row ++ reset,
        "4 box chars fit exactly in 4 columns",
      );
      Test.assertEqualStr(
        Element.padToWidth(row, 2),
        "\xE2\x94\x8C\xE2\x94\x80" ++ reset,
        "box chars truncate whole",
      );
    });
  });
};
