/*
 * Tests for the terminal model itself (test/vterm.re).
 *
 * These are what keep the model honest. Vterm is the yardstick every
 * painter grid test is measured against, so it cannot be "adjusted until
 * the painters look right" - it has to be independently pinned to xterm's
 * documented behaviour, and that is what this file does.
 *
 * The centrepiece is the DEFERRED WRAP trio plus "EL in pending-wrap erases
 * the last column". Those four assertions ARE the bug class that a green
 * 531-test suite missed: they are stated here from the spec side, with no
 * Matcha painter anywhere in sight.
 */

let esc = "\027";

let run = () =>
  Test.group("Vterm (terminal model)", () => {
    /* ========================================================================
     * Deferred wrap
     * ====================================================================== */

    Test.run("printing INTO the last column leaves the cursor on it", () => {
      let t = Vterm.create(~width=5, ~height=3);
      Vterm.feed(t, "abcde");
      Test.assertEqualStr(Vterm.row(t, 0), "abcde", "row 0 is full");
      Test.assertEqual(Vterm.cursor(t), (0, 4), "cursor is ON the last column");
      Test.assertTrue(Vterm.pendingWrap(t), "and the wrap is pending");
      Test.assertEqualStr(Vterm.row(t, 1), "     ", "nothing on row 1 yet");
    });

    Test.run("the NEXT printable is what takes the pending wrap", () => {
      let t = Vterm.create(~width=5, ~height=3);
      Vterm.feed(t, "abcdeX");
      Test.assertEqualStr(Vterm.row(t, 0), "abcde", "row 0 kept every cell");
      Test.assertEqualStr(Vterm.row(t, 1), "X    ", "X wrapped onto row 1");
      Test.assertEqual(Vterm.cursor(t), (1, 1), "cursor followed it down");
      Test.assertFalse(Vterm.pendingWrap(t), "flag cleared by the wrap");
    });

    Test.run("CR, LF and CUP clear pending-wrap without wrapping", () => {
      let check = (name, seq, expectedCursor) => {
        let t = Vterm.create(~width=5, ~height=3);
        Vterm.feed(t, "abcde");
        Test.assertTrue(Vterm.pendingWrap(t), name ++ ": wrap armed");
        Vterm.feed(t, seq);
        Test.assertFalse(Vterm.pendingWrap(t), name ++ ": wrap disarmed");
        Test.assertEqual(Vterm.cursor(t), expectedCursor, name ++ ": cursor");
        Test.assertEqualStr(
          Vterm.row(t, 0),
          "abcde",
          name ++ ": row 0 untouched",
        );
      };
      check("CR", "\r", (0, 0));
      /* LF keeps the column - the model sees a bare LF; a child process's
       * "\n" becomes CR LF at the pty because OPOST/ONLCR stays on. */
      check("LF", "\n", (1, 4));
      check("CUP", esc ++ "[1;1H", (0, 0));
      check("CUD", esc ++ "[1B", (1, 4));
    });

    Test.run("a width-2 glyph wraps whole rather than splitting", () => {
      let t = Vterm.create(~width=5, ~height=3);
      /* Four ASCII cells, then a CJK ideograph that needs two columns and
       * only has one left. */
      Vterm.feed(t, "abcd\xe6\x97\xa5");
      Test.assertEqualStr(
        Vterm.row(t, 0),
        "abcd ",
        "the odd last column is left blank, not half a glyph",
      );
      Test.assertEqualStr(
        Vterm.cellGlyph(t, ~row=1, ~col=0),
        "\xe6\x97\xa5",
        "the wide glyph starts row 1",
      );
      Test.assertEqualStr(
        Vterm.cellGlyph(t, ~row=1, ~col=1),
        "",
        "and owns a continuation cell",
      );
      Test.assertEqual(Vterm.cursor(t), (1, 2), "cursor advanced two columns");
    });

    Test.run("a width-2 glyph in the last two columns arms the wrap", () => {
      let t = Vterm.create(~width=4, ~height=3);
      Vterm.feed(t, "ab\xe6\x97\xa5");
      Test.assertEqual(Vterm.cursor(t), (0, 3), "cursor on the last column");
      Test.assertTrue(Vterm.pendingWrap(t), "wrap armed");
      Test.assertEqualStr(Vterm.row(t, 0), "ab\xe6\x97\xa5", "row holds it");
    });

    /* ========================================================================
     * EL - the bug class, stated from the spec side
     * ====================================================================== */

    Test.run("ESC[K in the pending-wrap state ERASES the last column", () => {
      /* This is the whole reason the model exists. A painter that emits
       * `content ++ ESC[0m ESC[K` on a row that spans the full width is
       * erasing the cell it just painted, because the cursor is still ON
       * the last column (deferred wrap), not past it. */
      let t = Vterm.create(~width=5, ~height=3);
      Vterm.feed(t, "abcde" ++ esc ++ "[K");
      Test.assertEqualStr(
        Vterm.row(t, 0),
        "abcd ",
        "the 'e' just painted is gone - EL erased the cursor's own cell",
      );
    });

    Test.run("ESC[K after a move to column 1 erases the whole row", () => {
      let t = Vterm.create(~width=5, ~height=3);
      Vterm.feed(t, "abcde");
      Vterm.feed(t, esc ++ "[1;1H" ++ esc ++ "[K");
      Test.assertEqualStr(Vterm.row(t, 0), "     ", "row cleared end to end");
      Test.assertEqual(Vterm.cursor(t), (0, 0), "cursor did not move");
    });

    Test.run("ESC[K erases only from the cursor forward", () => {
      let t = Vterm.create(~width=6, ~height=2);
      Vterm.feed(t, "abcdef" ++ esc ++ "[1;4H" ++ esc ++ "[K");
      Test.assertEqualStr(Vterm.row(t, 0), "abc   ", "columns 4..6 erased");
    });

    Test.run("ESC[1K erases from the start through the cursor", () => {
      let t = Vterm.create(~width=6, ~height=2);
      Vterm.feed(t, "abcdef" ++ esc ++ "[1;3H" ++ esc ++ "[1K");
      Test.assertEqualStr(Vterm.row(t, 0), "   def", "columns 1..3 erased");
    });

    /* ========================================================================
     * Scrolling
     * ====================================================================== */

    Test.run("LF at the bottom row scrolls the primary into scrollback", () => {
      let t = Vterm.create(~width=4, ~height=2);
      Vterm.feed(t, "aa\r\nbb\r\ncc");
      Test.assertEqualStr(Vterm.row(t, 0), "bb  ", "old row 1 moved up");
      Test.assertEqualStr(Vterm.row(t, 1), "cc  ", "new content at the bottom");
      Test.assertEqual(
        List.length(Vterm.scrollback(t)),
        1,
        "one line left the screen",
      );
      Test.assertContains(
        Vterm.scrollbackText(t),
        "aa",
        "and it is the one that scrolled off the top",
      );
    });

    Test.run("the alternate screen DISCARDS what scrolls off it", () => {
      let t = Vterm.create(~width=4, ~height=2);
      Vterm.feed(t, esc ++ "[?1049h");
      Vterm.feed(t, esc ++ "[1;1Haa\r\nbb\r\ncc");
      Test.assertEqualStr(Vterm.row(t, 1), "cc  ", "it scrolled");
      Test.assertEqual(
        Vterm.scrollback(t),
        [],
        "an alt screen has no scrollback at all",
      );
    });

    /* ========================================================================
     * ED / clearing
     * ====================================================================== */

    Test.run("ESC[2J clears the screen WITHOUT moving the cursor", () => {
      let t = Vterm.create(~width=4, ~height=3);
      Vterm.feed(t, "ab\r\ncd\r\nef");
      Vterm.feed(t, esc ++ "[2J");
      Test.assertEqualStr(Vterm.text(t), "\n\n", "every row is blank");
      Test.assertEqual(
        Vterm.cursor(t),
        (2, 2),
        "cursor is exactly where ESC[2J found it (hence the ESC[H that "
        ++ "always follows it in real writers)",
      );
    });

    Test.run("ESC[0J erases from the cursor to the end of the screen", () => {
      let t = Vterm.create(~width=4, ~height=3);
      Vterm.feed(t, "abcd\r\nefgh\r\nijkl");
      Vterm.feed(t, esc ++ "[2;3H" ++ esc ++ "[0J");
      Test.assertEqualStr(Vterm.row(t, 0), "abcd", "row above untouched");
      Test.assertEqualStr(Vterm.row(t, 1), "ef  ", "row erased from column 3");
      Test.assertEqualStr(Vterm.row(t, 2), "    ", "rows below fully erased");
    });

    /* ========================================================================
     * Alternate screen
     * ====================================================================== */

    Test.run("1049 enter/exit restores the primary screen and cursor", () => {
      let t = Vterm.create(~width=6, ~height=3);
      Vterm.feed(t, "primary\r\n");
      let before = Vterm.cursor(t);
      Test.assertFalse(Vterm.inAltScreen(t), "starts on the primary buffer");

      Vterm.feed(t, esc ++ "[?1049h");
      Test.assertTrue(Vterm.inAltScreen(t), "1049h switches");
      Test.assertEqualStr(
        Vterm.text(t),
        "\n\n",
        "the alternate buffer arrives CLEARED",
      );

      Vterm.feed(t, esc ++ "[1;1Halt");
      Test.assertContains(Vterm.row(t, 0), "alt", "painted on the alt buffer");

      Vterm.feed(t, esc ++ "[?1049l");
      Test.assertFalse(Vterm.inAltScreen(t), "1049l switches back");
      Test.assertContains(
        Vterm.text(t),
        "primar",
        "the primary buffer's contents came back",
      );
      Test.assertTrue(
        !Test.contains(Vterm.text(t), "alt"),
        "and the alt buffer's contents are gone from view",
      );
      Test.assertEqual(Vterm.cursor(t), before, "cursor restored");
    });

    /* ========================================================================
     * Modes and out-of-band sequences
     * ====================================================================== */

    Test.run("mode flags track ?25, ?2004 and ?1002;1006", () => {
      let t = Vterm.create(~width=4, ~height=2);
      Test.assertTrue(Vterm.cursorVisible(t), "cursor starts visible");
      Vterm.feed(t, esc ++ "[?25l");
      Test.assertFalse(Vterm.cursorVisible(t), "?25l hides it");
      Vterm.feed(t, esc ++ "[?25h");
      Test.assertTrue(Vterm.cursorVisible(t), "?25h shows it");

      Vterm.feed(t, esc ++ "[?2004h");
      Test.assertTrue(Vterm.bracketedPaste(t), "?2004h arms bracketed paste");
      Vterm.feed(t, esc ++ "[?2004l");
      Test.assertFalse(Vterm.bracketedPaste(t), "?2004l disarms it");

      Vterm.feed(t, esc ++ "[?1002;1006h");
      Test.assertTrue(Vterm.mouseReporting(t), "combined form turns mouse on");
      Vterm.feed(t, esc ++ "[?1002;1006l");
      Test.assertFalse(Vterm.mouseReporting(t), "and off again");
    });

    Test.run("DSR requests are counted and drained", () => {
      let t = Vterm.create(~width=4, ~height=2);
      Vterm.feed(t, esc ++ "[6n" ++ esc ++ "[6n");
      Test.assertEqual(Vterm.takeDsrRequests(t), 2, "two queries seen");
      Test.assertEqual(Vterm.takeDsrRequests(t), 0, "taking drains the count");
      Test.assertEqual(Vterm.unknownSeqs(t), [], "DSR is understood, not unknown");
    });

    Test.run("kitty push/pop and ?2026 parse cleanly and change nothing", () => {
      let t = Vterm.create(~width=6, ~height=2);
      Vterm.feed(
        t,
        esc ++ "[>1u" ++ esc ++ "[?2026h" ++ "hi" ++ esc ++ "[?2026l" ++ esc ++ "[<u",
      );
      Test.assertEqualStr(Vterm.row(t, 0), "hi    ", "only the text landed");
      Test.assertEqual(
        Vterm.unknownSeqs(t),
        [],
        "none of them is recorded as unknown",
      );
    });

    Test.run("genuinely unknown sequences are consumed and recorded", () => {
      let t = Vterm.create(~width=6, ~height=2);
      Vterm.feed(t, "a" ++ esc ++ "[5X" ++ "b");
      Test.assertEqualStr(
        Vterm.row(t, 0),
        "ab    ",
        "the unknown sequence did not corrupt the text around it",
      );
      Test.assertEqual(
        Vterm.unknownSeqs(t),
        [esc ++ "[5X"],
        "and it was recorded verbatim",
      );
    });

    /* ========================================================================
     * SGR
     * ====================================================================== */

    Test.run("SGR state is recorded per cell and reset by 0", () => {
      let t = Vterm.create(~width=6, ~height=2);
      Vterm.feed(t, esc ++ "[1m" ++ esc ++ "[31m" ++ "A" ++ esc ++ "[0m" ++ "B");
      Test.assertEqual(
        Vterm.cellSgr(t, ~row=0, ~col=0),
        [1, 31],
        "A carries both accumulated params",
      );
      Test.assertEqual(
        Vterm.cellSgr(t, ~row=0, ~col=1),
        [],
        "ESC[0m reset the list before B",
      );
    });

    Test.run("ESC[m with no parameters resets", () => {
      let t = Vterm.create(~width=6, ~height=2);
      Vterm.feed(t, esc ++ "[7m" ++ esc ++ "[m" ++ "x");
      Test.assertEqual(Vterm.cellSgr(t, ~row=0, ~col=0), [], "bare SGR resets");
    });

    /* ========================================================================
     * Chunking
     * ====================================================================== */

    Test.run("feeding one byte at a time gives the identical grid", () => {
      let stream =
        esc
        ++ "[2J"
        ++ esc
        ++ "[1;1H"
        ++ esc
        ++ "[1m"
        ++ "hello"
        ++ esc
        ++ "[0m"
        ++ esc
        ++ "[2;1H"
        ++ esc
        ++ "[K"
        ++ "w\xc3\xb6rld"
        ++ esc
        ++ "[?25l";
      let whole = Vterm.create(~width=10, ~height=4);
      Vterm.feed(whole, stream);
      let split = Vterm.create(~width=10, ~height=4);
      String.iter(c => Vterm.feed(split, String.make(1, c)), stream);
      Test.assertEqualStr(
        String.concat("|", Array.to_list(Vterm.snapshot(split))),
        String.concat("|", Array.to_list(Vterm.snapshot(whole))),
        "an escape sequence split across chunks parses the same",
      );
      Test.assertEqual(
        Vterm.cursor(split),
        Vterm.cursor(whole),
        "cursor agrees too",
      );
      Test.assertFalse(
        Vterm.cursorVisible(split),
        "the trailing ?25l was not lost at a chunk boundary",
      );
    });

    Test.run("a multi-byte UTF-8 character split across chunks survives", () => {
      let t = Vterm.create(~width=6, ~height=2);
      Vterm.feed(t, "a\xc3");
      Vterm.feed(t, "\xb6b");
      Test.assertEqualStr(Vterm.row(t, 0), "a\xc3\xb6b   ", "o-umlaut is intact");
    });

    /* ========================================================================
     * Resize
     * ====================================================================== */

    Test.run("resize crops top-left and clamps the cursor", () => {
      let t = Vterm.create(~width=8, ~height=4);
      Vterm.feed(t, "abcdefgh\r\n12345678");
      Vterm.resize(t, ~width=4, ~height=2);
      Test.assertEqual(Vterm.size(t), (4, 2), "new size in force");
      Test.assertEqualStr(Vterm.row(t, 0), "abcd", "row cropped from the right");
      Test.assertEqualStr(Vterm.row(t, 1), "1234", "second row survives");
      let (r, c) = Vterm.cursor(t);
      Test.assertTrue(r < 2 && c < 4, "cursor clamped into the new bounds");
    });
  });
