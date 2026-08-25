/*
 * Tests for Element.TextArea (lib/Element.re, module TextArea, ~line 687+)
 *
 * TextArea is a multi-line editor state machine. Cursor/selection state lives
 * outside the component (cursorRow, cursorCol, selection are props; setCursor/
 * setSelection/onChange are callbacks). This means the core logic in
 * `handleKeyDown` is a pure-ish function of (key, modifiers, value, cursor,
 * selection) that calls its callbacks with the new state - it can be driven
 * directly with a small harness of mutable refs, without a running app. We
 * also exercise a couple of scenarios through Runtime.startHeadless to prove
 * the wiring through the real <TextArea /> component works end to end.
 */
open Matcha;

/* ============================================================================
 * Harness for driving TextArea.handleKeyDown directly
 *
 * Mirrors the wiring in examples/textarea-demo/main.re: cursorRow/cursorCol
 * are tracked via a (row, col) pair, selection and value are plain state,
 * and the three callbacks (onChange/setCursor/setSelection) write back into
 * the harness so tests can assert on the resulting state.
 * ============================================================================ */

type harness = {
  mutable value: string,
  mutable cursorRow: int,
  mutable cursorCol: int,
  mutable selection: option(TextArea.selection),
  mutable submitted: bool,
};

let makeHarness = (initial: string): harness => {
  value: initial,
  cursorRow: 0,
  cursorCol: 0,
  selection: None,
  submitted: false,
};

let send = (h: harness, key: Key.t, mods: Key.modifiers): unit =>
  TextArea.handleKeyDown(
    key,
    mods,
    h.value,
    v => h.value = v,
    Some(() => h.submitted = true),
    h.cursorRow,
    h.cursorCol,
    ((row, col)) => {
      h.cursorRow = row;
      h.cursorCol = col;
    },
    h.selection,
    sel => h.selection = sel,
  );

let typeChar = (h: harness, c: char): unit =>
  send(h, Key.Char(c), Key.noModifiers);

let typeString = (h: harness, s: string): unit =>
  String.iter(c => typeChar(h, c), s);

let typeText = (h: harness, s: string): unit =>
  send(h, Key.Text(s), Key.noModifiers);

let shiftMods = {...Key.noModifiers, shift: true};
let altMods = {...Key.noModifiers, alt: true};
let metaMods = {...Key.noModifiers, meta: true};

/* UTF-8 byte sequences used by the unicode tests. Written as hex escapes so
 * the expectations stay readable as BYTES - the point of these tests is that
 * columns are cells while storage is still a plain byte string. */
let eacute = "\xC3\xA9"; /* U+00E9, 2 bytes, width 1 */
let combiningAcute = "\xCC\x81"; /* U+0301, 2 bytes, width 0 */
let eCombining = "e" ++ combiningAcute; /* 'e' + acute, 3 bytes, one cell */
let sun = "\xE6\x97\xA5"; /* U+65E5, 3 bytes, width 2 */
let book = "\xE6\x9C\xAC"; /* U+672C, 3 bytes, width 2 */
let lang = "\xE8\xAA\x9E"; /* U+8A9E, 3 bytes, width 2 */

let run = () => {
  /* ==========================================================================
   * Pure helpers: line splitting / joining / byte-index conversion
   * ========================================================================== */
  Test.group("TextArea - line utilities", () => {
    Test.run("textToLines splits on newline", () => {
      let lines = TextArea.textToLines("a\nb\nc");
      Test.assertEqual(Array.length(lines), 3, "3 lines");
      Test.assertEqualStr(lines[0], "a", "line 0");
      Test.assertEqualStr(lines[1], "b", "line 1");
      Test.assertEqualStr(lines[2], "c", "line 2");
    });

    Test.run("textToLines on empty string is a single empty line", () => {
      let lines = TextArea.textToLines("");
      Test.assertEqual(Array.length(lines), 1, "1 line");
      Test.assertEqualStr(lines[0], "", "empty line");
    });

    Test.run("linesToText joins with newline", () => {
      Test.assertEqualStr(
        TextArea.linesToText([|"a", "b", "c"|]),
        "a\nb\nc",
        "joined",
      )
    });

    Test.run("positionToIndex converts row/col to byte offset", () => {
      let text = "abc\ndef\nghi";
      Test.assertEqual(TextArea.positionToIndex(text, 0, 0), 0, "row0 col0");
      Test.assertEqual(TextArea.positionToIndex(text, 0, 2), 2, "row0 col2");
      Test.assertEqual(
        TextArea.positionToIndex(text, 1, 0),
        4,
        "row1 col0 (after 'abc\\n')",
      );
      Test.assertEqual(
        TextArea.positionToIndex(text, 2, 1),
        9,
        "row2 col1",
      );
    });

    Test.run("positionToIndex clamps col to line length", () => {
      let text = "ab\ncdef";
      Test.assertEqual(
        TextArea.positionToIndex(text, 0, 99),
        2,
        "col beyond line end clamps to line length",
      )
    });

    Test.run("positionToIndex with row beyond last line returns text length", () => {
      let text = "ab\ncd";
      Test.assertEqual(
        TextArea.positionToIndex(text, 5, 0),
        String.length(text),
        "out-of-range row clamps to end of text",
      )
    });
  });

  /* ==========================================================================
   * Character classification and word motion
   * ========================================================================== */
  Test.group("TextArea - character classification & word motion", () => {
    Test.run("classifyChar", () => {
      Test.assertEqual(TextArea.classifyChar('a'), TextArea.WordChar, "letter");
      Test.assertEqual(TextArea.classifyChar('9'), TextArea.WordChar, "digit");
      Test.assertEqual(TextArea.classifyChar('_'), TextArea.WordChar, "underscore");
      Test.assertEqual(TextArea.classifyChar(' '), TextArea.Whitespace, "space");
      Test.assertEqual(TextArea.classifyChar('\t'), TextArea.Whitespace, "tab");
      Test.assertEqual(TextArea.classifyChar('.'), TextArea.Punctuation, "period");
      Test.assertEqual(TextArea.classifyChar('-'), TextArea.Punctuation, "dash");
    });

    Test.run("findPrevWordStart from end of line jumps to start of last word", () => {
      Test.assertEqual(
        TextArea.findPrevWordStart("hello world", 11),
        6,
        "jumps to start of 'world'",
      )
    });

    Test.run("findPrevWordStart chains back through multiple words", () => {
      Test.assertEqual(
        TextArea.findPrevWordStart("hello world", 6),
        0,
        "jumps to start of 'hello' from start of 'world'",
      )
    });

    Test.run("findPrevWordStart at column 0 stays at 0", () => {
      Test.assertEqual(TextArea.findPrevWordStart("hello", 0), 0, "no-op at 0")
    });

    Test.run("findNextWordEnd from start of line jumps to end of first word", () => {
      Test.assertEqual(
        TextArea.findNextWordEnd("hello world", 0),
        5,
        "jumps to end of 'hello'",
      )
    });

    Test.run("findNextWordEnd chains forward through multiple words", () => {
      Test.assertEqual(
        TextArea.findNextWordEnd("hello world", 5),
        11,
        "jumps to end of 'world' from end of 'hello'",
      )
    });

    Test.run("findNextWordEnd at end of line stays at length", () => {
      Test.assertEqual(
        TextArea.findNextWordEnd("hello", 5),
        5,
        "no-op at end",
      )
    });
  });

  /* ==========================================================================
   * Selection utilities
   * ========================================================================== */
  Test.group("TextArea - selection utilities", () => {
    Test.run("normalizeSelection leaves forward selection unchanged", () => {
      Test.assertEqual(
        TextArea.normalizeSelection((0, 2, 0, 5)),
        (0, 2, 0, 5),
        "already normalized",
      )
    });

    Test.run("normalizeSelection swaps a backward same-row selection", () => {
      Test.assertEqual(
        TextArea.normalizeSelection((0, 5, 0, 2)),
        (0, 2, 0, 5),
        "swapped",
      )
    });

    Test.run("normalizeSelection swaps a backward multi-row selection", () => {
      Test.assertEqual(
        TextArea.normalizeSelection((1, 0, 0, 3)),
        (0, 3, 1, 0),
        "swapped across rows",
      )
    });

    Test.run("isInSelection single-row range is half-open [start, end)", () => {
      let sel = Some((0, 2, 0, 5));
      Test.assertFalse(TextArea.isInSelection(0, 1, sel), "before start");
      Test.assertTrue(TextArea.isInSelection(0, 2, sel), "at start");
      Test.assertTrue(TextArea.isInSelection(0, 4, sel), "inside");
      Test.assertFalse(TextArea.isInSelection(0, 5, sel), "at end (excluded)");
    });

    Test.run("isInSelection spans full middle rows", () => {
      let sel = Some((0, 3, 2, 2));
      Test.assertTrue(TextArea.isInSelection(0, 3, sel), "start row at col");
      Test.assertTrue(TextArea.isInSelection(0, 99, sel), "start row past col (rest of row selected)");
      Test.assertTrue(TextArea.isInSelection(1, 0, sel), "middle row fully selected");
      Test.assertTrue(TextArea.isInSelection(2, 1, sel), "end row before col");
      Test.assertFalse(TextArea.isInSelection(2, 2, sel), "end row at col (excluded)");
    });

    Test.run("isInSelection with None selection is always false", () => {
      Test.assertFalse(TextArea.isInSelection(0, 0, None), "no selection")
    });
  });

  /* ==========================================================================
   * Text manipulation: deleteSelection / insertAt
   * ========================================================================== */
  Test.group("TextArea - text manipulation", () => {
    Test.run("deleteSelection removes a same-line range", () => {
      let (text, row, col) =
        TextArea.deleteSelection("hello world", (0, 0, 0, 5));
      Test.assertEqualStr(text, " world", "'hello' removed");
      Test.assertEqual((row, col), (0, 0), "cursor at deletion start");
    });

    Test.run("deleteSelection collapses a multi-line range", () => {
      let (text, row, col) =
        TextArea.deleteSelection("abc\ndef\nghi", (0, 1, 2, 1));
      Test.assertEqualStr(text, "ahi", "middle line and edges collapsed");
      Test.assertEqual((row, col), (0, 1), "cursor at start of deletion");
    });

    Test.run("insertAt on a single line", () => {
      let (text, row, col) = TextArea.insertAt("hello", 0, 5, " world");
      Test.assertEqualStr(text, "hello world", "appended");
      Test.assertEqual((row, col), (0, 11), "cursor after inserted text");
    });

    Test.run("insertAt with multi-line text splits the target line", () => {
      let (text, row, col) = TextArea.insertAt("ab\ncd", 0, 1, "X\nY");
      Test.assertEqualStr(text, "aX\nYb\ncd", "line split at insertion point");
      Test.assertEqual((row, col), (1, 1), "cursor after last inserted line");
    });

    Test.run("insertAt clamps out-of-range column", () => {
      let (text, _row, col) = TextArea.insertAt("ab", 0, 99, "!");
      Test.assertEqualStr(text, "ab!", "clamped to line end");
      Test.assertEqual(col, 3, "cursor at true end");
    });
  });

  /* ==========================================================================
   * handleKeyDown: typing and deleting
   * ========================================================================== */
  Test.group("TextArea - typing and deleting via handleKeyDown", () => {
    Test.run("typing characters appends and advances cursor", () => {
      let h = makeHarness("");
      typeString(h, "hi");
      Test.assertEqualStr(h.value, "hi", "typed value");
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 2), "cursor at end");
    });

    Test.run("backspace removes the previous character", () => {
      let h = makeHarness("ab");
      h.cursorCol = 2;
      send(h, Key.Backspace, Key.noModifiers);
      Test.assertEqualStr(h.value, "a", "one char removed");
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 1), "cursor moved back");
    });

    Test.run("backspace at column 0 joins with the previous line", () => {
      let h = makeHarness("a\nb");
      h.cursorRow = 1;
      h.cursorCol = 0;
      send(h, Key.Backspace, Key.noModifiers);
      Test.assertEqualStr(h.value, "ab", "lines joined");
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 1), "cursor at join point");
    });

    Test.run("backspace at document start is a no-op", () => {
      let h = makeHarness("ab");
      send(h, Key.Backspace, Key.noModifiers);
      Test.assertEqualStr(h.value, "ab", "unchanged");
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 0), "cursor unchanged");
    });

    Test.run("backspace with an active selection deletes the selection", () => {
      let h = makeHarness("hello");
      h.selection = Some((0, 0, 0, 2));
      h.cursorRow = 0;
      h.cursorCol = 2;
      send(h, Key.Backspace, Key.noModifiers);
      Test.assertEqualStr(h.value, "llo", "'he' removed");
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 0), "cursor at deletion point");
      Test.assertEqual(h.selection, None, "selection cleared");
    });

    Test.run("delete forward removes the next character without moving cursor", () => {
      let h = makeHarness("ab");
      send(h, Key.Delete, Key.noModifiers);
      Test.assertEqualStr(h.value, "b", "first char removed");
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 0), "cursor stays put");
    });

    Test.run("delete forward at end of line joins with the next line", () => {
      let h = makeHarness("a\nb");
      h.cursorCol = 1;
      send(h, Key.Delete, Key.noModifiers);
      Test.assertEqualStr(h.value, "ab", "lines joined");
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 1), "cursor unchanged (still valid position)");
    });

    Test.run("delete forward at document end is a no-op", () => {
      let h = makeHarness("a");
      h.cursorCol = 1;
      send(h, Key.Delete, Key.noModifiers);
      Test.assertEqualStr(h.value, "a", "unchanged");
    });

    Test.run("delete forward with an active selection deletes the selection", () => {
      let h = makeHarness("hello");
      h.selection = Some((0, 1, 0, 3));
      send(h, Key.Delete, Key.noModifiers);
      Test.assertEqualStr(h.value, "hlo", "'el' removed");
      Test.assertEqual(h.selection, None, "selection cleared");
    });
  });

  /* ==========================================================================
   * handleKeyDown: cursor movement, including line boundaries
   * ========================================================================== */
  Test.group("TextArea - cursor movement via handleKeyDown", () => {
    Test.run("arrow right at end of line wraps to start of next line", () => {
      let h = makeHarness("ab\ncd");
      h.cursorCol = 2;
      send(h, Key.Arrow_right, Key.noModifiers);
      Test.assertEqual((h.cursorRow, h.cursorCol), (1, 0), "wrapped");
    });

    Test.run("arrow left at start of line wraps to end of previous line", () => {
      let h = makeHarness("ab\ncd");
      h.cursorRow = 1;
      h.cursorCol = 0;
      send(h, Key.Arrow_left, Key.noModifiers);
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 2), "wrapped");
    });

    Test.run("arrow left at document start is a no-op", () => {
      let h = makeHarness("ab");
      send(h, Key.Arrow_left, Key.noModifiers);
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 0), "unchanged");
    });

    Test.run("arrow right at document end is a no-op", () => {
      let h = makeHarness("ab");
      h.cursorCol = 2;
      send(h, Key.Arrow_right, Key.noModifiers);
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 2), "unchanged");
    });

    Test.run(
      "arrow left/right at a boundary still clears any active selection",
      () => {
      /* NOTE: moveWithSelection always calls setSelection(None) when there's
       * no shift modifier, even if the cursor itself doesn't move (boundary
       * case). This documents current behavior. */
      let h = makeHarness("ab");
      h.selection = Some((0, 0, 0, 1));
      send(h, Key.Arrow_left, Key.noModifiers);
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 0), "cursor unchanged");
      Test.assertEqual(h.selection, None, "selection cleared even though cursor didn't move");
    });

    Test.run(
      "arrow up/down at a boundary does NOT clear an active selection",
      () => {
      /* NOTE: possibly inconsistent with arrow left/right above - the up/down
       * handlers are wrapped in `if (cursorRow > 0) {...}` / `if (cursorRow <
       * numLines - 1) {...}` with no else branch, so at a boundary neither
       * setCursor nor setSelection is called at all. Documenting current
       * behavior rather than asserting it's correct. */
      let h = makeHarness("ab");
      h.selection = Some((0, 0, 0, 1));
      send(h, Key.Arrow_up, Key.noModifiers);
      Test.assertEqual(h.selection, Some((0, 0, 0, 1)), "selection left untouched at top boundary");
      send(h, Key.Arrow_down, Key.noModifiers);
      Test.assertEqual(h.selection, Some((0, 0, 0, 1)), "selection left untouched at bottom boundary (single line)");
    });

    Test.run("arrow up moves to previous line, clamping column", () => {
      let h = makeHarness("abcdef\nxy");
      h.cursorRow = 1;
      h.cursorCol = 2;
      send(h, Key.Arrow_up, Key.noModifiers);
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 2), "same column, longer line");
    });

    Test.run("arrow down clamps column to shorter next line", () => {
      let h = makeHarness("abcdef\nxy");
      h.cursorRow = 0;
      h.cursorCol = 5;
      send(h, Key.Arrow_down, Key.noModifiers);
      Test.assertEqual((h.cursorRow, h.cursorCol), (1, 2), "column clamped to 'xy' length");
    });
  });

  /* ==========================================================================
   * handleKeyDown: word motion (Alt+Arrow)
   * ========================================================================== */
  Test.group("TextArea - word motion via handleKeyDown (Alt+Arrow)", () => {
    Test.run("alt+left jumps back one word at a time", () => {
      let h = makeHarness("hello world");
      h.cursorCol = 11;
      send(h, Key.Arrow_left, altMods);
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 6), "jumped to start of 'world'");
      send(h, Key.Arrow_left, altMods);
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 0), "jumped to start of 'hello'");
    });

    Test.run("alt+right jumps forward one word at a time", () => {
      let h = makeHarness("hello world");
      send(h, Key.Arrow_right, altMods);
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 5), "jumped to end of 'hello'");
      send(h, Key.Arrow_right, altMods);
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 11), "jumped to end of 'world'");
    });

    Test.run("alt+shift+left extends selection by word", () => {
      let h = makeHarness("hello world");
      h.cursorCol = 11;
      send(h, Key.Arrow_left, {...altMods, shift: true});
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 6), "cursor jumped");
      Test.assertEqual(h.selection, Some((0, 11, 0, 6)), "selection anchored at original cursor");
    });
  });

  /* ==========================================================================
   * handleKeyDown: line/document motion (Cmd+Arrow)
   * ========================================================================== */
  Test.group("TextArea - line/document motion via handleKeyDown (Cmd+Arrow)", () => {
    Test.run("cmd+left moves to line start", () => {
      let h = makeHarness("ab\ncdef\ng");
      h.cursorRow = 1;
      h.cursorCol = 2;
      send(h, Key.Arrow_left, metaMods);
      Test.assertEqual((h.cursorRow, h.cursorCol), (1, 0), "at line start");
    });

    Test.run("cmd+right moves to line end", () => {
      let h = makeHarness("ab\ncdef\ng");
      h.cursorRow = 1;
      h.cursorCol = 2;
      send(h, Key.Arrow_right, metaMods);
      Test.assertEqual((h.cursorRow, h.cursorCol), (1, 4), "at line end ('cdef' is 4 chars)");
    });

    Test.run("cmd+up moves to document start", () => {
      let h = makeHarness("ab\ncdef\ng");
      h.cursorRow = 2;
      h.cursorCol = 1;
      send(h, Key.Arrow_up, metaMods);
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 0), "at document start");
    });

    Test.run("cmd+down moves to document end", () => {
      let h = makeHarness("ab\ncdef\ng");
      send(h, Key.Arrow_down, metaMods);
      Test.assertEqual((h.cursorRow, h.cursorCol), (2, 1), "at document end (last row, len('g')=1)");
    });
  });

  /* ==========================================================================
   * handleKeyDown: Home / End (same body as the Cmd+Left/Cmd+Right arms
   * above - Key.Home/Key.End are now their own variants per B3, produced by
   * ESC[H, ESC[1~, ESC[7~, ESCOH, etc.)
   * ========================================================================== */
  Test.group("TextArea - Home/End via handleKeyDown", () => {
    Test.run("Home moves to line start", () => {
      let h = makeHarness("ab\ncdef\ng");
      h.cursorRow = 1;
      h.cursorCol = 2;
      send(h, Key.Home, Key.noModifiers);
      Test.assertEqual((h.cursorRow, h.cursorCol), (1, 0), "at line start");
    });

    Test.run("End moves to line end", () => {
      let h = makeHarness("ab\ncdef\ng");
      h.cursorRow = 1;
      h.cursorCol = 2;
      send(h, Key.End, Key.noModifiers);
      Test.assertEqual((h.cursorRow, h.cursorCol), (1, 4), "at line end ('cdef' is 4 chars)");
    });

    Test.run("Shift+Home extends the selection to line start", () => {
      let h = makeHarness("ab\ncdef\ng");
      h.cursorRow = 1;
      h.cursorCol = 2;
      send(h, Key.Home, shiftMods);
      Test.assertEqual((h.cursorRow, h.cursorCol), (1, 0), "cursor at line start");
      Test.assertEqual(h.selection, Some((1, 2, 1, 0)), "selection anchored at original cursor");
    });

    Test.run("Shift+End extends the selection to line end", () => {
      let h = makeHarness("ab\ncdef\ng");
      h.cursorRow = 1;
      h.cursorCol = 2;
      send(h, Key.End, shiftMods);
      Test.assertEqual((h.cursorRow, h.cursorCol), (1, 4), "cursor at line end");
      Test.assertEqual(h.selection, Some((1, 2, 1, 4)), "selection anchored at original cursor");
    });
  });

  /* ==========================================================================
   * handleKeyDown: selection (Shift+Arrow)
   * ========================================================================== */
  Test.group("TextArea - selection via handleKeyDown (Shift+Arrow)", () => {
    Test.run("shift+right grows a selection from the cursor", () => {
      let h = makeHarness("hello");
      send(h, Key.Arrow_right, shiftMods);
      Test.assertEqual(h.selection, Some((0, 0, 0, 1)), "1 char selected");
      send(h, Key.Arrow_right, shiftMods);
      Test.assertEqual(h.selection, Some((0, 0, 0, 2)), "2 chars selected");
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 2), "cursor at selection end");
    });

    Test.run("shift+left shrinks the selection back towards the anchor", () => {
      let h = makeHarness("hello");
      h.cursorCol = 2;
      h.selection = Some((0, 0, 0, 2));
      send(h, Key.Arrow_left, shiftMods);
      Test.assertEqual(h.selection, Some((0, 0, 0, 1)), "shrunk by one");
    });

    Test.run("shift+down extends selection vertically", () => {
      let h = makeHarness("ab\ncd");
      send(h, Key.Arrow_down, shiftMods);
      Test.assertEqual(h.selection, Some((0, 0, 1, 0)), "selection spans to next line");
    });

    Test.run("typing with an active selection replaces it", () => {
      let h = makeHarness("hello");
      h.selection = Some((0, 0, 0, 2));
      typeChar(h, 'X');
      Test.assertEqualStr(h.value, "Xllo", "'he' replaced with 'X'");
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 1), "cursor after inserted char");
      Test.assertEqual(h.selection, None, "selection cleared");
    });
  });

  /* ==========================================================================
   * handleKeyDown: multi-line editing
   * ========================================================================== */
  Test.group("TextArea - multi-line editing via handleKeyDown", () => {
    Test.run("enter splits the current line", () => {
      let h = makeHarness("ab");
      h.cursorCol = 2;
      send(h, Key.Enter, Key.noModifiers);
      Test.assertEqualStr(h.value, "ab\n", "newline appended");
      Test.assertEqual((h.cursorRow, h.cursorCol), (1, 0), "cursor on new empty line");
    });

    Test.run("enter with a selection deletes it first, then splits", () => {
      let h = makeHarness("hello world");
      h.selection = Some((0, 0, 0, 5));
      send(h, Key.Enter, Key.noModifiers);
      Test.assertEqualStr(h.value, "\n world", "'hello' replaced by a newline");
      Test.assertEqual((h.cursorRow, h.cursorCol), (1, 0), "cursor at start of second line");
      Test.assertEqual(h.selection, None, "selection cleared");
    });

    Test.run("building multi-line content by typing and pressing enter", () => {
      let h = makeHarness("");
      typeString(h, "line one");
      send(h, Key.Enter, Key.noModifiers);
      typeString(h, "line two");
      Test.assertEqualStr(h.value, "line one\nline two", "two lines built up");
      Test.assertEqual((h.cursorRow, h.cursorCol), (1, 8), "cursor at end of second line");
    });
  });

  /* ==========================================================================
   * handleKeyDown: special keys (Tab, Kill Line, Kill Word, word-backspace)
   * ========================================================================== */
  Test.group("TextArea - special keys via handleKeyDown", () => {
    Test.run("tab inserts two spaces", () => {
      let h = makeHarness("ab");
      h.cursorCol = 1;
      send(h, Key.Tab, Key.noModifiers);
      Test.assertEqualStr(h.value, "a  b", "spaces inserted");
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 3), "cursor after inserted spaces");
    });

    Test.run("ctrl+U (KillLine) clears the current line only", () => {
      let h = makeHarness("ab\ncd\nef");
      h.cursorRow = 1;
      h.cursorCol = 1;
      send(h, Key.KillLine, Key.noModifiers);
      Test.assertEqualStr(h.value, "ab\n\nef", "only middle line cleared");
      Test.assertEqual((h.cursorRow, h.cursorCol), (1, 0), "cursor at start of cleared line");
    });

    Test.run("ctrl+W (KillWord) deletes the previous word", () => {
      let h = makeHarness("hello world");
      h.cursorCol = 11;
      send(h, Key.KillWord, Key.noModifiers);
      Test.assertEqualStr(h.value, "hello ", "'world' removed");
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 6), "cursor after 'hello '");
    });

    Test.run("alt+backspace deletes the previous word", () => {
      let h = makeHarness("hello world");
      h.cursorCol = 11;
      send(h, Key.Backspace, altMods);
      Test.assertEqualStr(h.value, "hello ", "'world' removed");
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 6), "cursor after 'hello '");
    });

    Test.run("cmd+backspace deletes to start of line", () => {
      let h = makeHarness("hello world");
      h.cursorCol = 5;
      send(h, Key.Backspace, metaMods);
      Test.assertEqualStr(h.value, " world", "'hello' removed");
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 0), "cursor at line start");
    });
  });

  /* ==========================================================================
   * handleKeyDown: submit (Cmd+Enter)
   * ========================================================================== */
  Test.group("TextArea - submit via handleKeyDown (Cmd+Enter)", () => {
    Test.run("cmd+enter calls onSubmit without modifying the text", () => {
      let h = makeHarness("hi");
      h.cursorCol = 2;
      send(h, Key.Enter, metaMods);
      Test.assertTrue(h.submitted, "onSubmit called");
      Test.assertEqualStr(h.value, "hi", "text unchanged");
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 2), "cursor unchanged");
    });

    Test.run("plain enter does not submit", () => {
      let h = makeHarness("hi");
      h.cursorCol = 2;
      send(h, Key.Enter, Key.noModifiers);
      Test.assertFalse(h.submitted, "onSubmit not called");
    });
  });

  /* ==========================================================================
   * Unicode cells
   *
   * Every column in TextArea (cursorCol, selection columns, the results of
   * the word-motion helpers) is a CELL index: one user-perceived character,
   * i.e. a base codepoint plus the zero-width marks fused onto it. Storage
   * stays a plain byte string, so the helpers translate cell columns into
   * byte offsets. For pure ASCII a cell is one byte and everything behaves
   * exactly as it did when the columns were byte offsets.
   * ========================================================================== */
  Test.group("TextArea - unicode cells", () => {
    /* renderLine with the default colors, 6 columns wide. A cursorRow of -1
     * keeps the cursor off this row so only the selection shows. */
    let render = (line, cursorRow, cursorCol, sel) =>
      TextArea.renderLine(
        line,
        0,
        cursorRow,
        cursorCol,
        sel,
        6,
        Element.White,
        Element.BrightBlack,
      );

    /* ---- cells vs bytes ---- */

    Test.run("a multi-byte character is one cell, not one byte per byte", () => {
      let text = "café";
      Test.assertEqual(String.length(text), 5, "5 bytes on disk");
      Test.assertEqual(TextArea.cellCount(text), 4, "but 4 editable cells");
      Test.assertEqual(TextArea.cellCount(sun), 1, "a wide ideograph is one cell");
      Test.assertEqual(
        TextArea.cellCount(eCombining),
        1,
        "base + combining mark fuse into one cell",
      );
    });

    Test.run("ASCII stays one cell per byte, tabs included", () => {
      /* A tab is zero-width, but it must not fuse onto the character before
       * it: for ASCII text a cell is always exactly one byte. */
      Test.assertEqual(TextArea.cellCount("a\tb"), 3, "tab is its own cell");
      Test.assertEqual(TextArea.cellToByte("a\tb", 2), 2, "byte offsets line up");
      let h = makeHarness("a\tb");
      h.cursorCol = 2;
      send(h, Key.Backspace, Key.noModifiers);
      Test.assertEqualStr(h.value, "ab", "backspace removes only the tab");
    });

    Test.run("cellToByte maps a cell column to a byte offset", () => {
      Test.assertEqual(TextArea.cellToByte("café", 0), 0, "start");
      Test.assertEqual(TextArea.cellToByte("café", 3), 3, "before 'é'");
      Test.assertEqual(TextArea.cellToByte("café", 4), 5, "after 'é' (2 bytes)");
      Test.assertEqual(TextArea.cellToByte("café", 99), 5, "clamped to end");
    });

    /* ---- typing multi-byte input via Key.Text ---- */

    Test.run("typing é via Key.Text advances the cursor one cell", () => {
      let h = makeHarness("caf");
      h.cursorCol = 3;
      typeText(h, eacute);
      Test.assertEqualStr(h.value, "café", "character appended");
      Test.assertEqual(String.length(h.value), 5, "5 bytes stored");
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 4), "cursor moved one cell");
    });

    Test.run("backspace after é removes both of its bytes", () => {
      let h = makeHarness("café");
      h.cursorCol = 4;
      send(h, Key.Backspace, Key.noModifiers);
      Test.assertEqualStr(h.value, "caf", "whole character removed");
      Test.assertEqual(String.length(h.value), 3, "no half-character left behind");
      Test.assertEqual(h.cursorCol, 3, "cursor moved back one cell");
    });

    Test.run("Key.Text with an active selection replaces it", () => {
      let h = makeHarness("hello");
      h.selection = Some((0, 0, 0, 2));
      h.cursorCol = 2;
      typeText(h, eacute);
      Test.assertEqualStr(h.value, eacute ++ "llo", "'he' replaced");
      Test.assertEqual(h.cursorCol, 1, "cursor after the inserted cell");
      Test.assertEqual(h.selection, None, "selection cleared");
    });

    Test.run("Key.Text with ctrl/alt/meta is not inserted", () => {
      let h = makeHarness("");
      send(h, Key.Text(eacute), altMods);
      Test.assertEqualStr(h.value, "", "alt+é does not type text");
    });

    /* ---- width-2 characters ---- */

    Test.run("typing a width-2 character advances one cell, not two", () => {
      let h = makeHarness("");
      typeText(h, sun);
      Test.assertEqualStr(h.value, sun, "ideograph inserted");
      Test.assertEqual((h.cursorRow, h.cursorCol), (0, 1), "one cell, though 2 columns wide");
      typeText(h, book);
      Test.assertEqualStr(h.value, sun ++ book, "second ideograph appended");
      Test.assertEqual(h.cursorCol, 2, "two cells");
    });

    Test.run("renderLine highlights both columns of a wide cell", () => {
      let cursorSpan = (s: string): string =>
        Element.styleToAnsi(Element.BgColor(Element.White))
        ++ Element.styleToAnsi(Element.FgColor(Element.Black))
        ++ s
        ++ Element.resetAnsi;
      let line = sun ++ "a";
      /* Cursor on the wide cell (cell column 0) */
      let rendered = render(line, 0, 0, None);
      Test.assertContains(
        rendered,
        cursorSpan(sun),
        "the whole ideograph sits inside one inverted run (2 columns)",
      );
      Test.assertEqualStr(
        Element.stripAnsi(rendered),
        line ++ "   ",
        "padded to 6 columns: 2 for the ideograph, 1 for 'a', 3 spaces",
      );
      Test.assertEqual(
        TextWidth.stringWidth(rendered),
        6,
        "rendered row is exactly 6 display columns",
      );
      /* Cursor on the cell AFTER the wide one */
      let rendered2 = render(line, 0, 1, None);
      Test.assertContains(rendered2, cursorSpan("a"), "cursor highlights 'a'");
      Test.assertTrue(
        String.length(rendered2) > 0
        && String.sub(rendered2, 0, String.length(sun)) == sun,
        "the ideograph is rendered unstyled before the cursor",
      );
    });

    Test.run("renderLine selection wraps whole cells", () => {
      let selSpan = (s: string): string =>
        Element.styleToAnsi(Element.BgColor(Element.BrightBlack))
        ++ s
        ++ Element.resetAnsi;
      let line = "a" ++ sun ++ "b";
      let rendered = render(line, (-1), 0, Some((0, 1, 0, 2)));
      Test.assertContains(
        rendered,
        selSpan(sun),
        "selected wide cell highlighted as a whole",
      );
      Test.assertEqual(
        TextWidth.stringWidth(rendered),
        6,
        "still 6 display columns",
      );
    });

    /* ---- combining marks ---- */

    Test.run("arrow-left jumps over a whole combining cluster", () => {
      let h = makeHarness("x" ++ eCombining);
      Test.assertEqual(String.length(h.value), 4, "4 bytes");
      h.cursorCol = 2; /* end of line: 'x' + the cluster = 2 cells */
      send(h, Key.Arrow_left, Key.noModifiers);
      Test.assertEqual(h.cursorCol, 1, "a single press clears the whole cluster");
      send(h, Key.Arrow_left, Key.noModifiers);
      Test.assertEqual(h.cursorCol, 0, "and another reaches the line start");
    });

    Test.run("backspace deletes base and combining mark together", () => {
      let h = makeHarness("x" ++ eCombining);
      h.cursorCol = 2;
      send(h, Key.Backspace, Key.noModifiers);
      Test.assertEqualStr(h.value, "x", "'e' and its acute removed together");
      Test.assertEqual(String.length(h.value), 1, "all 3 cluster bytes gone");
      Test.assertEqual(h.cursorCol, 1, "cursor back one cell");
    });

    Test.run("delete forward removes a whole combining cluster", () => {
      let h = makeHarness(eCombining ++ "x");
      h.cursorCol = 0;
      send(h, Key.Delete, Key.noModifiers);
      Test.assertEqualStr(h.value, "x", "cluster removed in one press");
      Test.assertEqual(h.cursorCol, 0, "cursor stays put");
    });

    /* ---- word motion ---- */

    Test.run("word motion treats a CJK run as one word", () => {
      let line = sun ++ book ++ lang ++ " abc";
      Test.assertEqual(TextArea.cellCount(line), 7, "3 ideographs + space + 3 letters");
      Test.assertEqual(
        TextArea.findNextWordEnd(line, 0),
        3,
        "forward motion stops after the CJK run",
      );
      Test.assertEqual(
        TextArea.findNextWordEnd(line, 3),
        7,
        "then after 'abc'",
      );
      Test.assertEqual(
        TextArea.findPrevWordStart(line, 7),
        4,
        "back to the start of 'abc'",
      );
      Test.assertEqual(
        TextArea.findPrevWordStart(line, 4),
        0,
        "back to the start of the CJK run",
      );
    });

    Test.run("alt+left/right move by word across a CJK run", () => {
      let h = makeHarness(sun ++ book ++ " ab");
      send(h, Key.Arrow_right, altMods);
      Test.assertEqual(h.cursorCol, 2, "end of the CJK run (2 cells)");
      send(h, Key.Arrow_right, altMods);
      Test.assertEqual(h.cursorCol, 5, "end of 'ab'");
      send(h, Key.Arrow_left, altMods);
      Test.assertEqual(h.cursorCol, 3, "start of 'ab'");
      send(h, Key.Arrow_left, altMods);
      Test.assertEqual(h.cursorCol, 0, "start of the CJK run");
    });

    Test.run("alt+backspace deletes a whole CJK word", () => {
      let h = makeHarness("ab " ++ sun ++ book);
      h.cursorCol = 5;
      send(h, Key.Backspace, altMods);
      Test.assertEqualStr(h.value, "ab ", "both ideographs removed");
      Test.assertEqual(h.cursorCol, 3, "cursor after 'ab '");
    });

    /* ---- a mixed line ---- */

    Test.run("mixed line: positionToIndex works in cell space", () => {
      let text = "a" ++ eacute ++ sun ++ "b"; /* 4 cells, 7 bytes */
      Test.assertEqual(String.length(text), 7, "7 bytes");
      Test.assertEqual(TextArea.cellCount(text), 4, "4 cells");
      Test.assertEqual(TextArea.positionToIndex(text, 0, 0), 0, "cell 0");
      Test.assertEqual(TextArea.positionToIndex(text, 0, 1), 1, "cell 1 (after 'a')");
      Test.assertEqual(TextArea.positionToIndex(text, 0, 2), 3, "cell 2 (after 'é')");
      Test.assertEqual(TextArea.positionToIndex(text, 0, 3), 6, "cell 3 (after the ideograph)");
      Test.assertEqual(TextArea.positionToIndex(text, 0, 4), 7, "end of line");
      Test.assertEqual(TextArea.positionToIndex(text, 0, 99), 7, "clamped to end");
    });

    Test.run("mixed line: Home/End land on cell columns", () => {
      let h = makeHarness("a" ++ eacute ++ sun ++ "b");
      send(h, Key.End, Key.noModifiers);
      Test.assertEqual(h.cursorCol, 4, "End lands after 'b' - 4 cells, not 7 bytes");
      send(h, Key.Home, Key.noModifiers);
      Test.assertEqual(h.cursorCol, 0, "Home returns to the line start");
    });

    Test.run("mixed line: arrow-right walks it one cell per press", () => {
      let h = makeHarness("a" ++ eacute ++ sun ++ "b");
      let cols = ref([]);
      for (_ in 1 to 5) {
        send(h, Key.Arrow_right, Key.noModifiers);
        cols := [h.cursorCol, ...cols^];
      };
      Test.assertEqual(
        List.rev(cols^),
        [1, 2, 3, 4, 4],
        "one cell per press, then stuck at the end",
      );
    });

    Test.run("mixed line: insertAt uses cell columns", () => {
      let (text, _row, col) =
        TextArea.insertAt("a" ++ eacute ++ sun ++ "b", 0, 2, "!");
      Test.assertEqualStr(text, "a" ++ eacute ++ "!" ++ sun ++ "b", "inserted after 'é'");
      Test.assertEqual(col, 3, "cursor one cell past the insertion");
    });

    Test.run("insertAt clamps to the cell count, not the byte length", () => {
      let (text, _row, col) = TextArea.insertAt("café", 0, 99, "!");
      Test.assertEqualStr(text, "café!", "appended at the end");
      Test.assertEqual(col, 5, "cursor at cell 5 (5 cells, 6 bytes)");
    });

    Test.run("deleteSelection uses cell columns", () => {
      let (text, row, col) =
        TextArea.deleteSelection("a" ++ eacute ++ sun ++ "b", (0, 1, 0, 3));
      Test.assertEqualStr(text, "ab", "'é' and the ideograph removed");
      Test.assertEqual((row, col), (0, 1), "cursor at the deletion point");
    });

    Test.run("arrow up/down clamp the column to the target line's cell count", () => {
      let h = makeHarness(sun ++ book ++ lang ++ "\nxy");
      h.cursorRow = 0;
      h.cursorCol = 3;
      send(h, Key.Arrow_down, Key.noModifiers);
      Test.assertEqual(
        (h.cursorRow, h.cursorCol),
        (1, 2),
        "clamped to 2 cells of 'xy'",
      );
      send(h, Key.Arrow_up, Key.noModifiers);
      Test.assertEqual(
        (h.cursorRow, h.cursorCol),
        (0, 2),
        "column 2 is a valid cell on the CJK line",
      );
    });
  });

  /* ==========================================================================
   * Soft wrapping
   *
   * The display mapping only: a logical line wider than ~maxWidth is painted
   * as several DISPLAY ROWS, the box grows between ~minHeight and ~maxHeight,
   * and past that it shows the window that keeps the cursor visible. Editing
   * is untouched - every column below is still a logical cell column.
   *
   * These read Element.render of the pure element (TextArea.make returns a
   * Text node, so Element.render is just its content), split into rows.
   * ========================================================================== */
  Test.group("TextArea - soft wrap", () => {
    let cursorBg = Element.styleToAnsi(Element.BgColor(Element.White));
    let selBg = Element.styleToAnsi(Element.BgColor(Element.BrightBlack));

    /* The painted rows of a TextArea, ANSI intact. */
    let rowsOf =
        (
          ~cursorVisible: bool=true,
          ~value: string,
          ~maxWidth: int,
          ~minHeight: int=1,
          ~maxHeight: int=100,
          ~cursorRow: int=0,
          ~cursorCol: int=0,
          ~selection: option(TextArea.selection)=?,
          (),
        )
        : array(string) => {
      let el =
        TextArea.make(
          ~cursorVisible,
          ~value,
          ~onChange=_ => (),
          ~maxWidth,
          ~minHeight,
          ~maxHeight,
          ~cursorRow,
          ~cursorCol,
          ~setCursor=_ => (),
          ~selection,
          ~setSelection=_ => (),
          (),
        );
      Array.of_list(String.split_on_char('\n', Element.render(el)));
    };

    let stripped = (rows: array(string)): array(string) =>
      Array.map(Element.stripAnsi, rows);

    /* A 25-cell line. */
    let long25 = "abcdefghijklmnopqrstuvwxy";

    Test.run("a line wider than the box is painted as several rows", () => {
      let rows = rowsOf(~value=long25, ~maxWidth=10, ());
      Test.assertEqual(Array.length(rows), 3, "25 cells at width 10 is 3 rows");
      let text = stripped(rows);
      Test.assertEqualStr(text[0], "abcdefghij", "cells 0-9");
      Test.assertEqualStr(text[1], "klmnopqrst", "cells 10-19");
      Test.assertEqualStr(text[2], "uvwxy     ", "cells 20-24, padded");
      Array.iteri(
        (i, row) =>
          Test.assertEqual(
            Element.visibleLength(row),
            10,
            "row " ++ string_of_int(i) ++ " is exactly 10 columns - nothing
             overflows the slot the box was given",
          ),
        rows,
      );
    });

    Test.run("a line that exactly fills its last row gets an empty row after it", () => {
      let value = String.make(20, 'a');
      let rows = rowsOf(~value, ~maxWidth=10, ~cursorCol=20, ());
      Test.assertEqual(
        Array.length(rows),
        3,
        "20 cells at width 10 is two full rows plus the row the end-of-line
         cursor lives on - type the character that fills a row and the cursor
         moves to the start of the next one",
      );
      Test.assertEqual(
        TextArea.measure(~value, ~maxWidth=10, ~minHeight=1, ()),
        3,
        "and measure says the same",
      );
      let text = stripped(rows);
      Test.assertEqualStr(text[2], "          ", "the third row is empty");
      Test.assertTrue(
        Test.contains(rows[2], cursorBg),
        "the end-of-line cursor is on that third row",
      );
      Test.assertFalse(
        Test.contains(rows[1], cursorBg),
        "and not at the end of the full second row",
      );
      Test.assertTrue(
        String.length(rows[2]) > String.length(cursorBg)
        && String.sub(rows[2], 0, String.length(cursorBg)) == cursorBg,
        "it sits at column 0 of that row",
      );
    });

    Test.run("a wide glyph moves whole to the next row", () => {
      /* 9 columns of ASCII, then a width-2 ideograph: it would straddle the
         boundary at width 10, so it starts the next row instead. */
      let rows = rowsOf(~value=String.make(9, 'a') ++ sun, ~maxWidth=10, ());
      Test.assertEqual(Array.length(rows), 2, "2 rows");
      let text = stripped(rows);
      Test.assertEqualStr(text[0], "aaaaaaaaa ", "the ideograph did not split");
      Test.assertEqualStr(text[1], sun ++ "        ", "it moved whole");
      Test.assertEqual(
        Element.visibleLength(rows[1]),
        10,
        "and the row is still exactly 10 columns wide",
      );
    });

    Test.run("the cursor renders on the continuation row it belongs to", () => {
      let rows = rowsOf(~value=long25, ~maxWidth=10, ~cursorCol=12, ());
      Test.assertFalse(Test.contains(rows[0], cursorBg), "not on row 0");
      Test.assertTrue(
        Test.contains(rows[1], cursorBg),
        "cell 12 is on the second display row",
      );
      Test.assertFalse(Test.contains(rows[2], cursorBg), "and not on row 2");
    });

    Test.run("a selection across a wrap boundary highlights both rows", () => {
      let rows =
        rowsOf(
          ~value=long25,
          ~maxWidth=10,
          ~cursorRow=(-1),
          ~selection=(0, 8, 0, 13),
          (),
        );
      Test.assertTrue(
        Test.contains(rows[0], selBg),
        "the cells before the boundary are highlighted",
      );
      Test.assertTrue(
        Test.contains(rows[1], selBg),
        "and so are the ones after it - isInSelection is asked the same
         logical (row, cell) question on both display rows",
      );
      Test.assertFalse(Test.contains(rows[2], selBg), "row 2 is outside it");
    });

    /* Eight display rows of 10 columns, the last one short, each tagged with
       its index so the window is readable. */
    let eightRows =
      String.concat(
        "",
        List.init(7, i => "row" ++ string_of_int(i) ++ "______"),
      )
      ++ "row7__";

    Test.run("taller than the box: the window follows the cursor", () => {
      Test.assertEqual(
        List.length(TextArea.displayRows(eightRows, 10)),
        8,
        "the fixture is 8 display rows",
      );

      let atEnd =
        stripped(
          rowsOf(
            ~value=eightRows,
            ~maxWidth=10,
            ~maxHeight=3,
            ~cursorCol=String.length(eightRows),
            (),
          ),
        );
      Test.assertEqual(Array.length(atEnd), 3, "the box is 3 rows tall");
      Test.assertContains(atEnd[0], "row5", "the LAST three rows show");
      Test.assertContains(atEnd[1], "row6", "row 6");
      Test.assertContains(atEnd[2], "row7", "and the cursor's own row last");

      let atStart =
        stripped(
          rowsOf(~value=eightRows, ~maxWidth=10, ~maxHeight=3, ~cursorCol=0, ()),
        );
      Test.assertEqual(Array.length(atStart), 3, "still 3 rows tall");
      Test.assertContains(atStart[0], "row0", "the FIRST three rows show");
      Test.assertContains(atStart[1], "row1", "row 1");
      Test.assertContains(atStart[2], "row2", "row 2");
      Test.assertFalse(
        Test.contains(String.concat("", Array.to_list(atStart)), "row3"),
        "and nothing below the window is painted",
      );
    });

    Test.run("wrapping applies per logical line and the rows accumulate", () => {
      let value = String.make(15, 'a') ++ "\n" ++ String.make(15, 'b');
      let rows = rowsOf(~value, ~maxWidth=10, ());
      Test.assertEqual(
        Array.length(rows),
        4,
        "two 15-cell lines at width 10 are 2 + 2 display rows",
      );
      let text = stripped(rows);
      Test.assertEqualStr(text[0], "aaaaaaaaaa", "line 0, row 0");
      Test.assertEqualStr(text[1], "aaaaa     ", "line 0, row 1");
      Test.assertEqualStr(text[2], "bbbbbbbbbb", "line 1, row 0");
      Test.assertEqualStr(text[3], "bbbbb     ", "line 1, row 1");
      Test.assertEqual(
        TextArea.measure(~value, ~maxWidth=10, ~minHeight=1, ()),
        4,
        "measure agrees",
      );
    });

    Test.run("measure reports the height the renderer will paint", () => {
      Test.assertEqual(
        TextArea.measure(~value="", ~maxWidth=10, ~minHeight=2, ()),
        2,
        "the empty value is one row, clamped up to minHeight",
      );
      Test.assertEqual(
        TextArea.measure(~value="", ~maxWidth=10, ~minHeight=1, ()),
        1,
        "one row at minHeight=1",
      );
      Test.assertEqual(
        TextArea.measure(~value=eightRows, ~maxWidth=10, ~minHeight=1, ~maxHeight=3, ()),
        3,
        "and it clamps down to maxHeight",
      );
      Test.assertEqual(
        TextArea.measure(~value=long25, ~maxWidth=10, ~minHeight=1, ()),
        Array.length(rowsOf(~value=long25, ~maxWidth=10, ())),
        "measure == the rows make actually paints (25 cells at width 10)",
      );
      Test.assertEqual(
        TextArea.measure(~value=long25, ~maxWidth=10, ~minHeight=1, ~maxHeight=2, ()),
        Array.length(
          rowsOf(~value=long25, ~maxWidth=10, ~maxHeight=2, ()),
        ),
        "and with the box clamped shorter than the content",
      );
    });

    Test.run("a long placeholder cannot overflow either", () => {
      let placeholder = "type something much longer than the box is wide";
      let el =
        TextArea.make(
          ~value="",
          ~onChange=_ => (),
          ~placeholder,
          ~maxWidth=10,
          ~minHeight=1,
          ~maxHeight=5,
          ~cursorRow=0,
          ~cursorCol=0,
          ~setCursor=_ => (),
          ~selection=None,
          ~setSelection=_ => (),
          (),
        );
      let rows = Array.of_list(String.split_on_char('\n', Element.render(el)));
      Test.assertEqual(
        Array.length(rows),
        1,
        "the placeholder fills only the rows the (empty) VALUE earned, so a
         container that asked measure gets the box it sized for",
      );
      Test.assertEqual(
        Element.visibleLength(rows[0]),
        10,
        "and that row is exactly the box width",
      );
      Test.assertEqualStr(
        Element.stripAnsi(rows[0]),
        "type somet",
        "wrapped, not overflowing",
      );
    });
  });

  /* ==========================================================================
   * Headless integration: drive the real <TextArea /> component end to end
   * ========================================================================== */
  Test.group("TextArea - headless integration", () => {
    module TAApp = {
      [@component]
      let make = () => {
        let (text, setText) = Hooks.useState("hi");
        let (pos, setPos) = Hooks.useState((0, 0));
        let (cursorRow, cursorCol) = pos;
        let (selection, setSelection) = Hooks.useState(None);

        Event.useKeyDown((key, modifiers) =>
          TextArea.handleKeyDown(
            key,
            modifiers,
            text,
            setText,
            None,
            cursorRow,
            cursorCol,
            setPos,
            selection,
            setSelection,
          )
        );

        <TextArea
          value=text
          onChange=setText
          cursorRow
          cursorCol
          setCursor=setPos
          selection
          setSelection
          minHeight=2
          maxHeight=5
          maxWidth=20
        />;
      };
    };

    Test.run("initial render shows the starting value", () => {
      let handle = Runtime.startHeadless((module TAApp));
      let output = handle.getOutput(true);
      Test.assertContains(output, "hi", "initial value rendered");
      handle.quit();
    });

    Test.run("typing a character through sendKey updates the render", () => {
      let handle = Runtime.startHeadless((module TAApp));
      handle.sendKey(Key.Arrow_right, Key.noModifiers);
      handle.sendKey(Key.Arrow_right, Key.noModifiers);
      handle.sendKey(Key.Char('!'), Key.noModifiers);
      let output = handle.getOutput(true);
      Test.assertContains(output, "hi!", "typed character appears in render");
      handle.quit();
    });

    Test.run("backspace through sendKey updates the render", () => {
      let handle = Runtime.startHeadless((module TAApp));
      handle.sendKey(Key.Arrow_right, Key.noModifiers);
      handle.sendKey(Key.Arrow_right, Key.noModifiers);
      handle.sendKey(Key.Backspace, Key.noModifiers);
      let output = handle.getOutput(true);
      Test.assertFalse(Test.contains(output, "hi"), "'hi' no longer present");
      handle.quit();
    });

    Test.run("enter through sendKey creates a second rendered line", () => {
      let handle = Runtime.startHeadless((module TAApp));
      handle.sendKey(Key.Arrow_right, Key.noModifiers);
      handle.sendKey(Key.Enter, Key.noModifiers);
      handle.sendKey(Key.Char('!'), Key.noModifiers);
      let output = handle.getOutput(true);
      Test.assertContains(output, "!i", "text inserted after the split point");
      handle.quit();
    });
  });

  /* ==========================================================================
   * The blinking cursor (lib/TextArea.re)
   *
   * <TextArea> is a real component wrapping Element.TextArea's pure
   * renderer, and it owns a useState/useInterval blink. The cursor is a
   * BACKGROUND style over a character that is drawn either way, so none of
   * this is visible to getOutput(true) - these tests read getOutput(false)
   * and look for the cursor's background escape. Time passes only through
   * the virtual clock (advanceTime); handle-based startHeadless is not the
   * MATCHA_HEADLESS=1 stream mode that suppresses the blink.
   * ========================================================================== */
  Test.group("TextArea - blinking cursor", () => {
    /* The escape the cursor cell paints, computed rather than hardcoded so
     * it tracks Element's palette. */
    let cursorBg = Element.styleToAnsi(Element.BgColor(Element.White));
    let placeholder = "Type here...";

    /* An empty, one-line input showing a placeholder. ~blink is threaded so
     * the same fixture serves the steady-cursor case. */
    module BlinkApp = {
      [@component]
      let make = (~blink: bool) => {
        let (text, setText) = Hooks.useState("");
        let (pos, setPos) = Hooks.useState((0, 0));
        let (cursorRow, cursorCol) = pos;
        let (selection, setSelection) = Hooks.useState(None);

        Event.useKeyDown((key, modifiers) =>
          TextArea.handleKeyDown(
            key,
            modifiers,
            text,
            setText,
            None,
            cursorRow,
            cursorCol,
            setPos,
            selection,
            setSelection,
          )
        );

        <TextArea
          value=text
          onChange=setText
          placeholder
          cursorRow
          cursorCol
          setCursor=setPos
          selection
          setSelection
          minHeight=1
          maxHeight=1
          blink
        />;
      };
    };

    module BlinkingApp = {
      [@component]
      let make = () => <BlinkApp blink=true />;
    };

    module SteadyApp = {
      [@component]
      let make = () => <BlinkApp blink=false />;
    };

    Test.run("an empty input still shows a cursor block", () => {
      let handle = Runtime.startHeadless((module BlinkingApp));
      Test.assertContains(
        handle.getOutput(false),
        cursorBg,
        "the placeholder's first cell carries the cursor background",
      );
      Test.assertContains(
        handle.getOutput(true),
        placeholder,
        "and the placeholder text itself is untouched",
      );
      handle.quit();
    });

    Test.run("the cursor blinks on the virtual clock", () => {
      let handle = Runtime.startHeadless((module BlinkingApp));
      Test.assertContains(
        handle.getOutput(false),
        cursorBg,
        "the first frame has the cursor on",
      );
      handle.advanceTime(530);
      Test.assertFalse(
        Test.contains(handle.getOutput(false), cursorBg),
        "one half-period later it is off",
      );
      handle.advanceTime(530);
      Test.assertContains(
        handle.getOutput(false),
        cursorBg,
        "and one more brings it back",
      );
      handle.quit();
    });

    Test.run("blink=false is a steady cursor with no timer", () => {
      let handle = Runtime.startHeadless((module SteadyApp));
      let before = handle.getOutput(false);
      Test.assertContains(before, cursorBg, "the cursor is drawn");
      handle.advanceTime(2000); /* several blink periods */
      let after = handle.getOutput(false);
      Test.assertContains(after, cursorBg, "and it is still drawn");
      Test.assertEqualStr(after, before, "nothing repainted at all");
      handle.quit();
    });

    Test.run("a typed-into input blinks the same way", () => {
      let handle = Runtime.startHeadless((module BlinkingApp));
      handle.sendKey(Key.Char('h'), Key.noModifiers);
      handle.sendKey(Key.Char('i'), Key.noModifiers);
      Test.assertContains(
        handle.getOutput(true),
        "hi",
        "the typed text is rendered",
      );
      Test.assertContains(
        handle.getOutput(false),
        cursorBg,
        "the non-empty path draws a cursor too",
      );
      handle.advanceTime(530);
      Test.assertFalse(
        Test.contains(handle.getOutput(false), cursorBg),
        "and it blinks off",
      );
      Test.assertContains(
        handle.getOutput(true),
        "hi",
        "the text stays put while the cursor is off",
      );
      handle.quit();
    });
  });
};
