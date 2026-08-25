/*
 * Tests for FrameDiff (lib/FrameDiff.re)
 *
 * FrameDiff.diff is pure (no I/O), so it's tested directly against expected
 * escape-sequence output rather than through a real terminal or PTY.
 *
 * ============================================================================
 * THE PAIRING RULE (referenced from test/liveregion_tests.re)
 * ============================================================================
 *
 * A byte-exact expectation is not a test of what the terminal DISPLAYS. It
 * is a test that the writer still emits the bytes the test author believed
 * were right - which encodes a MODEL of terminal behaviour inside the
 * assertion. When that model is wrong, the byte test cheerfully pins the
 * wrong bytes forever.
 *
 * That is not hypothetical: the painters used to emit
 * `content ++ ESC[0m ESC[K` per row. Every byte test agreed with the code,
 * every rendered frame's TEXT was correct, and the suite was green - while
 * on a real terminal every full-width row silently lost its last cell,
 * because printing into the last column leaves the cursor ON that column
 * (deferred wrap) and the EL then erases it. A human looking at a
 * screenshot found it; 531 tests did not.
 *
 * RULE: every byte-exact painter expectation must be PAIRED with a grid
 * assertion - the same painter output fed through test/vterm.re (an
 * independent xterm-semantics screen model) with the assertion made on the
 * resulting SCREEN. The byte tests say "the writer did not change"; the
 * grid tests say "the terminal shows the right thing". Neither is
 * sufficient alone.
 *
 * See the "Grid (Vterm)" group at the bottom of this file and of
 * test/liveregion_tests.re.
 */

open Matcha;

let run = () =>
  Test.group("Frame Diff", () => {
    /* Small local helper: does `s` begin with `prefix`? */
    let startsWith = (s: string, prefix: string): bool => {
      let plen = String.length(prefix);
      String.length(s) >= plen && String.sub(s, 0, plen) == prefix;
    };

    /* Small local helper: does `s` end with `suffix`? */
    let endsWith = (s: string, suffix: string): bool => {
      let slen = String.length(s);
      let suflen = String.length(suffix);
      slen >= suflen
      && String.sub(s, slen - suflen, suflen) == suffix;
    };

    Test.run("identical prev/next produces no output", () => {
      let frame = [|"a", "b", "c"|];
      let result = FrameDiff.diff(~prev=Some(frame), ~next=frame);
      Test.assertEqualStr(result, "", "no changes means empty string");
    });

    Test.run("identical prev/next (fresh arrays) produces no output", () => {
      let prev = [|"a", "b", "c"|];
      let next = [|"a", "b", "c"|];
      let result = FrameDiff.diff(~prev=Some(prev), ~next);
      Test.assertEqualStr(
        result,
        "",
        "structurally equal but distinct arrays still diff to empty",
      );
    });

    Test.run("prev=None does a full clear and paints every line", () => {
      let next = [|"line one", "line two", "line three"|];
      let result = FrameDiff.diff(~prev=None, ~next);
      Test.assertTrue(
        startsWith(result, "\027[?2026h\027[2J\027[H"),
        "first frame opens with sync guard + clear screen + home",
      );
      Test.assertContains(result, "line one", "paints first line");
      Test.assertContains(result, "line two", "paints second line");
      Test.assertContains(result, "line three", "paints third line");
      Test.assertContains(
        result,
        "\027[1;1H",
        "positions cursor at row 1 for the first line",
      );
      Test.assertContains(
        result,
        "\027[2;1H",
        "positions cursor at row 2 for the second line",
      );
      Test.assertContains(
        result,
        "\027[3;1H",
        "positions cursor at row 3 for the third line",
      );
    });

    Test.run("one changed middle line repaints only that row", () => {
      let prev = [|"a", "b", "c"|];
      let next = [|"a", "X", "c"|];
      let result = FrameDiff.diff(~prev=Some(prev), ~next);
      Test.assertContains(
        result,
        "\027[2;1H",
        "moves cursor to the changed row (row 2)",
      );
      Test.assertTrue(
        !Test.contains(result, "\027[1;1H"),
        "unchanged row 1 is not repainted",
      );
      Test.assertTrue(
        !Test.contains(result, "\027[3;1H"),
        "unchanged row 3 is not repainted",
      );
      Test.assertContains(result, "X", "contains the new content");
      Test.assertContains(
        result,
        "\027[0m\027[K",
        "resets attributes before clearing to end of line",
      );
    });

    Test.run("next longer than prev paints new tail lines", () => {
      let prev = [|"a", "b"|];
      let next = [|"a", "b", "c", "d"|];
      let result = FrameDiff.diff(~prev=Some(prev), ~next);
      Test.assertTrue(
        !Test.contains(result, "\027[1;1H"),
        "unchanged row 1 is not repainted",
      );
      Test.assertTrue(
        !Test.contains(result, "\027[2;1H"),
        "unchanged row 2 is not repainted",
      );
      Test.assertContains(
        result,
        "\027[3;1H",
        "paints new row 3 at the correct position",
      );
      Test.assertContains(
        result,
        "\027[4;1H",
        "paints new row 4 at the correct position",
      );
      Test.assertContains(result, "c", "contains new row 3 content");
      Test.assertContains(result, "d", "contains new row 4 content");
    });

    Test.run("next shorter than prev clears the leftover tail", () => {
      let prev = [|"a", "b", "c", "d"|];
      let next = [|"a", "b"|];
      let result = FrameDiff.diff(~prev=Some(prev), ~next);
      Test.assertContains(
        result,
        "\027[3;1H\027[J",
        "clears from the new last row to end of screen",
      );
    });

    Test.run("sync guards wrap the first-frame result", () => {
      let next = [|"only line"|];
      let result = FrameDiff.diff(~prev=None, ~next);
      Test.assertTrue(
        startsWith(result, "\027[?2026h"),
        "starts with the synchronized-update begin guard",
      );
      Test.assertTrue(
        endsWith(result, "\027[?2026l"),
        "ends with the synchronized-update end guard",
      );
    });

    Test.run("sync guards wrap a changed-line result", () => {
      let prev = [|"a", "b"|];
      let next = [|"a", "changed"|];
      let result = FrameDiff.diff(~prev=Some(prev), ~next);
      Test.assertTrue(
        startsWith(result, "\027[?2026h"),
        "starts with the synchronized-update begin guard",
      );
      Test.assertTrue(
        endsWith(result, "\027[?2026l"),
        "ends with the synchronized-update end guard",
      );
    });

    Test.run("no-change result is not wrapped in sync guards", () => {
      let frame = [|"same", "same2"|];
      let result = FrameDiff.diff(~prev=Some(frame), ~next=frame);
      Test.assertTrue(
        !Test.contains(result, "?2026"),
        "empty result has no sync guards at all",
      );
    });

    Test.run("erase-to-EOL comes BEFORE the content, never after it", () => {
      /* Regression: an EL after the content lands in the terminal's
         PENDING-WRAP state when the line spans the full width, and erases
         the last cell just painted - the fullscreen input box lost its
         right border to exactly this. The clear must ride directly on the
         absolute move to column 1. */
      let result =
        FrameDiff.diff(~prev=Some([|"old"|]), ~next=[|"new"|]);
      Test.assertContains(
        result,
        "\027[1;1H\027[0m\027[K" ++ "new",
        "move, clear from column 1, then paint",
      );
      Test.assertTrue(
        !Test.contains(result, "new\027[0m\027[K"),
        "and no erase trails the painted content",
      );
    });

    /* ========================================================================
     * Grid (Vterm) - the paired half of every byte test above.
     *
     * See THE PAIRING RULE in this file's header. These drive the REAL
     * FrameDiff output through the independent terminal model in
     * test/vterm.re and assert on the screen it produces.
     * ====================================================================== */

    /* Paint a sequence of frames into a fresh screen of the given size,
     * exactly as Runtime's Fullscreen path does: first frame with
     * prev=None, each later frame diffed against the one before. */
    let paintFrames =
        (~width: int, ~height: int, frames: list(array(string))): Vterm.t => {
      let vt = Vterm.create(~width, ~height);
      let prev = ref(None);
      List.iter(
        next => {
          Vterm.feed(vt, FrameDiff.diff(~prev=prev^, ~next));
          prev := Some(next);
        },
        frames,
      );
      vt;
    };

    Test.run("full-width rows keep their LAST column on the screen", () => {
      /* THIS TEST FAILS against the pre-fix `content ++ ESC[0m ESC[K`
         ordering, and passes with the clear-first form the module uses now.
         It is the machine-checkable statement of the bug a human had to
         catch in a screenshot: a row that spans the full terminal width
         leaves the cursor in the PENDING-WRAP state, still on the last
         column, so a trailing EL erases the cell just painted - the right
         border of the fullscreen input box, a scrollbar glyph, a box
         corner. Reorder FrameDiff.paintLine and watch this go red. */
      let width = 20;
      let border = "|" ++ String.make(width - 2, '-') ++ "|";
      let body = "|" ++ String.make(width - 2, ' ') ++ "|";
      let frame = [|border, body, border, ""|];
      let vt = paintFrames(~width, ~height=4, [frame]);
      Test.assertEqualStr(
        Vterm.cellGlyph(vt, ~row=0, ~col=width - 1),
        "|",
        "row 0's right border survived",
      );
      Test.assertEqualStr(
        Vterm.cellGlyph(vt, ~row=1, ~col=width - 1),
        "|",
        "row 1's right border survived",
      );
      Test.assertEqualStr(
        Vterm.cellGlyph(vt, ~row=2, ~col=width - 1),
        "|",
        "row 2's right border survived",
      );
      Test.assertEqualStr(
        Vterm.row(vt, 0),
        border,
        "and the whole row matches, character for character",
      );
    });

    Test.run("a first paint puts EXACTLY the frame on the screen", () => {
      /* prev=None opens with ESC[2J ESC[H. Nothing may survive it, and
         nothing beyond the frame may appear. */
      let vt =
        Vterm.create(~width=12, ~height=4);
      Vterm.feed(vt, "stale stale\r\nstale stale\r\nstale stale");
      let frame = [|"alpha", "beta", "gamma", "delta"|];
      Vterm.feed(vt, FrameDiff.diff(~prev=None, ~next=frame));
      Test.assertEqualStr(
        Vterm.text(vt),
        "alpha\nbeta\ngamma\ndelta",
        "the clear wiped the old contents and the frame is all that is left",
      );
    });

    Test.run("a shrinking frame leaves no stale tail on the grid", () => {
      let vt =
        paintFrames(
          ~width=12,
          ~height=5,
          [
            [|"one", "two", "three", "four"|],
            [|"one", "two"|],
          ],
        );
      Test.assertEqualStr(
        Vterm.text(vt),
        "one\ntwo\n\n\n",
        "rows 3 and 4 were erased, not left showing the taller frame",
      );
    });

    Test.run("a changed row is the ONLY row the screen changes", () => {
      let vt =
        paintFrames(
          ~width=12,
          ~height=4,
          [[|"aaa", "bbb", "ccc", "ddd"|], [|"aaa", "XXX", "ccc", "ddd"|]],
        );
      Test.assertEqualStr(
        Vterm.text(vt),
        "aaa\nXXX\nccc\nddd",
        "in-place repaint of one row",
      );
    });

    Test.run("a full-width row shrinking to a short one clears the tail", () => {
      let width = 16;
      let vt =
        paintFrames(
          ~width,
          ~height=2,
          [[|String.make(width, '#'), "x"|], [|"ok", "x"|]],
        );
      Test.assertEqualStr(
        Vterm.row(vt, 0),
        "ok" ++ String.make(width - 2, ' '),
        "the erase-before-content form still clears a longer previous row",
      );
    });

    Test.run("FrameDiff emits nothing the terminal model does not know", () => {
      let vt =
        paintFrames(
          ~width=20,
          ~height=4,
          [
            [|"a", "b", "c", "d"|],
            [|"a", "changed", "c", "d"|],
            [|"a"|],
            [|"a", "b", "c", "d"|],
          ],
        );
      Test.assertEqual(
        Vterm.unknownSeqs(vt),
        [],
        "every sequence FrameDiff writes is one Vterm implements",
      );
    });
  });
