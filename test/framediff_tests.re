/*
 * Tests for FrameDiff (lib/FrameDiff.re)
 *
 * FrameDiff.diff is pure (no I/O), so it's tested directly against expected
 * escape-sequence output rather than through a real terminal or PTY.
 *
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
  });
