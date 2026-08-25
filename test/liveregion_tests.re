/*
 * Tests for LiveRegion (lib/LiveRegion.re)
 *
 * LiveRegion.patch/erase are pure, so they are asserted BYTE FOR BYTE
 * against the exact escape sequence expected - not "contains ESC[2A"
 * somewhere. The whole module exists to get these bytes right: a stray
 * cursor move leaves the live region half a line off, and the error
 * compounds silently over frames, so an exact-match test is the only kind
 * worth having here.
 *
 * Two invariants every case below implicitly checks:
 * - relative addressing only (no ESC[<row>;<col>H, no ESC[2J anywhere), and
 * - the cursor ends at column 1 of the LAST live line (every non-empty patch
 *   finishes with "\r" after walking down to that line).
 *
 * THE PAIRING RULE applies here in full: byte-exact expectations encode a
 * model of terminal behaviour and must always be paired with a grid
 * assertion. The rule is written out once, in the header of
 * test/framediff_tests.re - read it there. The paired half of this file is
 * the "Grid (Vterm)" group at the bottom.
 */
open Matcha;

/* Local shorthands for the escape sequences, so the expectations below read
 * as sequences rather than as escape soup. */
let ss = "\027[?2026h"; /* synchronized update: begin */
let se = "\027[?2026l"; /* synchronized update: end */
let cl = "\027[0m\027[K"; /* reset attributes, clear to end of line */
let rs = "\027[0m"; /* reset attributes after a painted line */

/* One painted row: clear FIRST (from column 1), then the content, then a
 * reset. The clear must precede the content - an EL after a full-width line
 * would land in the pending-wrap state and erase the line's last cell (see
 * LiveRegion's MOVEMENT PRIMITIVES). */
let paint = (s: string): string => cl ++ s ++ rs;
let ej = "\027[0J"; /* erase from cursor to end of screen */
let up = n => "\027[" ++ string_of_int(n) ++ "A";
let dn = "\r\n"; /* down one line, column 1 (a LINE FEED - see LiveRegion) */

let run = () =>
  Test.group("Live Region", () => {
    Test.run("first paint writes the frame at the cursor", () => {
      let result =
        LiveRegion.patch(
          ~prev=None,
          ~staticLines=[],
          ~next=[|"a", "b"|],
          ~termHeight=10,
        );
      Test.assertEqualStr(
        result,
        ss ++ "\r" ++ paint("a") ++ dn ++ paint("b") ++ "\r" ++ se,
        "no clear, no absolute moves: paint here, end on the last line",
      );
    });

    Test.run("first paint commits static lines above the frame", () => {
      let result =
        LiveRegion.patch(
          ~prev=None,
          ~staticLines=["s1", "s2"],
          ~next=[|"live"|],
          ~termHeight=10,
        );
      Test.assertEqualStr(
        result,
        ss
        ++ "\r"
        ++ paint("s1")
        ++ dn
        ++ paint("s2")
        ++ dn
        ++ paint("live")
        ++ "\r"
        ++ se,
        "static lines are line-fed away, the frame follows",
      );
    });

    Test.run("a single changed row moves up, down to it, and repaints it", () => {
      let result =
        LiveRegion.patch(
          ~prev=Some([|"a", "b", "c"|]),
          ~staticLines=[],
          ~next=[|"a", "X", "c"|],
          ~termHeight=10,
        );
      Test.assertEqualStr(
        result,
        ss ++ "\r" ++ up(2) ++ dn ++ paint("X") ++ dn ++ "\r" ++ se,
        "only row 1 is repainted; rows 0 and 2 are stepped over",
      );
    });

    Test.run("a frame that grows paints only the new tail", () => {
      let result =
        LiveRegion.patch(
          ~prev=Some([|"a"|]),
          ~staticLines=[],
          ~next=[|"a", "b"|],
          ~termHeight=10,
        );
      Test.assertEqualStr(
        result,
        ss ++ "\r" ++ dn ++ paint("b") ++ "\r" ++ se,
        "the line feed onto the new row is what scrolls the screen if needed",
      );
    });

    Test.run("a frame that shrinks erases below and steps back up", () => {
      let result =
        LiveRegion.patch(
          ~prev=Some([|"a", "b", "c"|]),
          ~staticLines=[],
          ~next=[|"a", "b"|],
          ~termHeight=10,
        );
      Test.assertEqualStr(
        result,
        ss ++ "\r" ++ up(2) ++ dn ++ dn ++ ej ++ up(1) ++ "\r" ++ se,
        "walk to the row past the new end, erase from there, come back",
      );
    });

    Test.run("shrinking to an empty frame keeps one blank live line", () => {
      let result =
        LiveRegion.patch(
          ~prev=Some([|"a", "b"|]),
          ~staticLines=[],
          ~next=[|""|],
          ~termHeight=10,
        );
      Test.assertEqualStr(
        result,
        ss ++ "\r" ++ up(1) ++ paint("") ++ dn ++ ej ++ up(1) ++ "\r" ++ se,
        "the empty frame is one blank row, not a zero-height region",
      );
    });

    Test.run("static lines force a rebuild of the whole region", () => {
      let result =
        LiveRegion.patch(
          ~prev=Some([|"a", "b"|]),
          ~staticLines=["s1", "s2"],
          ~next=[|"x", "y"|],
          ~termHeight=10,
        );
      Test.assertEqualStr(
        result,
        ss
        ++ "\r"
        ++ up(1)
        ++ ej
        ++ paint("s1")
        ++ dn
        ++ paint("s2")
        ++ dn
        ++ paint("x")
        ++ dn
        ++ paint("y")
        ++ "\r"
        ++ se,
        "the region moves down, so it is erased and painted afresh",
      );
    });

    Test.run("static lines are committed even when the frame is unchanged", () => {
      let frame = [|"a", "b"|];
      let result =
        LiveRegion.patch(
          ~prev=Some(frame),
          ~staticLines=["s"],
          ~next=frame,
          ~termHeight=10,
        );
      Test.assertEqualStr(
        result,
        ss
        ++ "\r"
        ++ up(1)
        ++ ej
        ++ paint("s")
        ++ dn
        ++ paint("a")
        ++ dn
        ++ paint("b")
        ++ "\r"
        ++ se,
        "an identical frame still has to move down past the committed line",
      );
    });

    Test.run("identical frame with nothing to commit produces no bytes", () => {
      let result =
        LiveRegion.patch(
          ~prev=Some([|"a", "b"|]),
          ~staticLines=[],
          ~next=[|"a", "b"|],
          ~termHeight=10,
        );
      Test.assertEqualStr(result, "", "structurally equal frames diff to \"\"");
      Test.assertTrue(
        !Test.contains(result, "?2026"),
        "an empty patch carries no sync guards either",
      );
    });

    Test.run("an empty next array normalizes to one blank line", () => {
      let result =
        LiveRegion.patch(
          ~prev=None,
          ~staticLines=[],
          ~next=[||],
          ~termHeight=10,
        );
      Test.assertEqualStr(
        result,
        ss ++ "\r" ++ paint("") ++ "\r" ++ se,
        "[||] is treated as [|\"\"|] - the region is never zero lines",
      );
      Test.assertEqual(
        LiveRegion.normalize(~next=[||], ~termHeight=10),
        [|""|],
        "normalize spells the same rule out",
      );
    });

    Test.run("a frame taller than the screen is clamped to termHeight", () => {
      let result =
        LiveRegion.patch(
          ~prev=None,
          ~staticLines=[],
          ~next=[|"l0", "l1", "l2", "l3", "l4"|],
          ~termHeight=3,
        );
      Test.assertEqualStr(
        result,
        ss
        ++ "\r"
        ++ paint("l0")
        ++ dn
        ++ paint("l1")
        ++ dn
        ++ paint("l2")
        ++ "\r"
        ++ se,
        "only the first termHeight lines are painted",
      );
      Test.assertTrue(
        !Test.contains(result, "l3") && !Test.contains(result, "l4"),
        "lines past the bottom of the screen are dropped, not wrapped",
      );
    });

    Test.run("every patch is wrapped in the synchronized-update guards", () => {
      let cases = [
        LiveRegion.patch(
          ~prev=None,
          ~staticLines=[],
          ~next=[|"a"|],
          ~termHeight=10,
        ),
        LiveRegion.patch(
          ~prev=Some([|"a"|]),
          ~staticLines=[],
          ~next=[|"b"|],
          ~termHeight=10,
        ),
        LiveRegion.patch(
          ~prev=Some([|"a"|]),
          ~staticLines=["s"],
          ~next=[|"a"|],
          ~termHeight=10,
        ),
      ];
      List.iter(
        result => {
          let len = String.length(result);
          Test.assertTrue(
            String.length(result) > 0
            && String.sub(result, 0, String.length(ss)) == ss,
            "starts with the begin guard",
          );
          Test.assertTrue(
            String.sub(result, len - String.length(se), String.length(se))
            == se,
            "ends with the end guard",
          );
        },
        cases,
      );
    });

    Test.run("patches never use absolute addressing or clear the screen", () => {
      let results = [
        LiveRegion.patch(
          ~prev=None,
          ~staticLines=["s"],
          ~next=[|"a", "b"|],
          ~termHeight=10,
        ),
        LiveRegion.patch(
          ~prev=Some([|"a", "b"|]),
          ~staticLines=[],
          ~next=[|"a", "z"|],
          ~termHeight=10,
        ),
        LiveRegion.patch(
          ~prev=Some([|"a", "b", "c"|]),
          ~staticLines=["s"],
          ~next=[|"a"|],
          ~termHeight=10,
        ),
      ];
      List.iter(
        result => {
          Test.assertTrue(
            !Test.contains(result, "\027[2J"),
            "inline rendering never clears the screen",
          );
          Test.assertTrue(
            !Test.contains(result, ";1H"),
            "no absolute cursor positioning - rows move under scrolling",
          );
          Test.assertTrue(
            !Test.contains(result, "\027[B"),
            "moving down is a line feed, never ESC[B (which cannot scroll)",
          );
        },
        results,
      );
    });

    Test.run("erase removes the region and leaves the cursor on its top line", () => {
      Test.assertEqualStr(
        LiveRegion.erase(~prevHeight=3, ~termHeight=24),
        "\r" ++ up(2) ++ ej,
        "three-line region: up two lines, then erase everything below",
      );
      Test.assertEqualStr(
        LiveRegion.erase(~prevHeight=1, ~termHeight=24),
        "\r" ++ ej,
        "one-line region: no move needed",
      );
      Test.assertEqualStr(
        LiveRegion.erase(~prevHeight=0, ~termHeight=24),
        "",
        "nothing painted yet - emit nothing at all",
      );
      Test.assertEqualStr(
        LiveRegion.erase(~prevHeight=40, ~termHeight=10),
        "\r" ++ up(9) ++ ej,
        "a region taller than the screen was clamped when painted, so the "
        ++ "erase is clamped too",
      );
    });

    /* ========================================================================
     * Grid (Vterm) - the paired half of every byte test above.
     *
     * See THE PAIRING RULE in the header of test/framediff_tests.re. These
     * feed the REAL LiveRegion output to the independent terminal model in
     * test/vterm.re and assert on the screen.
     *
     * SETUP NOTE. Inline rendering is relative to wherever the cursor
     * already is, so every case has to place the vterm's cursor first,
     * exactly as a shell would have left it. `atRow` does that with an
     * absolute CUP - the only absolute addressing in this file, and it is
     * the harness placing the cursor, never LiveRegion.
     * ====================================================================== */

    /* A screen with the cursor parked at column 1 of screen row `row`
     * (1-based), which is where the interactive loop starts painting. */
    let atRow = (~width: int, ~height: int, ~row: int): Vterm.t => {
      let vt = Vterm.create(~width, ~height);
      Vterm.feed(vt, "\027[" ++ string_of_int(row) ++ ";1H");
      vt;
    };

    Test.run("full-width live rows keep their LAST column on the screen", () => {
      /* Same bug class as the FrameDiff pairing: painting a row that spans
         the full width leaves the cursor in the PENDING-WRAP state, ON the
         last column, so an EL emitted AFTER the content would erase the
         cell just painted. This fails against a `content ++ ESC[0m ESC[K`
         ordering and passes with LiveRegion's clear-first form. */
      let width = 20;
      let top = "+" ++ String.make(width - 2, '-') ++ "+";
      let mid = "|" ++ String.make(width - 2, ' ') ++ "|";
      let vt = atRow(~width, ~height=6, ~row=1);
      Vterm.feed(
        vt,
        LiveRegion.patch(
          ~prev=None,
          ~staticLines=[],
          ~next=[|top, mid, top|],
          ~termHeight=6,
        ),
      );
      Test.assertEqualStr(
        Vterm.cellGlyph(vt, ~row=0, ~col=width - 1),
        "+",
        "top border's right corner survived",
      );
      Test.assertEqualStr(
        Vterm.cellGlyph(vt, ~row=1, ~col=width - 1),
        "|",
        "middle row's right border survived",
      );
      Test.assertEqualStr(Vterm.row(vt, 2), top, "bottom border is exact");
      /* And the region did not spill onto a fourth row: a full-width line
         must NOT auto-wrap, because the "\r\n" after it consumes the
         deferred wrap instead of taking it. */
      Test.assertEqualStr(Vterm.row(vt, 3), String.make(width, ' '), "no spill");
    });

    Test.run("patching frame A into frame B leaves EXACTLY B on screen", () => {
      let vt = atRow(~width=16, ~height=6, ~row=1);
      let a = [|"alpha", "bravo", "charlie"|];
      let b = [|"alpha", "BRAVO!", "charlie"|];
      Vterm.feed(
        vt,
        LiveRegion.patch(~prev=None, ~staticLines=[], ~next=a, ~termHeight=6),
      );
      Vterm.feed(
        vt,
        LiveRegion.patch(
          ~prev=Some(a),
          ~staticLines=[],
          ~next=b,
          ~termHeight=6,
        ),
      );
      Test.assertEqualStr(
        Vterm.text(vt),
        "alpha\nBRAVO!\ncharlie\n\n\n",
        "only row 1 changed and no residue of the old row is left",
      );
      Test.assertEqual(
        Vterm.cursor(vt),
        (2, 0),
        "the invariant holds on the real screen: column 1 of the last live "
        ++ "line",
      );
    });

    Test.run("a shrinking region leaves no stale tail on the grid", () => {
      let vt = atRow(~width=16, ~height=6, ~row=1);
      let a = [|"one", "two", "three"|];
      let b = [|"one", "two"|];
      Vterm.feed(
        vt,
        LiveRegion.patch(~prev=None, ~staticLines=[], ~next=a, ~termHeight=6),
      );
      Vterm.feed(
        vt,
        LiveRegion.patch(
          ~prev=Some(a),
          ~staticLines=[],
          ~next=b,
          ~termHeight=6,
        ),
      );
      Test.assertEqualStr(
        Vterm.text(vt),
        "one\ntwo\n\n\n\n",
        "the third row was erased",
      );
      Test.assertEqual(Vterm.cursor(vt), (1, 0), "cursor back on the new last row");
    });

    Test.run("static lines end up ABOVE the live region", () => {
      let vt = atRow(~width=16, ~height=6, ~row=1);
      let a = [|"live-1", "live-2"|];
      Vterm.feed(
        vt,
        LiveRegion.patch(~prev=None, ~staticLines=[], ~next=a, ~termHeight=6),
      );
      Vterm.feed(
        vt,
        LiveRegion.patch(
          ~prev=Some(a),
          ~staticLines=["committed A", "committed B"],
          ~next=[|"live-1", "live-2"|],
          ~termHeight=6,
        ),
      );
      Test.assertEqualStr(
        Vterm.text(vt),
        "committed A\ncommitted B\nlive-1\nlive-2\n\n",
        "the statics are printed where the region was and the region moved "
        ++ "down below them",
      );
      Test.assertEqual(Vterm.scrollback(vt), [], "nothing scrolled yet");
    });

    Test.run(
      "at the bottom row, commits scroll the screen into the scrollback", () => {
      /* SETUP: the screen already has a transcript line at the top, and the
         cursor is parked on the LAST screen row - which is where an app
         launched from a shell prompt actually starts. From there the "\r\n"
         LiveRegion uses to step down CANNOT move down: it SCROLLS. That
         scroll is the mechanism by which the terminal's own scrollback
         swallows what leaves the top of the screen and committed <Static>
         output climbs out of the live region. ESC[B could not do any of
         this, which is why LiveRegion never uses it.

         Each patch below commits one static line, so each patch scrolls the
         screen by exactly one row. */
      let height = 4;
      let width = 16;
      let vt = Vterm.create(~width, ~height);
      Vterm.feed(vt, "\027[1;1Hold-top");
      Vterm.feed(vt, "\027[" ++ string_of_int(height) ++ ";1H");

      let live = [|"live"|];
      Vterm.feed(
        vt,
        LiveRegion.patch(
          ~prev=None,
          ~staticLines=[],
          ~next=live,
          ~termHeight=height,
        ),
      );
      Test.assertEqualStr(
        Vterm.row(vt, height - 1),
        "live" ++ String.make(width - 4, ' '),
        "the live region sits on the bottom row",
      );
      Test.assertEqual(Vterm.scrollback(vt), [], "nothing has scrolled yet");

      let commit = (s: string) =>
        Vterm.feed(
          vt,
          LiveRegion.patch(
            ~prev=Some(live),
            ~staticLines=[s],
            ~next=live,
            ~termHeight=height,
          ),
        );
      commit("s1");
      Test.assertContains(
        Vterm.scrollbackText(vt),
        "old-top",
        "the first commit scrolled the pre-existing top line off the screen",
      );
      commit("s2");
      commit("s3");
      commit("s4");

      Test.assertEqualStr(
        Vterm.text(vt),
        "s2\ns3\ns4\nlive",
        "committed lines climb up above the live region, which stays pinned "
        ++ "to the bottom row",
      );
      Test.assertContains(
        Vterm.scrollbackText(vt),
        "s1",
        "and the earliest commit has scrolled into the scrollback, where "
        ++ "<Static> content is supposed to end up",
      );
      Test.assertTrue(
        !Test.contains(Vterm.text(vt), "s1"),
        "s1 is scrollback now, not screen",
      );
    });

    Test.run("erase() removes the region's rows from the grid", () => {
      let vt = atRow(~width=16, ~height=6, ~row=2);
      Vterm.feed(vt, "\027[1;1Hkeep this\027[2;1H");
      let a = [|"r0", "r1", "r2"|];
      Vterm.feed(
        vt,
        LiveRegion.patch(~prev=None, ~staticLines=[], ~next=a, ~termHeight=6),
      );
      Test.assertContains(Vterm.text(vt), "r2", "region painted");
      Vterm.feed(vt, LiveRegion.erase(~prevHeight=3, ~termHeight=6));
      Test.assertEqualStr(
        Vterm.text(vt),
        "keep this\n\n\n\n\n",
        "only the live region was erased - the transcript above it stays",
      );
    });

    Test.run("LiveRegion emits nothing the terminal model does not know", () => {
      let vt = atRow(~width=20, ~height=8, ~row=1);
      let a = [|"a", "b", "c"|];
      Vterm.feed(
        vt,
        LiveRegion.patch(~prev=None, ~staticLines=["s"], ~next=a, ~termHeight=8),
      );
      Vterm.feed(
        vt,
        LiveRegion.patch(
          ~prev=Some(a),
          ~staticLines=[],
          ~next=[|"a", "z", "c"|],
          ~termHeight=8,
        ),
      );
      Vterm.feed(vt, LiveRegion.erase(~prevHeight=3, ~termHeight=8));
      Test.assertEqual(
        Vterm.unknownSeqs(vt),
        [],
        "every sequence LiveRegion writes is one Vterm implements",
      );
    });
  });
