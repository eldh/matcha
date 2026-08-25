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
  });
