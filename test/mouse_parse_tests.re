/*
 * Tests for Mouse.parseSgr / Mouse.encodeSgr - the pure SGR (1006) mouse
 * decode/encode core (B4/S6). InputDecoder's own framing (finding the
 * ESC[< ... M/m sequence in the byte stream) is covered separately in
 * paste_tests.re; this file only exercises the decode table itself.
 */
open Matcha;

let mkEvent =
    (~kind, ~button=Mouse.NoButton, ~x, ~y, ~shift=false, ~alt=false, ~ctrl=false, ())
    : Mouse.event => {
  kind,
  button,
  x,
  y,
  shift,
  alt,
  ctrl,
};

let run = () => {
  Test.group("Mouse.parseSgr", () => {
    Test.run("plain down, left button", () => {
      Test.assertEqual(
        Mouse.parseSgr("0;10;20M"),
        Some(mkEvent(~kind=Mouse.Down, ~button=Mouse.Left, ~x=9, ~y=19, ())),
        "Cb=0 M -> Down/Left, 1-based coords become 0-based",
      );
    });

    Test.run("plain down, middle button", () => {
      Test.assertEqual(
        Mouse.parseSgr("1;5;5M"),
        Some(mkEvent(~kind=Mouse.Down, ~button=Mouse.Middle, ~x=4, ~y=4, ())),
        "Cb=1 M -> Down/Middle",
      );
    });

    Test.run("plain down, right button", () => {
      Test.assertEqual(
        Mouse.parseSgr("2;1;1M"),
        Some(mkEvent(~kind=Mouse.Down, ~button=Mouse.Right, ~x=0, ~y=0, ())),
        "Cb=2 M -> Down/Right",
      );
    });

    Test.run("plain up, left button", () => {
      Test.assertEqual(
        Mouse.parseSgr("0;10;20m"),
        Some(mkEvent(~kind=Mouse.Up, ~button=Mouse.Left, ~x=9, ~y=19, ())),
        "Cb=0 m -> Up/Left",
      );
    });

    Test.run("plain up, middle button", () => {
      Test.assertEqual(
        Mouse.parseSgr("1;5;5m"),
        Some(mkEvent(~kind=Mouse.Up, ~button=Mouse.Middle, ~x=4, ~y=4, ())),
        "Cb=1 m -> Up/Middle",
      );
    });

    Test.run("plain up, right button", () => {
      Test.assertEqual(
        Mouse.parseSgr("2;1;1m"),
        Some(mkEvent(~kind=Mouse.Up, ~button=Mouse.Right, ~x=0, ~y=0, ())),
        "Cb=2 m -> Up/Right",
      );
    });

    Test.run("motion bit (+32) reports Move regardless of final byte", () => {
      Test.assertEqual(
        Mouse.parseSgr("32;3;4M"),
        Some(mkEvent(~kind=Mouse.Move, ~button=Mouse.Left, ~x=2, ~y=3, ())),
        "Cb=32 (0 + motion bit) -> Move/Left",
      );
      Test.assertEqual(
        Mouse.parseSgr("35;3;4M"),
        Some(mkEvent(~kind=Mouse.Move, ~button=Mouse.NoButton, ~x=2, ~y=3, ())),
        "Cb=35 (3 + motion bit) -> Move/NoButton (hover, no button held)",
      );
    });

    Test.run("wheel up / wheel down (Cb=64/65)", () => {
      Test.assertEqual(
        Mouse.parseSgr("64;1;1M"),
        Some(mkEvent(~kind=Mouse.ScrollUp, ~x=0, ~y=0, ())),
        "Cb=64 -> ScrollUp",
      );
      Test.assertEqual(
        Mouse.parseSgr("65;1;1M"),
        Some(mkEvent(~kind=Mouse.ScrollDown, ~x=0, ~y=0, ())),
        "Cb=65 -> ScrollDown",
      );
    });

    Test.run("shift+wheel (Cb=68/69) - modifier bits masked off FIRST", () => {
      Test.assertEqual(
        Mouse.parseSgr("68;1;1M"),
        Some(mkEvent(~kind=Mouse.ScrollUp, ~x=0, ~y=0, ~shift=true, ())),
        "Cb=68 (64 + shift bit) -> ScrollUp with shift=true, not misread as some other base code",
      );
      Test.assertEqual(
        Mouse.parseSgr("69;1;1M"),
        Some(mkEvent(~kind=Mouse.ScrollDown, ~x=0, ~y=0, ~shift=true, ())),
        "Cb=69 (65 + shift bit) -> ScrollDown with shift=true",
      );
    });

    Test.run("ctrl+wheel (Cb=80/81) - modifier bits masked off FIRST", () => {
      Test.assertEqual(
        Mouse.parseSgr("80;1;1M"),
        Some(mkEvent(~kind=Mouse.ScrollUp, ~x=0, ~y=0, ~ctrl=true, ())),
        "Cb=80 (64 + ctrl bit) -> ScrollUp with ctrl=true",
      );
      Test.assertEqual(
        Mouse.parseSgr("81;1;1M"),
        Some(mkEvent(~kind=Mouse.ScrollDown, ~x=0, ~y=0, ~ctrl=true, ())),
        "Cb=81 (65 + ctrl bit) -> ScrollDown with ctrl=true",
      );
    });

    Test.run("horizontal wheel (Cb=66/67) is not represented -> None", () => {
      Test.assertEqual(
        Mouse.parseSgr("66;1;1M"),
        None,
        "Cb=66 (64 + horizontal-wheel bit 2) -> None",
      );
      Test.assertEqual(
        Mouse.parseSgr("67;1;1M"),
        None,
        "Cb=67 (64 + horizontal-wheel bits 2,3) -> None",
      );
    });

    Test.run("malformed input -> None", () => {
      Test.assertEqual(Mouse.parseSgr(""), None, "empty string -> None");
      Test.assertEqual(
        Mouse.parseSgr("0;10;20"),
        None,
        "missing final M/m -> None",
      );
      Test.assertEqual(
        Mouse.parseSgr("0;10;20X"),
        None,
        "wrong final byte -> None",
      );
      Test.assertEqual(
        Mouse.parseSgr("0;10M"),
        None,
        "only two params -> None",
      );
      Test.assertEqual(
        Mouse.parseSgr("a;b;cM"),
        None,
        "non-integer params -> None",
      );
    });
  });

  Test.group("Mouse.encodeSgr round-trips through parseSgr", () => {
    let cases: list(Mouse.event) = [
      mkEvent(~kind=Mouse.Down, ~button=Mouse.Left, ~x=0, ~y=0, ()),
      mkEvent(~kind=Mouse.Down, ~button=Mouse.Middle, ~x=12, ~y=34, ()),
      mkEvent(~kind=Mouse.Down, ~button=Mouse.Right, ~x=79, ~y=23, ()),
      mkEvent(~kind=Mouse.Up, ~button=Mouse.Left, ~x=5, ~y=5, ()),
      mkEvent(~kind=Mouse.Move, ~button=Mouse.Left, ~x=1, ~y=2, ()),
      mkEvent(~kind=Mouse.Move, ~button=Mouse.NoButton, ~x=1, ~y=2, ()),
      mkEvent(~kind=Mouse.ScrollUp, ~x=3, ~y=3, ()),
      mkEvent(~kind=Mouse.ScrollDown, ~x=3, ~y=3, ()),
      mkEvent(~kind=Mouse.ScrollUp, ~x=3, ~y=3, ~shift=true, ()),
      mkEvent(~kind=Mouse.ScrollDown, ~x=3, ~y=3, ~ctrl=true, ()),
      mkEvent(
        ~kind=Mouse.Down,
        ~button=Mouse.Right,
        ~x=10,
        ~y=10,
        ~shift=true,
        ~alt=true,
        ~ctrl=true,
        (),
      ),
    ];
    List.iteri(
      (i, ev: Mouse.event) => {
        Test.run(
          "round-trip case " ++ string_of_int(i),
          () => {
            let wire = Mouse.encodeSgr(ev);
            Test.assertEqual(
              Mouse.parseSgr(wire),
              Some(ev),
              "encodeSgr(ev) |> parseSgr should reproduce ev (wire: " ++ wire ++ ")",
            );
          },
        )
      },
      cases,
    );
  });
};
