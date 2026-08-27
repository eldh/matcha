/*
 * Tests for Element utilities
 */
open Matcha;

let run = () => {
  Test.group("Element Utilities", () => {
    Test.run("stripAnsi removes escape codes", () => {
      let input = "\027[1mBold\027[0m Normal";
      let result = Element.stripAnsi(input);
      Test.assertEqualStr(result, "Bold Normal", "should strip ANSI codes");
    });

    Test.run("stripAnsi handles empty string", () => {
      let result = Element.stripAnsi("");
      Test.assertEqualStr(result, "", "empty string stays empty");
    });

    Test.run("stripAnsi preserves plain text", () => {
      let input = "Hello World";
      let result = Element.stripAnsi(input);
      Test.assertEqualStr(result, "Hello World", "plain text unchanged");
    });

    Test.run("stripAnsi handles color codes", () => {
      let input = "\027[32mGreen\027[0m";
      let result = Element.stripAnsi(input);
      Test.assertEqualStr(result, "Green", "color codes removed");
    });

    Test.run("stripAnsi handles multiple codes", () => {
      let input = "\027[1m\027[32mBold Green\027[0m";
      let result = Element.stripAnsi(input);
      Test.assertEqualStr(result, "Bold Green", "multiple codes removed");
    });

    Test.run("visibleLength ignores ANSI", () => {
      let input = "\027[1mHello\027[0m";
      let len = Element.visibleLength(input);
      Test.assertEqual(len, 5, "visible length should be 5");
    });

    Test.run("visibleLength counts plain text", () => {
      let len = Element.visibleLength("Hello");
      Test.assertEqual(len, 5, "plain text length");
    });

    Test.run("repeatString works", () => {
      let result = Element.repeatString("ab", 3);
      Test.assertEqualStr(result, "ababab", "repeated 3 times");
    });

    Test.run("repeatString zero times", () => {
      let result = Element.repeatString("x", 0);
      Test.assertEqualStr(result, "", "zero repeats is empty");
    });

    Test.run("splitLines single line", () => {
      let lines = Element.splitLines("hello");
      Test.assertEqual(List.length(lines), 1, "single line");
    });

    Test.run("splitLines multiple lines", () => {
      let lines = Element.splitLines("a\nb\nc");
      Test.assertEqual(List.length(lines), 3, "three lines");
    });
  });

  Test.group("Element: color emission (styleToAnsi)", () => {
    Test.run("named colors still emit a 256-color index", () =>
      /* Pinned because RgbFull's arrival rewrote styleToAnsi: everything
         that is NOT truecolor must come out byte-for-byte as before. */
      {
        Test.assertEqualStr(
          Element.styleToAnsi(Element.FgColor(Element.Red)),
          "\027[38;5;1m",
          "red foreground",
        );
        Test.assertEqualStr(
          Element.styleToAnsi(Element.BgColor(Element.BrightWhite)),
          "\027[48;5;15m",
          "bright white background",
        );
        Test.assertEqualStr(
          Element.styleToAnsi(Element.BgColor(Element.Rgb(0, 1, 0))),
          "\027[48;5;22m",
          "the 216-cube Rgb is unchanged too",
        );
      }
    );

    Test.run("RgbFull foreground emits 38;2;r;g;b", () =>
      Test.assertEqualStr(
        Element.styleToAnsi(Element.FgColor(Element.RgbFull(12, 200, 255))),
        "\027[38;2;12;200;255m",
        "24-bit direct color, foreground slot",
      )
    );

    Test.run("RgbFull background emits 48;2;r;g;b", () =>
      Test.assertEqualStr(
        Element.styleToAnsi(Element.BgColor(Element.RgbFull(0, 40, 8))),
        "\027[48;2;0;40;8m",
        "24-bit direct color, background slot",
      )
    );

    Test.run("channels are clamped into 0..255 at emission", () => {
      Test.assertEqualStr(
        Element.styleToAnsi(Element.FgColor(Element.RgbFull(-5, 300, 128))),
        "\027[38;2;0;255;128m",
        "below 0 clamps to 0, above 255 clamps to 255",
      );
      Test.assertEqualStr(
        Element.styleToAnsi(Element.BgColor(Element.RgbFull(999, -1, -1000))),
        "\027[48;2;255;0;0m",
        "clamping applies per channel, in the background slot too",
      );
    });

    Test.run("colorToCode down-samples RgbFull into the 216-cube", () => {
      /* Lossy fallback for callers that need a palette index; NOT the
         emission path. Pure black and pure white are the two ends. */
      Test.assertEqual(
        Element.colorToCode(Element.RgbFull(0, 0, 0)),
        16,
        "black is the first cube cell",
      );
      Test.assertEqual(
        Element.colorToCode(Element.RgbFull(255, 255, 255)),
        231,
        "white is the last cube cell",
      );
      Test.assertEqual(
        Element.colorToCode(Element.RgbFull(255, 0, 0)),
        Element.colorToCode(Element.Rgb(5, 0, 0)),
        "full red lands on the same cube cell as Rgb(5, 0, 0)",
      );
    });
  });
};
