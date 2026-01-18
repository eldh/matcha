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
};
