/*
 * Minimal test framework for Matcha
 *
 * Usage:
 *   Test.run("test name", () => {
 *     Test.assertEqual(1 + 1, 2, "basic math");
 *     Test.assertTrue(true, "truth");
 *   });
 *
 *   Test.finish(); // Call at end to print summary and exit
 */

let passCount = ref(0);
let failCount = ref(0);
let currentTest = ref("");
let errors: ref(list(string)) = ref([]);

/* ANSI colors */
let green = "\027[32m";
let red = "\027[31m";
let yellow = "\027[33m";
let reset = "\027[0m";
let bold = "\027[1m";

/* Run a test */
let run = (name: string, f: unit => unit): unit => {
  currentTest := name;
  print_string("  " ++ name ++ " ... ");
  flush(stdout);
  try({
    f();
    passCount := passCount^ + 1;
    print_endline(green ++ "PASS" ++ reset);
  }) {
  | e =>
    failCount := failCount^ + 1;
    let msg = Printexc.to_string(e);
    errors := [name ++ ": " ++ msg, ...errors^];
    print_endline(red ++ "FAIL" ++ reset);
    print_endline("    " ++ red ++ msg ++ reset);
  };
};

/* Assertions */
exception AssertionFailed(string);

let assertEqual = (actual: 'a, expected: 'a, msg: string): unit =>
  if (actual != expected) {
    raise(AssertionFailed(msg ++ " (values not equal)"));
  };

let assertEqualStr = (actual: string, expected: string, msg: string): unit =>
  if (actual != expected) {
    raise(
      AssertionFailed(
        msg
        ++ "\n      expected: \""
        ++ expected
        ++ "\"\n      actual:   \""
        ++ actual
        ++ "\"",
      ),
    );
  };

let assertTrue = (cond: bool, msg: string): unit =>
  if (!cond) {
    raise(AssertionFailed(msg));
  };

let assertFalse = (cond: bool, msg: string): unit =>
  if (cond) {
    raise(AssertionFailed(msg ++ " (expected false)"));
  };

/* Check if haystack contains needle */
let contains = (haystack: string, needle: string): bool => {
  let hlen = String.length(haystack);
  let nlen = String.length(needle);
  if (nlen == 0) {
    true;
  } else if (nlen > hlen) {
    false;
  } else {
    let rec check = i =>
      if (i > hlen - nlen) {
        false;
      } else if (String.sub(haystack, i, nlen) == needle) {
        true;
      } else {
        check(i + 1);
      };
    check(0);
  };
};

let assertContains = (haystack: string, needle: string, msg: string): unit =>
  if (!contains(haystack, needle)) {
    raise(
      AssertionFailed(
        msg
        ++ "\n      string: \""
        ++ haystack
        ++ "\"\n      missing: \""
        ++ needle
        ++ "\"",
      ),
    );
  };

/* Print summary and exit */
let finish = (): unit => {
  print_newline();
  let total = passCount^ + failCount^;

  if (failCount^ == 0) {
    print_endline(
      green
      ++ bold
      ++ "All "
      ++ string_of_int(total)
      ++ " tests passed!"
      ++ reset,
    );
    exit(0);
  } else {
    print_endline(
      red
      ++ bold
      ++ string_of_int(failCount^)
      ++ " of "
      ++ string_of_int(total)
      ++ " tests failed"
      ++ reset,
    );
    print_newline();
    print_endline(red ++ "Failures:" ++ reset);
    List.iter(err => print_endline("  - " ++ err), List.rev(errors^));
    exit(1);
  };
};

/* Group tests under a header */
let group = (name: string, f: unit => unit): unit => {
  print_endline(bold ++ name ++ reset);
  f();
  print_newline();
};
