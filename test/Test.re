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

/* Run a test that documents a known, not-yet-fixed bug.
 * It PASSES (reported as XFAIL) when f raises - the bug still exists.
 * It FAILS (reported as XPASS) when f succeeds - the bug appears fixed,
 * so convert the test to a regular Test.run to lock in the fix.
 */
let runExpectedFailure = (name: string, f: unit => unit): unit => {
  currentTest := name;
  print_string("  " ++ name ++ " ... ");
  flush(stdout);
  switch (f()) {
  | () =>
    failCount := failCount^ + 1;
    errors :=
      [
        name
        ++ ": expected failure, but the test passed. The bug appears fixed - convert this to Test.run.",
        ...errors^,
      ];
    print_endline(red ++ "XPASS (convert to Test.run)" ++ reset);
  | exception _ =>
    passCount := passCount^ + 1;
    print_endline(yellow ++ "XFAIL (known bug)" ++ reset);
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

/* ============================================================================
 * Mocking utilities
 * ============================================================================ */

/* Create a mock function that records calls and returns preset values.
 *
 * Usage:
 *   let mock = Test.Mock.fn([|"first call", "second call"|]);
 *   let result = mock.call();  // returns "first call"
 *   let result2 = mock.call(); // returns "second call"
 *   Test.assertEqual(mock.callCount(), 2, "called twice");
 */
module Mock = {
  type t('a, 'b) = {
    call: 'a => 'b,
    callCount: unit => int,
    calls: unit => list('a),
    reset: unit => unit,
  };

  /* Create a mock that returns values from an array in order */
  let fn = (returns: array('b)): t(unit, 'b) => {
    let callIdx = ref(0);
    let callList: ref(list(unit)) = ref([]);

    {
      call: () => {
        callList := [(), ...callList^];
        let idx = callIdx^;
        callIdx := idx + 1;
        if (idx < Array.length(returns)) {
          returns[idx];
        } else {
          returns[Array.length(returns) - 1]; /* Return last value if exhausted */
        };
      },
      callCount: () => List.length(callList^),
      calls: () => List.rev(callList^),
      reset: () => {
        callIdx := 0;
        callList := [];
      },
    };
  };

  /* Create a mock that takes an argument and returns values from array */
  let fnWithArg = (returns: array('b)): t('a, 'b) => {
    let callIdx = ref(0);
    let callList: ref(list('a)) = ref([]);

    {
      call: arg => {
        callList := [arg, ...callList^];
        let idx = callIdx^;
        callIdx := idx + 1;
        if (idx < Array.length(returns)) {
          returns[idx];
        } else {
          returns[Array.length(returns) - 1];
        };
      },
      callCount: () => List.length(callList^),
      calls: () => List.rev(callList^),
      reset: () => {
        callIdx := 0;
        callList := [];
      },
    };
  };

  /* Create a mock that always returns the same value */
  let const = (value: 'b): t(unit, 'b) => {
    let callList: ref(list(unit)) = ref([]);

    {
      call: () => {
        callList := [(), ...callList^];
        value;
      },
      callCount: () => List.length(callList^),
      calls: () => List.rev(callList^),
      reset: () => callList := [],
    };
  };

  /* Create a mock that calls a custom function */
  let custom = (f: 'a => 'b): t('a, 'b) => {
    let callList: ref(list('a)) = ref([]);

    {
      call: arg => {
        callList := [arg, ...callList^];
        f(arg);
      },
      callCount: () => List.length(callList^),
      calls: () => List.rev(callList^),
      reset: () => callList := [],
    };
  };
};

/* Fake timers for testing time-dependent code.
 *
 * Usage:
 *   let time = Test.FakeTime.create(1000.0);  // Start at 1000ms
 *   time.advance(500.0);                       // Now at 1500ms
 *   Test.assertEqual(time.now(), 1500.0, "time advanced");
 */
module FakeTime = {
  type t = {
    now: unit => float,
    advance: float => unit,
    set: float => unit,
  };

  let create = (initial: float): t => {
    let current = ref(initial);

    {
      now: () => current^,
      advance: delta => current := current^ +. delta,
      set: value => current := value,
    };
  };
};

/* Simple spy to track if/how a function was called.
 *
 * Usage:
 *   let spy = Test.spy();
 *   someFunction(~callback=spy.fn);
 *   Test.assertTrue(spy.wasCalled(), "callback was invoked");
 */
type spy('a) = {
  fn: 'a => unit,
  wasCalled: unit => bool,
  callCount: unit => int,
  lastCall: unit => option('a),
  calls: unit => list('a),
  reset: unit => unit,
};

let spy = (): spy('a) => {
  let callList: ref(list('a)) = ref([]);

  {
    fn: arg => callList := [arg, ...callList^],
    wasCalled: () => List.length(callList^) > 0,
    callCount: () => List.length(callList^),
    lastCall: () =>
      switch (callList^) {
      | [x, ..._] => Some(x)
      | [] => None
      },
    calls: () => List.rev(callList^),
    reset: () => callList := [],
  };
};
