/*
 * Regression tests for hooks bugs fixed in commits 6d81cdd and a621688.
 *
 * Both bugs were structural comparison (Reason == / !=, OCaml compare)
 * applied to Obj.t values wrapping closures, which raises
 * Invalid_argument("compare: functional value"). The fixes use physical
 * comparison (=== / !==) instead:
 * - depsEqual in Hooks.re (useMemo/useEffect deps containing functions)
 * - the former propsChanged in Hooks.re (component props containing
 *   functions; the props-memoization gate has since been removed entirely,
 *   but this suite remains as a render-path regression: props records
 *   carrying closures must never be structurally compared anywhere)
 */
open Matcha;

module MemoApp = {
  [@component]
  let make = () => {
    let (count, setCount) = Hooks.useState(0);
    /* A closure in the deps array - structural compare on this crashed */
    let transformer = x => x * 2;
    let result =
      Hooks.useMemo(
        () => transformer(count),
        [|(Obj.repr(transformer): Obj.t), (Obj.repr(count): Obj.t)|],
      );
    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Arrow_down => setCount(count + 1)
      | _ => ()
      }
    );
    <VStack>
      <Text> {"Count: " ++ string_of_int(count)} </Text>
      <Text> {"Result: " ++ string_of_int(result)} </Text>
    </VStack>;
  };
};

module FnChild = {
  [@component]
  let make = (~label: string, ~onPress: unit => unit) => {
    ignore(onPress);
    <Text> label </Text>;
  };
};

module FnPropsApp = {
  [@component]
  let make = () => {
    let (count, setCount) = Hooks.useState(0);
    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Arrow_up => setCount(count + 1)
      | _ => ()
      }
    );
    <VStack>
      <Text> {"Parent: " ++ string_of_int(count)} </Text>
      /* A closure in the props record - structural comparison crashed on this */
      <FnChild
        label={"Child: " ++ string_of_int(count)}
        onPress={() => setCount(count + 1)}
      />
    </VStack>;
  };
};

/* ============================================================================
 * String dependencies
 *
 * depsEqual compares slots with === (physical) so that a closure in the array
 * can never reach OCaml's structural compare. A STRING is a freshly allocated
 * block on every render, so under a purely physical comparison a string
 * dependency never matched and the memo holding it recomputed on every single
 * frame - silently, and forever. depEqual adds exactly one case for that.
 *
 * Every counter below is incremented INSIDE the memo/effect body, so what is
 * asserted is how many times the body actually ran, not what it returned.
 * ========================================================================== */

let stableMemoRuns = ref(0);
let changingMemoRuns = ref(0);
let stableEffectRuns = ref(0);
let emptyDepsMemoRuns = ref(0);
let mixedMemoRuns = ref(0);

module StringDepsApp = {
  [@component]
  let make = () => {
    let (n, setN) = Hooks.useState(0);
    /* Built fresh every render from two pieces, so it is a DIFFERENT block
       each time with the same contents - the exact shape the physical
       comparison could not see through. */
    let stable = "a" ++ "b";
    let changing = "n=" ++ string_of_int(n);

    let stableValue =
      Hooks.useMemo(
        () => {
          stableMemoRuns := stableMemoRuns^ + 1;
          String.uppercase_ascii(stable);
        },
        [|stable|],
      );
    let changingValue =
      Hooks.useMemo(
        () => {
          changingMemoRuns := changingMemoRuns^ + 1;
          String.uppercase_ascii(changing);
        },
        [|changing|],
      );
    /* useEffect shares depsEqual, so it must behave identically. */
    Hooks.useEffect(
      () => {
        stableEffectRuns := stableEffectRuns^ + 1;
        None;
      },
      [|stable|],
    );
    ignore(
      Hooks.useMemo(
        () => {
          emptyDepsMemoRuns := emptyDepsMemoRuns^ + 1;
          0;
        },
        [||],
      ),
    );
    /* A string beside an int: the int is an immediate (=== already works),
       the string is a block. Both slots have to agree for the memo to hold. */
    ignore(
      Hooks.useMemo(
        () => {
          mixedMemoRuns := mixedMemoRuns^ + 1;
          0;
        },
        [|Obj.repr(stable), Obj.repr(n)|],
      ),
    );

    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Arrow_up => setN(n + 1)
      | _ => ()
      }
    );
    <VStack>
      <Text> stableValue </Text>
      <Text> changingValue </Text>
    </VStack>;
  };
};

/* A closure dependency: a fresh one every render, so the memo must recompute
 * every render - and, above all, depsEqual must not hand it to compare. */
let closureMemoRuns = ref(0);

module ClosureDepApp = {
  [@component]
  let make = () => {
    let (n, setN) = Hooks.useState(0);
    let fn = x => x + n;
    ignore(
      Hooks.useMemo(
        () => {
          closureMemoRuns := closureMemoRuns^ + 1;
          fn(1);
        },
        [|Obj.repr(fn)|],
      ),
    );
    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Arrow_up => setN(n + 1)
      | _ => ()
      }
    );
    <Text> {"n=" ++ string_of_int(n)} </Text>;
  };
};

let bailRenderCount = ref(0);

module BailApp = {
  [@component]
  let make = () => {
    bailRenderCount := bailRenderCount^ + 1;
    let (n, setN) = Hooks.useState(7);
    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Char('s') => setN(7) /* same immediate value - must bail out */
      | Key.Arrow_up => setN(n + 1)
      | _ => ()
      }
    );
    <Text> {"N: " ++ string_of_int(n)} </Text>;
  };
};

let run = () => {
  Test.group("Hooks Regressions", () => {
    Test.run("setState with a physically identical value does not re-render", () => {
      let handle = Runtime.startHeadless((module BailApp));
      let after = bailRenderCount^;
      /* Same-value write: bail out, no new frame */
      handle.sendKey(Key.Char('s'), Key.noModifiers);
      Test.assertEqual(
        bailRenderCount^,
        after,
        "same-value setState must not schedule a render",
      );
      /* Different value still re-renders */
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      Test.assertTrue(
        bailRenderCount^ > after,
        "changed-value setState re-renders",
      );
      Test.assertContains(handle.getOutput(true), "N: 8", "state advanced");
      handle.quit();
    });

    Test.run("useMemo with functional value in deps does not crash", () => {
      let handle = Runtime.startHeadless((module MemoApp));
      Test.assertContains(handle.getOutput(true), "Result: 0", "initial memo");
      /* Each setState re-renders and re-compares deps containing a closure */
      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      let output = handle.getOutput(true);
      Test.assertContains(output, "Count: 2", "count updated");
      Test.assertContains(output, "Result: 4", "memo recomputed");
      handle.quit();
    });

    Test.run("a string dependency holds across a re-render", () => {
      stableMemoRuns := 0;
      changingMemoRuns := 0;
      stableEffectRuns := 0;
      emptyDepsMemoRuns := 0;
      mixedMemoRuns := 0;
      let config: Runtime.headlessConfig = {width: 30, height: 6};
      let handle = Runtime.startHeadless(~config, (module StringDepsApp));

      Test.assertEqual(stableMemoRuns^, 1, "the memo ran once on mount");
      Test.assertEqual(changingMemoRuns^, 1, "so did the changing one");
      Test.assertEqual(stableEffectRuns^, 1, "and the effect");
      Test.assertContains(handle.getOutput(true), "AB", "with the right value");

      /* A frame with no state change at all. Both dependency strings are
         rebuilt, so both are new blocks with unchanged contents. */
      ignore(handle.render());
      Test.assertEqual(
        stableMemoRuns^,
        1,
        "a rebuilt string with equal contents does not invalidate the memo",
      );
      Test.assertEqual(
        changingMemoRuns^,
        1,
        "nor does rebuilding a string that happens to be unchanged",
      );
      Test.assertEqual(
        stableEffectRuns^,
        1,
        "useEffect shares depsEqual and holds too",
      );
      Test.assertEqual(mixedMemoRuns^, 1, "a string beside an int holds");

      /* Now change what the string SAYS. */
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      Test.assertEqual(
        changingMemoRuns^,
        2,
        "different contents recompute the memo",
      );
      Test.assertEqual(
        stableMemoRuns^,
        1,
        "and the untouched string still holds on that same frame",
      );
      Test.assertEqual(mixedMemoRuns^, 2, "the int slot changed, so it reran");
      Test.assertEqual(
        stableEffectRuns^,
        1,
        "the effect's dependency did not change",
      );
      Test.assertContains(handle.getOutput(true), "N=1", "state advanced");

      Test.assertEqual(
        emptyDepsMemoRuns^,
        1,
        "an empty deps array still means compute exactly once",
      );
      handle.quit();
    });

    Test.run("a closure dependency is still compared physically", () => {
      closureMemoRuns := 0;
      let handle = Runtime.startHeadless((module ClosureDepApp));
      Test.assertEqual(closureMemoRuns^, 1, "computed on mount");
      /* A freshly allocated closure every render. If depsEqual ever reached
         OCaml's structural compare, this render would raise
         Invalid_argument("compare: functional value") instead of counting. */
      ignore(handle.render());
      Test.assertEqual(
        closureMemoRuns^,
        2,
        "a new closure is a new dependency, and comparing it did not raise",
      );
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      Test.assertTrue(
        closureMemoRuns^ > 2,
        "and it keeps recomputing across state changes without raising",
      );
      handle.quit();
    });

    Test.run("component props containing a closure do not crash", () => {
      let handle = Runtime.startHeadless((module FnPropsApp));
      Test.assertContains(handle.getOutput(true), "Child: 0", "initial child");
      /* Each re-render compares the child's props record, which holds a closure */
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      let output = handle.getOutput(true);
      Test.assertContains(output, "Parent: 2", "parent updated");
      Test.assertContains(output, "Child: 2", "child re-rendered with new props");
      handle.quit();
    });
  });
};
