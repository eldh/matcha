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
