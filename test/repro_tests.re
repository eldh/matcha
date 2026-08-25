/*
 * Core Refactor Repros - executable specifications for known core bugs
 *
 * XFAIL CONVENTION
 * ----------------
 * Every test in this file asserts the CORRECT (desired) behavior of the core,
 * not the behavior it has today. Because the bug still exists, the assertion
 * raises, so the test is wrapped in Test.runExpectedFailure:
 *
 *   XFAIL (known bug)      -> the body raised: the bug is still there (suite green)
 *   XPASS (convert to ...) -> the body passed: the bug is FIXED (suite red, on purpose)
 *
 * So the suite stays green while the bugs are open, and it goes loud the moment
 * a refactor step fixes one. When a test starts reporting XPASS, change its
 * Test.runExpectedFailure to Test.run - that locks the fixed behavior in as a
 * regular regression test. Do NOT weaken the assertion instead.
 *
 * STATUS: all six original repros have been fixed by the core refactor
 * (type-ID identity, tree-path identity, effect commit phase, per-instance
 * runtime state) and converted to Test.run - they now serve as permanent
 * regression guards for those fixes.
 *
 * NOTE ON SHARED STATE
 * --------------------
 * Runtime state now lives in a per-instance record (Hooks.instanceState);
 * each Runtime.startHeadless call gets a fresh instance, so tests are
 * isolated by construction. The unique ~key values below predate that fix
 * and are kept as belt-and-braces.
 */

open Matcha;

/* ============================================================================
 * t1 - effects must run once per rendered frame, also inside an HStack
 * ============================================================================ */

let t1EffectRuns = ref(0);

module T1Child = {
  [@component]
  let make = () => {
    Hooks.useEffectAlways(() => {
      t1EffectRuns := t1EffectRuns^ + 1;
      None;
    });
    <Text> "t1-child" </Text>;
  };
};

module T1App = {
  [@component]
  let make = () =>
    /* The child is wrapped in Sized so the Auto-measurement path in
       calculateChildSizes does not add a third render of its own; this keeps
       the repro pointed at the HStack measure-pass + real-pass double render. */
    <HStack>
      <Sized size={Element.Chars(12)}> <T1Child key="t1-child" /> </Sized>
      <Text> "t1-sibling" </Text>
    </HStack>;
};

/* ============================================================================
 * t2 - hook state must survive a sibling being inserted before a component
 * ============================================================================ */

module T2Extra = {
  [@component]
  let make = () => <Text> "t2-extra" </Text>;
};

module T2Counter = {
  [@component]
  let make = () => {
    let (count, setCount) = Hooks.useState(0);

    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Arrow_up => setCount(count + 1)
      | _ => ()
      }
    );

    <Text> {"T2Counter: " ++ string_of_int(count)} </Text>;
  };
};

module T2App = {
  [@component]
  let make = () => {
    /* Root context state is positionally safe, so the toggle lives here. */
    let (showExtra, setShowExtra) = Hooks.useState(false);

    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Char('t') => setShowExtra(!showExtra)
      | _ => ()
      }
    );

    <VStack>
      {showExtra
         ? <Sized size={Element.Chars(1)}>
             <T2Extra key="t2-extra" />
           </Sized>
         : Element.Empty}
      <Sized size={Element.Chars(1)}> <T2Counter key="t2-counter" /> </Sized>
    </VStack>;
  };
};

/* ============================================================================
 * t3 - two different component types at the same position are not the same
 *      instance and must not share hook slots
 * ============================================================================ */

module T3A = {
  [@component]
  let make = () => {
    let (n, setN) = Hooks.useState(0);

    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Arrow_up => setN(n + 1)
      | _ => ()
      }
    );

    <Text> {"A:" ++ string_of_int(n)} </Text>;
  };
};

module T3B = {
  [@component]
  let make = () => {
    let (n, _setN) = Hooks.useState(100);
    <Text> {"B:" ++ string_of_int(n)} </Text>;
  };
};

module T3App = {
  [@component]
  let make = () => {
    let (showB, setShowB) = Hooks.useState(false);

    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Char('t') => setShowB(!showB)
      | _ => ()
      }
    );

    /* Same tree position, same key, different component type. */
    showB ? <T3B key="t3-slot" /> : <T3A key="t3-slot" />;
  };
};

/* ============================================================================
 * t4 - key handlers must be dispatched in tree order
 * ============================================================================ */

let t4Dispatched: ref(list(string)) = ref([]);

module T4Item = {
  [@component]
  let make = (~tag: string) => {
    Event.useKeyDown((_key, _mods) =>
      t4Dispatched := [tag, ...t4Dispatched^]
    );
    <Text> {"t4-" ++ tag} </Text>;
  };
};

module T4App = {
  [@component]
  let make = () =>
    <VStack>
      <Sized size={Element.Chars(1)}> <T4Item key="t4-a" tag="a" /> </Sized>
      <Sized size={Element.Chars(1)}> <T4Item key="t4-b" tag="b" /> </Sized>
      <Sized size={Element.Chars(1)}> <T4Item key="t4-c" tag="c" /> </Sized>
      <Sized size={Element.Chars(1)}> <T4Item key="t4-d" tag="d" /> </Sized>
      <Sized size={Element.Chars(1)}> <T4Item key="t4-e" tag="e" /> </Sized>
    </VStack>;
};

/* ============================================================================
 * t5a - two startHeadless instances must not share component state
 * ============================================================================ */

module T5Counter = {
  [@component]
  let make = () => {
    let (n, setN) = Hooks.useState(0);

    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Arrow_up => setN(n + 1)
      | _ => ()
      }
    );

    <Text> {"T5Counter: " ++ string_of_int(n)} </Text>;
  };
};

module T5App = {
  [@component]
  let make = () => <T5Counter key="t5-counter" />;
};

/* ============================================================================
 * t5b - quit() must run effect cleanups
 * ============================================================================ */

let t5CleanupRan = ref(false);

module T5bChild = {
  [@component]
  let make = () => {
    Hooks.useEffect(
      () => Some(() => t5CleanupRan := true),
      [||]: array(unit),
    );
    <Text> "t5b-child" </Text>;
  };
};

module T5bApp = {
  [@component]
  let make = () => <T5bChild key="t5b-child" />;
};

/* ============================================================================
 * d1 - a component received as a PROP and rendered by hand with
 *      Element.render must still get its own hooks context
 *
 * This mirrors examples/people-list/SplitView.re, which takes ~left: Element.t
 * and calls Element.render(left) inside its own body. Element.render used to
 * call the child's render function raw, with the PARENT's context current, so
 * the child's hooks were appended to the parent's hook array.
 * ============================================================================ */

let d1EffectRuns = ref(0);

module D1Child = {
  [@component]
  let make = () => {
    let (count, setCount) = Hooks.useState(0);

    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Arrow_up => setCount(count + 1)
      | _ => ()
      }
    );

    Hooks.useEffectAlways(() => {
      d1EffectRuns := d1EffectRuns^ + 1;
      None;
    });

    <Text> {"D1Child: " ++ string_of_int(count)} </Text>;
  };
};

/* SplitView-like: renders an element it got as a prop, by hand. */
module D1Split = {
  [@component]
  let make = (~left: Element.t, ~showLeft: bool) => {
    let (ticks, setTicks) = Hooks.useState(0);

    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Char('t') => setTicks(ticks + 1)
      | _ => ()
      }
    );

    let leftContent =
      if (showLeft) {
        Element.render(left);
      } else {
        "(hidden)";
      };

    /* Declared AFTER the detached render on purpose. If the child's hooks are
       appended to THIS context, this slot is the child's as soon as the child
       stops rendering, and `mark` reads the child's counter instead of 7. */
    let (mark, setMark) = Hooks.useState(7);

    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Char('m') => setMark(mark + 1)
      | _ => ()
      }
    );

    <Text>
      {"[ticks:"
       ++ string_of_int(ticks)
       ++ " mark:"
       ++ string_of_int(mark)
       ++ "]\n"
       ++ leftContent}
    </Text>;
  };
};

module D1App = {
  [@component]
  let make = () => {
    let (showLeft, setShowLeft) = Hooks.useState(true);

    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Char('h') => setShowLeft(!showLeft)
      | _ => ()
      }
    );

    <D1Split left={<D1Child key="d1-child" />} showLeft />;
  };
};

/* ============================================================================
 * d2 - an Auto (un-Sized) component child of a stack must keep its own state
 *
 * calculateChildSizes measures every Auto child. Measurement used to go
 * through Element.render, so the child's body ran with the enclosing
 * component's context current and its hooks landed in that component's hook
 * array, after the parent's own - so the child's effect was scheduled on BOTH
 * contexts and committed twice per frame.
 * ============================================================================ */

let d2EffectRuns = ref(0);

module D2Child = {
  [@component]
  let make = () => {
    let (n, setN) = Hooks.useState(0);

    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Arrow_up => setN(n + 1)
      | _ => ()
      }
    );

    Hooks.useEffectAlways(() => {
      d2EffectRuns := d2EffectRuns^ + 1;
      None;
    });

    <Text> {"D2:" ++ string_of_int(n)} </Text>;
  };
};

module D2App = {
  [@component]
  let make = () => {
    /* The parent has a hook of its own, so a child hook leaking into this
       context would either shift or corrupt this slot. */
    let (label, setLabel) = Hooks.useState("x");

    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Char('t') => setLabel(label ++ "x")
      | _ => ()
      }
    );

    /* Deliberately NOT wrapped in Sized: the child is Auto, so the HStack
       measures it before rendering it. */
    <HStack>
      <D2Child key="d2-child" />
      <Text> {"|" ++ label} </Text>
    </HStack>;
  };
};

/* ============================================================================
 * The suite
 * ============================================================================ */

let run = () => {
  Test.group("Core Refactor Repros", () => {
    /* t1: the HStack measure pass renders each child once to learn its natural
       height, then renders it again for real. Both passes go through the
       Component branch of Runtime.renderElement, and the constraint change
       (availHeight 0 -> real height) defeats the memo check, so the component
       body - and Hooks.runEffects with it - runs twice for a single frame.
       Observed today: 2 effect runs for one frame. */
    /* FIXED: effects are queued during render and committed once per frame, so a component rendered twice by a layout pass still runs its effects once. */
    Test.run("effect runs once per frame inside HStack", () => {
      t1EffectRuns := 0;
      let handle = Runtime.startHeadless((module T1App));
      let runs = t1EffectRuns^;
      handle.quit();
      Test.assertEqual(
        runs,
        1,
        "useEffectAlways should run once per frame, not once per layout pass (got "
        ++ string_of_int(runs)
        ++ ")",
      );
    });

    /* t2: component identity is the flat render-order position, so inserting a
       sibling before the counter shifts it to a new position, which maps to a
       new stable id, a new hook context and therefore a reset count.
       Observed today: the counter falls back to "T2Counter: 0". */
    /* FIXED: component identity is now the tree path, so a sibling appearing before a component no longer shifts its identity. */
    Test.run("hook state survives conditional sibling toggle", () => {
      let handle = Runtime.startHeadless((module T2App));
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      Test.assertContains(
        handle.getOutput(true),
        "T2Counter: 2",
        "counter should reach 2 before the toggle",
      );
      handle.sendKey(Key.Char('t'), Key.noModifiers);
      let output = handle.getOutput(true);
      handle.quit();
      Test.assertContains(
        output,
        "T2Counter: 2",
        "counter state should survive a sibling appearing before it",
      );
    });

    /* t3: identity compares Obj.magic(renderFn) : nativeint, which reads the
       closure info word, not the code pointer. Every zero-argument component
       thunk has the same arity/env layout, so the words are equal and B adopts
       A's hook slots at the same position+key.
       Observed today: B renders "B:2" - A's count, not B's initial 100. */
    /* FIXED: component identity now uses a ppx-emitted stable type ID instead of the closure pointer. */
    Test.run(
      "distinct component types at same position don't share state",
      () => {
        let handle = Runtime.startHeadless((module T3App));
        handle.sendKey(Key.Arrow_up, Key.noModifiers);
        handle.sendKey(Key.Arrow_up, Key.noModifiers);
        Test.assertContains(
          handle.getOutput(true),
          "A:2",
          "A should reach 2 before the switch",
        );
        handle.sendKey(Key.Char('t'), Key.noModifiers);
        let output = handle.getOutput(true);
        handle.quit();
        Test.assertContains(
          output,
          "B:100",
          "B is a different component and must start from its own initial state",
        );
      },
    );

    /* t4: Hooks.collectKeyHandlers walks Hashtbl.iter over componentContexts,
       so handlers reach the root context in hash-bucket order rather than tree
       order. Observed today with five children: c,b,d,a,e. */
    /* FIXED: collectKeyHandlers now walks components in traversal order. */
    Test.run("key handlers dispatch in tree order", () => {
      t4Dispatched := [];
      let handle = Runtime.startHeadless((module T4App));
      handle.sendKey(Key.Char('x'), Key.noModifiers);
      let order = List.rev(t4Dispatched^);
      handle.quit();
      Test.assertEqual(
        order,
        ["a", "b", "c", "d", "e"],
        "handlers should fire in tree order, got: "
        ++ String.concat(",", order),
      );
    });

    /* t5a: nothing tears the registries down between runs, and the child's
       stable id is derived from (position, key, renderFn word), which is
       identical for the second instance - so it inherits the first instance's
       hook context. Observed today: the second instance opens on
       "T5Counter: 3". */
    /* FIXED: all runtime state (contexts, props, ID registry, ID counter) now lives in a per-instance record that every startHeadless call replaces with a fresh one. */
    Test.run("startHeadless instances are isolated", () => {
      let first = Runtime.startHeadless((module T5App));
      first.sendKey(Key.Arrow_up, Key.noModifiers);
      first.sendKey(Key.Arrow_up, Key.noModifiers);
      first.sendKey(Key.Arrow_up, Key.noModifiers);
      Test.assertContains(
        first.getOutput(true),
        "T5Counter: 3",
        "first instance should reach 3",
      );
      first.quit();

      let second = Runtime.startHeadless((module T5App));
      let output = second.getOutput(true);
      second.quit();
      Test.assertContains(
        output,
        "T5Counter: 0",
        "a fresh headless instance must start from initial state",
      );
    });

    /* t5b: startHeadless's quit only flips `running`; it never calls
       Hooks.runCleanups / cleanupUnmountedComponents, so effect cleanups of a
       still-mounted component never run. Observed today: the cleanup flag is
       still false after quit(). */
    /* FIXED: quit() now unmounts the tree and runs every effect cleanup once, for its own instance. */
    Test.run("quit() runs effect cleanups", () => {
      t5CleanupRan := false;
      let handle = Runtime.startHeadless((module T5bApp));
      Test.assertFalse(
        t5CleanupRan^,
        "cleanup must not run while the component is mounted",
      );
      handle.quit();
      Test.assertTrue(
        t5CleanupRan^,
        "quit() should unmount the tree and run effect cleanups",
      );
    });
  });

  Test.group("Detached component rendering", () => {
    /* d1a: the detached child owns its state. Before the fix its two hook
       slots were appended to D1Split's hook array, so the parent's own slot
       and the child's interleaved and neither survived a frame intact. */
    Test.run("component rendered via Element.render keeps its own state", () => {
      d1EffectRuns := 0;
      let handle = Runtime.startHeadless((module D1App));
      Test.assertContains(
        handle.getOutput(true),
        "D1Child: 0",
        "detached child renders its initial state",
      );
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      let output = handle.getOutput(true);
      handle.quit();
      Test.assertContains(
        output,
        "D1Child: 2",
        "detached child state increments and survives across frames",
      );
    });

    /* d1b: the owner's own hooks are unaffected, and a re-render driven by the
       owner does not reset the detached child. */
    Test.run("owner of a detached child keeps its own hooks working", () => {
      d1EffectRuns := 0;
      let handle = Runtime.startHeadless((module D1App));
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      handle.sendKey(Key.Char('t'), Key.noModifiers);
      handle.sendKey(Key.Char('m'), Key.noModifiers);
      let output = handle.getOutput(true);
      handle.quit();
      Test.assertContains(
        output,
        "[ticks:1 mark:8]",
        "the owner's own hook slots, before and after the detached render, both work",
      );
      Test.assertContains(
        output,
        "D1Child: 1",
        "the detached child is not reset by the owner re-rendering",
      );
    });

    /* d1b2: the owner's hook slot AFTER the Element.render call must stay its
       own when the child stops being rendered. With the child's hooks living
       in the owner's array, dropping the child shifts that slot onto the
       child's counter and `mark` reads 2 instead of 7. */
    Test.run("owner's hooks don't shift when detached child disappears", () => {
      d1EffectRuns := 0;
      let handle = Runtime.startHeadless((module D1App));
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      Test.assertContains(
        handle.getOutput(true),
        "mark:7",
        "mark is untouched while the child renders",
      );
      handle.sendKey(Key.Char('h'), Key.noModifiers);
      let output = handle.getOutput(true);
      handle.quit();
      Test.assertContains(
        output,
        "(hidden)",
        "the child is no longer rendered",
      );
      Test.assertContains(
        output,
        "mark:7",
        "the owner's own slot must not be taken over by the child's hooks",
      );
    });

    /* d1c: the detached child is rendered exactly once per frame, so its
       useEffectAlways commits once per frame. Frames here: the initial
       doRender, plus one doRender per state-changing sendKey (2 arrow keys +
       1 't'), i.e. 4. */
    Test.run("detached child effects run once per rendered frame", () => {
      d1EffectRuns := 0;
      let handle = Runtime.startHeadless((module D1App));
      Test.assertEqual(
        d1EffectRuns^,
        1,
        "initial frame should commit the effect once (got "
        ++ string_of_int(d1EffectRuns^)
        ++ ")",
      );
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      handle.sendKey(Key.Char('t'), Key.noModifiers);
      let runs = d1EffectRuns^;
      handle.quit();
      Test.assertEqual(
        runs,
        4,
        "one commit per rendered frame: initial + 3 state-changing keys (got "
        ++ string_of_int(runs)
        ++ ")",
      );
    });

    /* d1d: detached identity is derived from the owning component's stable ID,
       which lives in the per-instance registry - so two headless instances of
       the same app must not share the detached child's context. */
    Test.run("detached children of two headless instances are isolated", () => {
      d1EffectRuns := 0;
      let first = Runtime.startHeadless((module D1App));
      first.sendKey(Key.Arrow_up, Key.noModifiers);
      first.sendKey(Key.Arrow_up, Key.noModifiers);
      first.sendKey(Key.Arrow_up, Key.noModifiers);
      Test.assertContains(
        first.getOutput(true),
        "D1Child: 3",
        "first instance should reach 3",
      );
      first.quit();

      let second = Runtime.startHeadless((module D1App));
      let output = second.getOutput(true);
      second.quit();
      Test.assertContains(
        output,
        "D1Child: 0",
        "a fresh instance must start its detached child from initial state",
      );
    });

    /* d2: an Auto component child of an HStack gets measured before it is
       rendered. Measurement now goes through the runtime in measuring mode,
       so the measure visit uses the child's OWN context (the same one the
       real visit uses) rather than the parent's. */
    Test.run("Auto component child of an HStack keeps working state", () => {
      d2EffectRuns := 0;
      let handle = Runtime.startHeadless((module D2App));
      Test.assertContains(
        handle.getOutput(true),
        "D2:0",
        "Auto child renders its initial state",
      );
      /* The measure visit and the real visit share one context, so the body's
         effect is scheduled on one context and commits once. When measurement
         ran the body against the PARENT's context the effect was scheduled on
         both and ran twice per frame. */
      Test.assertEqual(
        d2EffectRuns^,
        1,
        "measured Auto child commits its effect once per frame (got "
        ++ string_of_int(d2EffectRuns^)
        ++ ")",
      );
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      Test.assertContains(
        handle.getOutput(true),
        "D2:2",
        "Auto child state increments across frames",
      );
      handle.sendKey(Key.Char('t'), Key.noModifiers);
      let output = handle.getOutput(true);
      handle.quit();
      Test.assertContains(
        output,
        "|xx",
        "the parent's own hook slot is not shifted by the child's hooks",
      );
      Test.assertContains(
        output,
        "D2:2",
        "Auto child state survives a parent-driven re-render",
      );
    });
  });
};
