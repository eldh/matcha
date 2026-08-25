/*
 * Tests for B1 focus: useFocus, useFocusManager, useInput, Tab/Shift+Tab
 * cycling, autoFocus, unmount succession, and the Tab back-compat contract.
 *
 * Every app below gives its FocusItem instances distinct ~key props (belt
 * and braces per the repro_tests.re convention), even though each test
 * starts its own headless instance and so is isolated by construction.
 */
open Matcha;

/* A focusable list item: shows "*id" while focused, " id" otherwise, and -
 * while focused - forwards keys to onKeyWhileFocused via useInput(~isActive).
 * Tests that don't care about per-item keys pass a no-op callback. */
module FocusItem = {
  [@component]
  let make =
      (
        ~id: string,
        ~autoFocus: bool,
        ~onKeyWhileFocused: (Key.t, Key.modifiers) => unit,
      ) => {
    let {Hooks.isFocused} = Hooks.useFocus(~autoFocus, ~id, ());
    Hooks.useInput(~isActive=isFocused, onKeyWhileFocused);
    <Text inverted=isFocused> {(isFocused ? "*" : " ") ++ id} </Text>;
  };
};

let noop = (_key: Key.t, _mods: Key.modifiers): unit => ();

/* ============================================================================
 * 1/2. Tab cycle + wrap, Shift+Tab
 * ============================================================================ */

module CycleApp = {
  [@component]
  let make = () =>
    <VStack>
      <FocusItem key="cy-a" id="a" autoFocus=true onKeyWhileFocused=noop />
      <FocusItem key="cy-b" id="b" autoFocus=false onKeyWhileFocused=noop />
      <FocusItem key="cy-c" id="c" autoFocus=false onKeyWhileFocused=noop />
    </VStack>;
};

/* ============================================================================
 * 3. autoFocus visible in the FIRST observable frame
 * ============================================================================ */

/* Same shape as CycleApp - a separate module only so each test gets its own
 * fresh Hooks instance/component identities via a distinct type. */
module AutoFocusApp = {
  [@component]
  let make = () =>
    <VStack>
      <FocusItem key="af-a" id="a" autoFocus=true onKeyWhileFocused=noop />
      <FocusItem key="af-b" id="b" autoFocus=false onKeyWhileFocused=noop />
    </VStack>;
};

/* ============================================================================
 * 4. focus(id) from a key handler (useFocusManager)
 * ============================================================================ */

module ManagerApp = {
  [@component]
  let make = () => {
    let fm = Hooks.useFocusManager();
    Event.useKeyDown((key, _mods) =>
      switch (key) {
      | Key.Char('g') => fm.focus("b")
      | _ => ()
      }
    );
    <VStack>
      <FocusItem key="mg-a" id="a" autoFocus=true onKeyWhileFocused=noop />
      <FocusItem key="mg-b" id="b" autoFocus=false onKeyWhileFocused=noop />
      <FocusItem key="mg-c" id="c" autoFocus=false onKeyWhileFocused=noop />
    </VStack>;
  };
};

/* ============================================================================
 * 5. Unmounting the focused item -> successor at the same position
 * ============================================================================ */

module UnmountApp = {
  [@component]
  let make = () => {
    let (removed, setRemoved) = Hooks.useState(false);
    Event.useKeyDown((key, _mods) =>
      switch (key) {
      | Key.Char('x') => setRemoved(true)
      | _ => ()
      }
    );
    <VStack>
      <FocusItem key="um-a" id="a" autoFocus=true onKeyWhileFocused=noop />
      {removed
         ? Element.Empty
         : <FocusItem key="um-b" id="b" autoFocus=false onKeyWhileFocused=noop />}
      <FocusItem key="um-c" id="c" autoFocus=false onKeyWhileFocused=noop />
    </VStack>;
  };
};

/* ============================================================================
 * 6. useInput(~isActive) gating: only the focused item's handler fires
 * ============================================================================ */

module CountingItem = {
  [@component]
  let make = (~id: string, ~autoFocus: bool) => {
    let {Hooks.isFocused} = Hooks.useFocus(~autoFocus, ~id, ());
    let (count, setCount) = Hooks.useState(0);
    Hooks.useInput(~isActive=isFocused, (key, _mods) =>
      switch (key) {
      | Key.Char('x') => setCount(count + 1)
      | _ => ()
      }
    );
    <Text> {id ++ ":" ++ string_of_int(count)} </Text>;
  };
};

module GatingApp = {
  [@component]
  let make = () =>
    <VStack>
      <CountingItem key="ci-a" id="a" autoFocus=true />
      <CountingItem key="ci-b" id="b" autoFocus=false />
    </VStack>;
};

/* ============================================================================
 * 7. Back-compat: a useKeyDown-only app (no useFocus anywhere) still
 *    receives Tab - focus.order is empty, so dispatchKey's Tab branch never
 *    consumes it.
 * ============================================================================ */

let backCompatSpy = Test.spy();

module BackCompatApp = {
  [@component]
  let make = () => {
    Event.useKeyDown((key, mods) => backCompatSpy.fn((key, mods)));
    <Text> "no focusables here" </Text>;
  };
};

/* ============================================================================
 * 8. disableFocus() lets Tab through to handlers
 * ============================================================================ */

let disableFocusSpy = Test.spy();

module DisableFocusApp = {
  [@component]
  let make = () => {
    let fm = Hooks.useFocusManager();
    Event.useKeyDown((key, mods) => {
      disableFocusSpy.fn((key, mods));
      switch (key) {
      | Key.Char('d') => fm.disableFocus()
      | _ => ()
      };
    });
    <VStack>
      <FocusItem key="df-a" id="a" autoFocus=true onKeyWhileFocused=noop />
      <FocusItem key="df-b" id="b" autoFocus=false onKeyWhileFocused=noop />
    </VStack>;
  };
};

/* ============================================================================
 * 9. Tab is consumed for a focusable app - a plain useKeyDown handler must
 *    NOT see it.
 * ============================================================================ */

let focusableTabSpy = Test.spy();

module FocusableTabApp = {
  [@component]
  let make = () => {
    Event.useKeyDown((key, mods) => focusableTabSpy.fn((key, mods)));
    <VStack>
      <FocusItem key="ft-a" id="a" autoFocus=true onKeyWhileFocused=noop />
      <FocusItem key="ft-b" id="b" autoFocus=false onKeyWhileFocused=noop />
    </VStack>;
  };
};

/* Whether a spy on (Key.t, Key.modifiers) ever saw a Tab keypress. */
let sawTab = (spy: Test.spy((Key.t, Key.modifiers))): bool =>
  List.exists(((key, _mods)) => key == Key.Tab, spy.calls());

let run = () => {
  Test.group("Focus", () => {
    Test.run("Tab cycles focus forward and wraps", () => {
      let handle = Runtime.startHeadless((module CycleApp));
      Test.assertEqual(
        handle.getFocusedId(),
        Some("a"),
        "autoFocus starts on a",
      );
      Input.pressTab(handle);
      Test.assertEqual(handle.getFocusedId(), Some("b"), "Tab: a -> b");
      Input.pressTab(handle);
      Test.assertEqual(handle.getFocusedId(), Some("c"), "Tab: b -> c");
      Input.pressTab(handle);
      Test.assertEqual(
        handle.getFocusedId(),
        Some("a"),
        "Tab wraps: c -> a",
      );
      handle.quit();
    });

    Test.run(
      "Shift+Tab cycles backward, via sendKey and via feedBytes ESC[Z",
      () => {
        let handle = Runtime.startHeadless((module CycleApp));
        Test.assertEqual(
          handle.getFocusedId(),
          Some("a"),
          "autoFocus starts on a",
        );
        Input.pressShiftTab(handle);
        Test.assertEqual(
          handle.getFocusedId(),
          Some("c"),
          "Shift+Tab wraps backward: a -> c (via sendKey)",
        );
        Input.feedBytes(handle, "\027[Z");
        Test.assertEqual(
          handle.getFocusedId(),
          Some("b"),
          "Shift+Tab: c -> b (via feedBytes ESC[Z backtab)",
        );
        handle.quit();
      },
    );

    Test.run(
      "autoFocus is visible in the first observable frame", () => {
      let handle = Runtime.startHeadless((module AutoFocusApp));
      Test.assertEqual(
        handle.getFocusedId(),
        Some("a"),
        "getFocusedId reflects autoFocus without any extra render call",
      );
      Test.assertContains(
        handle.getOutput(true),
        "*a",
        "the focused marker for a is in the very first getOutput()",
      );
      handle.quit();
    });

    Test.run("focusManager.focus(id) moves focus from a key handler", () => {
      let handle = Runtime.startHeadless((module ManagerApp));
      Test.assertEqual(
        handle.getFocusedId(),
        Some("a"),
        "autoFocus starts on a",
      );
      handle.sendKey(Key.Char('g'), Key.noModifiers);
      Test.assertEqual(
        handle.getFocusedId(),
        Some("b"),
        "fm.focus(\"b\") moved focus directly to b",
      );
      handle.quit();
    });

    Test.run(
      "unmounting the focused item focuses the successor at the same position",
      () => {
        let handle = Runtime.startHeadless((module UnmountApp));
        Input.pressTab(handle);
        Test.assertEqual(
          handle.getFocusedId(),
          Some("b"),
          "focus moved to b before it is removed",
        );
        handle.sendKey(Key.Char('x'), Key.noModifiers);
        Test.assertEqual(
          handle.getFocusedId(),
          Some("c"),
          "b unmounted - c (which slid into b's old position) inherits focus",
        );
        handle.quit();
      },
    );

    Test.run(
      "useInput(~isActive) gates keys to the focused item only", () => {
      let handle = Runtime.startHeadless((module GatingApp));
      handle.sendKey(Key.Char('x'), Key.noModifiers);
      Test.assertContains(
        handle.getOutput(true),
        "a:1",
        "a is focused (autoFocus) and receives the char",
      );
      Test.assertContains(
        handle.getOutput(true),
        "b:0",
        "b is not focused and does not receive the char",
      );
      Input.pressTab(handle);
      handle.sendKey(Key.Char('x'), Key.noModifiers);
      Test.assertContains(
        handle.getOutput(true),
        "a:1",
        "a no longer receives chars once focus moved away",
      );
      Test.assertContains(
        handle.getOutput(true),
        "b:1",
        "b now receives the char, having gained focus via Tab",
      );
      handle.quit();
    });

    Test.run(
      "back-compat: a useKeyDown-only app (no useFocus) still receives Tab",
      () => {
        let handle = Runtime.startHeadless((module BackCompatApp));
        Input.pressTab(handle);
        Test.assertTrue(
          sawTab(backCompatSpy),
          "with zero focusables, Tab is never consumed - it broadcasts exactly as before B1",
        );
        handle.quit();
      },
    );

    Test.run("disableFocus() lets Tab fall through to handlers", () => {
      let handle = Runtime.startHeadless((module DisableFocusApp));
      Test.assertEqual(
        handle.getFocusedId(),
        Some("a"),
        "autoFocus starts on a",
      );
      handle.sendKey(Key.Char('d'), Key.noModifiers);
      Input.pressTab(handle);
      Test.assertTrue(
        sawTab(disableFocusSpy),
        "focus disabled - Tab is broadcast instead of consumed",
      );
      Test.assertEqual(
        handle.getFocusedId(),
        Some("a"),
        "focus did not move - Tab was never handed to focusNext",
      );
      handle.quit();
    });

    Test.run(
      "Tab is consumed for a focusable app - a plain useKeyDown spy never sees it",
      () => {
        let handle = Runtime.startHeadless((module FocusableTabApp));
        Test.assertEqual(
          handle.getFocusedId(),
          Some("a"),
          "autoFocus starts on a",
        );
        Input.pressTab(handle);
        Test.assertFalse(
          sawTab(focusableTabSpy),
          "Tab was consumed by focus cycling, not broadcast to keyHandlers",
        );
        Test.assertEqual(
          handle.getFocusedId(),
          Some("b"),
          "and it did actually move focus, confirming consumption happened",
        );
        handle.quit();
      },
    );
  });
};
