/*
 * Tests for A3 timers: useRef, useInterval, useTimeout, and the headless
 * fake clock (advanceTime).
 */
open Matcha;

/* ============================================================================
 * useInterval
 * ============================================================================ */

module IntervalApp = {
  [@component]
  let make = () => {
    let (count, setCount) = Hooks.useState(0);
    Hooks.useInterval(() => setCount(count + 1), ~ms=100);
    <Text> {"Count: " ++ string_of_int(count)} </Text>;
  };
};

/* ============================================================================
 * useTimeout
 * ============================================================================ */

module TimeoutApp = {
  [@component]
  let make = () => {
    let (fired, setFired) = Hooks.useState(0);
    Hooks.useTimeout(() => setFired(fired + 1), ~ms=100);
    <Text> {"Fired: " ++ string_of_int(fired)} </Text>;
  };
};

/* ============================================================================
 * ms=0 disables the timer
 * ============================================================================ */

module DisabledApp = {
  [@component]
  let make = () => {
    let (count, setCount) = Hooks.useState(0);
    Hooks.useInterval(() => setCount(count + 1), ~ms=0);
    <Text> {"Count: " ++ string_of_int(count)} </Text>;
  };
};

/* ============================================================================
 * Unmounting a conditional child cancels its interval
 * ============================================================================ */

module TickingChild = {
  [@component]
  let make = (~onTick: unit => unit) => {
    Hooks.useInterval(() => onTick(), ~ms=100);
    <Text> "ticking" </Text>;
  };
};

module ToggleApp = {
  [@component]
  let make = () => {
    let (show, setShow) = Hooks.useState(true);
    let (ticks, setTicks) = Hooks.useState(0);
    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Char('t') => setShow(!show)
      | _ => ()
      }
    );
    <VStack>
      {show
         ? <TickingChild key="child" onTick={() => setTicks(ticks + 1)} />
         : Element.Empty}
      <Text> {"Ticks: " ++ string_of_int(ticks)} </Text>
    </VStack>;
  };
};

/* ============================================================================
 * Changing ms resets cadence
 * ============================================================================ */

module MsSwitchApp = {
  [@component]
  let make = () => {
    let (fast, setFast) = Hooks.useState(false);
    let (count, setCount) = Hooks.useState(0);
    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Char('s') => setFast(true)
      | _ => ()
      }
    );
    Hooks.useInterval(() => setCount(count + 1), ~ms=fast ? 50 : 100);
    <Text> {"Count: " ++ string_of_int(count)} </Text>;
  };
};

/* ============================================================================
 * useRef identity
 * ============================================================================ */

module RefApp = {
  [@component]
  let make = () => {
    let (renders, setRenders) = Hooks.useState(0);
    let r = Hooks.useRef(0);
    r := r^ + 1;
    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Char('r') => setRenders(renders + 1)
      | _ => ()
      }
    );
    <Text>
      {"Renders: "
       ++ string_of_int(renders)
       ++ " RefVal: "
       ++ string_of_int(r^)}
    </Text>;
  };
};

let run = () => {
  Test.group("Timers", () => {
    Test.run("useInterval: 250ms advance on a 100ms interval fires twice", () => {
      let handle = Runtime.startHeadless((module IntervalApp));
      Test.assertContains(
        handle.getOutput(true),
        "Count: 0",
        "no ticks before advancing",
      );
      handle.advanceTime(250);
      Test.assertContains(
        handle.getOutput(true),
        "Count: 2",
        "fires at 100ms and 200ms - 2 ticks",
      );
      handle.quit();
    });

    Test.run(
      "useInterval: 1000ms advance on a 100ms interval fires 10 times (not coalesced)",
      () => {
        let handle = Runtime.startHeadless((module IntervalApp));
        handle.advanceTime(1000);
        Test.assertContains(
          handle.getOutput(true),
          "Count: 10",
          "deadline-stepping under advanceTime fires every tick",
        );
        handle.quit();
      },
    );

    Test.run("useTimeout fires exactly once", () => {
      let handle = Runtime.startHeadless((module TimeoutApp));
      handle.advanceTime(200); /* past twice its ms=100 */
      Test.assertContains(
        handle.getOutput(true),
        "Fired: 1",
        "timeout fires once",
      );
      /* Advancing further must not fire it again - it was a one-shot */
      handle.advanceTime(500);
      Test.assertContains(
        handle.getOutput(true),
        "Fired: 1",
        "timeout does not repeat",
      );
      handle.quit();
    });

    Test.run("ms=0 never fires", () => {
      let handle = Runtime.startHeadless((module DisabledApp));
      handle.advanceTime(10_000);
      Test.assertContains(
        handle.getOutput(true),
        "Count: 0",
        "a disabled interval never fires, however far time advances",
      );
      handle.quit();
    });

    Test.run("unmounting a conditional child cancels its interval", () => {
      let handle = Runtime.startHeadless((module ToggleApp));
      handle.advanceTime(250);
      Test.assertContains(
        handle.getOutput(true),
        "Ticks: 2",
        "child interval ticks while mounted",
      );
      /* Unmount the child */
      handle.sendKey(Key.Char('t'), Key.noModifiers);
      handle.advanceTime(1000);
      Test.assertContains(
        handle.getOutput(true),
        "Ticks: 2",
        "no further ticks after the child unmounts",
      );
      handle.quit();
    });

    Test.run("changing ms via key resets cadence", () => {
      let handle = Runtime.startHeadless((module MsSwitchApp));
      handle.advanceTime(80); /* under the initial 100ms interval */
      Test.assertContains(
        handle.getOutput(true),
        "Count: 0",
        "no tick yet at 80ms into a 100ms interval",
      );
      /* Switch to a 50ms interval - cadence should reset (deps=[|ms|]
         cancels the old timer and reschedules 50ms from now, not from the
         original mount time). */
      handle.sendKey(Key.Char('s'), Key.noModifiers);
      handle.advanceTime(40); /* under the new 50ms interval */
      Test.assertContains(
        handle.getOutput(true),
        "Count: 0",
        "still no tick 40ms after the reset",
      );
      handle.advanceTime(20); /* now past the new 50ms interval */
      Test.assertContains(
        handle.getOutput(true),
        "Count: 1",
        "ticks on the new cadence",
      );
      handle.quit();
    });

    Test.run("useRef identity is stable across renders", () => {
      let handle = Runtime.startHeadless((module RefApp));
      Test.assertContains(
        handle.getOutput(true),
        "RefVal: 1",
        "ref mutated once on first render",
      );
      handle.sendKey(Key.Char('r'), Key.noModifiers);
      handle.sendKey(Key.Char('r'), Key.noModifiers);
      Test.assertContains(
        handle.getOutput(true),
        "Renders: 2",
        "component re-rendered twice",
      );
      Test.assertContains(
        handle.getOutput(true),
        "RefVal: 3",
        "the same ref survived and accumulated across renders",
      );
      handle.quit();
    });
  });
};
