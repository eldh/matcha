/*
 * Tests for headless mode
 */
open Matcha;

module CounterApp = {
  [@component]
  let make = () => {
    let (count, setCount) = Hooks.useState(0);
    let quit = Event.useQuit();

    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Arrow_up => setCount(count + 1)
      | Key.Arrow_down => setCount(max(0, count - 1))
      | Key.Char('r') => setCount(0)
      | Key.Char('q') => quit(PreserveScreen)
      | _ => ()
      }
    );

    <Text> {"Count: " ++ string_of_int(count)} </Text>;
  };
};

let run = () => {
  Test.group("Headless Mode", () => {
    Test.run("initial render", () => {
      let handle = Runtime.startHeadless((module CounterApp));
      let output = handle.getOutput(true);
      Test.assertContains(output, "Count: 0", "should show initial count");
      handle.quit();
    });

    Test.run("sendKey triggers state update", () => {
      let handle = Runtime.startHeadless((module CounterApp));
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      let output = handle.getOutput(true);
      Test.assertContains(output, "Count: 1", "count should increment");
      handle.quit();
    });

    Test.run("multiple key presses", () => {
      let handle = Runtime.startHeadless((module CounterApp));
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      let output = handle.getOutput(true);
      Test.assertContains(output, "Count: 3", "count should be 3");
      handle.quit();
    });

    Test.run("decrement key", () => {
      let handle = Runtime.startHeadless((module CounterApp));
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      let output = handle.getOutput(true);
      Test.assertContains(output, "Count: 1", "count should decrement");
      handle.quit();
    });

    Test.run("reset key", () => {
      let handle = Runtime.startHeadless((module CounterApp));
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      handle.sendKey(Key.Char('r'), Key.noModifiers);
      let output = handle.getOutput(true);
      Test.assertContains(output, "Count: 0", "count should reset");
      handle.quit();
    });

    Test.run("isRunning before quit", () => {
      let handle = Runtime.startHeadless((module CounterApp));
      Test.assertTrue(handle.isRunning(), "should be running");
      handle.quit();
    });

    Test.run("isRunning after quit", () => {
      let handle = Runtime.startHeadless((module CounterApp));
      handle.quit();
      Test.assertFalse(
        handle.isRunning(),
        "should not be running after quit",
      );
    });

    Test.run("quit via key", () => {
      let handle = Runtime.startHeadless((module CounterApp));
      handle.sendKey(Key.Char('q'), Key.noModifiers);
      Test.assertFalse(handle.isRunning(), "should quit on 'q' key");
    });

    Test.run("resize updates dimensions", () => {
      let handle = Runtime.startHeadless((module CounterApp));
      handle.resize(40, 10);
      let (w, h) = handle.getSize();
      Test.assertEqual(w, 40, "width should be 40");
      Test.assertEqual(h, 10, "height should be 10");
      handle.quit();
    });

    Test.run("getLines returns array", () => {
      let handle = Runtime.startHeadless((module CounterApp));
      let lines = handle.getLines(true);
      Test.assertTrue(
        Array.length(lines) >= 1,
        "should have at least 1 line",
      );
      handle.quit();
    });

    Test.run("custom config", () => {
      let config: Runtime.headlessConfig = {
        width: 40,
        height: 10,
      };
      let handle = Runtime.startHeadless(~config, (module CounterApp));
      let (w, h) = handle.getSize();
      Test.assertEqual(w, 40, "width from config");
      Test.assertEqual(h, 10, "height from config");
      handle.quit();
    });

    Test.run("render forces re-render", () => {
      let handle = Runtime.startHeadless((module CounterApp));
      let out1 = handle.render();
      let out2 = handle.render();
      Test.assertContains(out1, "Count:", "first render has content");
      Test.assertContains(out2, "Count:", "second render has content");
      handle.quit();
    });
  });
};
