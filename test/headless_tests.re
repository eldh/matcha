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

/* An app that themes itself off the terminal background. The None branch is
 * the one every real application needs: nothing answers OSC 11 in a headless
 * run (and plenty of real terminals never answer it either). */
module ThemedApp = {
  [@component]
  let make = () => {
    let label =
      switch (Hooks.useTerminalBackground()) {
      | None => "theme: unknown"
      | Some((r, g, b)) =>
        /* A crude sum is enough here - bdiff does the sRGB-correct thing. */
        r + g + b > 382 ? "theme: light" : "theme: dark"
      };
    <Text> label </Text>;
  };
};

let run = () => {
  Test.group("Runtime: OSC 11 payload parsing", () => {
    Test.run("xterm's rgb: form scales by DIGIT COUNT, not by magnitude", () => {
      /* The whole hazard: "1e1e" is 0x1e, not 0x1e1e. The digit count is the
         precision, and all-f is full intensity at every width. */
      Test.assertEqual(
        Runtime.parseOscColor("rgb:1e1e/1e1e/1e1e"),
        Some((30, 30, 30)),
        "4-digit components take their high byte",
      );
      Test.assertEqual(
        Runtime.parseOscColor("rgb:ffff/ffff/ffff"),
        Some((255, 255, 255)),
        "all-f at 4 digits is full intensity",
      );
      Test.assertEqual(
        Runtime.parseOscColor("rgb:f/f/f"),
        Some((255, 255, 255)),
        "all-f at ONE digit is full intensity too",
      );
      Test.assertEqual(
        Runtime.parseOscColor("rgb:ff/80/00"),
        Some((255, 128, 0)),
        "2-digit components pass through as-is",
      );
      Test.assertEqual(
        Runtime.parseOscColor("rgb:0000/0000/0000"),
        Some((0, 0, 0)),
        "black",
      );
    });

    Test.run("the #RRGGBB form is accepted too", () =>
      Test.assertEqual(
        Runtime.parseOscColor("#1e2f3a"),
        Some((30, 47, 58)),
        "some terminals answer in hash form",
      )
    );

    Test.run("surrounding whitespace is tolerated", () =>
      Test.assertEqual(
        Runtime.parseOscColor("  rgb:00/00/00 "),
        Some((0, 0, 0)),
        "trimmed before parsing",
      )
    );

    Test.run("malformed payloads are None, never a guess", () => {
      let bad = [
        "", /* empty */
        "rgb:00/00", /* two components */
        "rgb:00/00/00/00", /* four */
        "rgb:zz/00/00", /* non-hex */
        "rgb:00000/00/00", /* five digits */
        "cmyk:0/0/0/0", /* another colour space */
        "#12345", /* short hash */
        "1e1e/1e1e/1e1e" /* no prefix */,
      ];
      List.iter(
        p =>
          Test.assertEqual(
            Runtime.parseOscColor(p),
            None,
            "rejected: " ++ String.escaped(p),
          ),
        bad,
      );
    });
  });

  Test.group("Headless: terminal background (OSC 11)", () => {
    Test.run("unknown until the terminal answers", () => {
      let handle = Runtime.startHeadless(~config={width: 40, height: 6}, (module ThemedApp));
      Test.assertContains(
        handle.getOutput(true),
        "theme: unknown",
        "no terminal has answered, so useTerminalBackground is None",
      );
      handle.quit();
    });

    Test.run("setTerminalBackground re-renders with the light branch", () => {
      let handle = Runtime.startHeadless(~config={width: 40, height: 6}, (module ThemedApp));
      handle.setTerminalBackground((255, 255, 255));
      Test.assertContains(
        handle.getOutput(true),
        "theme: light",
        "the next frame shows the light theme",
      );
      Test.assertFalse(
        Test.contains(handle.getOutput(true), "theme: unknown"),
        "and the unknown branch is gone",
      );
      handle.quit();
    });

    Test.run("a dark answer takes the dark branch", () => {
      let handle = Runtime.startHeadless(~config={width: 40, height: 6}, (module ThemedApp));
      handle.setTerminalBackground((30, 30, 30));
      Test.assertContains(handle.getOutput(true), "theme: dark", "dark branch");
      handle.quit();
    });

    Test.run("two handles do not share a background", () => {
      /* The value lives on instanceState, so it is per-application like
         every other piece of runtime state. */
      let a = Runtime.startHeadless(~config={width: 40, height: 6}, (module ThemedApp));
      a.setTerminalBackground((250, 250, 250));
      let b = Runtime.startHeadless(~config={width: 40, height: 6}, (module ThemedApp));
      Test.assertContains(
        b.getOutput(true),
        "theme: unknown",
        "the second handle starts with no background of its own",
      );
      Test.assertContains(
        a.render(),
        "theme: light",
        "and the first handle still has its own",
      );
      a.quit();
      b.quit();
    });
  });

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
