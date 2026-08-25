/*
 * Tests for examples/chat - the capstone app - and, deliberately, THE
 * reference for how to test an application built on Matcha's input, timer,
 * static, focus, mouse and scrolling capabilities. Every technique an app
 * test needs is used here at least once, against the REAL app: the chat
 * example is structured as a component library (examples/chat/ChatApp.re)
 * plus a one-line launcher, and this suite starts that same component
 * through Runtime.startHeadless. Structure an app that way and its tests
 * always drive the real thing.
 *
 * The recipes, in the order they appear:
 *
 *   fresh handle per test        Runtime.startHeadless((module ChatApp))
 *                                - handles are cheap and independent; never
 *                                share one across tests
 *   end-to-end typing            Input.feedBytes(handle, "hi\r") - raw bytes
 *                                through a real InputDecoder, exactly what a
 *                                terminal would deliver
 *   asserting the live frame     handle.getOutput(true) / getLines(true) -
 *                                the CURRENT frame only, ANSI stripped
 *   asserting the transcript     handle.getStaticOutput(true) - everything
 *                                <Static> ever committed, accumulated; assert
 *                                "exactly once, ever" against this
 *   focus                        handle.getFocusedId() - never parse the
 *                                focus marker out of the frame
 *   gated input                  type while an unfocused TextArea is on
 *                                screen and assert nothing was inserted
 *   paste                        Input.feedPaste (handle level) and a raw
 *                                ESC[200~...ESC[201~ through feedBytes
 *   timers                       handle.advanceTime(ms) - the virtual clock;
 *                                tests never sleep
 *   wheel                        handle.sendMouse with ScrollUp/ScrollDown at
 *                                live-region coordinates
 *   click                        Input.clickAt(~x, ~y) - fires the Clickable
 *                                whose painted box contains the point
 *   quitting                     Ctrl+C as raw byte \003, then isRunning()
 */
open Matcha;

/* Number of non-overlapping occurrences of [needle] in [haystack]. */
let countOccurrences = (haystack: string, needle: string): int => {
  let hlen = String.length(haystack);
  let nlen = String.length(needle);
  if (nlen == 0 || nlen > hlen) {
    0;
  } else {
    let count = ref(0);
    let i = ref(0);
    while (i^ <= hlen - nlen) {
      if (String.sub(haystack, i^, nlen) == needle) {
        count := count^ + 1;
        i := i^ + nlen;
      } else {
        i := i^ + 1;
      };
    };
    count^;
  };
};

let assertOnce = (haystack: string, needle: string, msg: string): unit => {
  let actual = countOccurrences(haystack, needle);
  Test.assertTrue(
    actual == 1,
    msg ++ " (found " ++ string_of_int(actual) ++ " of \"" ++ needle ++ "\")",
  );
};

let start = () => Runtime.startHeadless((module ChatApp.App));

let run = () => {
  Test.group("chat: first frame", () => {
    Test.run("status, input, panel and hint are all in place", () => {
      let handle = start();
      let output = handle.getOutput(true);
      Test.assertContains(output, "* ready", "the status row is idle");
      Test.assertContains(
        output,
        "Type a message... (Enter to send)",
        "the empty input shows its placeholder",
      );
      Test.assertContains(output, "Context", "the panel has its title");
      Test.assertContains(
        output,
        "lib/Runtime.re",
        "and its first context row",
      );
      Test.assertContains(output, "Ctrl+C: quit", "the hint row is there");
      handle.quit();
    });

    Test.run("the panel window clips to five of the eight rows", () => {
      let handle = start();
      let output = handle.getOutput(true);
      Test.assertContains(output, "lib/ScrollView.re", "row 4 is the last visible");
      Test.assertFalse(
        Test.contains(output, "lib/StyledText.re"),
        "row 5 is below the window",
      );
      Test.assertFalse(
        Test.contains(output, "README.md"),
        "and so is the last row",
      );
      handle.quit();
    });

    Test.run("the input owns the focus from the first frame", () => {
      let handle = start();
      Test.assertEqual(
        handle.getFocusedId(),
        Some("chat-input"),
        "useFocus(~autoFocus=true) claimed it - assert focus through
         getFocusedId, never by parsing the frame",
      );
      handle.quit();
    });

    Test.run("nothing has been committed above the live region yet", () => {
      let handle = start();
      Test.assertEqualStr(
        handle.getStaticOutput(true),
        "",
        "an empty transcript is an empty drain",
      );
      handle.quit();
    });
  });

  Test.group("chat: typing and submitting", () => {
    Test.run("typed bytes land in the focused input", () => {
      let handle = start();
      /* Raw bytes through a real InputDecoder - the same path a terminal
       * read would take. */
      Input.feedBytes(handle, "hi");
      let output = handle.getOutput(true);
      Test.assertContains(output, "hi", "the two keystrokes were inserted");
      Test.assertFalse(
        Test.contains(output, "Type a message"),
        "and the placeholder is gone",
      );
      handle.quit();
    });

    Test.run("Enter commits the exchange to the transcript exactly once", () => {
      let handle = start();
      Input.feedBytes(handle, "hi\r");
      let static = handle.getStaticOutput(true);
      assertOnce(static, "> hi", "the prompt line was committed");
      assertOnce(static, "Echo: hi", "and the reply");
      Test.assertFalse(
        Test.contains(handle.getOutput(true), "Echo: hi"),
        "the transcript is above the live region, not in the frame",
      );
      Test.assertContains(
        handle.getOutput(true),
        "Type a message",
        "the input cleared back to its placeholder",
      );

      /* More frames must not re-commit: <Static>'s contract is exactly
       * once, EVER. */
      Input.feedBytes(handle, "x");
      ignore(handle.render());
      let static = handle.getStaticOutput(true);
      assertOnce(static, "> hi", "still one copy after further frames");
      assertOnce(static, "Echo: hi", "likewise the reply");
      handle.quit();
    });

    Test.run("messages accumulate in order", () => {
      let handle = start();
      Input.feedBytes(handle, "one\r");
      handle.advanceTime(2000); /* let the thinking window close */
      Input.feedBytes(handle, "two\r");
      let static = handle.getStaticOutput(true);
      assertOnce(static, "> one", "first message committed once");
      assertOnce(static, "> two", "second message committed once");
      Test.assertTrue(
        countOccurrences(static, "Echo:") == 2,
        "one reply each",
      );
      handle.quit();
    });

    Test.run("Enter on an empty input submits nothing", () => {
      let handle = start();
      Input.feedBytes(handle, "\r");
      Test.assertEqualStr(
        handle.getStaticOutput(true),
        "",
        "a blank message is not a message",
      );
      handle.quit();
    });
  });

  Test.group("chat: the thinking spinner (virtual clock)", () => {
    Test.run("idle time does not tick anything", () => {
      let handle = start();
      /* ms=0 disables useInterval entirely - nothing is registered, so
       * advancing the clock is a no-op. */
      handle.advanceTime(5000);
      Test.assertContains(handle.getOutput(true), "* ready", "still idle");
      handle.quit();
    });

    Test.run("a submit opens the thinking window and the spinner turns", () => {
      let handle = start();
      Input.feedBytes(handle, "hi\r");
      Test.assertContains(
        handle.getOutput(true),
        "thinking...",
        "submitting flips the status row",
      );
      let before = handle.getOutput(true);
      /* advanceTime is the ONLY way time passes headlessly - tests never
       * sleep. One 120ms step fires the interval once. */
      handle.advanceTime(120);
      Test.assertTrue(
        handle.getOutput(true) != before,
        "one interval tick advanced the spinner frame",
      );
      Test.assertContains(handle.getOutput(true), "thinking...", "still thinking");
      handle.quit();
    });

    Test.run("the 1.5s timeout closes the window and stops the spinner", () => {
      let handle = start();
      Input.feedBytes(handle, "hi\r");
      handle.advanceTime(2000);
      let output = handle.getOutput(true);
      Test.assertContains(output, "* ready", "useTimeout fired and reset the status");
      Test.assertFalse(
        Test.contains(output, "thinking"),
        "the spinner is gone",
      );
      /* And with isThinking false the interval is back to ms=0: time no
       * longer moves the frame. */
      let settled = handle.getOutput(true);
      handle.advanceTime(5000);
      Test.assertEqualStr(
        handle.getOutput(true),
        settled,
        "no timer left running after the window closed",
      );
      handle.quit();
    });
  });

  Test.group("chat: paste", () => {
    Test.run("a paste is one insertion, not keystrokes", () => {
      let handle = start();
      Input.feedPaste(handle, "line1\nline2");
      let output = handle.getOutput(true);
      Test.assertContains(output, "line1", "first pasted line in the input");
      Test.assertContains(output, "line2", "second line too - TextArea is multi-line");
      Test.assertEqualStr(
        handle.getStaticOutput(true),
        "",
        "the newline in the paste did NOT submit - paste is data, not keys",
      );
      handle.quit();
    });

    Test.run("raw bracketed-paste bytes arrive the same way", () => {
      let handle = start();
      /* What a real terminal sends: ESC[200~ ... ESC[201~ around the
       * payload. feedBytes runs it through a real InputDecoder. */
      Input.feedBytes(handle, "\027[200~pasted text\027[201~");
      Test.assertContains(
        handle.getOutput(true),
        "pasted text",
        "the decoder reassembled the paste and the app inserted it",
      );
      Test.assertEqualStr(handle.getStaticOutput(true), "", "and did not submit");
      handle.quit();
    });
  });

  Test.group("chat: focus routes the keyboard", () => {
    Test.run("Tab hands the keyboard to the panel and back", () => {
      let handle = start();
      Input.pressTab(handle);
      Test.assertEqual(
        handle.getFocusedId(),
        Some("context"),
        "Tab moved focus to the ScrollView",
      );

      /* The input's useInput is gated on ~isActive=inputFocused: these
       * keystrokes must go nowhere near it. */
      Input.feedBytes(handle, "zz");
      Test.assertContains(
        handle.getOutput(true),
        "Type a message",
        "an unfocused input inserts nothing - the placeholder is untouched",
      );

      /* The arrows now scroll the panel instead. */
      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      let output = handle.getOutput(true);
      Test.assertFalse(
        Test.contains(output, "lib/Runtime.re"),
        "the first row scrolled out",
      );
      Test.assertContains(output, "lib/StyledText.re", "and row 5 scrolled in");

      Input.pressShiftTab(handle);
      Test.assertEqual(
        handle.getFocusedId(),
        Some("chat-input"),
        "Shift+Tab cycles back",
      );
      Input.feedBytes(handle, "ok");
      Test.assertContains(
        handle.getOutput(true),
        "ok",
        "and the input takes keystrokes again",
      );
      handle.quit();
    });

    Test.run("arrows while the input is focused do not scroll the panel", () => {
      let handle = start();
      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      Test.assertContains(
        handle.getOutput(true),
        "lib/Runtime.re",
        "the panel did not move - the arrow went to the focused TextArea",
      );
      handle.quit();
    });
  });

  Test.group("chat: mouse", () => {
    /* Coordinates below are live-region (frame) coordinates, worked out
     * from the layout: at 80x24 the frame is status row (y=0), a 6-row
     * HStack (y=1..6) and the hint row (y=7). Inside the HStack the Flex(1)
     * input takes columns 0..56, one gap column, and the Chars(22) panel
     * columns 58..79 - its title on y=1 and its 5-row ScrollView window on
     * y=2..6. When a click lands somewhere unexpected, print
     * handle.getLines(true) and count rows - that is the whole debugging
     * loop. */
    let wheelAt = (~kind: Mouse.kind, ~x: int, ~y: int): Mouse.event => {
      Mouse.kind: kind,
      button: Mouse.NoButton,
      x,
      y,
      shift: false,
      alt: false,
      ctrl: false,
    };

    Test.run("the wheel scrolls the panel without taking focus", () => {
      let handle = start();
      handle.sendMouse(wheelAt(~kind=Mouse.ScrollDown, ~x=60, ~y=4));
      let output = handle.getOutput(true);
      Test.assertFalse(
        Test.contains(output, "lib/Runtime.re"),
        "one notch is three rows - the first row is gone",
      );
      Test.assertContains(output, "lib/Key.re", "row 3 is now the top of the window");
      Test.assertEqual(
        handle.getFocusedId(),
        Some("chat-input"),
        "wheel scrolling never moves focus",
      );
      handle.sendMouse(wheelAt(~kind=Mouse.ScrollUp, ~x=60, ~y=4));
      Test.assertContains(
        handle.getOutput(true),
        "lib/Runtime.re",
        "and a notch up brings it back",
      );
      handle.quit();
    });

    Test.run("a wheel event over the input does not scroll the panel", () => {
      let handle = start();
      handle.sendMouse(wheelAt(~kind=Mouse.ScrollDown, ~x=10, ~y=3));
      Test.assertContains(
        handle.getOutput(true),
        "lib/Runtime.re",
        "the pointer was not over the ScrollView",
      );
      handle.quit();
    });

    Test.run("clicking a context row pins it, clicking again unpins", () => {
      let handle = start();
      /* y=2 is the first ScrollView row: <ContextRow> for lib/Runtime.re,
       * wrapped in a <Clickable>. */
      Input.clickAt(handle, ~x=60, ~y=2);
      Test.assertContains(
        handle.getOutput(true),
        "* lib/Runtime.re",
        "the row is starred",
      );
      Input.clickAt(handle, ~x=60, ~y=2);
      Test.assertFalse(
        Test.contains(handle.getOutput(true), "* lib/Runtime.re"),
        "toggled back off",
      );
      handle.quit();
    });

    Test.run("after scrolling, a click hits the row NOW under the pointer", () => {
      let handle = start();
      handle.sendMouse(wheelAt(~kind=Mouse.ScrollDown, ~x=60, ~y=4));
      /* The window now starts at row 3: y=2 is lib/Key.re. */
      Input.clickAt(handle, ~x=60, ~y=2);
      Test.assertContains(
        handle.getOutput(true),
        "* lib/Key.re",
        "the click routed through the scrolled viewport to the right row",
      );
      Test.assertFalse(
        Test.contains(handle.getOutput(true), "* lib/Runtime.re"),
        "and not to the row that USED to be there",
      );
      handle.quit();
    });
  });

  Test.group("chat: the real binary, fed raw bytes", () => {
    Test.run("keystrokes batched into one read land one event per frame", () => {
      /* The whole string reaches the child's stream loop in a single
       * read(), so both keystrokes are decoded from one buffer. Each must
       * still see a fresh render: before Runtime's flushDirty, the 'i'
       * handler closed over the same pre-'h' state and the submit said
       * "Echo: i". A process-level test because the in-process handle
       * re-renders per sendKey and can never reproduce this. */
      let out = Golden.runExampleWithInput("chat", "hi\r");
      Test.assertContains(out, "> hi", "the full message was submitted");
      Test.assertContains(out, "Echo: hi", "and echoed back whole");
    });
  });

  Test.group("chat: quitting", () => {
    Test.run("Ctrl+C quits even while the input holds focus", () => {
      let handle = start();
      Input.feedBytes(handle, "half-typed");
      Test.assertTrue(handle.isRunning(), "running before");
      Input.feedBytes(handle, "\003"); /* the raw Ctrl+C byte */
      Test.assertFalse(
        handle.isRunning(),
        "the global useKeyDown saw it - useInput gating does not eat Ctrl+C",
      );
    });
  });
};
