/*
 * Tests for examples/claude-code - the Claude Code CLI mock, and Matcha's
 * FULLSCREEN example. Where test/chat_tests.re is the reference for
 * focus-driven, inline, <Static>-committing apps, this suite is the
 * reference for the opposite set of choices:
 *
 *   a FOCUS-FREE app             getFocusedId() stays None forever, and
 *                                Shift+Tab therefore reaches the app instead
 *                                of being eaten by the focus ring
 *   a FULL-HEIGHT frame          the transcript is a Flex(1) ScrollView, so
 *                                the frame is exactly termHeight rows with
 *                                the input pinned to the bottom
 *   NOTHING committed, ever      getStaticOutput(true) stays "" for the
 *                                app's whole life - a fullscreen app owns
 *                                its transcript (and <Static> would raise);
 *                                contrast chat_tests.re, which asserts
 *                                "committed exactly once, EVER" against that
 *                                same accessor
 *   a timer-driven STREAM        advanceTime(ms) plays a canned agent script
 *                                into the FRAME, one step per 350ms tick
 *   "no timers left"             advance a long way while idle and assert
 *                                getOutput is byte-identical
 *   stick-to-bottom history      the transcript ScrollView is controlled
 *                                from app state: it rides the newest line
 *                                until the wheel takes it back, and snaps
 *                                down again on the next appended item
 *   a CONTROLLED ScrollView      the slash palette is driven by ~offset /
 *                                ~onScroll from app state, with focusable=
 *                                false, so only the wheel and the keyboard
 *                                the app routes itself can move it
 *   Clickable rows               Input.clickAt on a palette row runs it;
 *                                coordinates calibrated from getLines(true)
 *   the real binary              Golden.runExampleWithInput pins that a
 *                                whole typed line arrives one event per
 *                                frame through the byte-fed loop
 *
 * NOTE ON SCREEN MODE: Runtime.startHeadless is screen-agnostic - it prints
 * frames and has no terminal to own - so this suite drives exactly the same
 * component the Fullscreen binary runs, with no ~screen involved. What the
 * app's fullscreen design changes here is WHERE things are asserted: the
 * transcript is in the frame, never in the static buffer.
 *
 * As in chat_tests.re, every case starts a FRESH handle on the REAL app
 * component (examples/claude-code is a library plus a one-line launcher).
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

let start = () => Runtime.startHeadless((module ClaudeCodeApp.App));

/* A wheel event at live-region coordinates. */
let wheelAt = (~kind: Mouse.kind, ~x: int, ~y: int): Mouse.event => {
  Mouse.kind: kind,
  button: Mouse.NoButton,
  x,
  y,
  shift: false,
  alt: false,
  ctrl: false,
};

let ctrlC = (handle: Runtime.headlessHandle): unit =>
  handle.sendKey(Key.Char('c'), {...Key.noModifiers, ctrl: true});

let shiftTab = (handle: Runtime.headlessHandle): unit =>
  handle.sendKey(Key.Tab, {...Key.noModifiers, shift: true});

/* ---------------------------------------------------------------------------
 * FRAME GEOMETRY, calibrated by printing handle.getLines(true) - never by
 * arithmetic. The default headless terminal is 80x24, and the root VStack is
 *
 *   Flex(1)  transcript ScrollView   <- absorbs all spare height
 *   Chars(1) status row
 *   Chars(3) bordered input box
 *   Chars(5) slash palette           <- only while it is open
 *   Chars(1) hint row
 *
 * so with the palette CLOSED the 24 rows are, 0-based:
 *   y=0..18  transcript      y=19 status   y=20..22 input   y=23 hint
 * and with the palette OPEN:
 *   y=0..13  transcript      y=14 status   y=15..17 input
 *   y=18..22 palette rows    y=23 hint
 * ------------------------------------------------------------------------- */

/* First palette row when the palette is open (five rows: y=18..22). */
let paletteTop = 18;

/* Just the palette's five rows, joined. The transcript is ordinary frame
   content now, so a bare substring search over the whole frame cannot tell a
   palette row from a line of history - the banner alone contains "/help".
   Slicing the rows the palette occupies is the honest question. */
let paletteText = (handle: Runtime.headlessHandle): string => {
  let lines = handle.getLines(true);
  String.concat("\n", Array.to_list(Array.sub(lines, paletteTop, 5)));
};

/* A row well inside the transcript window, in both layouts. */
let transcriptRow = 5;

/* Text only the welcome banner contains - the first transcript item, and so
   the first thing to scroll off the top once history grows. */
let bannerText = "Welcome to Claude Code mock";

/* ---------------------------------------------------------------------------
 * THE INPUT BOX, which is no longer a fixed three rows: the app measures the
 * TextArea every frame (TextArea.measure) and sizes the bordered box around
 * the answer, so a long prompt wraps and the box grows to at most 5 content
 * rows. The helpers below find it by its borders rather than by a row number.
 * ------------------------------------------------------------------------- */

let startsWith = (s: string, prefix: string): bool =>
  String.length(s) >= String.length(prefix)
  && String.sub(s, 0, String.length(prefix)) == prefix;

/* Index of the LAST frame row starting with [prefix]. The banner draws the
   same corners higher up, so "last" is what picks the input box. */
let lastRowStartingWith = (lines: array(string), prefix: string): int => {
  let found = ref(-1);
  Array.iteri(
    (i, line) =>
      if (startsWith(line, prefix)) {
        found := i;
      },
    lines,
  );
  found^;
};

/* (top border row, bottom border row) of the input box in the current frame. */
let boxBorders = (handle: Runtime.headlessHandle): (int, int) => {
  let lines = handle.getLines(true);
  let top = lastRowStartingWith(lines, "\xe2\x95\xad" /* U+256D */);
  let bottom = lastRowStartingWith(lines, "\xe2\x95\xb0" /* U+2570 */);
  Test.assertTrue(
    top >= 0 && bottom > top,
    "the input box has both of its borders in the frame",
  );
  (top, bottom);
};

/* How many CONTENT rows the input box currently has. */
let boxRows = (handle: Runtime.headlessHandle): int => {
  let (top, bottom) = boxBorders(handle);
  bottom - top - 1;
};

let run = () => {
  Test.group("claude-code: first frame", () => {
    Test.run("the banner is the top of the frame, not a commit", () => {
      let handle = start();
      assertOnce(
        handle.getOutput(true),
        bannerText,
        "the banner is the first transcript item, drawn in the frame itself",
      );
      Test.assertEqualStr(
        handle.getStaticOutput(true),
        "",
        "a fullscreen app commits NOTHING: there is no scrollback above the
         alternate screen to commit to, and <Static> would raise",
      );
      handle.quit();
    });

    Test.run("the frame fills the whole terminal", () => {
      let handle = start();
      let lines = handle.getLines(true);
      Test.assertEqual(
        Array.length(lines),
        24,
        "the Flex(1) transcript absorbs every spare row, so the very first
         frame is exactly the terminal's 24 rows tall",
      );
      Test.assertContains(
        lines[0],
        "\xe2\x95\xad",
        "and the banner's top border is row 0 - history starts at the top",
      );
      Test.assertContains(
        lines[23],
        "? for shortcuts",
        "while the hint row is the LAST row: the input block is pinned to the
         bottom of the viewport",
      );
      handle.quit();
    });

    Test.run("prompt, hint and mode label are in place", () => {
      let handle = start();
      let output = handle.getOutput(true);
      Test.assertContains(
        output,
        "Try \"fix a bug\" or / for commands",
        "the empty prompt shows its placeholder",
      );
      Test.assertContains(output, "? for shortcuts", "the hint row is there");
      Test.assertContains(
        output,
        "shift+tab to cycle",
        "and advertises the permission-mode cycle",
      );
      Test.assertFalse(
        Test.contains(output, "esc to interrupt"),
        "the status row is idle - nothing is streaming",
      );
      handle.quit();
    });

    Test.run("nothing in this app is focusable, on purpose", () => {
      let handle = start();
      Test.assertEqual(
        handle.getFocusedId(),
        None,
        "examples/claude-code registers NO useFocus and its ScrollView opts
         out with focusable=false. That is deliberate: with zero focusables
         the framework does not consume Tab/Shift+Tab for focus cycling, so
         Shift+Tab reaches the app and can cycle permission modes.",
      );
      handle.quit();
    });
  });

  Test.group("claude-code: submitting streams a canned script", () => {
    Test.run("Enter puts the prompt in the transcript and opens the status row", () => {
      let handle = start();
      Input.feedBytes(handle, "hi\r");
      assertOnce(
        handle.getOutput(true),
        "> hi",
        "the prompt was appended to the transcript, which the frame draws",
      );
      Test.assertContains(
        handle.getOutput(true),
        "esc to interrupt",
        "and the live status row says the mock agent is working",
      );
      handle.quit();
    });

    Test.run("each 350ms tick appends exactly one step", () => {
      let handle = start();
      Input.feedBytes(handle, "hi\r");
      handle.advanceTime(350);
      assertOnce(
        handle.getOutput(true),
        "\xe2\x8f\xba Read(lib/Hooks.re)",
        "one tick, one tool call - and it appears ONCE per frame",
      );
      Test.assertEqual(
        countOccurrences(handle.getOutput(true), "\xe2\x8f\xba Grep"),
        0,
        "and only one - step two has not run yet",
      );
      handle.quit();
    });

    Test.run("the whole script lands once each, then the timers stop", () => {
      let handle = start();
      Input.feedBytes(handle, "hi\r");
      handle.advanceTime(5000);
      let output = handle.getOutput(true);
      assertOnce(output, "Read(lib/Hooks.re)", "step 1");
      assertOnce(output, "Found 12 matches", "step 4");
      assertOnce(output, "All tests passed", "step 8");
      assertOnce(output, "canned reply", "and the assistant's closing reply");

      Test.assertFalse(
        Test.contains(output, "esc to interrupt"),
        "the status row went back to idle when the queue drained",
      );

      /* ms=0 disables both intervals, so advancing while idle is a cheap
         "nothing is running" assertion. */
      let settled = handle.getOutput(true);
      handle.advanceTime(2000);
      Test.assertEqualStr(
        handle.getOutput(true),
        settled,
        "no timer left running after the stream finished",
      );
      handle.quit();
    });

    Test.run("nothing is ever committed above the frame", () => {
      /* The point of the fullscreen model, pinned end to end. chat_tests.re
         asserts "committed exactly once, EVER" against this same accessor;
         here the correct value is the empty string forever, because the
         alternate screen has no scrollback and <Static>/useStdout raise. */
      let handle = start();
      Test.assertEqualStr(
        handle.getStaticOutput(true),
        "",
        "nothing on the first frame",
      );
      Input.feedBytes(handle, "hi\r");
      Test.assertEqualStr(
        handle.getStaticOutput(true),
        "",
        "nothing when a prompt is submitted",
      );
      handle.advanceTime(5000);
      Test.assertEqualStr(
        handle.getStaticOutput(true),
        "",
        "nothing across the whole canned stream",
      );
      Input.feedBytes(handle, "/mo\r");
      Test.assertEqualStr(
        handle.getStaticOutput(true),
        "",
        "and nothing when a slash command posts its notice",
      );
      handle.quit();
    });

    Test.run("further keys and frames never duplicate a step", () => {
      let handle = start();
      Input.feedBytes(handle, "hi\r");
      handle.advanceTime(5000);
      Input.feedBytes(handle, "x");
      ignore(handle.render());
      let output = handle.getOutput(true);
      assertOnce(output, "> hi", "the prompt is still a single copy");
      assertOnce(output, "Read(lib/Hooks.re)", "likewise step 1");
      assertOnce(output, "canned reply", "and the reply");
      handle.quit();
    });

    Test.run("a submit mid-stream is ignored", () => {
      let handle = start();
      Input.feedBytes(handle, "hi\r");
      handle.advanceTime(350);
      Input.feedBytes(handle, "again\r");
      Test.assertEqual(
        /* A submitted prompt starts its own transcript ROW; the same text
           sitting in the input box is preceded by the box's "| > ". */
        countOccurrences(handle.getOutput(true), "\n> again"),
        0,
        "a real CLI would queue it; the mock simply drops it",
      );
      handle.quit();
    });
  });

  Test.group("claude-code: the history scrolls, and snaps back", () => {
    /* The feature the fullscreen rework exists for. The transcript is a
       CONTROLLED ScrollView: app state holds None ("ride the newest line" -
       a huge offset that clamps to maxOffset) or Some(o) after a wheel
       gesture, and every append resets it to None. */
    let fillHistory = (handle: Runtime.headlessHandle): unit => {
      /* Two full runs of the canned script overflow the ~19-row transcript
         window several times over. advanceTime(5000) drains the stream, so
         the next submit is not dropped as mid-stream. */
      Input.feedBytes(handle, "one\r");
      handle.advanceTime(5000);
      Input.feedBytes(handle, "two\r");
      handle.advanceTime(5000);
    };

    Test.run("new output rides the bottom and the banner scrolls away", () => {
      let handle = start();
      fillHistory(handle);
      let output = handle.getOutput(true);
      Test.assertFalse(
        Test.contains(output, bannerText),
        "the banner is long gone off the top of the window",
      );
      Test.assertContains(
        output,
        "canned reply",
        "while the newest reply is on screen - the window stuck to the bottom
         as items were appended",
      );
      handle.quit();
    });

    Test.run("the wheel takes the window back up the history", () => {
      let handle = start();
      fillHistory(handle);
      /* One notch is three rows; a handful of them walks the window to the
         very top, where ScrollView clamps it at offset 0. */
      for (_ in 1 to 12) {
        handle.sendMouse(
          wheelAt(~kind=Mouse.ScrollUp, ~x=3, ~y=transcriptRow),
        );
      };
      let output = handle.getOutput(true);
      Test.assertContains(
        output,
        bannerText,
        "wheeling up over the transcript brought the first item back",
      );
      Test.assertFalse(
        Test.contains(output, "> two"),
        "and the newest lines - the second prompt and its reply - are now
         below the window",
      );
      Test.assertEqual(
        handle.getFocusedId(),
        None,
        "the history ScrollView is focusable=false, like the palette's -
         nothing in this app ever joins the focus ring",
      );
      handle.quit();
    });

    Test.run("submitting again snaps the window back to the bottom", () => {
      let handle = start();
      fillHistory(handle);
      for (_ in 1 to 12) {
        handle.sendMouse(
          wheelAt(~kind=Mouse.ScrollUp, ~x=3, ~y=transcriptRow),
        );
      };
      Test.assertContains(
        handle.getOutput(true),
        bannerText,
        "scrolled back, as the case above",
      );
      Input.feedBytes(handle, "three\r");
      let output = handle.getOutput(true);
      Test.assertContains(
        output,
        "> three",
        "the new prompt is visible: appending snapped the offset back to
         None, which is stick-to-bottom (mirrors the real CLI)",
      );
      Test.assertFalse(
        Test.contains(output, bannerText),
        "and the top of the history went away again",
      );
      handle.quit();
    });
  });

  Test.group("claude-code: interrupting", () => {
    Test.run("Esc stops the stream where it stands", () => {
      let handle = start();
      Input.feedBytes(handle, "hi\r");
      handle.advanceTime(350); /* one step: the Read tool call */
      handle.sendKey(Key.Escape, Key.noModifiers);
      assertOnce(
        handle.getOutput(true),
        "Interrupted by user",
        "the interruption is appended to the transcript",
      );
      Test.assertFalse(
        Test.contains(handle.getOutput(true), "esc to interrupt"),
        "and the status row is idle again",
      );

      handle.advanceTime(5000);
      Test.assertEqual(
        countOccurrences(handle.getOutput(true), "\xe2\x8f\xba Grep"),
        0,
        "step 3 never ran - the queue was emptied, not paused",
      );
      Test.assertEqual(
        countOccurrences(handle.getOutput(true), "canned reply"),
        0,
        "nor did the closing reply",
      );
      handle.quit();
    });
  });

  Test.group("claude-code: the slash palette", () => {
    Test.run("typing '/' opens it and typing more filters it", () => {
      let handle = start();
      Input.feedBytes(handle, "/");
      let output = handle.getOutput(true);
      Test.assertContains(output, "/clear", "the palette lists commands");
      Test.assertContains(output, "/doctor", "five rows fit the window");
      Test.assertFalse(
        Test.contains(output, "/status"),
        "and the twelfth is below it",
      );

      Input.feedBytes(handle, "mo");
      let rows = paletteText(handle);
      Test.assertContains(rows, "/model", "\"/mo\" matches only /model");
      Test.assertFalse(
        Test.contains(rows, "/help"),
        "/help is filtered out. (Asserted against the palette's rows, not the
         whole frame: the banner in the transcript says \"/help for help\".)",
      );
      Test.assertFalse(
        Test.contains(rows, "/clear"),
        "and so is /clear",
      );
      handle.quit();
    });

    Test.run("Enter runs the selected command and closes the palette", () => {
      let handle = start();
      Input.feedBytes(handle, "/mo\r");
      assertOnce(
        handle.getOutput(true),
        "Ran /model",
        "the command's notice was appended to the transcript once",
      );
      let output = handle.getOutput(true);
      Test.assertContains(
        output,
        "Try \"fix a bug\" or / for commands",
        "the input cleared back to its placeholder",
      );
      Test.assertFalse(
        Test.contains(output, "/clear"),
        "and with the input empty the palette is gone (/model is still on
         screen, but as the transcript notice this command just posted)",
      );
      handle.quit();
    });

    Test.run("/exit quits", () => {
      let handle = start();
      Test.assertTrue(handle.isRunning(), "running before");
      Input.feedBytes(handle, "/exit\r");
      Test.assertFalse(handle.isRunning(), "and stopped after");
    });

    Test.run("clicking a row runs that command", () => {
      let handle = start();
      Input.feedBytes(handle, "/");
      /* Frame rows, calibrated from handle.getLines(true) - see FRAME
         GEOMETRY at the top of this file. With the palette open its five
         rows are y=18..22, so paletteTop + 2 is the third visible command,
         /config. These y values moved when the app went fullscreen: the
         whole block now sits at the BOTTOM of a 24-row frame instead of at
         the top of a 10-row live region. */
      Input.clickAt(handle, ~x=3, ~y=paletteTop + 2);
      assertOnce(
        handle.getOutput(true),
        "Ran /config",
        "the Clickable under the pointer fired",
      );
      handle.quit();
    });

    Test.run("the wheel scrolls the palette without any focus", () => {
      let handle = start();
      Input.feedBytes(handle, "/");
      Test.assertContains(handle.getOutput(true), "/clear", "window starts at the top");
      handle.sendMouse(
        wheelAt(~kind=Mouse.ScrollDown, ~x=3, ~y=paletteTop + 2),
      );
      let output = handle.getOutput(true);
      Test.assertContains(
        output,
        "/init",
        "one notch is three rows - the window moved down",
      );
      Test.assertFalse(
        Test.contains(output, "/clear"),
        "and the first command scrolled out",
      );
      Test.assertEqual(
        handle.getFocusedId(),
        None,
        "a focusable=false ScrollView never joins the focus ring",
      );
      handle.quit();
    });

    Test.run("the arrows move the selection, not the text cursor", () => {
      let handle = start();
      Input.feedBytes(handle, "/");
      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      /* Selection 2 = /config; Enter runs it rather than submitting. */
      handle.sendKey(Key.Enter, Key.noModifiers);
      assertOnce(
        handle.getOutput(true),
        "Ran /config",
        "two Arrow_downs moved the palette selection",
      );
      handle.quit();
    });
  });

  Test.group("claude-code: Shift+Tab cycles the permission mode", () => {
    Test.run("no focusables means Shift+Tab reaches the app", () => {
      let handle = start();
      Test.assertContains(handle.getOutput(true), "default", "mode 0");
      shiftTab(handle);
      Test.assertContains(
        handle.getOutput(true),
        "auto-accept edits on",
        "mode 1 - the focus ring did not swallow the key, because there is
         no focus ring",
      );
      shiftTab(handle);
      Test.assertContains(handle.getOutput(true), "plan mode on", "mode 2");
      shiftTab(handle);
      Test.assertContains(handle.getOutput(true), "default", "back to mode 0");
      Test.assertContains(
        handle.getOutput(true),
        "Try \"fix a bug\" or / for commands",
        "and nothing was typed into the prompt - Shift+Tab is claimed before
         TextArea's Tab arm, which would have inserted two spaces",
      );
      handle.quit();
    });
  });

  Test.group("claude-code: double Ctrl+C", () => {
    Test.run("one press arms the exit, and it disarms itself", () => {
      let handle = start();
      ctrlC(handle);
      Test.assertContains(
        handle.getOutput(true),
        "ctrl-c again",
        "the hint row turned into the confirmation",
      );
      Test.assertTrue(handle.isRunning(), "but nothing quit");
      handle.advanceTime(2000);
      Test.assertContains(
        handle.getOutput(true),
        "? for shortcuts",
        "the 1.5s useTimeout disarmed it",
      );
      Test.assertTrue(handle.isRunning(), "still running");
      handle.quit();
    });

    Test.run("two presses back to back quit", () => {
      let handle = start();
      ctrlC(handle);
      ctrlC(handle);
      Test.assertFalse(handle.isRunning(), "the second press took the exit");
    });
  });

  Test.group("claude-code: paste", () => {
    Test.run("a multi-line paste is data, not keystrokes", () => {
      let handle = start();
      Input.feedPaste(handle, "fix this\nplease");
      let output = handle.getOutput(true);
      /* The prompt GROWS: the app measures the TextArea every frame and sizes
         the box around it (minHeight=1, maxHeight=5), so a two-line paste is
         shown in full on two rows. What matters is that the newline did not
         submit. */
      Test.assertContains(output, "fix this", "the first pasted line is in the box");
      Test.assertContains(
        output,
        "please",
        "and so is the second - the box grew to two rows to hold it",
      );
      Test.assertEqual(
        boxRows(handle),
        2,
        "which is exactly what the box is: two content rows between borders",
      );
      Test.assertFalse(
        Test.contains(output, "Try \"fix a bug\""),
        "the placeholder is gone, so the paste really landed",
      );
      Test.assertEqual(
        countOccurrences(output, "\n> fix this"),
        0,
        "the newline in the paste did NOT submit - paste is data, not keys.
         (A submitted prompt would start its own transcript row; the text in
         the input box is preceded by the box's \"| > \" instead.)",
      );
      Test.assertFalse(
        Test.contains(output, "esc to interrupt"),
        "and nothing started streaming",
      );
      handle.quit();
    });
  });

  Test.group("claude-code: a long prompt wraps and the box grows", () => {
    /* 120 characters of short words: long enough to wrap twice at the default
       80-column terminal (the TextArea is 74 columns wide inside the box),
       and made of words so the transcript's wrap=Wrap reply cannot split the
       "zebra" marker at the end. */
    let longPrompt =
      String.concat(" ", List.init(23, _ => "wrap")) ++ " zebra";

    Test.run("typing past the box width grows it instead of overflowing", () => {
      Test.assertEqual(String.length(longPrompt), 120, "120 characters");
      let handle = start();
      Test.assertEqual(boxRows(handle), 1, "the empty prompt is one row");

      Input.feedBytes(handle, longPrompt);
      Test.assertEqual(
        boxRows(handle),
        2,
        "120 characters at 74 columns wrap onto a second row, and the box
         grew to hold it",
      );

      let lines = handle.getLines(true);
      Test.assertEqual(
        Array.length(lines),
        24,
        "the frame is still exactly the terminal's height - the Flex(1)
         transcript gave the box the rows it needed",
      );
      Array.iteri(
        (i, line) =>
          Test.assertTrue(
            Element.visibleLength(line) <= 80,
            "row "
            ++ string_of_int(i)
            ++ " fits the 80-column terminal (an overflowing row would be
             hard-wrapped by the terminal and take the right border with it)",
          ),
        lines,
      );

      /* Every row of the box is closed on the right. */
      let (top, bottom) = boxBorders(handle);
      for (y in top to bottom) {
        let line = lines[y];
        Test.assertEqual(
          Element.visibleLength(line),
          80,
          "box row " ++ string_of_int(y) ++ " spans the terminal",
        );
        Test.assertTrue(
          String.length(line) >= 3
          && String.sub(line, String.length(line) - 3, 3) == "\xe2\x94\x82"
          || startsWith(line, "\xe2\x95\xad")
          || startsWith(line, "\xe2\x95\xb0"),
          "box row " ++ string_of_int(y) ++ " ends with its right border",
        );
      };
      handle.quit();
    });

    Test.run("submitting a wrapped prompt commits the whole thing", () => {
      let handle = start();
      Input.feedBytes(handle, longPrompt);
      Input.feedBytes(handle, "\r");
      Test.assertEqual(
        boxRows(handle),
        1,
        "the box shrank back to one row when the prompt was cleared",
      );

      /* Let the canned script run to the reply, which quotes the prompt back
         in a wrap=Wrap line - so the frame is read with its wraps undone. */
      handle.advanceTime(5000);
      let unwrapped =
        handle.getLines(true)
        |> Array.map(String.trim)
        |> Array.to_list
        |> String.concat(" ");
      Test.assertContains(
        unwrapped,
        "zebra",
        "the TAIL of the prompt reached the transcript: wrapping is display
         only, and the value submitted is the whole logical string",
      );
      Test.assertContains(
        unwrapped,
        "wrap wrap",
        "and so did its head",
      );
      handle.quit();
    });
  });

  Test.group("claude-code: the real binary, fed raw bytes", () => {
    Test.run("a whole typed line arrives one event per frame", () => {
      /* "hi\r" reaches the child in a single read(), so all three events are
       * decoded from one buffer; each must still see a fresh render or the
       * submit would close over pre-'h' state and commit "> i".
       *
       * NOTE: this asserts on the submit only, not on the canned reply. The
       * headless loop exits the moment stdin reaches EOF (see
       * startHeadlessLoop in lib/Runtime.re), and runExampleWithInput closes
       * the pipe right after writing - so the process never lives long
       * enough for the 350ms stream to tick. The in-process suite above
       * covers the stream with advanceTime; what this case pins is the
       * byte-fed delivery path.
       *
       * "> hi" appears in the printed FRAME now, not in a static stream
       * above it: the binary asks for ~screen=Fullscreen, but the headless
       * loop ignores screen modes entirely (there is no terminal to own), so
       * what comes out of the pipe is just the frames - transcript
       * included. */
      let out = Golden.runExampleWithInput("claude-code", "hi\r");
      Test.assertContains(out, "> hi", "the full line was submitted");
      Test.assertContains(
        out,
        "esc to interrupt",
        "and the mock agent started streaming",
      );
    });
  });
};
