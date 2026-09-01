/*
 * examples/command-menu, driven headlessly (B2).
 *
 * The example is the hardest overlay case on purpose - a log that keeps
 * streaming while a modal owns the keyboard - so these are the cases that
 * only the whole stack together can answer:
 *
 *   - the palette composites over a MOVING frame (advanceTime with it open
 *     still grows the log), and
 *   - the palette's keys are CAPTURED (an arrow moves the selection and does
 *     not scroll the ScrollView underneath), while the app's globals are not.
 *
 * The component is started in-process from the example's own library, so
 * this drives the exact code the binary runs.
 */
open Matcha;

/* 100x30, not 80x24: 80x24 is the constraints default, the headless-config
 * default and the getSize fallback all at once. */
let width = 100;
let height = 30;

let start = (): Runtime.headlessHandle =>
  Runtime.startHeadless(~config={width, height}, (module CommandMenuApp.App));

/* "40 lines" out of the status bar - how the test observes that the stream
 * is (or is not) still running underneath the palette. */
let lineCount = (handle: Runtime.headlessHandle): int => {
  let out = handle.getOutput(true);
  let needle = " lines";
  let hlen = String.length(out);
  let nlen = String.length(needle);
  let found = ref(-1);
  for (i in 0 to hlen - nlen) {
    if (found^ < 0 && String.sub(out, i, nlen) == needle) {
      found := i;
    };
  };
  if (found^ < 0) {
    -1;
  } else {
    /* Walk back over the digits. */
    let e = found^;
    let s = ref(e);
    while (s^ > 0 && out.[s^ - 1] >= '0' && out.[s^ - 1] <= '9') {
      s := s^ - 1;
    };
    s^ == e ? (-1) : int_of_string(String.sub(out, s^, e - s^));
  };
};

/* The first log row on screen - what must NOT move when a captured key is
 * pressed. */
let firstLogRow = (handle: Runtime.headlessHandle): string =>
  String.trim(handle.getLines(true)[1]);

let openPalette = (handle: Runtime.headlessHandle): unit =>
  /* Ctrl+K is byte 11 (decimal), which no terminal claims for itself. Fed
     as raw bytes through a real InputDecoder, the way a terminal delivers
     it. */
  Input.feedBytes(handle, "\011");

let run = () =>
  Test.group("Example: command-menu", () => {
    Test.run("the first frame is the log, with no dialog on it", () => {
      let handle = start();
      let out = handle.getOutput(true);
      Test.assertContains(out, "log stream", "the pane header");
      Test.assertContains(out, "RUNNING", "the status bar says the stream is live");
      Test.assertContains(out, "40 lines", "seeded with 40 lines, so frame one has content");
      Test.assertFalse(
        Test.contains(out, Element.BoxChars.topLeft),
        "no border anywhere - the <Modal> is closed and costs nothing",
      );
      handle.quit();
    });

    Test.run("Ctrl+K opens the palette and focuses its input", () => {
      let handle = start();
      let before = Array.length(handle.getLines(true));
      openPalette(handle);
      let out = handle.getOutput(true);
      Test.assertContains(out, "Commands", "the dialog's title is on screen");
      Test.assertContains(out, "Pause / resume the stream", "and its first command");
      Test.assertEqual(
        handle.getFocusedId(),
        Some("cmd-input"),
        "focus moved into the layer, by autoFocus",
      );
      Test.assertEqual(
        Array.length(handle.getLines(true)),
        before,
        "and the frame is the same height - the dialog took no layout row",
      );
      handle.quit();
    });

    Test.run("the log keeps streaming while the palette is open", () => {
      /* THE POINT OF THIS EXAMPLE. A modal owns the keyboard, not the clock:
         the useInterval underneath keeps firing, and every one of those
         frames is composited afresh. */
      let handle = start();
      openPalette(handle);
      let before = lineCount(handle);
      Test.assertEqual(before, 40, "40 lines when the palette opened");
      handle.advanceTime(1000);
      let after = lineCount(handle);
      Test.assertEqual(after, 45, "five more 200ms ticks landed with the modal open");
      Test.assertContains(
        handle.getOutput(true),
        "Pause / resume the stream",
        "and the palette is still composited over the newer frames",
      );
      handle.quit();
    });

    Test.run("typing narrows the list", () => {
      let handle = start();
      openPalette(handle);
      Input.feedBytes(handle, "filter");
      let out = handle.getOutput(true);
      Test.assertContains(out, "Filter: warnings only", "a matching command survives");
      Test.assertFalse(
        Test.contains(out, "Toggle dim theme"),
        "a non-matching one is gone",
      );
      Test.assertContains(out, "filter", "and the query is echoed in the input row");
      handle.quit();
    });

    Test.run("Arrow_down moves the selection and does NOT scroll the log", () => {
      let handle = start();
      openPalette(handle);
      let logBefore = firstLogRow(handle);
      Test.assertContains(
        handle.getOutput(true),
        "> Pause / resume the stream",
        "the first command starts selected",
      );
      handle.sendKey(Key.Arrow_down, Key.noModifiers);
      let out = handle.getOutput(true);
      Test.assertContains(out, "> Show all levels", "the selection moved down one");
      Test.assertFalse(
        Test.contains(out, "> Pause / resume the stream"),
        "and off the first command",
      );
      Test.assertEqualStr(
        firstLogRow(handle),
        logBefore,
        "the <ScrollView> underneath did not move: its useInput is captured "
        ++ "by the layer, which is the whole capture rule in one assertion",
      );
      handle.quit();
    });

    Test.run("Enter runs the command, closes the palette and restores focus", () => {
      let handle = start();
      /* Give the log pane focus first, so restored-focus and first-entry-of-
         the-ring are distinguishable answers. */
      Input.pressTab(handle);
      Test.assertEqual(handle.getFocusedId(), Some("log"), "the log pane holds focus");

      openPalette(handle);
      Test.assertEqual(handle.getFocusedId(), Some("cmd-input"), "the palette took it");
      /* The first command is "Pause / resume the stream". */
      handle.sendKey(Key.Enter, Key.noModifiers);

      let out = handle.getOutput(true);
      Test.assertContains(out, "PAUSED", "the command had a visible effect");
      Test.assertFalse(
        Test.contains(out, Element.BoxChars.topLeft),
        "and the palette closed itself",
      );
      Test.assertEqual(
        handle.getFocusedId(),
        Some("log"),
        "focus came back to the pane that had it, not to the ring's first entry",
      );

      let before = lineCount(handle);
      handle.advanceTime(2000);
      Test.assertEqual(
        lineCount(handle),
        before,
        "PAUSED really means paused - the interval is deregistered (~ms=0), "
        ++ "so advancing two seconds adds nothing",
      );
      handle.quit();
    });

    Test.run("Esc closes the palette without running anything", () => {
      let handle = start();
      openPalette(handle);
      Test.assertContains(handle.getOutput(true), "Commands", "open");
      Input.feedBytes(handle, "\027");
      let out = handle.getOutput(true);
      Test.assertFalse(Test.contains(out, Element.BoxChars.topLeft), "closed");
      Test.assertContains(out, "RUNNING", "and nothing was executed on the way out");
      handle.quit();
    });

    Test.run("Ctrl+C is still delivered while the palette is open", () => {
      /* The unquittable-app guard, at the application level: the global quit
         binding is a useKeyDown, so the layer never captures it. */
      let handle = start();
      openPalette(handle);
      Test.assertTrue(handle.isRunning(), "running with the palette open");
      Input.feedBytes(handle, "\003");
      Test.assertFalse(
        handle.isRunning(),
        "Ctrl+C quit the app from under the modal - raw mode has no ISIG, so "
        ++ "an app that cannot receive this key cannot be exited",
      );
      handle.quit();
    });
  });
