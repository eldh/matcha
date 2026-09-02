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

/* Run a built example binary headlessly with a stdin pipe that is opened and
 * then NEVER written to and NEVER closed - the shape of every caller that
 * forgets `< /dev/null` - and report how the child ended.
 *
 * Returns (exited, code, elapsedSeconds). `exited` is false if the child had
 * to be killed at the deadline, which is the regression this exists to
 * catch. It polls with WNOHANG rather than blocking in waitpid, so a broken
 * MATCHA_HEADLESS_MAX_MS FAILS this test instead of hanging the suite. */
let runWithOpenStdin =
    (~extraEnv: list(string), ~killAfter: float, name: string)
    : (bool, int, float) => {
  let relPath = "../examples/" ++ name ++ "/main.exe";
  let path =
    if (Sys.file_exists(relPath)) {
      relPath;
    } else {
      "_build/default/examples/" ++ name ++ "/main.exe";
    };

  let (stdinRead, stdinWrite) = Unix.pipe();
  Unix.set_close_on_exec(stdinWrite);
  let devnull = Unix.openfile("/dev/null", [Unix.O_WRONLY], 0o644);

  let pathEnv =
    switch (Sys.getenv_opt("PATH")) {
    | Some(p) => p
    | None => "/usr/bin:/bin"
    };
  let env =
    Array.append(
      [|"PATH=" ++ pathEnv, "MATCHA_HEADLESS=1"|],
      Array.of_list(extraEnv),
    );

  let started = Unix.gettimeofday();
  let pid =
    Unix.create_process_env(path, [|path|], env, stdinRead, devnull, devnull);
  Unix.close(stdinRead);

  /* Poll for the child, up to killAfter seconds. */
  let result = ref(None);
  while (result^ == None && Unix.gettimeofday() -. started < killAfter) {
    switch (Unix.waitpid([Unix.WNOHANG], pid)) {
    | (0, _) => ignore(Unix.select([], [], [], 0.02))
    | (_, Unix.WEXITED(c)) => result := Some(c)
    | (_, _) => result := Some(-1)
    | exception _ => result := Some(-1)
    };
  };
  let elapsed = Unix.gettimeofday() -. started;

  /* The pipe's write end is what kept the child's stdin open; closing it
     only now means the child never saw EOF while we were waiting. */
  Unix.close(stdinWrite);
  Unix.close(devnull);

  switch (result^) {
  | Some(c) => (true, c, elapsed)
  | None =>
    (try(Unix.kill(pid, Sys.sigkill)) {
     | _ => ()
     });
    (try(ignore(Unix.waitpid([], pid))) {
     | _ => ()
     });
    (false, -1, elapsed);
  };
};

let run = () => {
  /* An INLINE app must be SHORTER than the terminal: the live region is
     painted at the cursor, so a frame with as many rows as the terminal
     scrolls the user's prompt and scrollback away, and nothing can un-scroll
     a terminal. Runtime.inlineFrameTooTall holds that whole decision, and is
     pure so the boundary can be checked without a terminal. The end-to-end
     half - a real binary on a real pty refusing to paint, and the same
     binary one row shorter painting normally - is in test/pty_tests.re. */
  Test.group("Runtime: inline frame height guard", () => {
    Test.run("one row short of the terminal is allowed", () => {
      Test.assertFalse(
        Runtime.inlineFrameTooTall(~frameHeight=29, ~termHeight=30),
        "a 29-row frame fits above the cursor on a 30-row terminal",
      );
    });

    Test.run("EQUAL is already too tall", () => {
      /* The boundary the rule turns on: a frame that exactly fills the
         terminal still scrolls it, because the region's last line is at the
         cursor and the cursor is not at row 1. */
      Test.assertTrue(
        Runtime.inlineFrameTooTall(~frameHeight=30, ~termHeight=30),
        "a 30-row frame on a 30-row terminal must be refused",
      );
    });

    Test.run("taller than the terminal is too tall", () => {
      Test.assertTrue(
        Runtime.inlineFrameTooTall(~frameHeight=31, ~termHeight=30),
        "a 31-row frame on a 30-row terminal must be refused",
      );
    });

    Test.run("a terminal height we do not have never refuses", () => {
      /* A failed ioctl must not become a crash: with no trustworthy height
         there is no evidence the frame is too tall. */
      Test.assertFalse(
        Runtime.inlineFrameTooTall(~frameHeight=40, ~termHeight=0),
        "termHeight 0 (unknown size) allows any frame",
      );
      Test.assertFalse(
        Runtime.inlineFrameTooTall(~frameHeight=40, ~termHeight=-1),
        "a negative termHeight allows any frame too",
      );
    });

    Test.run("an empty frame is never too tall", () => {
      Test.assertFalse(
        Runtime.inlineFrameTooTall(~frameHeight=0, ~termHeight=30),
        "zero rows fit anywhere",
      );
    });
  });

  /* The headless stream loop reads stdin until EOF, so a caller that hands
     it a pipe nobody ever closes hangs forever - the hazard behind every
     `timeout N ... < /dev/null` in the docs. MATCHA_HEADLESS_MAX_MS bounds
     the loop by wall clock instead.

     Spawned rather than run in-process: the loop owns stdin and exits the
     process, so there is nothing in-process to call. The FIRST case is what
     makes the second one mean something - it shows the hang is real and that
     the harness would notice it. */
  Test.group("Runtime: MATCHA_HEADLESS_MAX_MS bounds the headless loop", () => {
    Test.run("without it, a pipe that never closes runs forever", () => {
      let (exited, _, _) =
        runWithOpenStdin(
          ~extraEnv=["MATCHA_WIDTH=100", "MATCHA_HEIGHT=20"],
          ~killAfter=1.0,
          "counter",
        );
      Test.assertFalse(
        exited,
        "the unbounded loop is still running after 1s - if this ever passes, "
        ++ "the bounded case below has stopped proving anything",
      );
    });

    Test.run("with it, the loop leaves cleanly on its own", () => {
      let (exited, code, elapsed) =
        runWithOpenStdin(
          ~extraEnv=[
            "MATCHA_WIDTH=100",
            "MATCHA_HEIGHT=20",
            "MATCHA_HEADLESS_MAX_MS=300",
          ],
          ~killAfter=6.0,
          "counter",
        );
      Test.assertTrue(exited, "the bounded loop exited on its own");
      Test.assertEqual(code, 0, "and exited cleanly");
      Test.assertTrue(
        elapsed < 3.0,
        "a 300ms budget must not take seconds (took "
        ++ string_of_float(elapsed)
        ++ "s)",
      );
    });
  });

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
