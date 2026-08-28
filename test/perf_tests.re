/*
 * Tests for lib/Perf.re - the performance tracer.
 *
 * Three properties matter more than the numbers themselves, and each has a
 * test below:
 *   - OFF is free and invisible (1) and writes nothing (1).
 *   - The summary is PARSEABLE by an agent: fixed headers, and every span row
 *     ends in five numeric columns (2).
 *   - Tracing cannot change what an application prints (6) and cannot be
 *     fooled by the headless virtual clock (5).
 *
 * EVERY test that calls Perf.enable restores with Perf.disable through
 * Fun.protect. Leaving tracing on would make the process-wide at_exit hook
 * write a trace file no later test asked for.
 */
open Matcha;

/* A scratch path that does not exist yet: temp_file creates the file, and
 * Perf must be the thing that creates it. */
let scratchPath = (prefix: string): string => {
  let p = Filename.temp_file(prefix, ".json");
  Sys.remove(p);
  p;
};

let cleanup = (path: string): unit => {
  let rm = p =>
    if (Sys.file_exists(p)) {
      try(Sys.remove(p)) {
      | _ => ()
      };
    };
  rm(path);
  rm(path ++ ".summary.txt");
};

let readFile = (path: string): string => {
  let ic = open_in_bin(path);
  Fun.protect(~finally=() => close_in_noerr(ic), () =>
    really_input_string(ic, in_channel_length(ic))
  );
};

/* Whitespace-separated fields of a line. The summary is built so that this
 * is enough to parse it: no name ever contains a space (phases are
 * hyphenated, component typeIds are file:line:col source locations). */
let fields = (line: string): list(string) =>
  String.split_on_char(' ', line) |> List.filter(s => String.length(s) > 0);

let lastN = (n: int, l: list('a)): list('a) => {
  let len = List.length(l);
  List.filteri((i, _) => i >= len - n, l);
};

let lines = (s: string): list(string) => String.split_on_char('\n', s);

/* Find the spans-table row for `name` and return (count, total, mean, max,
 * self) from its last five columns. */
let spanRow = (summary: string, name: string): option((int, float, float, float, float)) => {
  let rec go = ls =>
    switch (ls) {
    | [] => None
    | [line, ...rest] =>
      let fs = fields(line);
      switch (fs) {
      | [head, ..._] when head == name =>
        switch (lastN(5, fs)) {
        | [c, total, mean, mx, self] =>
          Some((
            int_of_string(c),
            float_of_string(total),
            float_of_string(mean),
            float_of_string(mx),
            float_of_string(self),
          ))
        | _ => None
        }
      | _ => go(rest)
      };
    };
  go(lines(summary));
};

/* Duration of the slowest frame, from the "#1 start=.. dur=.." line. */
let slowestFrameMs = (summary: string): option(float) => {
  let rec go = ls =>
    switch (ls) {
    | [] => None
    | [line, ...rest] =>
      switch (fields(line)) {
      | [head, _start, dur]
          when String.length(head) > 1 && head.[0] == '#' && String.length(dur) > 4 =>
        Some(float_of_string(String.sub(dur, 4, String.length(dur) - 4)))
      | _ => go(rest)
      }
    };
  go(lines(summary));
};

let countChar = (s: string, c: char): int => {
  let n = ref(0);
  String.iter(ch => if (ch == c) {n := n^ + 1}, s);
  n^;
};

let trim = (s: string): string => String.trim(s);

/* ============================================================================
 * Fixtures
 * ============================================================================ */

/* A CHILD component, so the tree actually contains an Element.Component node
 * the renderer can time. The root of a headless app is invoked through
 * C.make() and never becomes a Component node, so a one-module fixture would
 * record no component spans at all. */
module PerfLabel = {
  [@component]
  let make = (~count: int) => <Text> {"Count: " ++ string_of_int(count)} </Text>;
};

module PerfCounter = {
  [@component]
  let make = () => {
    let (count, setCount) = Hooks.useState(0);
    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Char('+') => setCount(count + 1)
      | _ => ()
      }
    );
    <VStack>
      <PerfLabel count />
      <Text> "traced" </Text>
    </VStack>;
  };
};

module PerfTicker = {
  [@component]
  let make = () => {
    let (n, setN) = Hooks.useState(0);
    Hooks.useInterval(() => setN(n + 1), ~ms=100);
    <Text> {"Tick: " ++ string_of_int(n)} </Text>;
  };
};

/* ============================================================================
 * Tests
 * ============================================================================ */

let run = () => {
  Test.group("Perf tracing", () => {
    Test.run("disabled, span is a transparent no-op and flush writes nothing", () => {
      let path = scratchPath("matcha-perf-off-");
      Fun.protect(
        ~finally=
          () => {
            Perf.disable();
            cleanup(path);
          },
        () => {
          Perf.disable();
          Test.assertFalse(Perf.isEnabled(), "tracing starts off");
          let v = Perf.span("never-recorded", () => 21 * 2);
          Test.assertEqual(v, 42, "span returns f()'s value untouched");
          Perf.flush();
          Test.assertFalse(
            Sys.file_exists(path),
            "flush with tracing off wrote no trace",
          );

          /* And disabling an ENABLED recording discards it: nothing on disk. */
          Perf.enable(path);
          Perf.span("recorded-then-dropped", () => ());
          Perf.disable();
          Perf.flush();
          Test.assertFalse(
            Sys.file_exists(path),
            "disable() drops the recording; flush writes nothing after it",
          );
        },
      );
    });

    Test.run("nested spans produce a parseable summary with self time", () => {
      let path = scratchPath("matcha-perf-nest-");
      Fun.protect(
        ~finally=
          () => {
            Perf.disable();
            cleanup(path);
          },
        () => {
          Perf.enable(path);
          Test.assertTrue(Perf.isEnabled(), "enable turns tracing on");
          Perf.span("outer-span", () => {
            Unix.sleepf(0.005);
            Perf.span("inner-span", () => Unix.sleepf(0.005));
          });
          Perf.flush();

          Test.assertTrue(
            Sys.file_exists(path ++ ".summary.txt"),
            "flush wrote the summary next to the trace",
          );
          let summary = readFile(path ++ ".summary.txt");
          Test.assertContains(summary, "MATCHA PERF SUMMARY", "summary header");
          Test.assertContains(summary, "trace: " ++ path, "summary names the trace");
          Test.assertContains(summary, "== spans (ms) ==", "spans section");
          Test.assertContains(summary, "== slowest frames (ms) ==", "frames section");

          switch (spanRow(summary, "outer-span")) {
          | None => Test.assertTrue(false, "no outer-span row in the spans table")
          | Some((count, total, _mean, mx, self)) =>
            Test.assertEqual(count, 1, "outer-span ran once");
            Test.assertTrue(total >= 9.0, "outer-span total covers both sleeps");
            Test.assertTrue(mx >= 9.0, "outer-span max equals its only run");
            Test.assertTrue(
              self < total,
              "self time excludes the nested inner-span",
            );
            Test.assertTrue(self >= 0.0, "self time is not negative");
          };
          switch (spanRow(summary, "inner-span")) {
          | None => Test.assertTrue(false, "no inner-span row in the spans table")
          | Some((count, total, _, _, self)) =>
            Test.assertEqual(count, 1, "inner-span ran once");
            Test.assertTrue(total >= 4.0, "inner-span total covers its sleep");
            Test.assertTrue(
              self >= total -. 1.0,
              "a leaf span's self time is its total",
            );
          };
        },
      );
    });

    Test.run("the trace file is structurally valid Chrome Trace JSON", () => {
      let path = scratchPath("matcha-perf-json-");
      Fun.protect(
        ~finally=
          () => {
            Perf.disable();
            cleanup(path);
          },
        () => {
          Perf.enable(path);
          Perf.span("json-probe", () => Perf.instant("json-marker"));
          Perf.flush();

          let json = trim(readFile(path));
          Test.assertTrue(String.length(json) > 2, "trace is non-empty");
          Test.assertTrue(json.[0] == '[', "trace starts with [");
          Test.assertTrue(
            json.[String.length(json) - 1] == ']',
            "trace ends with ]",
          );
          Test.assertEqual(
            countChar(json, '{'),
            countChar(json, '}'),
            "braces are balanced",
          );
          Test.assertEqual(
            countChar(json, '"') mod 2,
            0,
            "quote count is even",
          );
          Test.assertContains(json, "\"ph\":\"X\"", "complete events");
          Test.assertContains(json, "json-probe", "the span name is in the trace");
          Test.assertContains(json, "json-marker", "the instant is in the trace");
        },
      );
    });

    Test.run("a traced headless run attributes frames, phases and components", () => {
      let path = scratchPath("matcha-perf-headless-");
      Fun.protect(
        ~finally=
          () => {
            Perf.disable();
            cleanup(path);
          },
        () => {
          Perf.enable(path);
          let handle = Runtime.startHeadless((module PerfCounter));
          handle.sendKey(Key.Char('+'), Key.noModifiers);
          Test.assertContains(
            handle.getOutput(true),
            "Count: 1",
            "the traced app still behaves normally",
          );
          handle.quit();
          Perf.flush();

          let summary = readFile(path ++ ".summary.txt");
          Test.assertContains(summary, "frame", "frames were recorded");
          Test.assertContains(summary, "render", "the render phase was recorded");
          Test.assertContains(
            summary,
            "dispatch-key",
            "the key dispatch was recorded",
          );
          Test.assertContains(
            summary,
            "perf_tests",
            "component rows are labelled with the ppx typeId (file:line:col)",
          );
          switch (spanRow(summary, "frame")) {
          | None => Test.assertTrue(false, "no frame row in the spans table")
          | Some((count, _, _, _, _)) =>
            Test.assertTrue(count >= 2, "initial render plus the keypress frame")
          };
        },
      );
    });

    Test.run("the virtual headless clock cannot leak into a measurement", () => {
      let path = scratchPath("matcha-perf-clock-");
      Fun.protect(
        ~finally=
          () => {
            Perf.disable();
            cleanup(path);
          },
        () => {
          Perf.enable(path);
          let handle = Runtime.startHeadless((module PerfTicker));
          /* Jump the app's clock a full minute. Perf reads gettimeofday
             directly, so this must not show up as a 60-second frame. */
          handle.advanceTime(60_000);
          handle.quit();
          Perf.flush();

          let summary = readFile(path ++ ".summary.txt");
          switch (slowestFrameMs(summary)) {
          | None => Test.assertTrue(false, "no frames in the summary")
          | Some(ms) =>
            Test.assertTrue(
              ms < 5000.0,
              Printf.sprintf(
                "slowest frame is %.1fms - the virtual clock leaked (would be >= 60000)",
                ms,
              ),
            )
          };
        },
      );
    });

    Test.run("tracing an example changes nothing about its output", () => {
      let path = scratchPath("matcha-perf-golden-");
      Fun.protect(~finally=() => cleanup(path), () => {
        /* The SAME golden the untraced run is checked against. Perf writes
           files only - never stdout - so the frames are unchanged. */
        Golden.checkExample(~extraEnv=["MATCHA_TRACE=" ++ path], "counter");
        Test.assertTrue(
          Sys.file_exists(path),
          "the child process wrote its trace at exit",
        );
        Test.assertTrue(
          String.length(readFile(path)) > 2,
          "the child's trace is non-empty",
        );
        Test.assertContains(
          readFile(path ++ ".summary.txt"),
          "MATCHA PERF SUMMARY",
          "the child wrote a summary too",
        );
      });
    });
  });
};
