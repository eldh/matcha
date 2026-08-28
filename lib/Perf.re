/*
 * Perf - performance tracing for Matcha applications.
 *
 * Records nested time spans (frames, render phases, individual components,
 * application-level spans) and, on flush, writes TWO files:
 *
 *   <path>                - Chrome Trace Event JSON. Loads in Perfetto
 *                           (https://ui.perfetto.dev) or chrome://tracing.
 *   <path>.summary.txt    - a plain-text digest meant to be READ by an agent:
 *                           a span table with self-time and the slowest
 *                           frames broken down by phase.
 *
 * Turning it on:
 *   - binaries: set MATCHA_TRACE=/path/to/trace.json in the environment.
 *   - tests:    Perf.enable(path) ... Perf.flush() ... Perf.disable().
 *
 * Everything is OFF by default and costs nothing when off: `span` with
 * tracing disabled is literally `f()`, and the per-component hot path in
 * Runtime is a bool read plus a branch.
 *
 * ============================ IMPORTANT RULES ============================
 *
 * 1. Perf NEVER writes to stdout or stderr. Interactive stdout is escape
 *    sequences and stream-headless stdout is the frames the golden tests
 *    parse - a single stray print would corrupt both. Traces go to FILES.
 *
 * 2. The clock is `Unix.gettimeofday` DIRECTLY, never
 *    `Hooks.instanceState.now`. The headless handle's clock is virtual
 *    (advanceTime jumps it by minutes at a time); reading it here would
 *    report a 60-second frame for a test that ran in 200µs.
 *
 * 3. Module-level mutable state - a DELIBERATE, documented exception to the
 *    "no module-level app state" gotcha in CLAUDE.md. Tracing is
 *    process-global tooling: one trace spans every headless handle a test
 *    run creates, and it must survive `Hooks.instanceState` being swapped
 *    out from under it. Single-threaded, like the rest of the runtime.
 *
 * Accepted caveats:
 *   - gettimeofday is wall-clock and therefore non-monotonic. An NTP step
 *     mid-recording shows up as a nonsense duration. In practice recordings
 *     last seconds; this has never been observed to matter.
 *   - `span` deliberately does NOT use Fun.protect (it is the hot path). An
 *     exception escaping `f` loses that span and its unclosed ancestors;
 *     at_exit still writes everything already recorded.
 *   - The "timers" spans around fireDueTimers fire once per loop iteration,
 *     so an idle interactive app records a stream of ~0-duration entries.
 *     Harmless noise; they aggregate into one row.
 */

/* One complete (ph:"X") trace event.
 *
 * evTs is microseconds since `epoch` (the moment tracing was enabled), NOT
 * since the Unix epoch - Chrome's trace viewer only cares that timestamps
 * share an origin, and small numbers are easier to read in the summary. */
type event = {
  evName: string,
  evTs: float, /* µs from epoch */
  evDur: float, /* µs */
  evMeasuring: bool, /* component rendered in a measure pass */
  evId: int /* stableId, or -1 when not a component */
};

let tracePath: ref(option(string)) = ref(None);
let events: ref(list(event)) = ref([]);
let epoch: ref(float) = ref(0.0);

/* Tracing is on exactly when a destination path is set. Pattern-matched, not
 * compared: `!=` on an option is OCaml's structural `<>`, which would be a
 * trap the day this option ever holds something closure-bearing. */
let isEnabled = (): bool =>
  switch (tracePath^) {
  | Some(_) => true
  | None => false
  };

/* Start recording to `path`. Discards anything recorded earlier and resets
 * the timestamp origin, so back-to-back recordings in one process do not
 * bleed into each other. Writes nothing yet - see `flush`. */
let enable = (path: string): unit => {
  tracePath := Some(path);
  events := [];
  epoch := Unix.gettimeofday();
};

/* Stop recording. Writes NOTHING - call `flush` first if the trace matters.
 * Tests should always end with this (via Fun.protect), otherwise the
 * at_exit hook writes a file the next test did not ask for. */
let disable = (): unit => {
  tracePath := None;
  events := [];
};

/* Microseconds since tracing was enabled. */
let nowUs = (): float => (Unix.gettimeofday() -. epoch^) *. 1_000_000.0;

let record =
    (~name: string, ~ts: float, ~dur: float, ~measuring: bool, ~id: int)
    : unit =>
  events :=
    [
      {evName: name, evTs: ts, evDur: dur, evMeasuring: measuring, evId: id},
      ...events^,
    ];

/* Time `f` under `name`. When tracing is off this is exactly `f()`: no
 * clock reads, no allocation beyond the closure the caller already built. */
let span = (name: string, f: unit => 'a): 'a =>
  switch (tracePath^) {
  | None => f()
  | Some(_) =>
    let t0 = nowUs();
    let result = f();
    record(~name, ~ts=t0, ~dur=nowUs() -. t0, ~measuring=false, ~id=(-1));
    result;
  };

/* One rendered frame. The summary's "slowest frames" section is built from
 * these, and the frame count is just how many of them there are. */
let frame = (f: unit => 'a): 'a => span("frame", f);

/* A zero-duration marker. */
let instant = (name: string): unit =>
  switch (tracePath^) {
  | None => ()
  | Some(_) => record(~name, ~ts=nowUs(), ~dur=0.0, ~measuring=false, ~id=(-1))
  };

/* The per-component hot path, called once per component per render pass.
 * Deliberately closure-free: the caller reads `isEnabled()` once, stashes a
 * `nowUs()` in a local, and calls this at the end - so a component render
 * costs one bool read when tracing is off, and never allocates a closure.
 *
 * `name` must be the ppx typeId (a source location), never the component's
 * tree path: paths use control characters (\031/\030/\029) as separators,
 * which would have to be escaped in JSON and would make every instance its
 * own unreadable row. `id` is the stableId, so a specific instance is still
 * identifiable in the trace viewer.
 *
 * Unguarded on purpose - the caller's `if (perfOn)` is the guard. */
let recordComponent =
    (~name: string, ~t0: float, ~measuring: bool, ~id: int): unit =>
  record(~name, ~ts=t0, ~dur=nowUs() -. t0, ~measuring, ~id);

/* ==========================================================================
 * Writing
 * ======================================================================== */

let escapeJson = (s: string): string => {
  let buf = Buffer.create(String.length(s) + 8);
  String.iter(
    c =>
      switch (c) {
      | '\\' => Buffer.add_string(buf, "\\\\")
      | '"' => Buffer.add_string(buf, "\\\"")
      | c when Char.code(c) < 0x20 => Buffer.add_char(buf, ' ')
      | c => Buffer.add_char(buf, c)
      },
    s,
  );
  Buffer.contents(buf);
};

/* Measure-pass renders are a separate row: a stack visits an Auto child
 * twice per frame, and the measure pass is real, attributable cost that
 * would otherwise silently double a component's total. */
let labelOf = (e: event): string =>
  if (e.evMeasuring) {
    e.evName ++ "~measuring";
  } else {
    e.evName;
  };

/* Sorted so the stack walk below sees a well-formed nesting: a parent always
 * comes before its children (ts asc), and when a parent and its first child
 * start in the same microsecond the longer one - the parent - comes first. */
let sortedEvents = (): list(event) =>
  List.sort(
    (a, b) =>
      if (a.evTs < b.evTs) {
        (-1);
      } else if (a.evTs > b.evTs) {
        1;
      } else if (a.evDur > b.evDur) {
        (-1);
      } else if (a.evDur < b.evDur) {
        1;
      } else {
        0;
      },
    events^,
  );

type stat = {
  mutable stCount: int,
  mutable stTotal: float,
  mutable stMax: float,
  mutable stSelf: float,
};

/* Containment slop, in µs. Two spans that start and end in the same
 * microsecond are common at this resolution; without the epsilon a child
 * whose end lands exactly on its parent's end would pop the parent. */
let eps = 1.0;

/* Aggregate per name, computing SELF time (total minus the time attributed
 * to nested spans) with a single stack walk over the sorted events. */
let computeStats = (evs: list(event)): Hashtbl.t(string, stat) => {
  let tbl: Hashtbl.t(string, stat) = Hashtbl.create(64);
  let stack: ref(list((event, ref(float)))) = ref([]);

  let close = ((e: event, childSum: ref(float))) => {
    let key = labelOf(e);
    let s =
      switch (Hashtbl.find_opt(tbl, key)) {
      | Some(s) => s
      | None =>
        let s = {stCount: 0, stTotal: 0.0, stMax: 0.0, stSelf: 0.0};
        Hashtbl.replace(tbl, key, s);
        s;
      };
    s.stCount = s.stCount + 1;
    s.stTotal = s.stTotal +. e.evDur;
    s.stSelf = s.stSelf +. (e.evDur -. childSum^);
    if (e.evDur > s.stMax) {
      s.stMax = e.evDur;
    };
  };

  List.iter(
    (e: event) => {
      let eEnd = e.evTs +. e.evDur;
      let rec popClosed = () =>
        switch (stack^) {
        | [] => ()
        | [(top, _) as entry, ...rest] =>
          let topEnd = top.evTs +. top.evDur;
          /* Two ways `e` is NOT a child of `top`: it outlives it, or it
             starts after it ended. The second test is not redundant - the
             epsilon in the first would otherwise adopt a zero-duration span
             recorded in the microsecond right after a span closed (a
             dispatch-key immediately following a frame, say). */
          if (eEnd > topEnd +. eps || e.evTs > topEnd) {
            stack := rest;
            close(entry);
            popClosed();
          };
        };
      popClosed();
      switch (stack^) {
      | [(_, childSum), ..._] => childSum := childSum^ +. e.evDur
      | [] => ()
      };
      stack := [(e, ref(0.0)), ...stack^];
    },
    evs,
  );
  List.iter(close, stack^);
  tbl;
};

/* The render phases wrapped in Runtime, in the order they run. Anything else
 * inside a frame is a component or an application span. */
let phaseNames = [
  "render",
  "static-drain",
  "paint",
  "effects",
  "unmount-sweep",
  "collect-handlers",
  "focus-commit",
  "mouse-sync",
];

let isPhase = (name: string): bool => List.mem(name, phaseNames);

let writeTrace = (oc: out_channel, evs: list(event)): unit => {
  output_string(oc, "[\n");
  let first = ref(true);
  List.iter(
    (e: event) => {
      if (! first^) {
        output_string(oc, ",\n");
      };
      first := false;
      let args =
        if (e.evMeasuring || e.evId >= 0) {
          Printf.sprintf(
            ",\"args\":{\"measuring\":%b,\"id\":%d}",
            e.evMeasuring,
            e.evId,
          );
        } else {
          "";
        };
      output_string(
        oc,
        Printf.sprintf(
          "{\"name\":\"%s\",\"ph\":\"X\",\"pid\":1,\"tid\":1,\"ts\":%.1f,\"dur\":%.1f%s}",
          escapeJson(e.evName),
          e.evTs,
          e.evDur,
          args,
        ),
      );
    },
    evs,
  );
  output_string(oc, "\n]\n");
};

let writeSummary =
    (oc: out_channel, path: string, evs: list(event), wallMs: float): unit => {
  let frames = List.filter((e: event) => e.evName == "frame", evs);
  output_string(oc, "MATCHA PERF SUMMARY\n");
  Printf.fprintf(oc, "trace: %s\n", path);
  Printf.fprintf(oc, "events: %d\n", List.length(evs));
  Printf.fprintf(oc, "frames: %d\n", List.length(frames));
  Printf.fprintf(oc, "wall: %.1f ms\n", wallMs);
  output_string(
    oc,
    "\nNote: dispatch-key/dispatch-paste/dispatch-mouse and timers spans sit\n"
    ++ "BETWEEN frames, so they appear in the global spans table only, never\n"
    ++ "under a frame. Rows suffixed ~measuring are layout measure passes.\n",
  );

  /* ---- spans table ---- */
  let tbl = computeStats(evs);
  let rows =
    Hashtbl.fold((name, s, acc) => [(name, s), ...acc], tbl, [])
    |> List.sort(((_, a: stat), (_, b: stat)) =>
         if (a.stTotal > b.stTotal) {
           (-1);
         } else if (a.stTotal < b.stTotal) {
           1;
         } else {
           0;
         }
       );
  output_string(oc, "\n== spans (ms) ==\n");
  Printf.fprintf(
    oc,
    "%-40s %6s %10s %10s %10s %10s\n",
    "name",
    "count",
    "total",
    "mean",
    "max",
    "self",
  );
  List.iter(
    ((name, s: stat)) =>
      Printf.fprintf(
        oc,
        "%-40s %6d %10.1f %10.2f %10.1f %10.1f\n",
        name,
        s.stCount,
        s.stTotal /. 1000.0,
        s.stTotal /. 1000.0 /. float_of_int(max(1, s.stCount)),
        s.stMax /. 1000.0,
        s.stSelf /. 1000.0,
      ),
    rows,
  );

  /* ---- slowest frames ---- */
  output_string(oc, "\n== slowest frames (ms) ==\n");
  let slowest =
    List.sort(
      (a: event, b: event) =>
        if (a.evDur > b.evDur) {
          (-1);
        } else if (a.evDur < b.evDur) {
          1;
        } else {
          0;
        },
      frames,
    );
  let rec take = (n, l) =>
    switch (n, l) {
    | (0, _)
    | (_, []) => []
    | (n, [x, ...rest]) => [x, ...take(n - 1, rest)]
    };
  List.iteri(
    (i, f: event) => {
      Printf.fprintf(
        oc,
        "#%d start=%.1f dur=%.1f\n",
        i + 1,
        f.evTs /. 1000.0,
        f.evDur /. 1000.0,
      );
      let fEnd = f.evTs +. f.evDur;
      /* Contained in this frame: starts inside it and ends inside it. The
         start test is strict (no epsilon) so that a zero-duration span
         recorded just after the frame closed is not adopted by it. */
      let inside =
        List.filter(
          (e: event) =>
            !(e.evTs == f.evTs && e.evDur == f.evDur && e.evName == "frame")
            && e.evTs >= f.evTs
            && e.evTs <= fEnd
            && e.evTs +. e.evDur <= fEnd +. eps,
          evs,
        );
      let sumOf = name =>
        List.fold_left(
          (acc, e: event) =>
            if (e.evName == name) {
              acc +. e.evDur;
            } else {
              acc;
            },
          0.0,
          inside,
        );
      let phases =
        List.filter_map(
          name => {
            let total = sumOf(name);
            if (total > 0.0) {
              Some(Printf.sprintf("%s=%.1f", name, total /. 1000.0));
            } else {
              None;
            };
          },
          phaseNames,
        );
      switch (phases) {
      | [] => ()
      | ps => Printf.fprintf(oc, "  phases: %s\n", String.concat(" ", ps))
      };
      /* Non-phase spans inside this frame: components and app spans. */
      let agg: Hashtbl.t(string, stat) = Hashtbl.create(32);
      List.iter(
        (e: event) =>
          if (!isPhase(e.evName) && e.evName != "frame") {
            let key = labelOf(e);
            let s =
              switch (Hashtbl.find_opt(agg, key)) {
              | Some(s) => s
              | None =>
                let s = {stCount: 0, stTotal: 0.0, stMax: 0.0, stSelf: 0.0};
                Hashtbl.replace(agg, key, s);
                s;
              };
            s.stCount = s.stCount + 1;
            s.stTotal = s.stTotal +. e.evDur;
          },
        inside,
      );
      let top =
        Hashtbl.fold((name, s, acc) => [(name, s), ...acc], agg, [])
        |> List.sort(((_, a: stat), (_, b: stat)) =>
             if (a.stTotal > b.stTotal) {
               (-1);
             } else if (a.stTotal < b.stTotal) {
               1;
             } else {
               0;
             }
           )
        |> take(3);
      switch (top) {
      | [] => ()
      | ts =>
        Printf.fprintf(
          oc,
          "  top: %s\n",
          String.concat(
            " ",
            List.map(
              ((name, s: stat)) =>
                Printf.sprintf(
                  "%s=%.1f(%d)",
                  name,
                  s.stTotal /. 1000.0,
                  s.stCount,
                ),
              ts,
            ),
          ),
        )
      };
    },
    take(5, slowest),
  );
};

/* Write the trace and its summary, then clear the recorded events. A no-op
 * when tracing is off or nothing was recorded, so the at_exit hook is silent
 * for every process that never enabled tracing.
 *
 * I/O failures are swallowed: this runs from at_exit, and a perf tool must
 * never be the reason an application dies on the way out. */
let flush = (): unit =>
  switch (tracePath^, events^) {
  | (None, _)
  | (_, []) => ()
  | (Some(path), _) =>
    let wallMs = nowUs() /. 1000.0;
    let evs = sortedEvents();
    events := [];
    try({
      let oc = open_out(path);
      Fun.protect(~finally=() => close_out_noerr(oc), () =>
        writeTrace(oc, evs)
      );
      let oc2 = open_out(path ++ ".summary.txt");
      Fun.protect(~finally=() => close_out_noerr(oc2), () =>
        writeSummary(oc2, path, evs, wallMs)
      );
    }) {
    | _ => ()
    };
  };

/* MATCHA_TRACE=<path> turns tracing on for a whole binary (same env-read
 * pattern as TextArea's MATCHA_HEADLESS check). Registered unconditionally:
 * a test that calls `enable` later still gets its trace written if it
 * forgets to flush. */
let () =
  switch (Sys.getenv_opt("MATCHA_TRACE")) {
  | Some(p) when String.length(p) > 0 => enable(p)
  | _ => ()
  };

let () = at_exit(flush);
