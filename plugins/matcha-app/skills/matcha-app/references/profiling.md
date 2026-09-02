# Profiling a matcha app

Reference material for the `matcha-app` skill. Load this when you are asked
to make a matcha application faster, or need to back up a performance claim
about one — it covers turning on `Perf` tracing, recording a scripted
interaction, and reading the resulting summary.

Every layer above answers "is it correct". `lib/Perf.re` (matcha's own
performance module) answers "where did the time go", with attribution down to
the individual component. It is off by default, costs nothing off, and
**only ever writes files — never stdout or stderr** (interactive stdout is
escape bytes; stream-headless stdout is the frames the goldens parse).

## Turning it on

For a **binary**, set the destination in the environment:

```
MATCHA_TRACE=/tmp/before.json
```

For a **test**, drive it directly — and always restore, or the process-wide
`at_exit` hook writes a trace the next test never asked for:

```reason
let path = Filename.temp_file("trace-", ".json");
Sys.remove(path);
Fun.protect(~finally=() => Perf.disable(), () => {
  Perf.enable(path);
  /* ... drive the headless handle ... */
  Perf.flush();
});
```

## Recording an interaction

Profile a *script*, not a session: the same bytes each run, so before and
after are comparable. Both HANG TRAPS still apply — `timeout`,
`MATCHA_HEADLESS=1`, and stdin that reaches EOF, together. Run this from your
app's own repo root, against your own built binary:

```
printf '\033[B\033[B\033[B\033[Bq' | timeout 60 env \
  MATCHA_HEADLESS=1 MATCHA_WIDTH=200 MATCHA_HEIGHT=45 \
  MATCHA_TRACE=/tmp/before.json ./myapp > /dev/null
```

`\033[B` is arrow-down, `\033[A` arrow-up, `\033[C`/`\033[D` right/left. Set
the geometry explicitly: frame cost scales with it, and a default 80×24 will
not reproduce what the user sees.

## Reading the summary

Read `/tmp/before.json.summary.txt` (the trace itself is Chrome Trace Event
JSON — drop it into <https://ui.perfetto.dev> for a flame view when the
summary is not enough).

1. **`== slowest frames ==` first.** Each entry gives the frame's duration,
   its `phases:` split (`render`/`paint`/`effects`/`unmount-sweep`/…) and
   `top:`, the three most expensive non-phase spans inside it. If one frame
   is 50× the others, that frame is the bug.
2. **`== spans ==` second, and read the `self` column.** `total` includes
   nested work, so a component's `total` is mostly its children;
   `self` is the time actually spent in it.
3. Rows suffixed `~measuring` are the **layout measure pass**: a stack renders
   an `Auto` child twice per frame, once to measure it. A fat `~measuring`
   row means measuring is what costs, not painting.
4. `dispatch-key`/`dispatch-paste`/`dispatch-mouse` and `timers` run
   *between* frames, so they show up in the spans table only, never under a
   frame.

Component rows are labelled with the ppx `typeId` — `file:line:col` of the
`[@component] let make`, so a row points straight at a source line in your
app.

## Application spans

Wrap app-level work so it nests under the component that ran it:

```reason
Perf.span("tokenize-file", () => Highlight.highlight(~path, lines));
```

Names must not contain spaces (the summary is whitespace-column parsed);
hyphenate them.

## Below span level

`Perf` bottoms out at whatever you wrapped. To go finer, use macOS
`sample <pid>` or Instruments on the running binary. (`olly`/OCaml
`runtime_events` would be the native answer but is not installed in matcha's
own switch and would be a new opam dependency — check your own app's switch
before assuming it's available there either.)

## The rule

**Never claim a performance win without a before AND an after summary of the
same scripted interaction**, and quote both `== slowest frames ==` sections.
"It feels faster" is not a measurement, and neither is a single after-only
recording.
