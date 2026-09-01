---
name: matcha-dev
description: Use when working on the Matcha framework's own ReasonML/OCaml source (lib/, ppx/, test/, examples/) — adding or modifying elements/components, writing or debugging headless tests, working with the [@component] PPX and JSX expansion, or diagnosing dune build errors specific to this repo's dune/ppx/warnings setup.
---

# Matcha framework development

Matcha is a React-like TUI framework (see `/CLAUDE.md` at the repo root for
the architecture map and toolchain). This skill covers the recipes you need
to actually change the framework's own code correctly.

## Adding a new element or component

Element types live in `lib/Element.re` as a single variant `Element.t`. If
you're adding a genuinely new element kind (not just a new JSX component
built out of existing ones — most new UI widgets should just be a
`[@component]` function, see below), you must update these places or the new
variant will type-check but silently do nothing (or crash) at render time:

1. **`lib/Element.re`: add the variant to `type t`** and give it a
   constructor function (see `text`, `vstack`, `sized`, etc. for the
   pattern).
2. **`lib/Runtime.re`: add a case to `renderElement` for BOTH modes.**
   `renderElement` is the one real renderer, and it has two modes selected by
   the `~measuring` argument:
   - **real mode** (`~measuring=false`, the default) applies layout —
     consume `constraints.availWidth`/`availHeight`, distribute space, pad or
     truncate. This is what a printed frame uses.
   - **measuring mode** (`~measuring=true`) must be **layout-free**: it
     returns the element's *natural* content size, which is how `Auto`-sized
     children get measured (`calculateChildSizes` → `measureContentSize`).
     Stacks have dedicated `when measuring` cases that just join children
     with newlines (VStack) or concatenate them (HStack), ignoring sizes,
     gaps, align and justify.

   For a leaf-ish variant, one case that threads `~measuring` through to its
   recursive calls covers both. For anything that *lays out* its children,
   write a `when measuring` case too — otherwise every `Auto` parent
   containing your element will measure it at full available width, not at
   its content width. Missing the measuring case is the classic "why is my
   `Auto` child suddenly as wide as the screen" bug.
3. **`lib/Element.re`: optionally add a case to `Element.render`** — the
   simple, non-layout renderer applications can call by hand on a subtree
   (see `SplitView.re` in `examples/people-list`). This is *no longer* on the
   measurement path: `Runtime.measureContentSize` goes through
   `renderElement(~measuring=true, ...)`, not `Element.render`. So skipping
   this case only affects hand-rolled `Element.render` calls, not `Auto`
   sizing. Add it anyway unless the variant is meaningless outside layout —
   `Element.render` is a total match, so omitting it is a build error, not a
   silent gap.
4. **`lib/Matcha.re` *and* `lib/Matcha.rei`: export it** if it should be
   public API — either as a `module Foo = Element.Foo` re-export (see how
   `Text`/`VStack`/`HStack`/`Sized`/`TextArea` are done, declared in the
   `.rei` as `module Foo: (module type of Element.Foo)`) or a `let` alias for
   a constructor/function (see `flex`/`percent`/`chars`, the color
   constructors). **Both files** — the `.rei` pins the public surface, so a
   `let` added only to `Matcha.re` is not exported. Forgetting this step is
   why `Element.Fragment` exists but isn't usable as `<Fragment>` after `open
   Matcha` today — don't repeat that gap for new additions unless it's
   deliberate.

For a new **component** (the common case — a reusable widget composed from
existing elements), you don't touch the variant type at all: write a
`[@component] let make = (~prop1, ~prop2) => { <VStack> ... </VStack> }` in
its own module, following the pattern in any `examples/*/main.re` file, and
export the module from `lib/Matcha.re` **and `lib/Matcha.rei`** only if it
belongs in the public library (most one-off app components, like the
examples' `Box` or `Section`, stay local to the app).

### Making a component responsive

Ask for the region, not the window: `useContainerSize()` returns the box of
the nearest enclosing `<Container>`, and the whole frame when there is none.
There is no `useLayout` — it was removed, because "the slot my immediate
parent gave me" is almost never the thing a responsive decision should
depend on.

Consequences worth knowing before you write the component:

- If it must react to **its own** allocated box (a bordered pane that draws
  itself to fill the slot, a box that stretches under `AlignStretch`), the
  caller has to wrap it: `<Sized size={Flex(1)}><Container><Pane
  /></Container></Sized>`. `examples/layout-demo` and
  `examples/layout-alignment` are the two worked examples in the repo.
- `<Sized>` and `<ScrollView>` are deliberately **not** boundaries, and
  `Percent(n)` stays parent-relative. Containers change queries only.
- `Element.Container` must stay layout-transparent. If you touch its case in
  `Runtime.renderElement`, `test/container_tests.re`'s with/without
  byte-identical comparison is the gate — plus the whole golden suite, since
  the migrated examples wrap real content.
- Test it at a **non-80x24** size and, where the point is that the container
  and the frame disagree, at a size where they visibly do (a `Chars(30)`
  pane in a 100-column frame).

Any change under 1–3 above changes rendering, so expect `dune runtest` to
report **golden** mismatches (`test/golden_tests.re` vs `test/goldens/*.txt`).
Read the diff before you regenerate — an unintended golden change is a bug.
When the change genuinely is intended, regenerate from the repo root with
`UPDATE_GOLDENS=1 dune exec test/run_tests.exe`.

## How the `[@component]` PPX expands

`ppx/ppx_component.ml` rewrites `[@component] let make = (~a: ta, ~b:
tb=?) => body` into:

```
type props('ty vars if any) = { a: ta, b: option(tb) };

let make = (props) => {
  let { a, b } = props;
  body
};

let componentTypeId = "<source location of this definition>";

let createElement = (~key=?, ~a: ta, ~b: tb=?, ()) =>
  Element.createComponent(~key?, ~typeId=componentTypeId, {a, b}, () =>
    make({a, b})
  );
```

(Check `test/ppx/*.expected` for the exact, current output — those snapshot
files are the ground truth, verified by `dune build @test/ppx/runtest`.)

Key points that matter when debugging or hand-writing the expansion (as
`lib/Element.re`'s own component modules do, since the PPX doesn't run over
`lib/`):

- Every `createElement` gets an **optional `~key` parameter** prepended,
  used for React-style identity across renders. Identity is
  **tree path + `typeId` + `key`** — nothing else. The `renderFn` pointer is
  *not* part of it (it's a fresh closure every frame), and neither are the
  props. So the same position with the same component type is the same
  instance with the same hook state, and a different `key` is a different
  instance; see `examples/keyed-switch` for why that matters.
- The ppx also emits a **`~typeId`** derived from the definition's source
  location, which is what keeps two *different* component types rendered at
  the same tree position from sharing hook state.
- `createElement` always ends in a **trailing `()`** (`Nolabel` unit param)
  so it works as a curried function with only labeled/optional arguments
  before it — this is what makes the JSX call `<Foo a b />` (which desugars
  to `Foo.createElement(~a, ~b, ())`) type-check regardless of how many
  optional props there are.
- A no-argument `[@component] let make = () => body` skips the props record
  entirely and generates `createElement = (~key=?, ()) =>
  Element.createComponent(~key?, ~typeId=componentTypeId, (), () => make())`.
- The props record is passed to `createComponent` and stored (erased to
  `Obj.t`) on the `Element.Component` node, but **nothing reads it today** —
  there is no props-based memoization; every visited component re-renders.
  The field is kept so a future memoization pass has something to compare.
- The generated `createElement`/`make` bindings get `[@warning "-32"]` on
  `createElement` to suppress "unused value" — remember dev profile treats
  warnings as errors (see Common dune errors below), so if you hand-write a
  component module, add the same attribute or make sure it's actually used.
- The PPX also rewrites JSX itself: any `Module.createElement(~prop=v,
  ~children=[...], ())` call — which is what the JSX syntax extension turns
  `<Module prop=v> child </Module>` into — gets its `children` argument
  unwrapped from a list to a single value when there's exactly one child, and
  passed through as a list otherwise.
- `[@component]` only applies where `(preprocess (pps ppx_component))` is in
  the enclosing `dune` file. That's `test/dune` and every `examples/*/dune`
  — **not** `lib/dune`. Code inside `lib/` cannot use `[@component]` or JSX;
  it's written in the fully-expanded form by hand.

## Writing a headless test

Headless tests drive a real component tree without a terminal, using
`Runtime.startHeadless`. Follow the pattern in `test/headless_tests.re`:

```
open Matcha;

module CounterApp = {
  [@component]
  let make = () => {
    let (count, setCount) = Hooks.useState(0);
    let quit = Event.useQuit();

    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Arrow_up => setCount(count + 1)
      | Key.Char('q') => quit(PreserveScreen)
      | _ => ()
      }
    );

    <Text> {"Count: " ++ string_of_int(count)} </Text>;
  };
};

let run = () => {
  Test.group("Counter headless", () => {
    Test.run("sendKey triggers state update", () => {
      let handle = Runtime.startHeadless((module CounterApp));
      handle.sendKey(Key.Arrow_up, Key.noModifiers);
      let output = handle.getOutput(true /* stripAnsi */);
      Test.assertContains(output, "Count: 1", "count should increment");
      handle.quit();
    });
  });
};
```

Notes:

- `startHeadless` renders once immediately on creation; `getOutput`/
  `getLines` return the *last rendered* frame, so call `sendKey`/`render`/
  `resize` before reading if you expect updated content.
- `sendKey` only triggers a re-render if the handler actually changed state.
  The gate is the **root** context's `needsRerender`; a `setState` inside a
  component flags both its own context and the root, which is what the loop
  watches. A key with no matching handler branch is a no-op and `getOutput`
  will be unchanged. Use `handle.render()` to force a frame regardless.
- Each `startHeadless` call gets its **own `Hooks.instanceState`**, and every
  handle method re-installs it first, so several handles in one test file
  can't corrupt each other's hook state. Don't try to interleave handles
  across threads.
- A component that renders is rendered **every** time it is visited, and a
  stack visits an `Auto` child twice per frame (measure, then real). Effects
  still fire once per frame — they're committed after the whole tree renders.
  If you're counting renders in a test, count effect runs, not body calls.
- Always call `handle.quit()` at the end of a test, even if you're only
  asserting on output — it's cheap and keeps behavior consistent with real
  app teardown (mirrors `useQuit`'s cleanup path).
- Register the new suite's `run()` in `test/run_tests.re` alongside the
  existing `Element_tests.run(); Headless_tests.run(); ...` calls, and add
  its module to the `(modules ...)` list in `test/dune`'s `(test ...)`
  stanza, or `dune runtest` won't pick it up.

## Testing apps that use input, focus, timers, Static, mouse or scrolling

`test/chat_tests.re` is the canonical reference — it drives the real
`examples/chat` component and uses every recipe below at least once. The
helpers live in `test/input.re`. The recipes:

- **Structure the app as a library + thin launcher** so tests can start the
  real component in-process: `examples/chat/ChatApp.re` (a `(library)` in
  the example's `dune`) + a one-line `main.re`. The test then does
  `Runtime.startHeadless((module ChatApp.App))` — never copy the component
  into the test.
- **End-to-end typing**: `Input.feedBytes(handle, "hi\r")` runs raw bytes
  through a real `InputDecoder` — the same path a terminal read takes —
  and delivers each decoded event through the handle. Use it over bare
  `sendKey` when the test is about what a terminal would actually deliver
  (Enter is `"\r"`, Ctrl+C is `"\003"`, a real bracketed paste is
  `"\027[200~...\027[201~"`).
- **Live frame vs transcript**: `handle.getOutput(true)`/`getLines(true)`
  are the *current* frame only; `handle.getStaticOutput(true)` is
  everything `<Static>`/`useStdout` ever committed, accumulated. Assert
  "committed exactly once, ever" against the latter (count occurrences,
  then render more frames and count again); assert what's on screen against
  the former. A committed item must never appear in `getOutput`.
- **Focus**: assert `handle.getFocusedId() == Some("my-id")` — never parse
  the focus marker out of the frame. Drive the ring with `Input.pressTab`/
  `pressShiftTab`. Prove `~isActive` gating by typing at an unfocused
  input and asserting nothing changed.
- **Paste**: `Input.feedPaste(handle, "line1\nline2")` for the handle-level
  path; assert the newline did *not* trigger Enter-bound behavior — a paste
  is data, not keystrokes.
- **Timers**: `handle.advanceTime(ms)` is the only way time passes
  headlessly — tests never sleep. It fires `useInterval`/`useTimeout`
  deadlines in order, re-rendering after each. An interval with `~ms=0` is
  disabled (the Ink `delay={null}` idiom), so advancing a huge amount while
  idle is a cheap "nothing is running" assertion.
- **Mouse**: `Input.clickAt(~x, ~y)` in live-region coordinates (0-based,
  frame top-left) fires the innermost `<Clickable>` whose painted box
  contains the point; `handle.sendMouse({kind: ScrollDown, ...})` wheel-
  scrolls the innermost *wheel-interested* component under the pointer (a
  `<ScrollView>`, through any plain Clickables on top of it). When a click
  lands somewhere unexpected, print `handle.getLines(true)` and count rows
  — that is the whole debugging loop.
- **Fresh handle per test.** Handles are cheap and independent; never share
  one across `Test.run` cases.

## Writing assertions with `test/Test.re`

`test/Test.re` is a small hand-rolled framework, not an external test
library:

- `Test.group("Name", () => { ...Test.run calls... })` — prints a header,
  groups related tests.
- `Test.run("test name", () => { ...assertions... })` — runs the body,
  catches any exception (assertion failures raise `Test.AssertionFailed`),
  and records pass/fail; prints PASS/FAIL inline.
- `Test.assertEqual(actual, expected, msg)` — structural `!=` compare (fine
  here since these are typically ints/strings/tuples, not closures).
- `Test.assertEqualStr(actual, expected, msg)` — like `assertEqual` but
  prints an expected/actual diff on failure; prefer this for string
  assertions.
- `Test.assertContains(haystack, needle, msg)` — substring check; the usual
  choice for headless render output, since exact frame text is often
  fragile.
- `Test.assertTrue` / `Test.assertFalse(cond, msg)`.
- `Test.runExpectedFailure("name", () => ...)` — like `run`, but the body is
  *expected* to raise; use it to pin a known-broken behavior.
- `Test.Mock.fn`/`fnWithArg`/`const`/`custom` — call-recording mocks.
  `Test.spy()` — a lighter-weight "was this called" tracker.
  `Test.FakeTime` — controllable clock.
- `Test.finish()` — call once at the very end of `run_tests.re`; prints the
  summary and `exit(1)` if anything failed (this is what makes `dune
  runtest` fail the build on a test failure).

## Golden frame tests (`test/golden.re`, `test/golden_tests.re`)

Goldens pin *rendered frames*, so they're the safety net for any layout or
rendering change. Helpers in `test/golden.re`:

- `Golden.checkComponent("name", (module SomeComponent))` — renders a small
  in-process component headlessly and compares against
  `test/goldens/name.txt`.
- `Golden.checkExample("counter")` — spawns the `matcha-example-counter`
  binary headlessly (with a timeout and closed stdin) and compares against
  `test/goldens/example-counter.txt`.
- `Golden.check("name", actualString)` — the raw form behind both.

Frames are compared after `stripAnsi` and trailing-whitespace normalization,
so padding noise doesn't cause spurious failures. Regenerate **only** for an
intentional rendering change, from the repo root:

```
UPDATE_GOLDENS=1 dune exec test/run_tests.exe
```

Then read `git diff test/goldens/` and confirm every changed frame is a
change you meant to make.

## Terminal-truth tests: `test/vterm.re` and `test/pty.re`

Everything above sees **frame text**. Nothing above sees what a terminal
does with the escape bytes that carry it, and nothing above touches the TTY.
Two layers close that gap.

### Which layer to reach for

| You changed… | Reach for |
|---|---|
| component logic, hooks, layout math, element utilities | headless handle (`Runtime.startHeadless`) |
| what a frame looks like | goldens |
| a **painter** (`lib/FrameDiff.re`, `lib/LiveRegion.re`) or anything that emits escape bytes | byte test **plus** a Vterm grid assertion |
| `lib/Terminal.re`, raw mode, signal handling, mode enter/exit, resize, quit/restore, input batching | a `test/pty_tests.re` case |

- **Vterm** (`test/vterm.re`) is a screen grid fed raw bytes: cursor,
  deferred wrap, EL/ED erasure, scrollback, alternate screen, SGR per cell,
  DEC modes, DSR. Readers: `Vterm.row/snapshot/text/scrollback/fullText`,
  `cellGlyph`, `cellSgr`, `cursor`, `pendingWrap`, `inAltScreen`,
  `unknownSeqs`. Its own unit tests are `test/vterm_tests.re`.
- **Pty** (`test/pty.re` + `test/pty_stubs.c`) runs a real example binary on
  a real pseudo-terminal, with a Vterm attached to the master.

### The two rules that made these layers necessary

1. **Pairing rule.** A byte-exact painter expectation must always be paired
   with a Vterm grid assertion. A byte test only says "the writer still
   emits what its author believed was right" — it bakes a *model* of
   terminal behaviour into the assertion and pins that model true or false.
   The painters once emitted `content ++ ESC[0m ESC[K` per row: frames were
   correct, byte tests were green, and every full-width row silently lost
   its last cell on a real terminal, because printing into the last column
   leaves the cursor *on* that column (deferred wrap) and the EL then erases
   it. Rule written out in full in `test/framediff_tests.re`'s header.
2. **Non-default-size rule.** Any test touching sizing, wrapping, flex,
   truncation or resize must run at least one **non-80x24** size. 80x24 is
   simultaneously the constraints default, the headless-config default and
   the `caml_get_terminal_size` non-TTY fallback, so at that size a stale
   or defaulted value is indistinguishable from a computed one. See
   "coincidence defaults" in `/CLAUDE.md`.

Vterm is written from xterm semantics, deliberately **not** from reading
Matcha's painters. If the model and a painter disagree, that is a finding to
investigate — never adjust the model until the painter's output looks right.

### `Pty` usage sketch

```reason
Pty.withSession(~width=100, ~height=30, "examples/chat/main.exe", [], s => {
  Pty.drain(~quietMs=250, ~timeoutMs=5000, s);          /* poll till quiet */
  Test.assertContains(Pty.screen(s), "Context", "panel painted");
  Pty.send(s, "hi\r");                                   /* ONE write = batched */
  Pty.drain(s);
  Pty.resize(s, ~width=120, ~height=40);                 /* real SIGWINCH */
  Pty.drain(s);
  Pty.send(s, "\003");
  switch (Pty.waitExit(s)) {
  | Pty.Exited(0) => ()
  | other => Test.assertTrue(false, Pty.exitStatusToString(other))
  };
});
```

`withSession` kills and reaps in a `Fun.protect` finaliser, so a failing
assertion cannot leak a child sitting in raw mode — always use it. `drain`
polls until the child has been quiet for `quietMs` and auto-answers DSR
(`ESC[6n`) from the Vterm's cursor, so inline startup's cursor tracking runs
for real; **never assert after a bare sleep.** An exit reported as
`Signaled(n)` rather than `Exited(0)` is the ISIG bug class: the key reached
the kernel instead of the application, and the terminal restore never ran.
Keep this suite to a handful of cases — it is by far the slowest layer.

## Profiling a matcha app

Every layer above answers "is it correct". `lib/Perf.re` answers "where did
the time go", with attribution down to the individual component. It is off by
default, costs nothing off, and **only ever writes files — never stdout or
stderr** (interactive stdout is escape bytes; stream-headless stdout is the
frames the goldens parse).

### Turning it on

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

### Recording an interaction

Profile a *script*, not a session: the same bytes each run, so before and
after are comparable. Both HANG TRAPS still apply — `timeout`,
`MATCHA_HEADLESS=1`, and stdin that reaches EOF, together:

```
printf '\033[B\033[B\033[B\033[Bq' | timeout 60 env \
  MATCHA_HEADLESS=1 MATCHA_WIDTH=200 MATCHA_HEIGHT=45 \
  MATCHA_TRACE=/tmp/before.json ./myapp > /dev/null
```

`\033[B` is arrow-down, `\033[A` arrow-up, `\033[C`/`\033[D` right/left. Set
the geometry explicitly: frame cost scales with it, and a default 80×24 will
not reproduce what the user sees.

### Reading the summary

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
`[@component] let make`, so a row points straight at a source line.

### Application spans

Wrap app-level work so it nests under the component that ran it:

```reason
Perf.span("tokenize-file", () => Highlight.highlight(~path, lines));
```

Names must not contain spaces (the summary is whitespace-column parsed);
hyphenate them.

### Below span level

`Perf` bottoms out at whatever you wrapped. To go finer, use macOS
`sample <pid>` or Instruments on the running binary. (`olly`/OCaml
`runtime_events` would be the native answer but is not installed in this
switch and would be a new opam dependency.)

### The rule

**Never claim a performance win without a before AND an after summary of the
same scripted interaction**, and quote both `== slowest frames ==` sections.
"It feels faster" is not a measurement, and neither is a single after-only
recording.

## The `==`/`!=` vs `===`/`!==` rule (exact failure mode)

In Reason, `==`/`!=` are OCaml's **structural** `=`/`<>`; `===`/`!==` are
OCaml's **physical** `==`/`!=`. Two real bugs in this codebase
(`Hooks.depsEqual`, and the since-removed `Hooks.propsChanged`) came from
using structural comparison on `Obj.t` values that can wrap closures — e.g. a
`useEffect`/`useMemo` deps array containing a callback, or a component's props
record containing an `onChange` function. OCaml's structural `compare`/`=`
raises:

```
Invalid_argument("compare: functional value")
```

the moment it has to descend into a function value, which crashes the
render loop. The fix in both cases was switching to `!==` (physical
inequality): safe for `Obj.t` because it compares pointers for heap values
and the raw value for immediates, and never tries to look inside the
value. **Do not "fix" this with `Obj.magic(x) : nativeint` — it segfaults on
immediate (unboxed) values under OCaml 5.** Rule of thumb: any time you're
comparing something that *might* contain a closure (deps arrays, props,
memoized callbacks, anything typed `Obj.t` in `Hooks.re`), use `!==`/`===`,
never `!=`/`==`.

The props-comparison side of this hazard is gone — component identity is now
plain string tree paths and nothing compares props — but `Hooks.depsEqual`
still does exactly this for `useEffect`/`useMemo`, and
`test/hooks_regression_tests.re` still guards the "props containing a
closure" case as a render-path regression. Keep both.

## Common dune errors and fixes

- **"Warning 26/27 [unused-var]" (or similar) fails the build.** Dev profile
  promotes warnings to errors. Fix: remove/use the binding, or prefix an
  intentionally-unused name with `_` (e.g. `_unused`). Don't reach for
  `[@warning "-N"]` unless you're mirroring what the PPX itself already does
  for generated code.
- **PPX / AST version mismatch** (something like "ppxlib and the compiler
  disagree on the AST version", or a cryptic `Migrate_parsetree` error) —
  usually means the active opam switch's `ppxlib`/OCaml version drifted from
  what `ppx/dune` and `dune-project` expect (`ocaml >= 5.3.0`, `ppxlib >=
  0.36.0`). Confirm you're in the repo's local switch (`_opam/`) and that
  `opam list ppxlib` matches the `dune-project` constraint; reinstall/pin if
  not.
- **"Unbound module ppx_component" or JSX not expanding** — check the
  `dune` file for the directory you're editing has `(preprocess (pps
  ppx_component))`. `lib/dune` deliberately does not; if you're trying to
  use `[@component]`/JSX inside `lib/`, that's the wrong approach — hand
  expand it, or move the code to `test/`/`examples/`.
- **A new test file isn't running under `dune runtest`** — it must be both
  listed in `test/dune`'s `(test (modules run_tests headless_tests
  element_tests mock_tests ...))` line and called from `test/run_tests.re`
  (see the `Element_tests.run(); ... Golden_tests.run();` list there).
  Dune won't auto-discover it.
- **"The value X is required but not provided" against
  `lib/Matcha.re.mli`** — `lib/Matcha.rei` declares something `lib/Matcha.re`
  doesn't define. That file pins the public API on purpose; fix whichever
  side is wrong rather than deleting the declaration reflexively.
- **Example hangs when you try to smoke-test it** — you forgot
  `MATCHA_HEADLESS=1`, or forgot to redirect/close stdin. See the hang-trap
  warnings in `/CLAUDE.md`; the safe invocation is always `timeout N env
  MATCHA_HEADLESS=1 dune exec <example> < /dev/null`.
