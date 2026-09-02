---
name: matcha-dev
description: >-
  Use when working on the Matcha framework's own ReasonML/OCaml source
  (lib/, ppx/, test/, examples/) — adding or modifying elements/components,
  writing or debugging headless tests, working with the [@component] PPX
  and JSX expansion, or diagnosing dune build errors specific to this
  repo's dune/ppx/warnings setup. Do not use for building an application
  on top of matcha - see the matcha-app skill for that.
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

### Adding a floating layer

`Runtime.compositeOverlays` splices `Overlay` layers over the finished frame
and MUST return `base` physically unchanged when none is open — the gate on
any compositor change is `git status test/goldens/` showing zero
modifications, since every existing golden depends on that fast path.
`isInvisibleToLayout` sees through `Lazy` but not `Component`, which is why
`Modal.createElement` returns `Lazy(() => isOpen ? Overlay(...) : Empty)`
rather than a component wrapping one. App-authoring guidance — `<Modal>`/
`<Overlay>` usage, `useInput` vs `useKeyDown`, dialog container sizing —
lives in the **matcha-app** skill.

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

> Writing a headless test for an application built on matcha, and testing
> apps that use input, focus, timers, `<Static>`, mouse or scrolling, is
> covered by the **matcha-app** skill (`references/testing.md`). What
> follows here is matcha's own test framework and the layers that pin the
> framework's own rendering.

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
- `Golden.checkExample("counter")` — spawns the built `examples/counter/main.exe`
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

> Profiling an application built on matcha with `lib/Perf.re` tracing is
> covered by the **matcha-app** skill (`references/profiling.md`).

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
