# Testing a matcha application

Reference material for the `matcha-app` skill. Load this when you are
writing or reviewing tests for an application built on matcha — headless
component tests, or an app that uses input, focus, timers, `<Static>`,
mouse, or scrolling and needs more than a single `sendKey`/`getOutput`
round trip.

## Writing a headless test

Headless tests drive a real component tree without a terminal, using
`Runtime.startHeadless`. Follow the pattern in matcha's own
`test/headless_tests.re`:

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
- Register the new suite's `run()` in your `test/run_tests.re` alongside your
  other suites' `run()` calls, and add its module to the `(modules ...)` list
  in your `test/dune`'s `(test ...)` stanza, or `dune runtest` won't pick it
  up.

## Testing apps that use input, focus, timers, Static, mouse or scrolling

Matcha's own `test/chat_tests.re` is the canonical reference — it drives the
real `examples/chat` component and uses every recipe below at least once.
The helpers live in matcha's `test/input.re`; copy it into your own app the
same way you copy `Test.re` (see below). The recipes:

- **Structure the app as a library + thin launcher** so tests can start the
  real component in-process: an `App.re` (a `(library)` in your app's
  `dune`) + a one-line `main.re`. The test then does
  `Runtime.startHeadless((module App))` — never copy the component into the
  test.
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

## Copy `Test.re` and `Input.re` into your own `test/`

Matcha's test helpers (`Test.re`, the small hand-rolled assertion framework,
and `Input.re`, the byte-feeding/click/paste helpers used above) are not part
of the installed opam package, and matcha's own `test/` directory must never
be added to an app's build — your app depends only on the installed `matcha`
library. Copy both files into your app's `test/` directory instead. The
header comment both `/Users/eldh/Code/hog/test/Test.re` and
`/Users/eldh/Code/bdiff/test/Test.re` carry on their local copy says it
plainly:

> A tiny test framework - the same shape as Matcha's own test/Test.re, kept
> local so bdiff depends only on the INSTALLED matcha library (matcha's test
> helpers are not installed, and its test directory must never be added to
> this build).

Your own copy is also the place to add assertions matcha's own `test/Test.re`
doesn't carry. Both `hog` and `bdiff` add `assertEqualInt` (an int-specific
equality check that prints `expected`/`actual` as integers rather than
`Obj.t`-compared values) and `countOccurrences` (counts non-overlapping
occurrences of a substring — the primitive `assertContains`/
`assertNotContains` are built on, and what you need directly for the
"committed exactly once" style of assertion above).

## Fixtures build real trees on disk

When your app's own tests need real files, real directories, or real git
repositories rather than in-memory fakes, build a `Fixture.re` alongside
`Test.re` and `Input.re`. Two worked examples: `/Users/eldh/Code/hog/test/Fixture.re`
(throwaway directory trees for a disk-usage tool) and
`/Users/eldh/Code/bdiff/test/Fixture.re` (throwaway git repositories for a
diff tool).

The shape in both is the same:

- **`withTmp`/`withRepo`** create a fresh temp directory (or a fresh `git
  init`'d repo on a known branch, with identity pinned via `-c
  user.email=...`/`-c user.name=...` so nothing depends on the developer's
  global git config), hand it to the test function, and remove it
  afterwards **unconditionally**, via a `Fun.protect` finaliser — not just on
  the success path. hog's version restores permissions too, because "every
  fixture cleans up in a `Fun.protect` finaliser, INCLUDING restoring
  permissions" — otherwise a test that fails halfway through a
  `withUnreadable` case leaves a chmod-0 directory behind, and the next run
  can't even delete it, turning one red test into a permanently broken
  suite.
- **`withTree`/`withStandardRepo`** layer a *spec* on top — a list of
  `(relative path, size)` pairs, or a scripted sequence of commits — that
  builds a specific, named shape once and hands the root to the test.

The rule that matters most: **fixture sizes must be exact and distinct,
never random.** hog's `Fixture.re` states it as the second of its two
governing rules: "File sizes are exact and distinct, never random. Ranking
assertions are about which entry is bigger; a tie or a size that changes
between runs turns a real failure into a coin flip." A fixture built from
`Random.int` sizes can pass by accident (two files end up in the right order
by luck) or fail by accident (two files tie); an assertion should be about
arithmetic — "700,000 is bigger than 5,000" — never about which of two equal
or randomized numbers happened to win this run.

## `(deps (universe))` in `test/dune`

Any test suite that touches the filesystem — reads real files, shells out to
git, builds fixture trees — is not cacheable on its source alone, and dune's
default caching will happily serve a stale pass. Both `hog` and `bdiff` mark
their whole suite uncacheable in `test/dune`. hog's comment:

> The suites build real directory trees on disk and read them back, so
> nothing about them is cacheable on their source alone.

bdiff's:

> The suites shell out to git and read the filesystem, so nothing about them
> is cacheable on their source alone.

Both express it the same way:

```
(deps
 (universe))
```

Add this to your own `test/dune` the moment any test in the suite touches
the filesystem, a subprocess, or anything else outside dune's source-tracked
inputs — not just to the specific stanza that needs it, since dune caches at
the granularity of the whole `(test ...)` target.

## Never test at 80x24

80x24 is matcha's constraints default, its headless-config default, *and*
its non-TTY `getSize` fallback, all at once — so a test at that size cannot
tell a computed layout value from one that silently fell back to a default.
A bug that leaves a stale or defaulted size in place is invisible exactly at
that size. hog's `test/app_tests.re` states the consequence directly:

> SIZES ARE DELIBERATELY NEVER 80x24. That is simultaneously Matcha's
> constraints default, its headless default and its non-TTY fallback, so a
> layout assertion that happens to hold there can hold by coincidence rather
> than because anything computed it.

hog picks a different size for each layout regime it needs to distinguish:
120×40 for the default split view, 96×30 for the stacked layout, the
breakpoint pair 99×36/97×36 to pin the exact column where the layout mode
flips, and 60×20 for the case where a bar is dropped for lack of room.
bdiff's `test/app_tests.re` does the same with its own breakpoint: 120×40 or
216×40 for the unified (unsplit) view, 217×40 for the exact split boundary,
and 240×40 for a comfortably split frame — with a comment noting 216 is
"one column too narrow to split" on purpose, so the suite has a case sitting
right on both sides of the line. Pick sizes the same way: one per layout
regime you need to tell apart, plus the exact boundary between any two
regimes your component switches on.

## Make background work deterministic instead of sleeping

An app that scans a filesystem, watches for changes, or does any other
work on a background thread cannot be tested by starting it and sleeping a
guessed number of milliseconds — that is a flake waiting for a slow CI
machine, and it hides races rather than proving their absence.

hog's `lib/Scan.re` is the worked example: a directory-tree walk that runs on
a background thread in the real app, published to the UI through a single
`Atomic.t` snapshot. Its header lays out the synchronization invariant (the
arena has exactly one writer at a time — the scan thread while scanning, the
UI thread otherwise — and every field the UI reads without a lock is either
write-once or a single word, safe under the one-domain-at-a-time guarantee
of `threads.posix`) and then the test seam built on top of it: **one
traversal, three entry points.**

- **`start`** — the real one: `Thread.create`, what the shipped binary uses.
- **`scanSync`** — the identical traversal run synchronously on the caller's
  thread, returning a handle already at its final generation. This is what
  most tests use: no thread, no timing, just a finished result to assert on.
- **`stepped`** — no implicit progress at all. The walk runs on a thread but
  blocks at a *gate* after each directory, parked until the test calls
  `step()` to let it proceed by exactly one more unit of work. This is what
  a test uses when it needs to observe an in-progress scan (a partial
  snapshot, a "scanning…" phase in the UI) without ever guessing at timing.

The piece that makes this transferable to your own app: **the UI component
takes the scan handle as a parameter**, rather than constructing its own
`Scan.start` internally. A production binary passes `Scan.start(...)`; a
test passes `Scan.stepped(...)` (or `scanSync`) into the same component via
`Runtime.startHeadless`, and drives it with a virtual clock. Scan.re's own
header says it plainly: "The app takes a handle as a prop, so tests drive
the real component with `stepped` and a virtual clock instead of sleeping.
That is the only line that differs between a test and the binary: every call
the UI makes is the real one." That last sentence is the design goal — the
seam is in what constructs the handle, never in the component logic that
consumes it.

hog's `test/scan_tests.re` then holds exactly one test of the genuinely threaded
path, using a bounded retry rather than a timer, because its only job is to
prove `start` and `scanSync` agree — if the two ever diverge, every other
test in the file (all built on the non-threaded entry points) would be
testing behavior the shipped binary doesn't actually have. The file's header
states its two governing rules directly:

> 1. Nothing here sleeps. A test that waits a fixed number of milliseconds
>    for a background thread is a flake waiting for a slow machine.
>    `stepped` hands control back and forth explicitly, so every assertion
>    below runs while the walk is parked and touching nothing.
>
> 2. There is exactly ONE test of the genuinely threaded path, and it waits
>    on a bounded retry rather than a timer. Its only job is to prove that
>    `start` and `scanSync` agree - if the two traversals ever diverge, every
>    other test in this file is testing something the binary does not do.

## Prove a guard is not vacuous

An assertion that only ever passes is not evidence that it protects
anything — it may simply never have been in a position to fail. Before you
trust a test that guards against a regression, temporarily break the exact
code path it claims to protect and confirm the test goes red; then revert
the breakage. If the test stays green while the guarded behavior is
actually broken, the test was never testing that behavior.

Two worked examples from hog:

- **Quitting while a dialog is open.** hog's `lib/HogApp.re` has an
  `Event.useKeyDown` handler that switches on the confirmation state first:
  while a dialog is up
  (the `Confirm`/`Failed` case) the only bound key is
  `(Key.Char('c'), {ctrl: true, _}) => quitApp()` — a separate Ctrl+C arm
  from the ones bound in the "dialog closed" branches. It has to be
  `useKeyDown`, not `useInput`: raw mode disables ISIG, so Ctrl+C is an
  ordinary keypress, and `useInput` handlers go deaf while a modal layer is
  open. `test/app_tests.re`'s `"ctrl+c quits while the dialog is open"`
  opens the dialog, sends Ctrl+C, and asserts `t.h.isRunning()` is now
  false. To prove that test is not vacuous, delete the dialog-open branch's
  Ctrl+C arm (so it falls through to `_ => ()`) and confirm the test now
  fails — the app stays running, stuck behind the dialog, instead of
  quitting. Revert once it fails as expected.
- **No blank rows in the visible window.** hog's `lib/Rows.re` has an
  `ensureRange(t, ~fromRow, ~toRow)` that lazily bakes row text for the
  clamped range
  `[max(0, fromRow), min(n, toRow))`, marking each row baked so a later call
  skips it; unbaked rows sit at their initial empty-string value.
  `test/app_tests.re`'s `"every visible list row carries an entry at every
  offset"` asserts every visible row in a 21-row viewport contains an entry
  (checking for a substring every real row has) after scrolling, Page Down,
  and jump-to-end. To prove that test is not vacuous, shift `ensureRange`'s
  clamp by one at either boundary (e.g. `min(n, toRow - 1)`) and confirm the
  test now fails: the runtime paints a row index `ensureRange` never baked,
  which is still `""` from initialization, so the assertion for the
  substring fails on a genuinely blank row. Revert once it fails as
  expected.

This is not a one-time exercise to run and forget: do it whenever you add a
guard test whose entire value depends on a specific code path staying
present, and you are not yet confident the test would actually notice that
path disappearing.
