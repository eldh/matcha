---
name: matcha-app
description: >-
  Use when building or changing a terminal application that DEPENDS ON the
  matcha framework - scaffolding a new app, structuring its root component
  and state, rendering rows, wiring keyboard and mouse input, keeping slow
  work off the render path, or testing it headlessly. Do not use for work on
  matcha's own source in lib/, ppx/ or its examples - see the matcha-dev
  skill for that.
---

# Building an application with matcha

matcha is a React-like terminal UI framework for ReasonML/OCaml. This skill
is about writing an app *on* it. Every rule below is written with the bug it
prevents, because most of them are bugs that already shipped once.

`lib/Matcha.rei` in the matcha checkout is the **only** authoritative list of
what is exported. Prose lags; the interface file does not. Read it before
believing any claim about what matcha supports, including claims made here.

Paths beginning `examples/` are in the matcha repository itself and are the
best worked references you have - `examples/chat` exercises most of the
framework, `examples/claude-code` is the fullscreen shape, `examples/
command-menu` the overlay one. Two larger applications are also cited by
name below, **`bdiff`** (a git branch-diff viewer) and **`hog`** (a
disk-space explorer); they are separate repositories and you probably do not
have them. Every rule here is stated in full, so a citation you cannot open
costs you nothing.

| Reference | Load when |
|---|---|
| [references/scaffold.md](references/scaffold.md) | Starting a new app - every file to create, with contents |
| [references/testing.md](references/testing.md) | Writing or fixing tests |
| [references/profiling.md](references/profiling.md) | Something feels slow and you need attribution |

## The shape of an app

**All of the app is a library; `bin/main.re` is a launcher that does nothing
but parse, validate and start.** The library exposes a factory:

```reason
let app: (~cwd: string, ~config: Cli.config) => (module Runtime.HooksComponent);
```

This is not organisational taste. It is what lets a test start **the same
component the binary starts**, through `Runtime.startHeadless`, so there is
never a second copy of the UI that tests pass against and users never see.

**Parameterize the factory over anything a test must stub.** `hog`'s factory
takes `~trash: string => result(unit, string)` and a `Scan.handle`; the
binary passes the real ones, the tests pass a recording stub and a
deterministic scan. That single line is the difference between a test suite
that can assert "the move was never *attempted*" and one that can only
assert "the row is still there".

**Every failure path exits before `Runtime.start`.** Bad flag, missing
directory, unreadable config - print to stderr and `exit(1)` in `main.re`.
Discover the same failure after the runtime starts and it flashes the
alternate screen on and off around a message the user then has to scroll
back for.

## Five rules that will bite you

**1. `~screen=Fullscreen` if your root Flexes to fill the screen.** Inline
mode paints at the cursor, so a frame as tall as the terminal forces it to
*scroll*, pushing the user's prompt and history up and away. Quitting erases
the region correctly, but nothing can un-scroll a terminal, and the user is
left with a screenful of blank rows. `examples/command-menu` shipped this
way and a real user hit it. Under Fullscreen, `<Static>` and `useStdout`
**raise** - there is no scrollback to commit to - so keep messages in state.

**2. Bind Ctrl+C with `useKeyDown`, never `useInput`.** Raw mode disables
ISIG, so Ctrl+C is an ordinary keypress that only your app can act on. And
`useInput` is suppressed for everything outside the topmost overlay - so a
`useInput` binding means the moment your own confirmation dialog opens, the
app cannot be quit at all. It surfaces as an exit reported `Signaled(2)`.
Test it: send `\003` with a modal open and assert `isRunning() == false`.

**3. Every `useMemo` dependency must be an immediate through `Obj.repr`.**
Dependencies are compared by **physical** equality. A tuple, record, array
or built string is a fresh block every render, so the comparison never
matches and the memo recomputes every frame - silently reintroducing exactly
the cost it was added to remove. Ints and bools only.

When the thing you actually depend on is a string or a record, keep a
**generation counter beside it in one state cell** and depend on the
counter:

```reason
let (queryState, setQueryState) = Hooks.useState((0, ""));
let (gen, query) = queryState;
/* ... */
let visible = Hooks.useMemo(() => filter(entries, query), [|Obj.repr(gen)|]);
```

One cell, not two: holding the counter separately lets a frame read a new
counter against an old value. See `hog/lib/HogApp.re` and
`bdiff/lib/BdiffApp.re`, which both do this for their filter query.

**4. `<ScrollView rows>` strings must be style-self-contained.** The
renderer starts painting at row N without reading rows 0..N-1, so every row
must open the styles it needs and end with `Element.resetAnsi`. A row that
inherits colour from its predecessor renders unstyled the moment you scroll
past that predecessor. The non-obvious consequence: a *whole-row* style, like
`Inverted` on the selection, has to be carried in every piece of the row, not
opened once at the start.

**Sanitize any text you did not author.** A filename containing an escape
sequence injects styling into every row painted after it, and one containing
a newline turns one row into two and desynchronises the list from its own
offsets. Both are legal filenames. `hog/lib/Rows.re` maps bytes below `0x20`
and `0x7f` to `?`.

**5. Never run the binary without `timeout`, `MATCHA_HEADLESS=1` and closed
stdin - all three.** Without the second it puts the terminal in raw mode and
blocks on a TTY forever. Without the third it blocks on stdin forever, even
headless. Missing either hangs the agent, not just the app.

```
timeout 20 env MATCHA_HEADLESS=1 MATCHA_WIDTH=140 MATCHA_HEIGHT=40 \
  dune exec bin/main.exe -- <args> < /dev/null
```

To drive it, pipe bytes that eventually end: `(sleep 3; printf 'jjq') | ...`.

## Is your root a `[@component]`?

Usually yes. But **if you route mouse clicks yourself from a root
`useMouse`, it must not be.**

`Hooks.dispatchMouse` ends with an unconditional fan-out to the **root
context's** mouse handlers. `Runtime` runs the component you hand it inside
the root context - so a bare `make` *is* the root. Wrap it in `[@component]`
and it gets its own child context instead, at which point a `<ScrollView>`,
which registers `useMouse` for the wheel, becomes the innermost mouse target
over your list and swallows every click. Click-to-select then silently never
fires, with no error anywhere.

- `bdiff/lib/BdiffApp.re` uses `[@component] Root` and is fine: its rows are
  `<Clickable>` child components, which are hit-tested normally.
- `hog/lib/HogApp.re` puts the body directly in the module, because its list
  is `~rows` mode and therefore has **no child elements at all** to click -
  so it does the arithmetic itself (`row = ev.y - listTop`, `i = offset + row`).

If you take that second path, also pass `~wheel=false` to the root
`useMouse`, or the root declares wheel interest and eats the notch that
should have reached the `ScrollView`.

## Selection, scope and identity

**Make selection explicit state, not a function of the scroll offset,
whenever a destructive key acts on "the selected thing".** If selection
follows the offset, a wheel notch silently retargets the delete key. `bdiff`
*does* derive its active file from scroll position, and is right to: its two
panes are one view of one document. `hog` does not, because `d` moves a
folder to the Trash.

Moving the selection nudges the offset to keep it visible; scrolling moves
the offset alone. **Clamp the selection during render, never in an effect** -
the visible list shrinks under it on every filter keystroke, and an effect
costs a frame and shows a flash.

**Hold scope and selection as stable identifiers - usually paths - not as
indices or ids into a model you rebuild.** Row indices are meaningless after
a re-sort, and ids are meaningless after a rescan. Re-resolve to an index
each generation.

## Keeping slow work off the render path

Four patterns, in increasing order of effort. Use the least you can.

**Memoize on a counter.** Do the expensive load *synchronously inside a
`useMemo`* keyed on a reload counter, not in an effect. The first frame is
then already the finished frame: no loading flash interactively, and no
"advance the clock, then assert" in tests. `bdiff` shells out to git this
way.

**Build geometry up front, fill it lazily.** Allocate the row array at its
final length and bake nothing; then, from the render body, fill only the
visible window plus a forward cushion:

```reason
Rows.ensureRange(store, ~fromRow=top, ~toRow=top + viewportH + 16);
Rows.ensureSelected(store, ~sel=selClamped);   /* AFTER ensureRange */
```

**Both must be idempotent.** matcha renders the tree twice per frame - a
measure pass, then a paint pass - and both run the render body. Prefetch
*forward only*; prefetching backwards can force re-deriving an entire
earlier region on every arrow-down, which is the cost you were avoiding.

**Background threads may call `setState` directly** - the setter wakes the
render loop through a pipe, and `examples/async-fetch` does exactly this.
But prefer **publishing to an atomic and polling a counter**:

```reason
Hooks.useInterval(poll, ~ms = scanning ? 100 : 0);   /* ms=0 registers no timer */
```

because `useInterval` runs on the headless **virtual clock**, so
`handle.advanceTime(100)` drives the whole progress path in tests with no
sleeping and no flakes.

**Give slow work a test seam.** `hog/lib/Scan.re` puts one traversal behind
three entry points: `start` (a real thread), `scanSync` (the same function on
the caller's thread), and `stepped` (parked between units of work, released
one at a time). The binary passes `start`, tests pass `stepped`. One
implementation, so the tests drive what ships. Details in
[references/testing.md](references/testing.md).

If a background thread and the render thread share mutable state, write the
single-writer invariant down as a comment and keep to it. Under
`threads.posix` there is one domain behind a master lock, so single-word
reads cannot tear - but that guarantee evaporates the moment anyone reaches
for `Domain.spawn`.

## Input

**`useKeyDown` always fires. `useInput` is captured by the topmost overlay.**
That is the whole model. Globals - quit, help, mode switches - go in
`useKeyDown`. Anything a dialog should be able to take over goes in
`useInput`. A `<ScrollView>` goes quiet under a modal for free, because it is
`useFocus` plus `useInput` and nothing else.

**There is no `stopPropagation`**, so "the text input consumes this key" is
an explicit branch, not a return value:

```reason
Event.useKeyDown((key, mods) =>
  switch (confirm) {
  | Closed => filterOpen ? filterChords(key, mods) : normalMap(key, mods)
  | _ => /* dialog open: only Ctrl+C */
  });
```

Without that outermost branch, `d` at a confirmation dialog opens a second
dialog and `q` quits with the dialog on screen.

For `<TextArea>`, use **intercept-then-delegate**: one `useInput` that
matches the keys you claim, with a catch-all that forwards the rest to
`TextArea.handleKeyDown`. Claim arrows explicitly if they drive a list, or
they move the cursor instead. `examples/claude-code` is the reference.

`Key.t` traps worth knowing before you debug one:

- **Ctrl+A arrives as `(Arrow_left, {meta: true})`** and Ctrl+E as
  `(Arrow_right, {meta: true})` - readline emulation. So binding `Arrow_left`
  as navigation while a text field is focused makes Ctrl+A navigate.
- **Codes 8 and 127 both arrive as `Backspace`.** One arm handles both.
- **Tab is swallowed by focus cycling** whenever at least one focusable is
  registered. If you want Tab yourself, register none - `<ScrollView
  focusable=false>` driven through `~offset`/`~onScroll` is the way, and it
  also stops the ScrollView claiming the arrow keys.
- A `Char(c)` arm needs `{ctrl: false, alt: false, meta: false}` **and**
  `32 <= Char.code(c) < 127`, or a stray control byte lands in your input as
  a character that cannot be typed away.
- `Key.Text(s)` is one complete multi-byte codepoint and needs its own arm.
  `Key.Paste` has no arm in `TextArea.handleKeyDown` at all.

## Layout

`useContainerSize()` reports the nearest enclosing `<Container>`, or the
whole frame when there is none. **`<Sized>` and `<ScrollView>` are not
boundaries** - wrapping something to nudge its layout must never silently
re-target its descendants' responsive queries. Declare a `<Container>` where
you want one, typically one per pane.

**An unsized `<ScrollView>` never scrolls.** It is exactly as tall as its
content. Wrap it: `<Sized size={Flex(1)}><ScrollView ... /></Sized>`.

**In `~rows` mode the usable width is `availWidth - 1` whenever
`showScrollbar` is set** - the column is reserved whether or not the content
actually overflows. Bake to the full width and every row is one cell too
wide.

**Derive a responsive breakpoint from the columns your content needs, not
from a round number.** Add up the chrome, add the minimum readable content
width, add the rule and scrollbar columns, and let the number fall out.
`bdiff` switches to a split diff at 160 because each side needs 80; `hog`
switches its details pane from a column to a strip at 98 for the same kind
of reason. Neither number appears as a literal in a condition.

## Floating layers

**Write the `<Modal>` directly in the stack.** It costs no row, no gap slot
and no justify share, open or closed. Never wrap it in a `<Sized>`, and
never return one from a component - a component wrapper puts the modal's own
hooks *outside* the layer, where its own modal suppresses them, and costs a
blank layout row.

Layer keys go **inside** with `useInput`; globals stay **outside** with
`useKeyDown`. A modal's box, position and clip resolve against the whole
frame, not the slot it was written in, so one opened from inside a
`<ScrollView>` floats over the window rather than inheriting that scroller's
visible rect. `useContainerSize()` inside a dialog reports the dialog.

## Before you claim it works

- `dune build` is clean. The dev profile promotes warnings to errors; fix
  the code rather than adding `[@warning]`.
- `dune runtest` is green, and the tests drive the real component - see
  [references/testing.md](references/testing.md).
- A headless smoke run with the three-part invocation above renders what you
  expect.
- If you changed anything about rendering cost, a before **and** an after
  trace of the same scripted interaction - see
  [references/profiling.md](references/profiling.md). "It feels faster" is
  not a measurement, and neither is an after-only recording.
