# Matcha

Matcha is a React-like terminal UI (TUI) framework for ReasonML/OCaml: a
component tree with hooks (`useState`, `useEffect`, `useMemo`, `useInterval`,
`useFocus`, `useInput`, `useMouse`, ...), a flexbox-like layout engine
(`VStack`/`HStack`/`Sized` with `Flex`/`Percent`/`Chars`/`Auto` sizing),
unicode-aware text measurement and wrapping, a normalized keyboard/paste/mouse
input model, focus management, an append-only `<Static>` transcript above an
inline live region, `<ScrollView>`/`<Clickable>`, floating layers
(`<Overlay>`/`<Modal>`), and both an interactive terminal runtime and a
headless runtime for testing/agents. The public API surface is
`lib/Matcha.re`, pinned by the interface file `lib/Matcha.rei`; everything
else under `lib/` is implementation. `examples/` holds 15 runnable sample apps
(`examples/chat` is the capstone that uses most capabilities at once, and
`examples/command-menu` is the overlay one); `test/` holds a hand-rolled test
suite (currently 674 tests, including golden frame tests).

## Toolchain

- OCaml >= 5.3.0, dune >= 3.0, Reason >= 3.12.0, ppxlib >= 0.36.0.
- Local opam switch lives in `_opam/` — activate it (`opam switch` /
  `eval $(opam env)`) before running dune commands if it's not already active.
- Build: `dune build` (~seconds for incremental; a clean build is closer to a
  minute depending on machine).
- Test: `dune runtest` (hand-rolled framework, `test/Test.re` /
  `test/run_tests.re`; runs in a few seconds — it's not a real process-spawn
  suite, just OCaml function calls).

**Dev profile promotes warnings to errors.** An unused binding, unused open,
or similar warning will hard-fail `dune build`/`dune runtest` in dev profile
(the default). Don't suppress with `[@warning]` unless the PPX itself needs
it (it already does, for generated `createElement` bindings) — fix the
underlying unused code instead.

## Verification loop

1. **`dune runtest`** for logic changes — hooks, layout math, key parsing,
   element utilities. This is the fast, primary feedback loop. For testing an
   *application* built on Matcha (input, focus, timers, static output, mouse,
   scrolling), `test/chat_tests.re` is the reference: it drives the real
   `examples/chat` component headlessly and demonstrates every technique an
   app test needs, one per test group.
2. **Golden/snapshot rendering tests** — `test/golden.re` (the helpers) and
   `test/golden_tests.re` (the cases), comparing rendered frames against
   fixture files in `test/goldens/*.txt`. Frames are compared after
   `stripAnsi` and trailing-whitespace normalization. Cases cover a few small
   in-process components plus every example binary rendered headlessly. Any
   change to layout or rendering shows up here first — **a golden diff you did
   not intend is a bug, not a fixture to refresh.** After an *intentional*
   rendering change, regenerate from the repo root:

   ```
   UPDATE_GOLDENS=1 dune exec test/run_tests.exe
   ```

   Prefer adding to this pattern over inventing a new one.
3. **Headless smoke run** for end-to-end sanity on a real example app:

   ```
   timeout 10 env MATCHA_HEADLESS=1 dune exec examples/counter/main.exe < /dev/null
   ```

   This prints each rendered frame to stdout and reads keys from stdin until
   EOF. It is not a substitute for `dune runtest` — use it to eyeball that an
   example still renders and responds to input, not as your only check.
4. **Terminal-truth tests — `test/vterm.re` (a screen model) and
   `test/pty.re` (a real pseudo-terminal).** Steps 1–3 all see *frame text*.
   Neither sees what a terminal does with the escape bytes that carry that
   text, and neither touches the TTY layer at all. Five real bugs hid in
   exactly that gap, including one where every full-width row silently lost
   its last cell while the frames and the byte-exact tests were all
   "correct".

   | Layer | Sees | Cannot see |
   |---|---|---|
   | headless handle (`Runtime.startHeadless`) | component logic, hooks, layout, frame text | anything about escape bytes, the terminal, or the process |
   | Vterm grid (`test/vterm.re`, fed painter output) | what a terminal would *display* — wrapping, erasure, scrollback, alt screen, SGR per cell | termios, signals, timing, real input |
   | PTY session (`test/pty.re`, runs the real binary) | raw mode/ISIG, DSR round trip, mode switching, SIGWINCH, batched reads, exit-by-signal, the restore sequence | nothing above it — but it is the slowest layer, so keep it to a handful of cases |

   **PAIRING RULE: a byte-exact painter expectation must always be paired
   with a Vterm grid assertion.** A byte test only says "the writer still
   emits what its author believed was right" — it encodes a *model* of
   terminal behaviour inside the assertion, and pins that model whether or
   not it is true. The grid assertion is the independent check on the model.
   The rule is written out in full in the header of
   `test/framediff_tests.re`; the paired groups live at the bottom of that
   file and of `test/liveregion_tests.re`.

   Vterm is written from xterm semantics, deliberately **not** from reading
   Matcha's painters. If the model and a painter disagree, that disagreement
   is a finding — never "fix" the model until the painter looks right.
5. **Performance — `lib/Perf.re` tracing.** Steps 1–4 answer "is it
   correct". This one answers "where did the time go", with attribution:
   frame → phase → component → application span. The loop is
   **record → read → optimize → re-record**, and it is the only acceptable
   evidence for a performance claim.

   Record a *scripted* interaction — the same bytes every time, so the
   before and after are comparable:

   ```
   printf '\033[B\033[B\033[Bq' | timeout 60 env MATCHA_HEADLESS=1 \
     MATCHA_WIDTH=200 MATCHA_HEIGHT=45 \
     MATCHA_TRACE=/tmp/before.json dune exec examples/counter/main.exe > /dev/null
   ```

   (`\033[B` is arrow-down, `\033[A` arrow-up. The HANG TRAPS below still
   apply: `timeout`, `MATCHA_HEADLESS=1`, and stdin that reaches EOF.)

   Then **read `/tmp/before.json.summary.txt`**, in this order:
   - `== slowest frames ==` first. It names the worst frame, splits it into
     phases (`render`/`paint`/`effects`/…), and lists the three most
     expensive non-phase spans inside it. That is usually the whole answer.
   - `== spans ==` second, and look at the **self** column, not `total` —
     `total` double-counts nested work. Rows suffixed `~measuring` are the
     layout measure pass over an `Auto` child (a stack renders such a child
     twice per frame); a large `~measuring` row means the *measuring* is
     expensive, not the painting.
   - `dispatch-*` and `timers` spans sit *between* frames, so they appear in
     the spans table only, never under a frame.

   The trace itself (`/tmp/before.json`) is Chrome Trace Event JSON — open it
   in <https://ui.perfetto.dev> for a flame view when the summary is not
   enough. Below span granularity, use macOS `sample <pid>` or Instruments.

   Wrap application-level work in `Perf.span("name", () => ...)` so it shows
   up nested under the component that ran it.

   **NEVER claim a performance win without BOTH a before and an after
   summary of the same scripted interaction.** Quote the two
   `== slowest frames ==` sections side by side.

   Tracing is off unless `MATCHA_TRACE` is set (or `Perf.enable` is called),
   costs nothing when off, and **writes to files only — never to stdout or
   stderr**. That is why goldens stay valid with tracing on; there is a test
   (`test/perf_tests.re`) that renders an example against its existing golden
   with `MATCHA_TRACE` set, to keep it that way. Perf reads
   `Unix.gettimeofday` directly, so the headless virtual clock
   (`advanceTime`) cannot corrupt a measurement — never route it through
   `Hooks.instanceState.now`.

**HANG TRAP: never run an example without `MATCHA_HEADLESS=1`.** Without it,
`Runtime.start` puts the terminal in raw mode and blocks waiting for a real
TTY/keyboard — it will hang the calling process/agent indefinitely.

**HANG TRAP: even in headless mode, the process blocks until stdin EOF.**
`MATCHA_HEADLESS=1 dune exec ...` alone will still hang if stdin is not
closed or redirected. Always combine `timeout N`, `MATCHA_HEADLESS=1`, and
`< /dev/null` (or a pipe that eventually closes) together, exactly as in the
verification-loop command above.

**The ONE exception is `test/pty.re`**, which runs a binary on a real pty
*without* `MATCHA_HEADLESS` on purpose — that is the whole point of the
layer. It is safe only because every session goes through `Pty.withSession`,
which kills and reaps the child in a `Fun.protect` finaliser, and because
`Pty.drain` polls with a hard timeout instead of blocking. Never spawn a
session outside `withSession`, and never wait on a child with a bare sleep.

## Architecture map

| Module | Responsibility | Approx. size |
|---|---|---|
| `lib/Element.re` | Element tree type (`Text`, `Styled`, `VStack`, `HStack`, `Sized`, `Component`, `Lazy`, `WithContext`, `Static`, `WrappedText`, `Viewport`, `Container`, `Overlay`, `Empty`); the `overlayAlign`/`overlayOptions` records behind a floating layer; the `color` type (16 named ANSI colors, `Rgb` into the 216-cube, `RgbFull` 24-bit truecolor emitted as `38;2`/`48;2`) and the ANSI escape/style utilities that emit it; string utils (`visibleLength`, `padToWidth`, `stripAnsi`, `repeatString`, `splitLines` — all cell-based via `TextWidth`); JSX-compatible `Text` (with `~wrap`)/`VStack`/`HStack`/`Sized`/`Container`/`Overlay`/`Fragment`/`TextArea` (the *pure* editor renderer — `renderSegment`/`renderLine`/`make` take `~cursorVisible`; the blinking `<TextArea>` apps use is `lib/TextArea.re`). Its **soft-wrap display mapping** — `wrapSegments`/`displayRows`/`cursorDisplayRow` turn logical lines into `(logicalRow, startCell, cellCount)` display rows, `make` paints the window that keeps the cursor visible, and `measure` reports that height so a container can size itself around a growing input — is display only: `handleKeyDown` and every cursor/selection column stay logical/`Static` component modules; the simple non-layout `render` function, which delegates `Component` nodes to Runtime through the `componentRenderer` ref. `Component(typeId, key, props, renderFn)` is a pure *description* of a call site — no mutable per-instance state, no output cache. | ~1800 lines |
| `lib/Runtime.re` | Layout engine (flex distribution, align/justify, size resolution) and `renderElement` — one recursive renderer with a real mode (applies layout) and a **measuring mode** (`~measuring=true`, layout-free, used to find an `Auto` child's natural size, so stacks visit those children twice per frame), plus `~origin` threading for mouse bounds (the *committed* pass is `!measuring && origin != None`); component identity as a tree path (`childPath`/`componentPath` → the per-instance path→stableId registry); the commit phase (render, `Hooks.commitEffects`, unmount sweep, key-handler collection, `commitFocus`, static drain); detached rendering for `Element.render`; the interactive main loop (`start(~screen: screenMode=Inline, ...)`) — `Inline` rendering through `LiveRegion` (DSR cursor tracking, relative addressing) or `Fullscreen` rendering on the alternate screen through `FrameDiff.diff` (frame padded to `termHeight`, absolute addressing, `<Static>`/`useStdout` rejected), terminal setup, SIGWINCH, wake-pipe, `InputDecoder`-fed event dispatch, interest-driven mouse-mode enable; the container-query stack (`containerStack`/`getContainerSize`, seeded with the frame by every loop and pushed by `Element.Container` *and* by an `Overlay`); floating layers (`frameSize`, the `overlayFrame` queue + `recordOverlay`, and `compositeOverlays` — the splice that paints layers over the finished frame, publishes them to `Hooks`, and returns `base` physically unchanged when none is open); headless support (`startHeadless` and its handle: `sendKey`/`sendPaste`/`sendMouse`/`getOutput`/`getLines`/`getStaticOutput`/`getFocusedId`/`advanceTime`/`setTerminalBackground`/`resize`/`quit`); `getConstraints`/`getContainerSize`/`constraints`. Read its module header for the full render model. | ~2280 lines |
| `lib/Hooks.re` | Hook storage (`StateHook`/`EffectHook`/`MemoHook`/`RefHook`) and per-component render contexts; slot hooks `useState`/`useEffect`/`useEffectAlways`/`useMemo`/`useRef` and registration hooks `useKeyDown`/`useInput`/`useMouse`/`useFocus`/`useFocusManager`/`useQuit`/`useStdout`/`useTerminalBackground` (the terminal's own background color, from the startup OSC 11 probe — `None` until it answers, and possibly forever); timers (`useInterval`/`useTimeout`, virtual-clock backed headlessly); the `instanceState` record that holds *all* per-application state (component contexts, path→ID registry, root context, effect commit queue, focus state, timers, static/raw output queues, component bounds, `terminalBg`) — Runtime installs a fresh one per start; effect scheduling with commit-phase dep writes; the `overlayLayer` stack (members/box/onDismiss, published by `compositeOverlays`, topmost first) that `collectKeyHandlers`, `commitFocus` and `dispatchMouse` all filter against; `dispatchKey` (Tab focus cycling, then `keyHandlers` then the captured `inputHandlers`) and `dispatchMouse` (innermost-wins, wheel-interest, member-only under a layer, outside-Down dismisses and is swallowed whole). | ~1630 lines |
| `lib/Key.re` | `Key.t` ADT (incl. `Text` for multi-byte input and `Paste`) and the raw-byte escape-sequence parser (`parse`) that normalizes terminal input — arrows, Ctrl/Alt/Meta/Shift combinations, backtab, CSI-u/kitty sequences, Backspace/Delete/Tab/KillLine/KillWord. | ~380 lines |
| `lib/TextWidth.re` | UTF-8 decoding and terminal display width: `decodeUtf8`, `charWidth` (wcwidth-style), ANSI-aware `stringWidth`, and the `cell` splitter `toCells`. All layout measurement is done in the columns this reports. | ~260 lines |
| `lib/StyledText.re` | ANSI-aware wrapping/truncation of already-rendered styled text: `parse`/`bake` (styled string ↔ per-cell chunks), `wrapString` (behind `<Text wrap>`), truncate variants, `sliceLines` (behind `Viewport`), and the splice pair `splitAtWidth`/`padChunksToWidth` (behind `compositeOverlays`: cut a row at a column keeping both halves — a double-width cell straddling the cut blanks both sides — and pad a short row out, which is what makes an overlay opaque). Its SGR parser knows the closed set Matcha emits, including `38;2`/`48;2` truecolor, so a clipped or wrapped row re-opens the exact same color. Pure. | ~560 lines |
| `lib/InputDecoder.re` | Stateful byte-stream assembler between `Terminal.readBytes` and dispatch: reassembles raw reads into `KeyEvent`/`PasteEvent` (bracketed paste)/`MouseEvent`/`CursorReport`/`OscReport` (an OSC string reply, split into code + payload; `ESC ]` … BEL or ST) regardless of how bytes were split across reads (`feed`/`flush`; lone-ESC 25ms deadline). An unterminated OSC is discarded at flush, never replayed as keys. | ~400 lines |
| `lib/LiveRegion.re` | Pure inline frame patcher with RELATIVE cursor addressing: `patch` turns the painted live region into the next frame, committing `<Static>`/`useStdout` lines above it; `erase` removes the region. What the interactive loop writes — no `ESC[2J`, sync guards around each paint. | ~270 lines |
| `lib/Perf.re` | Performance tracing, off by default. Records nested spans (`span`/`frame`/`instant`, plus the closure-free `recordComponent` the renderer calls per component) and, on `flush` or at process exit, writes a Chrome Trace Event JSON plus a plain-text `.summary.txt` digest (span table with **self** time, slowest frames broken down by phase). Enabled by `MATCHA_TRACE=<path>` or `Perf.enable`. Two invariants: it NEVER writes to stdout/stderr (goldens stay valid with tracing on), and it reads `Unix.gettimeofday` directly, never the headless virtual clock. Its module-level state is a deliberate, documented exception to the "no module-level app state" gotcha — tracing is process-global tooling that outlives any one `instanceState`. | ~430 lines |
| `lib/Mouse.re` | SGR (1006) mouse event types, `parseSgr`/`encodeSgr`, and rect helpers (`contains`/`intersect`) for the bounds registry. Pure. | ~210 lines |
| `lib/ScrollView.re` | `<ScrollView>` — a focusable, wheel-scrollable window onto taller content, built on `Element.Viewport`; uncontrolled by default, controllable via `~offset`/`~onScroll`; `scrollbarMetrics` is the pure thumb geometry. Two content modes: children (rendered whole, then clipped — O(total content) per frame) or `~rows`, an array of pre-baked style-self-contained rows that ignores the child and touches only the visible window — O(viewport) per frame, for long pre-rendered content. | ~250 lines |
| `lib/Modal.re` | `<Modal isOpen title width height align shadow onDismiss>` — a bordered dialog floating over the frame, built on `Element.Overlay`. `createElement` returns `Lazy(() => isOpen ? Overlay(component, opts) : Empty)`, **never a component wrapping an Overlay** (see the gotcha): that keeps its cost at zero layout rows in both states and puts its hooks INSIDE the layer, where they are members. The inner component reads its box from `useContainerSize()` (the overlay pushes it), draws the `BoxChars` border with an optional title, owns Esc through a captured `useInput`, and saves/restores `focus.focusedId` across its own mount/unmount. May depend on `Runtime`; nothing in `Runtime` may reference it. | ~230 lines |
| `lib/TextArea.re` | `<TextArea>` as applications get it (`Matcha.TextArea`): `include Element.TextArea` for everything pure, plus a shadowing `createElement` that wraps the renderer in a real component owning the cursor blink (`useState` + `useInterval` at 530ms, feeding `~cursorVisible`). `~blink=false` opts out; the blink is disabled under `MATCHA_HEADLESS=1` stream mode. Adds `~key` support the element-level `createElement` never had. | ~105 lines |
| `lib/Clickable.re` | `<Clickable onClick>` — click target sized to the box its parent allocated; innermost-under-pointer wins; wheel passes through unless `~onMouseDown` is given. | ~80 lines |
| `lib/FrameDiff.re` | Pure line-diff between frames with ABSOLUTE addressing on a cleared screen. This is what paints **Fullscreen** (alternate-screen) mode; `Inline` mode paints via `LiveRegion` instead. | ~115 lines |
| `lib/Terminal.re` | The only module doing real terminal I/O: raw mode (via a C stub, `terminal_stubs`), cursor show/hide, screen clear, terminal size, raw byte reads, bracketed-paste/kitty/mouse mode toggles, `queryBackground` (the OSC 11 theme probe). `restoreTerminal` owns the exit sequence and its ordering matters: `ESC[<u`, `ESC[?1049l`, `ESC[<u` (kitty stacks are per screen buffer — see the gotcha below), then `?2004l`, `?1002;1006l`, show cursor. | ~215 lines |
| `lib/Context.re` | React-style context: `create`/`provide`/`use`, plus the `Context.Make` functor for typed provider/consumer modules. | ~120 lines |
| `lib/Matcha.re` + `lib/Matcha.rei` | Public API surface — re-exports the modules above plus convenience aliases (`flex`/`percent`/`chars`, color constructors, `useContainerSize`, `useStdout`, headless helpers). The `.rei` **pins** that surface: adding a `let` to `Matcha.re` alone does not export it, and removing one is a build error. Read these first when answering "does Matcha support X". | ~120 + ~285 lines |
| `lib/Component.re`, `lib/Event.re` | Thin convenience re-exports of a subset of `Hooks` (`Component.useState`; `Event.useQuit`/`useKeyDown`/`useFocus`/`useFocusManager`/`useInput`/`useMouse`) used throughout the examples. | ~16/~28 lines |
| `ppx/ppx_component.ml` | The `[@component]` PPX: rewrites JSX (`Module.createElement(~prop=v, ~children=[...], ())`) and expands `[@component] let make = (~a, ~b) => {...}` into a generated `props` record type, `make: props => Element.t`, and a labeled `createElement` that wraps the render in `Element.createComponent`. | ~430 lines |

## Gotchas

- **Component identity = tree path + ppx `typeId` + `key`.** Not render
  order, not the `renderFn` pointer. `Runtime.componentPath` appends a
  component's type ID (and `key`, if given) to its parent's path, and
  `childPath` appends a stack child's index; the resulting string is mapped
  to a stable `componentId` through the per-instance `componentIdRegistry`,
  and that ID keys the hooks context. Because a path depends only on the tree
  *above* a component, a conditional sibling appearing or disappearing does
  not shift anyone else's state. Two components of the same type at the same
  position are the same instance — that's what `key` is for.
- **Effects commit once per frame, after the whole tree renders.** Rendering
  a body only *schedules* effects onto its context (`useEffect` compares
  deps) and queues that context (`Hooks.enqueueEffects`). `Hooks.commitEffects`
  then drains the queue — children before parents, root last — and runs each
  context's pending effects. The dep slot is written **at commit time, after
  the effect ran**, never at schedule time: a component can render twice in
  one frame (a stack measures an `Auto` child, then renders it for real), and
  both passes must schedule the same effect so the frame commits it exactly
  once. If you touch effect scheduling, preserve that ordering.
- **Every visited component is always rendered; there is no output cache.**
  A skipped render is not an optimization here, it's data loss: the skipped
  subtree never reaches `recordRenderedComponent`, so
  `Hooks.cleanupUnmountedComponents` treats those descendants as unmounted and
  destroys their contexts. (A per-element `cachedOutput`/`stableIdRef` pair
  used to exist and was removed for exactly this reason.) The `props` field on
  `Element.Component` is still carried for a possible future memoization pass,
  but nothing reads it today.
- **Each `start`/`startHeadless` gets a fresh `Hooks.instanceState`; `quit()`
  runs cleanups.** All per-application state (contexts, ID registry, ID
  counter, root context, effect queue) lives in that record, so two apps
  started in the same process — e.g. several headless handles in one test
  run — can't see each other. Exactly one instance is in force at a time
  (`Hooks.currentInstance`); each headless handle re-installs its own before
  every operation. Interleaving instances across threads is unsupported. Don't
  add new mutable app state at module level — put it in `instanceState`.
- **`==`/`!=` vs `===`/`!==` — and why `Hooks.re` uses the physical ones.**
  In Reason, `==`/`!=` are OCaml's structural `=`/`<>`; `===`/`!==` are
  OCaml's physical `==`/`!=`. Two real bugs (`Hooks.depsEqual`, and the
  now-removed `Hooks.propsChanged`) came from using structural comparison
  on `Obj.t` values that can wrap closures — OCaml's structural `compare`
  raises `Invalid_argument("compare: functional value")` when it hits a
  function inside the compared value. Both were fixed to use `!==`
  (physical inequality), which is safe for `Obj.t` (compares pointers for
  heap values, values directly for immediates) and doesn't attempt to look
  inside the value. **Do not use `Obj.magic` to `nativeint` for this either
  — it segfaults on immediate values in OCaml 5** (this hazard has since been
  removed from the identity path, which is now plain string paths, but the
  rule still governs `depsEqual` and any new `Obj.t` comparison). If you're
  comparing anything that might contain a closure (deps arrays, props,
  cached callbacks), use `!==`/`===`, never `!=`/`==`.
- **Slot hooks vs registration hooks — only slot hooks have the "no
  conditionals" rule.** `useState`/`useEffect`/`useEffectAlways`/`useMemo`/
  `useRef` (and the timers built on them) consume a numbered hook slot and
  must run unconditionally, in the same order, every render. `useKeyDown`/
  `useInput`/`useMouse`/`useFocus`/`useFocusManager`/`useQuit`/`useStdout`
  are *registrations* re-collected from scratch each frame — calling them
  conditionally is safe and idiomatic (that is how `~isActive` gating
  works).
- **The kitty keyboard stack is kept PER SCREEN BUFFER; restore must leave
  the alt screen BETWEEN its two pops.** kitty, Ghostty and recent iTerm2
  hold a separate keyboard-flag stack for the main and the alternate
  screen. `Terminal.setRawMode` pushes `ESC[>1u` on whichever screen is
  live — the main one — and `Runtime.start`'s `Fullscreen` setup pushes
  again (`Terminal.pushKittyKeyboard`, the single definition of that
  emission) right after `enterAltScreen`, because the alt screen would
  otherwise start with default flags and a fullscreen app would silently
  lack the disambiguation the inline path has. `Terminal.restoreTerminal`
  therefore writes **`ESC[<u`, `ESC[?1049l`, `ESC[<u`** in that order: pop
  the current screen, leave the alternate screen, pop the main screen's
  stack. Popping once and *then* leaving the alt screen — the order this
  code shipped with — pops the alt stack, discards it with the screen
  switch, and strands the main screen's push, so the user's shell emits
  CSI-u for Ctrl+C for the rest of the session. That was a real
  user-reported bug. Per the kitty spec an over-pop clamps harmlessly, so
  the double pop is safe inline and on a crash before any push; terminals
  without the protocol ignore all of it. `test/vterm.re` models both
  stacks (`kittyDepth`, `kittyDepthMain`) and both `test/pty_tests.re`
  lifecycle cases assert `kittyDepthMain == 0` after exit.

- **An INLINE app must be SHORTER than the terminal — a full-height app
  belongs in `Fullscreen`.** Inline paints the live region at the cursor, so
  a frame as tall as the terminal forces the terminal to *scroll* to make
  room, pushing the user's prompt and history up and away. Quitting erases
  the region correctly, but nothing can un-scroll a terminal, so the user is
  left with a screenful of blank rows between their last command and the new
  prompt. This is not a bug in the erase — it is an app that asked for the
  whole screen while rendering inline, and `examples/command-menu` shipped
  that way until a user hit exactly this. **If your root Flexes to fill the
  screen, pass `~screen=Fullscreen`**: the alternate screen is restored
  exactly on exit. Compact inline shape: `examples/chat`,
  `examples/static-demo`. Fullscreen shape: `examples/claude-code`,
  `examples/command-menu`. Pinned by a PTY pair — the fullscreen case
  asserts no inline region erase ever appears, and the chat case asserts the
  same detector *does* fire, so the guard cannot pass vacuously.
- **The interactive loop renders INLINE by default; `quit(ClearScreen)`
  erases only the live region.** In `Inline` mode there is no
  `ESC[2J`/alt-screen: the app paints at the current cursor position via
  relative movements (`LiveRegion.patch`, inside `ESC[?2026h/l` sync
  guards), `<Static>`/`useStdout` output is committed above it into normal
  scrollback, and quitting erases the region while leaving the transcript.
  `Runtime.start(~screen=Fullscreen, ...)` is the other mode: alternate
  screen, frame padded to the full viewport, painted absolutely through
  `FrameDiff.diff`, no DSR/`bottomRow` tracking, `liveTop` fixed at 1, and
  restore-by-leaving-the-alt-screen for both quit behaviors. Because the alt
  screen has no scrollback, `<Static>` and `useStdout().write` **raise
  `Invalid_argument` there** rather than silently dropping output — one
  flag, `Hooks.instanceState.staticAllowed`, which `start` clears for
  Fullscreen and both call sites check. Every fullscreen difference is
  pattern-matched on the mode; Inline's bytes are unchanged. Headless
  ignores `~screen` entirely and keeps `<Static>` working. Anything that
  writes to stdout directly mid-run
  corrupts the region — use `useStdout` for that. Raw mode disables ISIG
  and IXON, so Ctrl+C/Ctrl+Z/Ctrl+S arrive as ordinary key events — an app
  must bind its own quit key (every example does), and nothing kills the
  process out from under the `at_exit` terminal restore. Startup also emits
  **one OSC 11 background-color query** (`ESC]11;?BEL`) in *both* screen
  modes — the theme-detection probe behind `Hooks.useTerminalBackground`.
  It is fire-and-forget like the DSR query, and it is the only byte sequence
  an interactive session emits that it did not emit before; the PTY
  unknown-sequence audit (`test/pty_tests.re`) is what pins that.
  `InputDecoder` frames the reply out of the input stream as an
  `OscReport(11, payload)` — it is never a key event — and `deliverAll`
  parses it and marks the root dirty **only when the value changed**, so a
  theme costs exactly one extra frame.
- **Byte-fed loops deliver one event per frame.** When one `read()` yields
  several decoded events (fast typing, scripted stdin), `deliverAll`
  re-renders between them (`~flushDirty`) so each handler closes over fresh
  state — without this, two value-based `setState` keystrokes in one batch
  clobber each other ("hi" becomes "i"). The headless *handle* path gets the
  same guarantee by re-rendering inside every `sendKey`/`sendPaste`/
  `sendMouse`. Preserve this if you touch either loop; the process-level
  regression lives in `test/chat_tests.re` ("batched into one read").
- **`<Static items renderItem>` is append-only, watermark-committed.** Each
  Static node commits `items[watermark..]` once, on the frame that first
  renders them, then never again — items are *output*, not live state: an
  item's component mounts on its commit frame and unmounts on the next, so
  it must not own ongoing state (spinners, subscriptions). Mutating or
  reordering already-committed items does nothing. **Inline only**: under
  `~screen=Fullscreen` both `<Static>` and `useStdout().write` raise
  `Invalid_argument` (there is no scrollback on the alternate screen), so a
  fullscreen app keeps its transcript in state and renders it — see
  `examples/claude-code`. See `examples/static-demo`'s header comment for
  the full inline contract.
- **Tab is consumed by focus cycling only while at least one focusable is
  registered.** `Hooks.dispatchKey` moves focus on Tab/Shift+Tab *before*
  app key handlers and swallows the event — but only when `useFocus`
  registrations exist, so a no-focus app still sees raw Tab. `useInput`
  handlers fire regardless of focus unless gated with
  `~isActive=isFocused` (the standard idiom — see `test/focus_tests.re`).
- **Wheel events route by wheel *interest*, not plain innermost-wins.**
  `dispatchMouse` sends ScrollUp/ScrollDown to the innermost containing
  component whose `useMouse` declared wheel interest (`~wheel`, default
  `true`); `<Clickable>` without `~onMouseDown` opts out, so wheel over a
  clickable row inside a `<ScrollView>` scrolls the list. All other mouse
  events go to the innermost component with any handler; there is no
  bubbling.
- **The `[@component]` PPX only runs over `test/` and `examples/`, not
  `lib/` itself.** Check any `dune` file's `(preprocess (pps
  ppx_component))` stanza before assuming JSX/`[@component]` works in a
  given directory; `lib/dune` has no such stanza, so `lib/Element.re`'s
  `Text`/`VStack`/etc. modules are hand-written in the expanded form the PPX
  would otherwise generate (`type props`, `make`, `createElement`).
- **Coincidence defaults: never test layout at 80x24 only.** A test whose
  size happens to equal the codebase's fallback value cannot tell a computed
  value from a defaulted one. `Runtime`'s constraints default to 80x24,
  `getHeadlessConfigFromEnv` defaults to 80x24, and
  `caml_get_terminal_size`'s non-TTY fallback is 80x24 — so a stale-state
  bug that leaves the root's `useContainerSize()` reading the *default*
  constraints instead of the current frame's is completely invisible to an
  80x24 test. That is exactly how the root stale-constraints bug survived:
  the buggy fallback and the test default were the same numbers. Any test
  that touches sizing, wrapping, flex distribution, truncation or resize
  must run at least one **non-default** size (the PTY resize case uses
  100x30, the golden components use 40x16/40x10/40x8). A size assertion that
  reads "80" is not evidence unless something in the test made it 80.
- **Responsive queries are container-relative, and boundaries are
  explicit.** `useContainerSize()` (the *only* size hook — `useLayout` is
  gone) answers with the nearest enclosing `<Container>`'s box, or with the
  whole frame when there is none. So a root-level component reads the
  terminal, and a component inside a wrapped pane reads the pane.
  - `<Sized>` and `<ScrollView>` are **not** boundaries. Wrapping something
    to nudge its layout must never silently re-target its descendants'
    responsive queries — declare a `<Container>` where you want one. That is
    why `examples/layout-demo` and `examples/layout-alignment` now wrap their
    self-sizing children explicitly: those components want their *own* slot,
    which is a container they have to declare.
  - `Percent(n)` is untouched: it stays parent-relative, like CSS `%`.
    Containers affect **queries only**, never layout.
  - `Element.Container(child)` is layout-transparent by contract — same
    constraints, same origin, same tree path, so adding or removing one can
    move no cell and reset no hook. `getSizeHint`, `unwrapSized` and
    `isInvisibleToLayout` all see through it for that reason (`unwrapSized`
    unwraps *inside* it and keeps the node — dropping it would drop the
    boundary). `test/container_tests.re` pins the transparency with a
    byte-identical with/without frame comparison; if you change the
    `Container` case, that test is the gate.
  - The stack is pushed in **measuring mode too**. A stack measures an `Auto`
    child and then renders it for real; a component whose output depends on
    its container must answer the same in both passes or its measured size
    will not match what it paints.
  - Nothing in the tree calls `Terminal.getSize()` any more except `Runtime`
    itself. Application code that did (two `TerminalContext` providers and
    `layout-demo`'s header) bypassed `MATCHA_WIDTH`/`MATCHA_HEIGHT` entirely
    and reported 80x24 under every headless run.
- **`useKeyDown` always fires; `useInput` is captured by the topmost
  overlay.** They are two separate handler lists on the render context, and
  `collectKeyHandlers` filters them differently: every component's
  `useKeyDown` handlers are collected unconditionally, while `useInput`
  handlers are collected only from the topmost open layer's **members** (the
  root context's own `useInput` is dropped entirely while a layer is open —
  the root is base, not a member). With nothing open, both lists come from
  everything, exactly as before overlays existed.
  - **Bind Ctrl+C with `useKeyDown` or your app is unquittable under a
    modal.** Raw mode disables ISIG, so Ctrl+C is an ordinary keypress that
    only the application can act on. A `useInput` binding for it would be
    suppressed the moment a dialog opened, and there would be no way out.
    `test/modal_tests.re` and the `command-menu` PTY case both guard this;
    an exit reported as `Signaled(2)` is that bug.
  - `<ScrollView>` goes quiet under a modal for free — it is `useFocus` +
    `useInput` and nothing else.
  - **Recorded ordering change:** all `useKeyDown` handlers now run before
    all `useInput` handlers, where the two used to interleave in tree order.
    Harmless because there is no `stopPropagation`, and it moved no test.
- **A layer's members are what is rendered INSIDE the `Overlay`.** Runtime's
  `Overlay` case snapshots `renderedComponentIds` before rendering the child
  and walks the newly-prepended prefix with `===` back to the saved cons
  cell; that prefix is exactly the membership set. Two consequences:
  - `<Modal>` is `Lazy(() => isOpen ? Overlay(component) : Empty)`, **never a
    component that returns an `Overlay`**. A component wrapper would put the
    modal's own hooks *outside* the layer, where its own modal would suppress
    them (its Esc binding would stop working the moment it opened) — and it
    would cost a layout row, because `isInvisibleToLayout` deliberately does
    not look through `Component` while it does look through `Lazy`.
  - The same reasoning applies to anything you write yourself: a component
    that merely *returns* an `Overlay` still takes a blank row in every stack
    that holds it. Put the node in the stack.
- **Overlays are invisible to layout and composited after it.** An
  `<Overlay>`/`<Modal>` in a stack consumes no row, no gap slot and no
  justify share; `Runtime.compositeOverlays` splices the recorded layers over
  the finished frame, right after the render walk and before the static drain
  and the paint. Two invariants hold it together:
  - **Nothing open returns `base` physically unchanged** — not re-parsed, not
    re-baked. That is what keeps every existing golden valid, and why the
    `composite` Perf span is *absent* from a frame with no modal.
  - The box, its position, and its clip all resolve against `frameSize` (the
    whole frame) and never against the enclosing slot or the enclosing clip:
    a modal opened from inside a `<ScrollView>` floats over the window rather
    than inheriting that scroller's visible rect. The overlay pushes its box
    onto `containerStack` too, so `useContainerSize()` inside a dialog reports
    the dialog.
  - Cost, when one IS open: `compositeOverlays` parses and re-bakes the whole
    frame, so it is O(frame). Measured at 100x30 on `examples/command-menu` it
    is ~0.9 ms/frame — small in absolute terms (≈5% of a 60 fps budget) but a
    visible share of that app's unusually cheap 3 ms frame. Baking only the
    spliced rows is the obvious optimization and is *not* free: a row emitted
    verbatim is only correct if no style was left open entering it.
- **The README can lag reality; `lib/Matcha.rei` is the API source of
  truth.** It's short and odoc-commented — read it before trusting prose docs
  (including this file) about what's exported, and edit it (together with
  `Matcha.re`) when you deliberately change the public surface. Notably,
  `Element.Fragment` exists but is *not* re-exported at the top level (it's
  still reachable as `Matcha.Element.Fragment`).
- **`matcha` is a published opam package, and it installs a library and
  nothing else.** Never give an `examples/*/dune` executable a
  `(public_name ...)`: that puts it in the `bin:` section of
  `matcha.install`, so `opam install matcha` drops a demo binary into the
  user's PATH. This actually shipped in 0.1.0, with all 15 of them.
  `test/packaging_tests.re` fails if a `bin:` section reappears, and
  `scripts/release.sh` refuses to tag. Run examples by path —
  `dune exec examples/counter/main.exe`. Likewise, `matcha.opam` is
  generated: **edit `dune-project`**, or `matcha.opam.template` for the two
  fields dune cannot express (`available:`, `x-maintenance-intent:`).

## Releasing

`CHANGELOG.md` is the record; `RELEASING.md` is the procedure;
`scripts/release.sh` (checks, tag, GitHub release) and `scripts/opam-pr.sh`
(the opam-repository pull request) carry it out. Both take `--dry-run`.

The version lives **only** in the git tag — `dune-project` has no version
field, so a release needs no version bump and no release commit, just a
`## <version>` section in `CHANGELOG.md` before you start.
