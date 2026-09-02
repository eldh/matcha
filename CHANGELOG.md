# Changelog

All notable changes to Matcha are recorded here. Versions follow semantic
versioning, with the 0.x caveat that a minor bump may break the API.

## 0.3.0 — 2026-09-02

Four traps that application authors kept falling into, turned into
behaviour the framework enforces or removes. A minor bump rather than a
patch: one of them makes a program that "worked" raise instead.

### Breaking

- **An Inline frame as tall as the terminal now raises `Invalid_argument`**
  instead of painting. Inline mode paints at the cursor, so such a frame
  forces the terminal to scroll, pushing the user's prompt and scrollback
  away permanently — nothing can un-scroll a terminal. The message names
  the fix (`Runtime.start(~screen=Fullscreen, ...)`), and the check runs
  before the first offending frame reaches the terminal rather than after
  the damage. `Runtime.inlineFrameTooTall` is the pure predicate behind it.

  Turning this on immediately caught four of matcha's own examples —
  `scroll-demo`, `people-list`, `layout-demo` and `layout-alignment` — all
  of which had been doing this at 80x24 since they were written. They are
  `Fullscreen` now. Headless ignores `~screen`, which is why no golden
  moved and why nothing had ever noticed.

### Fixed

- **`useMemo` and `useEffect` now treat equal strings as equal
  dependencies.** They were compared physically, so a string dependency —
  a fresh block every render — never matched, and every memo holding one
  recomputed on every frame while appearing to be memoized. Immediates
  already worked. Nothing further is compared structurally, deliberately:
  `compare` raises on a closure and loops forever on a cyclic structure,
  so widening this would trade a slow memo for a hang.

- **`<ScrollView>` no longer swallows clicks.** Its mouse handler acts on
  the wheel and ignores everything else, but registering it made the
  ScrollView a hit-test target for clicks too — so a click on a `~rows`
  list, which has no child elements to hit, was consumed and dropped with
  no error anywhere. `Hooks.useMouse` gains **`~click`** (default `true`),
  the mirror of the existing `~wheel`, and `<ScrollView>` passes
  `~click=false`.

### Added

- **`MATCHA_HEADLESS_MAX_MS`** bounds the headless loop by wall-clock,
  whatever stdin is doing. And under `MATCHA_HEADLESS=1`, a stdin that is a
  **terminal** now renders one frame and exits rather than blocking on
  input that is never coming. Both make the documented
  `timeout` + `MATCHA_HEADLESS=1` + `< /dev/null` invocation harder to get
  wrong; neither replaces it when stdin is a pipe.

- **A `matcha-app` Claude Code skill**, distributed from this repository as
  a plugin marketplace (`.claude-plugin/marketplace.json`). It teaches
  building an application on matcha, and consumers can opt in per project
  from their own `.claude/settings.json` — see the README.

## 0.2.0 — 2026-09-01

The first release with a pinned public interface. `lib/Matcha.rei` now
declares the full API surface, so a symbol is public only if it appears
there. 0.1.0 exported whatever `Matcha.re` happened to contain.

This release rewrites most of the framework. Treat it as a new API rather
than an upgrade: the layout primitives, the text API and the runtime entry
points all changed.

### Breaking

- **Layout primitives replaced.** `Column`, `Row` and `Box` are gone. Use
  `VStack`, `HStack` and `Sized`, with sizes given as `Flex(n)`,
  `Percent(n)`, `Chars(n)` or `Auto`.
- **Text API replaced.** The `Bold`, `Dim`, `Italic`, `Underline` and
  `Inverted` wrapper components are gone. Style is now given as props on
  `<Text>` (`~bold`, `~dim`, `~italic`, `~underline`, `~inverted`,
  `~color`, `~backgroundColor`).
- **The public surface is pinned by `lib/Matcha.rei`.** Anything that was
  reachable through `Matcha` in 0.1.0 but is absent from the interface file
  is no longer exported.
- **Minimum OCaml is 5.3.0**, and `ppxlib` is constrained to
  `>= 0.36.0 & < 0.39.0`.
- **The `matcha-example-*` binaries are no longer installed.** They were
  never meant to ship; installing 0.1.0 put 15 demo executables in your
  PATH. Run them from a clone with `dune exec examples/<dir>/main.exe`.
- **Windows is declared unsupported** (`available: os-family != "windows"`).
  The terminal layer needs `termios` and `ioctl`. This documents what was
  already true in 0.1.0.

### Added

- **Headless runtime.** `Matcha.startHeadless` returns a handle that renders
  frames to strings, sends keys, advances timers and reads the output — no
  terminal, no TTY, no subprocess. `MATCHA_HEADLESS=1` runs any application
  the same way from the command line, with `MATCHA_WIDTH` / `MATCHA_HEIGHT`
  setting the simulated size. This is what lets an application be tested, or
  driven by an agent, without a human at a keyboard.
- **Fullscreen mode.** `Runtime.start(~screen=Fullscreen, ...)` uses the
  alternate screen and paints with absolute cursor addressing. The default
  stays `Inline`, which paints a live region at the cursor and leaves the
  scrollback intact. `<Static>` and `useStdout` are rejected in `Fullscreen`,
  where there is no scrollback to commit to.
- **Overlays and modals.** `<Modal>` and the lower-level `<Overlay>` composite
  over the finished frame without taking part in layout. An open modal
  captures `useInput`, contains the focus ring and restores focus when it
  closes. `useKeyDown` still fires, so an application under a modal stays
  quittable.
- **Container queries.** `<Container>` declares a query boundary and
  `useContainerSize` reports the nearest one, so a component is responsive to
  the region it was placed in rather than to the window.
- **`<ScrollView>`**, with a focus ring, wheel scrolling, controlled and
  uncontrolled offsets, and a virtualized `~rows` mode that keeps frame cost
  proportional to the viewport rather than to the content.
- **`<Static>`**, which commits finished output to the scrollback above the
  live region, and `useStdout` for writing there directly.
- **`<TextArea>`**, a single- or multi-line input with a cursor, selection,
  soft wrapping and bracketed paste.
- **`<Clickable>` and mouse support.** `Mouse` and `useMouse` deliver click,
  drag, wheel and motion events, decoded from SGR mouse reporting.
- **Truecolor and background detection.** `rgbFull` emits 24-bit colour, and
  `useTerminalBackground` reports the terminal's background through OSC 11,
  so an application can adapt to a light or dark theme.
- **`Matcha.Perf`**, a span tracer. `MATCHA_TRACE=<path>` writes a Chrome
  trace plus a text summary, which makes "why is this frame slow" a
  measurement rather than a guess.
- **More hooks**, on `Matcha.Hooks`: `useMemo`, `useRef`, `useInterval`,
  `useTimeout`, `useFocus`, `useFocusManager`, `useInput` and `useMouse`.
  `useContainerSize`, `useStdout` and `useTerminalBackground` are also
  aliased at the top level.
- **`StyledText`** and **`TextWidth`**, which handle style-preserving slicing
  and East Asian wide characters, emoji and combining marks, so wide text
  no longer shifts the columns to its right.
- **`Element.Fragment`** for returning several children without a wrapper.
  It is reachable as `Matcha.Element.Fragment`, not at the top level.

### Fixed

- A segfault from `Obj.magic` to `nativeint` on immediate values. Component
  identity is now a plain string tree path.
- `useMemo` raising `Invalid_argument("compare: functional value")` when a
  dependency held a closure. Dependencies are compared physically.
- Component state being lost when two different trees rendered the same
  component hierarchy.
- The last column of a full-width row being erased by the `ESC[K` that
  followed it. Both painters now clear before they paint.
- The kitty keyboard protocol leaking past a fullscreen quit, which left
  Ctrl+C broken in the shell afterwards.
- Keystrokes being dropped while typing quickly, because batched input
  events were delivered without re-rendering between them.
- Ctrl+C being swallowed by the terminal driver instead of reaching the
  application.

## 0.1.0 — 2025-12-10

First release. Component tree, hooks (`useState`, `useEffect`, `useMemo`),
Context, `Column` / `Row` / `Box` layout, keyboard input, and the
`[@component]` PPX with JSX.
