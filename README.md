# Matcha

A React-like terminal UI (TUI) framework for ReasonML/OCaml. Build interactive
command-line applications with a component-based architecture, hooks, JSX
syntax, and a flexbox-like layout engine.

```
open Matcha;

[@component]
let make = () => {
  let quit = Event.useQuit();
  let (count, setCount) = Component.useState(0);

  Event.useKeyDown((key, _modifiers) => {
    switch (key) {
    | Key.Char('q') => quit(ClearScreen)
    | Key.Arrow_up => setCount(count + 1)
    | Key.Arrow_down => setCount(max(0, count - 1))
    | _ => ()
    }
  });

  <VStack>
    <Text bold=true> "Counter" </Text>
    <Text> {"Count: " ++ string_of_int(count)} </Text>
    <Text dim=true> "up/down to change, q to quit" </Text>
  </VStack>;
};

module App = { let make = make; };
let () = Runtime.start((module App));
```

## Features

- **Component model** — functions marked `[@component]` become JSX-usable
  components with props, backed by a PPX that rewrites JSX and generates
  boilerplate.
- **Hooks** — `useState`, `useEffect`, `useEffectAlways`, `useMemo`, `useRef`,
  `useKeyDown`, `useQuit`, `useContainerSize`, `useInterval`, `useTimeout`,
  `useFocus`, `useInput`, `useMouse`, `useStdout` — familiar to anyone who has
  used React or Ink.
- **Context API** — pass data through the tree without prop drilling, with a
  typed `Context.Make` functor for ergonomic provider/consumer modules.
- **Flexbox-style layout** — `VStack` / `HStack` containers with `Flex`,
  `Percent`, `Chars`, and `Auto` sizing, plus CSS-flexbox-like `align` and
  `justify`.
- **Unicode-aware text** — measurement, padding and truncation are done in
  terminal display cells (`TextWidth`), so CJK, emoji and combining marks lay
  out correctly; `<Text wrap=Wrap>` word-wraps styled text without breaking
  ANSI sequences.
- **Styled text** — bold, dim, italic, underline, inverted, 16 named colors,
  and a 216-color RGB cube, applied as props on `<Text>` rather than wrapper
  elements.
- **Multi-line text editing** — `TextArea` with cursor movement, word-jump,
  selection, and clipboard-free editing shortcuts out of the box.
- **Inline rendering with a static transcript** — apps paint an in-place live
  region at the cursor (no alternate screen, no full clears);
  `<Static items renderItem>` and `useStdout` commit lines *above* it into
  normal terminal scrollback, Ink-style, exactly once each. A fullscreen
  alternate-screen mode also exists —
  `Runtime.start(~screen=Fullscreen, (module App))` — for htop-style apps
  that own the whole viewport (no scrollback, so `<Static>`/`useStdout` are
  inline-only and raise there).
- **Keyboard, paste and mouse input** — a normalized `Key.t` type abstracts
  over raw ANSI escape sequences (arrows, Ctrl/Alt/Meta/Shift combinations,
  CSI-u/kitty sequences, multi-byte UTF-8 as `Key.Text`); bracketed paste
  arrives whole as `Key.Paste`; SGR mouse events power `<Clickable>` and
  wheel scrolling, with mouse reporting enabled only while something listens.
- **Focus management** — `useFocus`/`useFocusManager`/`useInput` with
  Tab/Shift+Tab cycling, Ink-compatible semantics.
- **Scrolling** — `<ScrollView>` windows onto tall content: keyboard scrolling
  while focused, wheel scrolling under the pointer, optional scrollbar,
  controlled or uncontrolled.
- **Overlays and modals** — `<Modal>` floats a bordered dialog over the
  finished frame: no layout cost, keyboard capture, focus containment and
  restore, click-outside to dismiss, and a drop shadow that *dims* the app
  underneath instead of painting over it. Dialog content is responsive to the
  dialog, not to the terminal.
- **Headless mode** — run any app without a TTY for scripting, CI, or agent
  interaction, either via an environment variable or a programmatic handle
  with a virtual clock, synthetic input (keys, paste, mouse), and separate
  live-frame vs transcript accessors.

## Installation

Matcha targets OCaml >= 5.3.0 with Reason >= 3.12.0 and ppxlib >= 0.36.0
(below 0.39.0). It is built with dune and depends on `unix` (bundled with
the OCaml distribution). The terminal layer needs `termios` and `ioctl`, so
Windows is not supported.

```
opam install matcha
```

To depend on Matcha from another dune project, add it to your `libraries`
stanza and add `matcha.ppx` to your `preprocess` stanza (the PPX is what
turns `[@component]` and JSX into plain ReasonML — see the `ppx/` directory;
`ppx_component` is its internal name and only resolves inside this repo):

```
(executable
 (name main)
 (libraries matcha)
 (preprocess
  (pps matcha.ppx)))
```

To work on Matcha itself, clone it and use a local opam switch, as this
repository does:

```
opam switch create . 5.3.0
opam install . --deps-only
dune build
```

## Quick start

Every Matcha app is a root component passed to `Runtime.start`. A component
is a function annotated with `[@component]` that returns `Element.t` (via
JSX). This is `examples/counter/main.re`, trimmed slightly:

```
open Matcha;

[@component]
let make = () => {
  let quit = Event.useQuit();
  let (count, setCount) = Component.useState(0);

  Event.useKeyDown((key, modifiers) => {
    switch (key, modifiers) {
    | (Key.Char('q'), _) => quit(ClearScreen)
    | (Key.Arrow_up, _) => setCount(count + 1)
    | (Key.Arrow_down, _) => setCount(max(0, count - 1))
    | _ => ()
    }
  });

  <VStack>
    <Text bold=true> "Counter Example" </Text>
    <HStack>
      <Text> "Count: " </Text>
      <Text bold=true> {string_of_int(count)} </Text>
    </HStack>
    <Text dim=true> "up/down: change  q: quit" </Text>
  </VStack>;
};

module App = {
  let make = make;
};

let () = Runtime.start((module App));
```

Build and run it (a real terminal is required — see Headless mode below for
running without one):

```
dune exec examples/counter/main.exe
```

## Components & hooks reference

`open Matcha` brings the following into scope.

### Layout & text components

| Component | Purpose |
|---|---|
| `<Text>` | Renders a string, with optional style props and `~wrap`. |
| `<VStack>` | Stacks children vertically (flex column). |
| `<HStack>` | Stacks children horizontally (flex row). |
| `<Sized>` | Wraps a single child to give it an explicit size within a parent Stack. |
| `<Container>` | Declares a container-query boundary: `useContainerSize()` inside it reports *this* box. Layout-transparent. |
| `<TextArea>` | Multi-line, controlled text editor with cursor/selection state. |
| `<Static>` | Append-only output committed above the live region, into scrollback. |
| `<ScrollView>` | Scrolling window onto content taller than its box; `~rows` virtualizes very long pre-rendered content. |
| `<Modal>` | Bordered dialog floating over the frame. No layout cost, captures `useInput`, contains and restores focus, Esc / click-outside to dismiss. See [Overlays and modals](#overlays-and-modals). |
| `<Overlay>` | The raw floating layer `<Modal>` is built on — same box, capture and container behaviour, no chrome. |
| `<Clickable>` | Runs a callback when its box is clicked. |

`Fragment` (groups children without adding a Stack) lives at `Element.Fragment`
and is not re-exported at the top level of `Matcha`; alias it locally if you
want `<Fragment>` in JSX: `module Fragment = Element.Fragment;`.

**Text** — props are all optional booleans/colors layered onto the string
child:

```
<Text bold=true> "bold" </Text>
<Text dim=true italic=true> "dim italic" </Text>
<Text color=Red bgColor=White> "red on white" </Text>
<Text color={Rgb(5, 0, 0)}> "custom RGB foreground" </Text>
<Text bgColor={RgbFull(0, 40, 8)}> "24-bit truecolor background" </Text>
```

Colors come in three flavors: the 16 named ANSI colors (`Red`, `BrightBlue`,
…, which follow the user's own terminal palette), `Rgb(r, g, b)` with each
channel in **0..5** — the 216-color cube, safe everywhere — and
`RgbFull(r, g, b)` with each channel in **0..255**, which is 24-bit
truecolor emitted as `38;2;r;g;b` / `48;2;r;g;b` and needs a truecolor-capable
terminal. `Matcha.rgb` and `Matcha.rgbFull` are the convenience constructors.
Truecolor survives Matcha's own wrapping and truncation intact: `StyledText`
parses the direct-color escapes back out, so a clipped or wrapped row re-opens
the exact same color.

Props: `bold`, `dim`, `italic`, `underline`, `inverted` (`bool`), `color`,
`bgColor` (`color`), `wrap` (`Element.wrap`; omitted means no wrapping):
`Wrap` — cell-accurate, ANSI-preserving word wrap to the allocated width —
and the truncation modes `Truncate`, `TruncateStart`, `TruncateMiddle`.

```
<Text wrap=Wrap> longParagraph </Text>
<Text wrap=Truncate> "very long single line…" </Text>
```

**VStack / HStack** — props: `gap` (`int`, default `0`), `align` (cross-axis,
default `AlignStretch`), `justify` (main-axis, default `JustifyStart`). Wrap
individual children in `<Sized size=...>` to control how they share space;
unwrapped children default to `Auto` (sized to their own content).

```
<VStack gap=1 align=AlignCenter justify=JustifySpaceBetween>
  <Sized size={Flex(2)}> heavyChild </Sized>
  <Sized size={Chars(10)}> fixedWidthSidebar </Sized>
  regularChild /* not wrapped in Sized, so it sizes to its own content (Auto) */
</VStack>
```

Conditional children follow the React `null` idiom:
`{showPanel ? <Panel /> : Element.Empty}` — an `Element.Empty` child is
invisible to stack layout (no line, no gap slot, no justify share), so a
collapsed branch costs nothing.

**TextArea** — a controlled component; you own the value, cursor position,
and selection as state, and forward key events to
`TextArea.handleKeyDown`:

```
let (text, setText) = Component.useState("");
let (cursorRow, cursorCol, setCursor) = {
  let (pos, setPos) = Component.useState((0, 0));
  let (row, col) = pos;
  (row, col, setPos);
};
let (selection, setSelection) = Component.useState(None);

Event.useKeyDown((key, modifiers) =>
  TextArea.handleKeyDown(
    key, modifiers, text, setText, None,
    cursorRow, cursorCol, setCursor, selection, setSelection,
  )
);

<TextArea
  value=text
  onChange=setText
  placeholder="Type something..."
  maxWidth=60
  minHeight=5
  maxHeight=20
  cursorRow
  cursorCol
  setCursor
  selection
  setSelection
/>;
```

Editing shortcuts baked into `handleKeyDown`: arrows to move, Cmd+Arrow to
jump to line/document start/end, Alt+Arrow to move by word, Shift+Arrow to
select, Ctrl+U to kill the line, Ctrl+W to kill the previous word, Enter for
newline, Cmd+Enter to call `onSubmit`.

Content **soft-wraps** at `maxWidth`, cell-accurately: a logical line too wide
for the box is painted on as many display rows as it needs, and a wide glyph
(an ideograph, an emoji) is never split across the boundary. Editing stays
logical — the cursor and selections still address a logical row and a column
of it, so wrapping is display only. The box **grows** with the content between
`minHeight` and `maxHeight`; past `maxHeight` it scrolls vertically, always
keeping the cursor's row in view.

A container sizes itself around a growing input with `TextArea.measure`, which
answers with exactly the number of rows the `<TextArea>` will paint (same
wrapping, same clamping — pass it the same `maxWidth`/`minHeight`/`maxHeight`).
`examples/claude-code` draws its bordered prompt that way:

```
let inputRows =
  TextArea.measure(
    ~value=inputText, ~maxWidth=max(1, width - 6),
    ~minHeight=1, ~maxHeight=5, (),
  );
```

and then sizes the box `Chars(inputRows + 2)` — the rows, plus its two borders.

The cursor blinks by itself, about every 530ms, and it is drawn on an empty
input too — the block sits on the first character of the placeholder, so you
can always see where typing will land. Pass `blink=false` for a steady cursor
(no timer is registered at all). Blinking is suppressed under
`MATCHA_HEADLESS=1` stream mode, which prints a frame per re-render, so frame
streams and example goldens stay deterministic.

**Static** — Ink-style append-only output. Items are rendered once, on the
frame that first sees them, and committed *above* the live region into the
terminal's normal scrollback; the node occupies no layout space and the
committed lines never re-render. Treat `items` as append-only — mutating or
reordering already-committed items does nothing:

```
let (messages, setMessages) = Hooks.useState([]);
/* on submit: setMessages(messages @ [newMessage]) */
<Static items=messages renderItem={(msg, _i) => <Entry msg />} />
```

For committing plain text (log lines, debug output) there is also the
`useStdout` escape hatch: `let stdout = Hooks.useStdout();
stdout.write("done");` — safe to call from render, effects, key handlers or
a background thread; the line appears above the live region on the next
frame. See `examples/static-demo` and `examples/chat`.

**ScrollView** — a scrolling window onto content taller than its box. Give
it a size (`<Sized size={Flex(1)}>` or `Chars(n)`) — an `Auto`-sized
ScrollView is as tall as its content and never scrolls. It joins the focus
ring by default and scrolls with the arrows, PageUp/PageDown and Home/End
while focused; the wheel scrolls the innermost ScrollView under the pointer
regardless of focus. Uncontrolled by default; pass `~offset` to drive it
yourself and take the clamped position back through `~onScroll`:

```
<Sized size={Flex(1)}>
  <ScrollView id="log">
    <VStack> ...manyRows </VStack>
  </ScrollView>
</Sized>
```

For **very long** content — a log, a syntax-highlighted diff, tens of
thousands of lines — pass `~rows` instead of children. Clipping a rendered
child means parsing all of it every frame (a style opened above the window
has to be re-opened on the first visible row), so the ordinary mode costs
O(total content); `~rows` takes content the application already holds as one
pre-rendered string per row, ignores the child, and touches only the visible
rows, so a frame costs O(viewport). Each row must be **self-contained**: it
opens the styles it needs and inherits nothing from the row above — that
independence is exactly what lets the runtime start at row N without reading
rows 0..N-1. The array is re-read every frame, so mutating it in place is
fine. Write it self-closing:

```
<Sized size={Flex(1)}> <ScrollView id="log" rows=myRows /> </Sized>
```

Props: `rows` (`array(string)`, the virtualized mode above), `offset`
(`int`), `onScroll` (`int => unit`), `showScrollbar` (`bool`, default
`true`), `focusable` (`bool`, default `true`), `id` (`string`, its focus
id), `mouse` (`bool`, default `true` — wheel handling). See
`examples/scroll-demo`.

**Clickable** — fires `onClick` when the left button goes down anywhere in
the box its parent allocated it. The innermost Clickable under the pointer
wins. Mouse reporting is switched on automatically while any mouse handler
exists, and off again when the last one unmounts:

```
<Clickable onClick={() => select(i)}>
  <Text> label </Text>
</Clickable>
```

The optional `~onMouseDown` receives every *other* event (release, wheel,
drag motion) that lands on the component, with coordinates rebased to its
own box. Without it, a Clickable is transparent to the wheel — so clickable
rows inside a `<ScrollView>` still wheel-scroll the list.

### Hooks

The *slot* hooks (`useState`, `useEffect`, `useEffectAlways`, `useMemo`,
`useRef`, and the timers built on them) must be called unconditionally, in
the same order, on every render (same rule as React). The *registration*
hooks (`useKeyDown`, `useInput`, `useMouse`, `useFocus`, `useFocusManager`,
`useQuit`, `useStdout`) are re-collected from scratch each frame and are safe
to call conditionally. All are available via `Hooks.*` directly, or via the
convenience re-export modules `Component` (`useState`, plus `useContext`
which forwards to `Context.use`) and `Event` (`useKeyDown`, `useQuit`, and
the focus/input/mouse hooks) that the examples use.

| Hook | Signature | Notes |
|---|---|---|
| `useState` | `'a => ('a, 'a => unit)` | `Component.useState` or `Hooks.useState`. Setting a physically-equal value is a no-op (no re-render). |
| `useEffect` | `(unit => option(unit => unit), array('a)) => unit` | Runs after render when deps change (compared by physical equality); return `Some(cleanup)` to run cleanup before the next effect or on unmount. |
| `useEffectAlways` | `(unit => option(unit => unit)) => unit` | Like `useEffect` but runs after every render — use sparingly. |
| `useMemo` | `(unit => 'a, array('b)) => 'a` | Recomputes only when deps change (physical equality). |
| `useRef` | `'a => ref('a)` | A mutable cell that survives re-renders without triggering them. |
| `useInterval` | `(unit => unit, ~ms: int) => unit` | Fires the callback every `ms` milliseconds. `~ms=0` (or negative) disables it — the Ink `delay={null}` idiom; change `ms` to re-arm. |
| `useTimeout` | `(unit => unit, ~ms: int) => unit` | Fires once after `ms` milliseconds; `~ms=0` disables. |
| `useKeyDown` | `((Key.t, Key.modifiers) => unit) => unit` | `Event.useKeyDown` or `Hooks.useKeyDown`; handlers are re-registered every render and all fire on every key. |
| `useInput` | `(~isActive: bool=?, (Key.t, Key.modifiers) => unit) => unit` | Like `useKeyDown` with an activity gate: `~isActive=false` unregisters it. Pair with `useFocus` (`~isActive=isFocused`) to route the keyboard to the focused widget. |
| `useFocus` | `(~autoFocus: bool=?, ~isActive: bool=?, ~id: string=?, ()) => {isFocused: bool}` | Joins the Tab/Shift+Tab focus ring. `~autoFocus` claims focus on mount; `~id` names the focusable for `getFocusedId`/`focusManager.focus`. |
| `useFocusManager` | `unit => focusManager` | Imperative control: `focusNext`/`focusPrevious`/`focus(id)`/`enableFocus`/`disableFocus`. |
| `useMouse` | `(~wheel: bool=?, Mouse.event => unit) => unit` | Receives mouse events whose coordinates fall in this component's painted box, rebased to it; innermost component wins, no bubbling. `~wheel=false` makes it transparent to scroll events. |
| `useStdout` | `unit => {write: string => unit}` | Commits plain text above the live region (Ink's `useStdout`); callable from anywhere, including background threads. |
| `useQuit` | `unit => (quitBehavior => unit)` | `Event.useQuit` or `Hooks.useQuit`; call the returned function with `ClearScreen` (erases the live region, keeps the static transcript) or `PreserveScreen`. |
| `useContainerSize` | `unit => constraints` | `Matcha.useContainerSize`; returns `{availWidth: int, availHeight: int}`, the box of the nearest enclosing `<Container>` — or the whole frame when there is none. See [Container queries](#container-queries). |
| `useTerminalBackground` | `unit => option((int, int, int))` | The terminal's own background color, 0..255 per channel, once it has answered Matcha's startup OSC 11 query. `None` until then — and possibly forever. |

```
let quit = Event.useQuit();
quit(ClearScreen);    /* clear the terminal and exit */
quit(PreserveScreen); /* exit but leave the last frame on screen */
```

```
Hooks.useEffect(() => {
  let timer = startPolling();
  Some(() => stopPolling(timer)); /* cleanup */
}, [|dep|]);
```

```
let {Runtime.availWidth, availHeight} = useContainerSize();
```

#### Container queries

Responsive decisions in Matcha are **container-relative by default**, the way
CSS container queries work. `useContainerSize()` answers with the box of the
nearest enclosing `<Container>`, and with the whole frame when there is no
container above the caller — so a root-level component reads the terminal
size, and a component inside a pane reads the pane.

```
<HStack>
  <Sized size={Percent(40)}>
    <Container> <Sidebar /> </Container>   /* Sidebar queries the 40% pane */
  </Sized>
  <Sized size={Flex(1)}>
    <Container> <Detail /> </Container>    /* Detail queries the rest */
  </Sized>
</HStack>
```

```
/* Inside Sidebar: 40% of a 200-column terminal is wide, 40% of an 80-column
   one is not - and this component never has to know which it is in. */
let {Runtime.availWidth: width, _} = useContainerSize();
let layout = width >= 40 ? `Full : `Compact;
```

Three rules keep it predictable:

- **`<Container>` is layout-transparent.** It renders its child with the
  constraints it received, at the same tree path — adding or removing one
  never moves a cell and never resets a hook. Its *only* effect is on
  queries.
- **Boundaries are explicit.** `<Sized>` and `<ScrollView>` are **not**
  containers. Wrapping something to nudge its layout must not silently
  re-target its descendants' responsive queries, so declare a `<Container>`
  where you want one.
- **`Percent(n)` is unaffected.** It stays parent-relative, like CSS `%`.
  Containers change *queries*, not layout.

#### Theme detection

`Runtime.start` asks the terminal for its background color once at startup
(an OSC 11 query, sent in both screen modes) and hands the answer to
`useTerminalBackground`. When the reply lands the application re-renders
exactly once with the new value, so a color scheme picked from it settles
within a frame of launch.

Many terminals — and every pipe, CI job and headless run — never answer, so
`None` is permanent there and **every caller needs a default**. The usual
idiom assumes dark:

```
let isLight =
  switch (Hooks.useTerminalBackground()) {
  | Some((r, g, b)) => relativeLuminance(r, g, b) > 0.5
  | None => false /* assume dark */
  };
let theme = isLight ? lightTheme : darkTheme;
```

In a headless test, `handle.setTerminalBackground((250, 250, 250))` supplies
the value a real terminal would have sent, and re-renders once.

### Context

`Context.Make` generates a typed module with plain (non-JSX) `provide`/`use`
functions — `provide` is `(value, children) => Element.t`, not a component,
so call it directly rather than as a JSX tag:

```
module Theme = Context.Make({
  type t = string;
  let default = "dark";
});

Theme.provide("light", children); /* wraps children in a provider element */
/* elsewhere, inside a descendant component: */
let theme = Theme.use();
```

If you want a JSX `<Provider>` tag (as `examples/nested-components` and
`examples/people-list` do), wrap `provide` in your own `[@component]`:

```
module Provider = {
  [@component]
  let make = (~children: Element.t) => {
    Theme.provide("light", children);
  };
};
/* usage: <Theme.Provider> children </Theme.Provider> */
```

Or use the untyped API directly: `Context.create`, `Context.provide`,
`Context.use`.

## Layout system

Sizing (used inside `<Sized size=...>`, type `Element.size` / `Matcha.size`):

| Constructor | Meaning |
|---|---|
| `Auto` | Size to content. This is what a child gets if it isn't wrapped in `<Sized>` at all. |
| `Flex(n)` | Share of remaining space, like CSS `flex-grow: n`. |
| `Percent(n)` | `n`% of the parent container's size. |
| `Chars(n)` | Fixed size in character cells. |

Convenience constructors are also exported at the top level:
`Matcha.flex(n)`, `Matcha.percent(n)`, `Matcha.chars(n)`.

Cross-axis alignment (`align`, type `Element.align`): `AlignStart`,
`AlignEnd`, `AlignCenter`, `AlignStretch` (default).

Main-axis distribution (`justify`, type `Element.justify`): `JustifyStart`
(default), `JustifyEnd`, `JustifyCenter`, `JustifySpaceBetween`,
`JustifySpaceAround`, `JustifySpaceEvenly`.

For `HStack`, `align` controls vertical alignment and `justify` controls
horizontal distribution; for `VStack` it's the reverse.

## Overlays and modals

`<Modal>` floats a bordered dialog over the finished frame. Write it
**directly in the stack**, next to the rest of your app:

```reason
<VStack>
  <Sized size={Flex(1)}> <LogPane /> </Sized>
  <Modal
    isOpen=paletteOpen
    title="Commands"
    align={OverlayTop(2)}
    onDismiss={() => setPaletteOpen(false)}>
    <Palette onRun=run />
  </Modal>
  <Sized size={Chars(1)}> <StatusBar /> </Sized>
</VStack>
```

It costs that stack **no row, no gap slot and no justify share**, open or
closed — the layer is composited after layout, not laid out.

| Prop | Default | Meaning |
|---|---|---|
| `isOpen` | *(required)* | Closed renders nothing at all. |
| `width` | `Percent(60)` | Resolved against the **frame**, not the enclosing slot. `Chars(n)` is clamped to the frame, `Flex(_)` fills it, `Auto` is the content's natural width. |
| `height` | `Auto` | As above; `Auto` is the content plus the two border rows. |
| `align` | `OverlayCenter` | Vertical placement: `OverlayCenter`, `OverlayTop(n)`, `OverlayBottom(n)`. Horizontal is always centred. |
| `title` | *(none)* | Drawn into the top border. |
| `shadow` | `true` | A drop shadow that **adds `Dim` to the cells underneath** rather than painting over them, so live content stays readable through it. |
| `onDismiss` | *(none)* | Run by Esc, and by a mouse click outside the box. |

**The capture rule.** Everything rendered *inside* the modal is a **member**
of its layer. While a layer is open:

- `useInput` fires **only for members**. A `<ScrollView>` underneath goes
  quiet for free, and a dialog's Esc closes the top dialog rather than all of
  them.
- `useKeyDown` **always** fires. This is the escape hatch for globals — and it
  is not optional for Ctrl+C: raw mode disables ISIG, so Ctrl+C is an ordinary
  keypress. **Bind it with `useKeyDown`, or your app cannot be quit while a
  modal is open.**
- Focus is contained to the dialog (Tab cannot leave it) and restored to
  whatever held it when the dialog closes.
- A mouse `Down` outside the box runs `onDismiss` and is swallowed whole, so
  a dismissing click cannot also press what is underneath it.

**The dialog is its own container.** `useContainerSize()` inside a modal
reports the *modal's* box, so dialog content is responsive to the dialog and
never to the window.

**Inline apps: prefer `~align={OverlayTop(n)}`.** The default centres the
dialog in the whole frame, and in Inline mode the frame is the terminal — so
centring grows the live region to the full terminal height even when the app
itself is six rows tall.

**A full-height app should be `Fullscreen`, not inline.** Inline paints at
the cursor, so a frame as tall as the terminal makes the terminal scroll to
fit it, pushing your prompt away. Quitting erases the region, but nothing
un-scrolls a terminal, so you are left with a screenful of blank rows above
the new prompt. If your root Flexes to fill the screen — as a log viewer or
a dashboard does — start it with
`Runtime.start(~screen=Fullscreen, (module App))`, which the terminal
restores exactly on exit. Keep inline for apps that stay a handful of rows
tall (`examples/chat`, `examples/static-demo`).

`<Overlay>` (`Element.Overlay`) is the raw layer underneath `<Modal>`: the
same box, capture, container and dismiss behaviour, without the border, the
Esc binding, the focus restore or `~isOpen` (an `<Overlay>` in the tree is
always open — use the usual `cond ? ... : Element.Empty`).

See `examples/command-menu` for the worked case.

## Keys & modifiers

`Key.t` (from `lib/Key.re`) normalizes raw terminal escape sequences:

```
type t =
  | Arrow_up | Arrow_down | Arrow_left | Arrow_right
  | Char(char)        /* single-byte (ASCII) */
  | Text(string)      /* one complete multi-byte UTF-8 codepoint, e.g. "é", "日" */
  | Escape | Enter | Backspace | Tab | Delete
  | KillLine          /* Ctrl+U */
  | KillWord          /* Ctrl+W */
  | Home | End | Insert | Page_up | Page_down
  | F(int)            /* F1..F12 */
  | Paste(string)     /* a whole bracketed-paste payload */
  | Unknown;

type modifiers = { ctrl: bool, alt: bool, shift: bool, meta: bool };
```

Some combinations are normalized directly into a key rather than left as a
modifier flag — e.g. Ctrl+H and DEL both become `Backspace`, Ctrl+U becomes
`KillLine`, Ctrl+I becomes `Tab`. Modifier flags on `Char` cover cases like
Ctrl+letter (`Char('c')` with `ctrl: true`). Shift+Tab (backtab), CSI-u and
kitty-protocol sequences are recognized too. A paste made with bracketed
paste enabled (the interactive runtime enables it) arrives as one
`Key.Paste(text)` event, not as individual keystrokes.

When at least one `useFocus` focusable is registered, Tab/Shift+Tab are
consumed by focus cycling before application key handlers see them.

The interactive runtime's raw mode disables terminal signal handling and
flow control: Ctrl+C, Ctrl+Z, Ctrl+S and Ctrl+Q all arrive as ordinary key
events (`Char('c')` with `ctrl: true`, etc.) instead of raising signals, so
an app must bind its own quit key — every example binds Ctrl+C or `q`.

```
Event.useKeyDown((key, modifiers) => {
  switch (key, modifiers) {
  | (Key.Char('c'), {Key.ctrl: true, _}) => quit(ClearScreen)
  | (Key.Arrow_left, {Key.meta: true, _}) => goToLineStart()
  | (Key.Arrow_left, {Key.alt: true, _}) => goToPrevWord()
  | (Key.Char(c), _) => insertChar(c)
  | _ => ()
  }
});
```

## Screen modes

`Runtime.start` takes an optional `~screen`:

```
Matcha.Runtime.start((module App));                      /* Inline (default) */
Matcha.Runtime.start(~screen=Fullscreen, (module App));  /* alternate screen */
```

`Inline` is everything described above: a live region at the cursor, with
`<Static>`/`useStdout` committing into the terminal's real scrollback.

`Fullscreen` runs on the terminal's **alternate screen**, like vim or htop.
The frame is padded to the full viewport, so the app owns every row; there is
no scrollback to scroll away to; and on exit the terminal's previous contents
come straight back (both `quit` behaviors restore identically). Give the root
layout a `Flex(1)` region and the rest of the UI stays pinned to the bottom of
the screen.

There is no scrollback on the alternate screen, so "commit this above the live
region" has no meaning there: **`<Static>` and `useStdout().write` raise
`Invalid_argument` under `~screen=Fullscreen`** rather than silently dropping
output that could never appear. A fullscreen app owns its transcript — keep it
in state and render it, e.g. inside a `<ScrollView>`. `examples/claude-code` is
the worked example; `examples/chat` and `examples/static-demo` are the inline
counterpart. Headless mode ignores `~screen` entirely — it has no terminal to
own, and `<Static>` keeps working there.

## Headless mode

Matcha apps can run without a real TTY, useful for tests, CI, and driving an
app programmatically (e.g. from an agent).

### 1. Environment-variable headless mode

Set `MATCHA_HEADLESS=1` and `Runtime.start` switches to a loop that prints
each rendered frame to stdout and reads keys from stdin, exiting cleanly on
stdin EOF. `MATCHA_WIDTH` / `MATCHA_HEIGHT` control the simulated terminal
size (default `80x24`).

Because it still blocks reading stdin until EOF, always redirect stdin and
cap runtime with `timeout` when scripting it — otherwise it hangs:

```
timeout 10 env MATCHA_HEADLESS=1 dune exec examples/counter/main.exe < /dev/null
```

Pipe keys in via stdin bytes to drive it interactively from a script.

### 2. Programmatic headless mode — `startHeadless`

For automated tests, `Matcha.startHeadless` (`Runtime.startHeadless`) returns
a handle you drive directly, with no environment variables and no stdin/TTY
involved at all:

```
let handle = Matcha.startHeadless((module App));
handle.sendKey(Key.Arrow_up, Key.noModifiers);
handle.sendPaste("multi\nline paste");       /* arrives as Key.Paste */
handle.sendMouse({Mouse.kind: Mouse.Down, button: Mouse.Left,
                  x: 3, y: 1, shift: false, alt: false, ctrl: false});
let output = handle.getOutput(true /* stripAnsi */); /* the CURRENT frame */
let lines = handle.getLines(true);
let transcript = handle.getStaticOutput(true); /* all <Static>/useStdout
                                                  output ever committed */
let focused = handle.getFocusedId();     /* the id useFocus currently owns */
handle.advanceTime(1500);  /* virtual clock: fires useInterval/useTimeout */
handle.setTerminalBackground((250, 250, 250)); /* what OSC 11 would answer */
handle.resize(40, 10);
let (w, h) = handle.getSize();
handle.render();       /* force a re-render, returns the new frame */
handle.isRunning();
handle.quit();
```

`startHeadless` optionally takes `~config: Runtime.headlessConfig =
{width, height}` (default `Matcha.defaultHeadlessConfig`, `80x24`).

`Matcha.isHeadless()` checks whether `MATCHA_HEADLESS=1` is set, if you need
to branch application code on it.

Time never passes on its own headlessly: `advanceTime` is the only clock, so
timer-driven UIs (spinners, debounces) are tested deterministically, without
sleeping. Mouse coordinates are frame coordinates (0-based, top-left).

For a worked example of testing a full application this way — typing, paste,
focus cycling, timers, static transcript assertions, wheel scrolling and
clicks — see `test/chat_tests.re`, which drives the `examples/chat` app, and
`test/input.re` for helpers (`feedBytes`, `feedPaste`, `clickAt`,
`pressTab`) that simulate real terminal byte streams end to end.

Headless mode answers "does the app render and behave correctly", but not
"does a terminal display that correctly" — for that the suite has two more
layers, worth knowing about if you contribute to rendering or terminal code.
`test/vterm.re` is a small VT/xterm screen model (cursor, deferred wrap,
erasure, scrollback, alternate screen, SGR per cell): painter output is fed
to it and assertions are made on the resulting **grid**, which is how
full-width rows are checked to keep their last column. `test/pty.re` runs a
real example binary on a real pseudo-terminal, so raw mode, Ctrl+C reaching
the application rather than the kernel, alternate-screen enter/exit, a real
SIGWINCH resize and the exit restore sequence are all covered by
`dune runtest` too. See `CLAUDE.md` for when each layer applies.

## Examples

All examples live under `examples/` and build as separate dune executables.
Run any of them with `dune exec examples/<directory>/main.exe` in a real
terminal, or with the headless invocation shown above.

| Directory | Demonstrates |
|---|---|
| `hello-world` | Minimal app, `useQuit`, `useKeyDown`. |
| `counter` | `useState`, `useMemo`, key handling. |
| `layout-demo` | `Sized`, `Flex`/`Percent`/`Chars`, `<Container>` + `useContainerSize`. |
| `layout-alignment` | `align` / `justify` on `HStack`/`VStack`, container-relative self-sizing. |
| `nested-components` | Composing components, `Context.Make`. |
| `keyed-switch` | Component identity via the `key` prop. |
| `optional-params` | Optional component props (`~second: string=?`). |
| `textarea-demo` | `TextArea`, cursor/selection state. |
| `async-fetch` | Background-thread state updates waking the render loop. |
| `people-list` | Multi-module app: context, a filterable list, custom split-pane layout. |
| `static-demo` | `<Static>` transcript above the live region, `useStdout`. |
| `scroll-demo` | `<ScrollView>`, focus ring, wheel scrolling. |
| `chat` | The capstone: `<Static>` transcript, focused `<TextArea>` with paste, `useInterval` spinner, `<ScrollView>` panel with `<Clickable>` rows. Tested end to end by `test/chat_tests.re`. |
| `command-menu` | The **overlay showcase**: a live log viewer (a `useInterval` stream into a virtualized `<ScrollView rows>`, inside a `<Container>`) with a Ctrl+K command palette in a `<Modal>`. The log keeps streaming while the palette is open, which is the point — a modal owns the keyboard, not the clock. Tested end to end by `test/commandmenu_tests.re`, plus a real-PTY case. |
| `claude-code` | The **fullscreen showcase** (`~screen=Fullscreen`): a mock of the Claude Code CLI that fills the terminal on the alternate screen with the prompt pinned to the bottom. Its transcript is app state in a stick-to-bottom controlled `<ScrollView>` (no `<Static>` — there is no scrollback to commit to), plus a timer-driven status row, a slash-command palette on a second controlled `<ScrollView>`, Shift+Tab permission modes and double-Ctrl+C to quit. Deliberately focus-free — see `test/claudecode_tests.re`. |

## Development

Build everything:

```
dune build
```

Run the test suite (hand-rolled framework in `test/Test.re`, entry point
`test/run_tests.re`):

```
dune runtest
```

Run a specific example interactively:

```
dune exec examples/counter/main.exe
```

Run a specific example headlessly (see Headless mode above):

```
timeout 10 env MATCHA_HEADLESS=1 dune exec examples/counter/main.exe < /dev/null
```

Note: the dev profile promotes warnings to errors, so unused bindings will
fail the build — clean up unused code rather than suppressing warnings.

See `CLAUDE.md` for a deeper architecture map and contributor-facing
workflow notes, and `.claude/skills/matcha-dev/SKILL.md` for step-by-step
recipes (adding a component, writing a headless test, etc.).

## Building an app, with Claude Code

This repository is also a Claude Code plugin marketplace. It ships a
**`matcha-app`** skill that teaches an agent how to build an application on
matcha: project layout, the root component and its state, row rendering,
keyboard and mouse input, keeping slow work off the render path, and
headless testing. It is distilled from two real applications rather than
from the API docs, so it leads with the mistakes rather than the surface.

**Per project.** Commit this to your app's `.claude/settings.json` and
anyone who clones the repository gets the skill with no further step:

```json
{
  "extraKnownMarketplaces": {
    "matcha": { "source": { "source": "github", "repo": "eldh/matcha" } }
  },
  "enabledPlugins": { "matcha-app@matcha": true }
}
```

**Per machine**, if you would rather have it everywhere:

```
/plugin marketplace add eldh/matcha
/plugin install matcha-app@matcha
```

Either way Claude Code refreshes the skill from GitHub in the background, so
it tracks the framework. The source is under `plugins/matcha-app/` and is
worth reading on its own — most of it is a list of failure modes with the
reason each one happens.

## Releasing

`CHANGELOG.md` records what changed in each version. `RELEASING.md`
describes how a version reaches opam, and `scripts/release.sh` and
`scripts/opam-pr.sh` do it.
