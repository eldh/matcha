/**
 Matcha - A React-like framework for building terminal UIs.

 This interface IS the public API surface. Everything a Matcha application can
 reach lives here; anything under [lib/] that is not listed below is
 implementation detail and may change without notice.

 The point of pinning the surface in a [.rei] is that widening or narrowing the
 public API becomes a deliberate, compiler-enforced edit: adding a value to
 [Matcha.re] alone does not export it, and removing one breaks the build here
 rather than silently at a call site.

 Modules are re-exported with [module type of], so their full signature stays
 public and this file does not have to be kept in step with each of them.
 */;

/* ============================================================================
 * Core modules
 * ============================================================================ */

/** Element tree type, ANSI/style helpers, string utilities, and the
    JSX-compatible element component modules. */

module Element: (module type of Element);

/** Thin convenience re-export of a subset of {!Hooks} ([useState]). */

module Component: (module type of Component);

/** React-style context: [create]/[provide]/[use] and the [Context.Make]
    functor for typed provider/consumer modules. */

module Context: (module type of Context);

/** Keyboard event helpers ([useQuit], [useKeyDown]) and focus helpers
    ([useFocus], [useFocusManager], [useInput]) re-exported from {!Hooks}. */

module Event: (module type of Event);

/** React-style hooks: [useState], [useEffect], [useEffectAlways], [useMemo],
    [useKeyDown], [useQuit], plus the render-context machinery the runtime
    drives. */

module Hooks: (module type of Hooks);

/** Normalized keyboard input: the [Key.t] ADT, modifiers, and the raw-byte
    escape-sequence parser. */

module Key: (module type of Key);

/** UTF-8 decoding and terminal display width: [decodeUtf8], [charWidth],
    [stringWidth] (ANSI-aware) and the [cell] splitter [toCells]. All layout
    measurement in Matcha is done in the columns this module reports. */

module TextWidth: (module type of TextWidth);

/** ANSI-aware wrapping and truncation of already-rendered styled text:
    [parse]/[bake] (styled string <-> per-cell chunks) and [wrapString], the
    [Element.wrap]-mode transform behind the [Text] component's [~wrap] prop. */

module StyledText: (module type of StyledText);

/** SGR (1006) mouse event types, decoding ([parseSgr]/[encodeSgr]), and the
    rect helpers ([contains]/[intersect]) B4's bounds registry builds on. */

module Mouse: (module type of Mouse);

/** Stateful byte-stream assembler sitting between {!Terminal.readBytes} and
    key dispatch: reassembles raw terminal reads into [KeyEvent]/
    [PasteEvent]/[MouseEvent]/[CursorReport] regardless of how bytes were
    split across reads ([feed]/[flush]). Applications don't call this
    directly - it's what powers [Runtime.start] - but it's exposed for
    testing and for callers building their own event loop. */

module InputDecoder: (module type of InputDecoder);

/** Application lifecycle: [start], [startHeadless], the layout-aware
    [renderElement], and layout [constraints]. [start] takes a
    [screenMode]: [Inline] (the default) renders at the cursor as a live
    region with [Static]/[useStdout] committing into scrollback above it,
    while [Fullscreen] takes over the whole viewport on the alternate
    screen - where there is no scrollback, so [Static]/[useStdout] raise
    instead of committing. */

module Runtime: (module type of Runtime);

/** Low-level terminal I/O: raw mode, cursor, screen clear, terminal size. */

module Terminal: (module type of Terminal);

/** Pure line-diff between rendered frames, with ABSOLUTE cursor addressing
    on a cleared screen. Retained as a utility (and fully tested); the
    interactive loop paints inline through {!LiveRegion} instead. */

module FrameDiff: (module type of FrameDiff);

/** Pure inline frame patcher with RELATIVE cursor addressing: [patch] turns
    the painted live region into the next frame, committing [<Static>] lines
    above it, and [erase] removes the region without touching the transcript.
    This is what the interactive loop writes. */

module LiveRegion: (module type of LiveRegion);

/* ============================================================================
 * JSX element components available at the top level
 * ============================================================================ */

/** Text with optional styling props ([bold], [dim], [color], ...). */

module Text: (module type of Element.Text);

/** Vertical flex stack. */

module VStack: (module type of Element.VStack);

/** Horizontal flex stack. */

module HStack: (module type of Element.HStack);

/** Size wrapper for a stack child. */

module Sized: (module type of Element.Sized);

/** Append-only output committed above the live region and left in the
    terminal's scrollback: [<Static items renderItem={(item, i) => ...} />].
    Occupies no layout space; every item is rendered exactly once. */

module Static: (module type of Element.Static);

/** Multi-line text editor: [<TextArea value onChange cursorRow cursorCol
    setCursor selection setSelection />]. Cursor and selection are the
    application's state, same shape as a controlled React input.

    The cursor blinks on its own (~530ms), including over the first
    character of the placeholder while the value is empty, so an empty
    input still shows where typing will land. [~blink=false] gives a
    steady cursor and registers no timer. Blinking is suppressed under
    [MATCHA_HEADLESS=1] stream mode so frame streams stay deterministic. */

module TextArea: (module type of TextArea);

/** Click target: [<Clickable onClick>...</Clickable>] fires [onClick] when
    the left button goes down anywhere in the box its parent allocated it.
    The innermost Clickable under the pointer wins; the optional
    [~onMouseDown] receives every other event (release, wheel, drag motion)
    that lands on it, with coordinates rebased to its own box. Without
    [~onMouseDown] the wheel passes through it, so clickable rows inside a
    [<ScrollView>] still scroll. Mouse reporting is enabled automatically
    while any handler exists. */

module Clickable: (module type of Clickable);

/** Scrolling window onto content taller than its box:
    [<ScrollView>...</ScrollView>]. Size it ([<Sized size={Flex(1)}>] or
    [Chars(n)]) - an Auto-sized ScrollView is as tall as its content and
    never scrolls. Scrolls with the arrows, PageUp/PageDown and Home/End
    while focused, and with the wheel (innermost one under the pointer
    wins). Uncontrolled by default; pass [~offset] to drive it from the
    application and take the clamped position back through [~onScroll].
    [scrollbarMetrics] is the pure thumb geometry behind its scrollbar. */

module ScrollView: (module type of ScrollView);

/* ============================================================================
 * Layout
 * ============================================================================ */

/** Size hint for a stack child: [Auto], [Flex], [Percent] or [Chars]. */

type size = Element.size;

/** Flex units, like CSS [flex-grow]. */

let flex: int => Element.size;

/** Percentage of the parent container. */

let percent: int => Element.size;

/** An absolute character count. */

let chars: int => Element.size;

/** Space available to the element currently rendering. */

type constraints = Runtime.constraints;

/** Get the current layout constraints (width/height available to this
    component). Call it inside a component body to see what the parent stack
    allocated. */

let useLayout: unit => Runtime.constraints;

/* ============================================================================
 * Output above the live region
 * ============================================================================ */

/** Handle returned by [useStdout]: [{write: string => unit}]. */

type stdoutHandle = Hooks.stdoutHandle;

/** Ink-style escape hatch for committing plain text above the live region.
    Callable from render, effects, key handlers or a background thread; the
    text appears on the next frame. Prefer [<Static>] for rendered items. */

let useStdout: unit => Hooks.stdoutHandle;

/* ============================================================================
 * Colors
 * ============================================================================ */

/** Terminal color: one of the 16 named ANSI colors, or [Rgb] into the
    216-color cube. */

type color = Element.color;

let black: Element.color;
let red: Element.color;
let green: Element.color;
let yellow: Element.color;
let blue: Element.color;
let magenta: Element.color;
let cyan: Element.color;
let white: Element.color;
let brightBlack: Element.color;
let brightRed: Element.color;
let brightGreen: Element.color;
let brightYellow: Element.color;
let brightBlue: Element.color;
let brightMagenta: Element.color;
let brightCyan: Element.color;
let brightWhite: Element.color;

/** [rgb(r, g, b)] with each component in 0..5, mapped to the 216-color cube. */

let rgb: (int, int, int) => Element.color;

/* ============================================================================
 * String and drawing utilities
 * ============================================================================ */

/** Box drawing characters: ┌ ┐ └ ┘ ─ │ ├ ┤ ┬ ┴ ┼ */

module BoxChars: (module type of Element.BoxChars);

/** [repeatString(s, n)] concatenates [s] with itself [n] times. */

let repeatString: (string, int) => string;

/** Pad with spaces, or truncate, to an exact VISIBLE width (ANSI aware). */

let padToWidth: (string, int) => string;

/** Visible length of a string, ignoring ANSI escape codes. */

let visibleLength: string => int;

/** Split a string into its lines. */

let splitLines: string => list(string);

/** Strip ANSI escape codes. Useful when asserting on rendered output. */

let stripAnsi: string => string;

/* ============================================================================
 * Headless mode
 * ============================================================================ */

/** Terminal size for a headless run. */

type headlessConfig = Runtime.headlessConfig;

/** Handle for driving a headless application: [sendKey], [sendPaste],
    [sendMouse] (a mouse event in live-region coordinates, routed to the
    component under it), [getOutput], [getLines], [getStaticOutput]
    (everything [<Static>] and
    [useStdout] have committed, accumulated across frames), [render],
    [resize], [getSize], [isRunning], [quit], [advanceTime] (fake-clock
    control for [useInterval]/[useTimeout]), [getFocusedId] (the id
    [useFocus] currently owns, if any). */

type headlessHandle = Runtime.headlessHandle;

/** Start an application without a terminal, for tests and agents. Renders
    once immediately and returns a handle. */

let startHeadless:
  (~config: Runtime.headlessConfig=?, (module Runtime.HooksComponent)) =>
  Runtime.headlessHandle;

/** The default headless size (80x24). */

let defaultHeadlessConfig: Runtime.headlessConfig;

/** Whether [MATCHA_HEADLESS=1] is set in the environment. */

let isHeadless: unit => bool;
