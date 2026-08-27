/*
 * Matcha - A React-like framework for building terminal UIs
 *
 * Matcha provides a declarative, component-based approach to building
 * interactive terminal applications in ReasonML.
 *
 * Core Modules:
 * - Element: UI element tree and rendering
 * - Hooks: React-style hooks (useState, useEffect)
 * - Event: Keyboard event handling
 * - Context: Data propagation through component tree
 * - Key: Keyboard input types
 * - Terminal: Low-level terminal operations
 * - Runtime: Application lifecycle and main loop
 */

/* Core modules */
module Element = Element;
module Component = Component;
module Context = Context;
module Event = Event;
module Hooks = Hooks;
module Key = Key;
module TextWidth = TextWidth;
module StyledText = StyledText;
module Mouse = Mouse;
module InputDecoder = InputDecoder;
module Runtime = Runtime;
module Terminal = Terminal;
module FrameDiff = FrameDiff;
module LiveRegion = LiveRegion;

/* JSX Element components available at top level */
module Text = Element.Text; /* Text with optional styling props */
module VStack = Element.VStack; /* Vertical flex stack */
module HStack = Element.HStack; /* Horizontal flex stack */
module Sized = Element.Sized; /* Size wrapper for stack children */
module Static = Element.Static; /* Append-only output above the live region */
module Clickable = Clickable; /* Wrap an element to make it click-target-able */
module ScrollView = ScrollView; /* Scrolling window onto oversized content */
module TextArea = TextArea; /* Multi-line text editor with a blinking cursor */

/* Size type for flex layout */
type size = Element.size;

/* Size constructors for convenient access */
let flex = (n: int) => Element.Flex(n);
let percent = (n: int) => Element.Percent(n);
let chars = (n: int) => Element.Chars(n);

/* Layout constraints type and accessor */
type constraints = Runtime.constraints;

/* Get the current layout constraints (width/height available to this component).
 * Call this within a component to get the space allocated by the parent Stack.
 */
let useLayout = Runtime.getConstraints;

/* Write plain text above the live region (Ink's useStdout). See
 * Hooks.useStdout; prefer <Static> for lists of rendered items. */
type stdoutHandle = Hooks.stdoutHandle;
let useStdout = Hooks.useStdout;

/* Color type and variants for text styling */
type color = Element.color;

/* Re-export color constructors for convenient access */
let black = Element.Black;
let red = Element.Red;
let green = Element.Green;
let yellow = Element.Yellow;
let blue = Element.Blue;
let magenta = Element.Magenta;
let cyan = Element.Cyan;
let white = Element.White;
let brightBlack = Element.BrightBlack;
let brightRed = Element.BrightRed;
let brightGreen = Element.BrightGreen;
let brightYellow = Element.BrightYellow;
let brightBlue = Element.BrightBlue;
let brightMagenta = Element.BrightMagenta;
let brightCyan = Element.BrightCyan;
let brightWhite = Element.BrightWhite;
let rgb = (r, g, b) => Element.Rgb(r, g, b);

/* 24-bit truecolor: channels 0..255, emitted as SGR 38;2/48;2. `rgb` above
 * stays the 216-color cube (channels 0..5). */
let rgbFull = (r, g, b) => Element.RgbFull(r, g, b);

/* The terminal's own background color, once it has told us. See
 * Hooks.useTerminalBackground - None until (and unless) the terminal
 * answers Matcha's OSC 11 query at startup. */
let useTerminalBackground = Hooks.useTerminalBackground;

/* Box drawing utilities */
module BoxChars = Element.BoxChars; /* Box drawing chars: ┌ ┐ └ ┘ ─ │ */

/* Repeat a string n times */
let repeatString = Element.repeatString;

/* Pad or truncate string to exact visible width */
let padToWidth = Element.padToWidth;

/* Get visible length of string (ignoring ANSI codes) */
let visibleLength = Element.visibleLength;

/* Split string into lines */
let splitLines = Element.splitLines;

/* Strip ANSI escape codes from a string (useful for testing) */
let stripAnsi = Element.stripAnsi;

/* ============================================================================
 * Headless Mode
 * ============================================================================ */

/* Headless mode types */
type headlessConfig = Runtime.headlessConfig;
type headlessHandle = Runtime.headlessHandle;

/* Start app in headless mode for testing/agent interaction */
let startHeadless = Runtime.startHeadless;

/* Default headless configuration (80x24) */
let defaultHeadlessConfig = Runtime.defaultHeadlessConfig;

/* Check if MATCHA_HEADLESS=1 environment variable is set */
let isHeadless = Runtime.isHeadless;
