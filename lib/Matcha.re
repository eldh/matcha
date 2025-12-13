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
module Runtime = Runtime;
module Terminal = Terminal;

/* JSX Element components available at top level */
module Text = Element.Text; /* Plain text element */
module Bold = Element.Bold; /* Bold styled text */
module Dim = Element.Dim; /* Dim styled text */
module Italic = Element.Italic; /* Italic styled text */
module Underline = Element.Underline; /* Underlined text */
module Inverted = Element.Inverted; /* Inverted (reverse video) text */
module Column = Element.Column; /* Vertical stack of elements */
module Row = Element.Row; /* Horizontal row of elements */
module Box = Element.Box; /* Fixed-size box container */

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
