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
module Text = Element.Text;
module Bold = Element.Bold;
module Dim = Element.Dim;
module Italic = Element.Italic;
module Underline = Element.Underline;
module Inverted = Element.Inverted;
module Column = Element.Column;
module Row = Element.Row;
module Box = Element.Box;

/* Box drawing utilities */
module BoxChars = Element.BoxChars;
let repeatString = Element.repeatString;
let padToWidth = Element.padToWidth;
let visibleLength = Element.visibleLength;
let splitLines = Element.splitLines;
