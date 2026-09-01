/*
 * The launcher for examples/command-menu. The app itself lives in
 * CommandMenuApp.re, as a library, so that test/commandmenu_tests.re can
 * drive the very same component headlessly - see the header comment there.
 *
 * Inline mode (the default): the palette floats over the live region, and
 * the log scrolls inside it rather than into the terminal's scrollback.
 */
let () = Matcha.Runtime.start((module CommandMenuApp.App));
