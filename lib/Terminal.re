/*
 * Terminal - Low-level terminal I/O operations
 *
 * This module provides direct control over the terminal, including:
 * - Raw mode input (character-by-character, no line buffering)
 * - Cursor visibility and positioning
 * - Screen clearing
 * - Terminal size detection
 * - Key reading with escape sequence parsing
 *
 * Note: This module uses Unix-specific APIs and is not available on Windows.
 */

/* Get the current terminal size. Returns (columns, rows). */
external getSize: unit => (int, int) = "caml_get_terminal_size";

/* Stored terminal settings for restoration on exit */
let originalTermio = ref(None);

/* Clear the entire screen and move cursor to top-left.
 * Uses ANSI escape codes: ESC[2J (clear) + ESC[H (home).
 */
let clearScreen = () => {
  print_string("\027[2J\027[H");
  flush(stdout);
};

/* Hide the terminal cursor.
 * Call this during rendering to prevent cursor flicker.
 */
let hideCursor = () => {
  print_string("\027[?25l");
  flush(stdout);
};

/* Show the terminal cursor.
 * Called automatically when restoring terminal state.
 */
let showCursor = () => {
  print_string("\027[?25h");
  flush(stdout);
};

/* Move the cursor to a specific position.
 * Row and col are 1-based (top-left is 1,1).
 */
let moveCursor = (row: int, col: int) => {
  Printf.printf("\027[%d;%dH", row, col);
};

/* Put the terminal into raw mode for character-by-character input.
 *
 * Disables:
 * - Canonical mode (line buffering)
 * - Echo (typed characters aren't shown)
 * - Signal generation (ISIG): Ctrl+C/Ctrl+Z arrive as ordinary bytes
 *   (0x03/0x1a) for the application's own key handlers instead of raising
 *   SIGINT/SIGTSTP - a SIGINT would kill the process before any useKeyDown
 *   handler OR the at_exit terminal restore ran, leaving the terminal in
 *   raw+mouse mode. Every example binds Ctrl+C (or q) itself.
 * - Flow control (IXON): Ctrl+S/Ctrl+Q arrive as bytes too, rather than
 *   silently freezing/unfreezing output.
 *
 * Sets non-blocking read with 100ms timeout.
 * The original settings are saved for later restoration.
 */
let setRawMode = () => {
  let termio = Unix.tcgetattr(Unix.stdin);
  originalTermio := Some(termio);
  let rawTermio = {
    ...termio,
    Unix.c_icanon: false,
    Unix.c_echo: false,
    Unix.c_isig: false,
    Unix.c_ixon: false,
    Unix.c_vmin: 0,
    Unix.c_vtime: 1,
  };
  Unix.tcsetattr(Unix.stdin, Unix.TCSANOW, rawTermio);
  /* Push the kitty keyboard protocol in disambiguate-only mode: legacy
   * keys keep sending their legacy sequences, but otherwise-ambiguous
   * ones (ESC itself, modified Enter/Tab) arrive as CSI-u instead.
   * Ignored outright by terminals that don't support it. */
  print_string("\027[>1u");
  /* Enable bracketed paste (B2/S6): pasted text arrives wrapped in
   * ESC[200~ ... ESC[201~ instead of as indistinguishable keystrokes.
   * InputDecoder recognizes the wrapper and surfaces the body as one
   * Key.Paste event. Ignored by terminals that don't support it. */
  print_string("\027[?2004h");
  flush(stdout);
};

/* Turn on mouse reporting (B4).
 *
 * ESC[?1002h is BUTTON-EVENT tracking: presses, releases, wheel, and motion
 * only WHILE a button is held - deliberately not ?1003 (any-motion), which
 * would flood the input stream with a report per cursor cell moved.
 * ESC[?1006h asks for the SGR encoding, whose coordinates are not capped at
 * column/row 223 the way the original X10 encoding is.
 *
 * Called by the interactive loop when the UI starts wanting mouse events
 * (Hooks.hasMouseHandlers), never in headless mode. Note that while this is
 * on, the terminal's own text selection typically needs a modifier
 * (Shift/Option, depending on the terminal).
 */
let enableMouse = () => {
  print_string("\027[?1002;1006h");
  flush(stdout);
};

/* Turn mouse reporting back off - the exact inverse of enableMouse. Called
 * when the last useMouse handler goes away; restoreTerminal also sends it
 * unconditionally on every exit path. */
let disableMouse = () => {
  print_string("\027[?1002;1006l");
  flush(stdout);
};

/* Switch to the ALTERNATE screen buffer (ESC[?1049h).
 *
 * The alternate screen is a second, scrollback-less buffer: the current
 * screen contents (and the scroll position) are saved, the app gets a blank
 * viewport it owns entirely, and exitAltScreen puts the previous contents
 * back. This is what vim/htop/less do, and what Runtime's Fullscreen screen
 * mode uses. Ignored by terminals that don't support it.
 */
let enterAltScreen = () => {
  print_string("\027[?1049h");
  flush(stdout);
};

/* Leave the alternate screen buffer (ESC[?1049l) - the exact inverse of
 * enterAltScreen. The terminal restores whatever was on the normal screen
 * before the app started, which IS the correct restore for a fullscreen app.
 * restoreTerminal also sends it unconditionally on every exit path.
 */
let exitAltScreen = () => {
  print_string("\027[?1049l");
  flush(stdout);
};

/* Ask the terminal what its BACKGROUND color is (OSC 11 with a "?" value).
 *
 * The answer arrives on stdin as `ESC]11;rgb:RRRR/GGGG/BBBB` terminated by
 * BEL or ST; InputDecoder frames it as an `OscReport(11, payload)`, Runtime
 * parses the payload and stores it on the running instance, and
 * [Hooks.useTerminalBackground] is how an application reads it - which is
 * what lets a UI pick light-terminal or dark-terminal colors.
 *
 * Fire-and-forget, exactly like Runtime's DSR query: a terminal that does
 * not implement OSC 11 (or answers late, or is not a terminal at all) simply
 * never replies and costs nothing. That is why useTerminalBackground returns
 * an option and why every caller needs a sensible default.
 */
let queryBackground = () => {
  print_string("\027]11;?\007");
  flush(stdout);
};

/* Restore terminal to its original state.
 *
 * Re-enables canonical mode and echo, shows cursor,
 * and prints a newline. Called automatically on exit.
 */
let restoreTerminal = () => {
  switch (originalTermio^) {
  | Some(termio) => Unix.tcsetattr(Unix.stdin, Unix.TCSANOW, termio)
  | None => ()
  };
  /* Pop the kitty keyboard protocol stack unconditionally - safe to send
   * even if it was never successfully pushed (or the terminal doesn't
   * support it at all), and must run on every exit path (crash-safe via
   * the existing at_exit registration). */
  print_string("\027[<u");
  /* Disable bracketed paste and SGR mouse reporting unconditionally, for
   * the same crash-safety reason: both are TTY-path-only escape emissions
   * (setRawMode / mouse auto-enable), so disabling them here even when
   * they were never turned on this run is harmless, and this is the one
   * path that reliably runs on every exit (normal quit, crash, or
   * kill - via the existing at_exit registration). */
  print_string("\027[?2004l");
  print_string("\027[?1002;1006l");
  /* Leave the alternate screen unconditionally, same crash-safety
   * rationale: only Runtime's Fullscreen screen mode ever enters it, and
   * leaving an alternate screen that was never entered is a no-op on every
   * terminal - so sending it here, on the one path that reliably runs on
   * every exit, is what guarantees a crashed fullscreen app cannot strand
   * the user on a blank alternate buffer. */
  print_string("\027[?1049l");
  showCursor();
  print_newline();
};

/* Read a key press from the terminal.
 *
 * Non-blocking: returns None if no key is available.
 * Handles escape sequences for arrow keys, function keys, etc.
 *
 * Superseded by readBytes + InputDecoder (B2/S6): a single 8-byte read
 * handed straight to Key.parse can't correctly frame a bracketed paste, a
 * mouse report, or even a fast multi-character read (several keys in one
 * read() used to come back as one indistinguishable Unknown - see
 * InputDecoder's doc comment). Runtime's loops no longer call this. Kept
 * exported for API stability - it's a reasonable one-shot read for a
 * caller that only cares about simple keys and doesn't want the decoder's
 * statefulness.
 */
let readKey = (): option((Key.t, Key.modifiers)) => {
  let buf = Bytes.create(8);
  let n =
    try(Unix.read(Unix.stdin, buf, 0, 8)) {
    | Unix.Unix_error(Unix.EINTR, _, _) => 0
    };

  if (n > 0) {
    Some(Key.parse(buf, n));
  } else {
    None;
  };
};

/* Read up to 4096 raw bytes from stdin into a fresh buffer.
 *
 * This is the read side of the InputDecoder pipeline (B2/S6):
 * `Terminal.readBytes() -> InputDecoder.feed` replaces the old
 * `readKey`/`Key.parse` pipeline in Runtime's loops. Non-blocking (relies
 * on the caller having already selected on stdin, same as readKey);
 * returns None on EOF (a zero-byte read), Some(buf, n) otherwise, where
 * only buf[0..n-1] is valid - the rest of a freshly allocated buffer is
 * unspecified.
 */
let readBytes = (): option((bytes, int)) => {
  let buf = Bytes.create(4096);
  let n =
    try(Unix.read(Unix.stdin, buf, 0, 4096)) {
    | Unix.Unix_error(Unix.EINTR, _, _) => (-1)
    };

  if (n > 0) {
    Some((buf, n));
  } else if (n == 0) {
    None; /* EOF */
  } else {
    /* EINTR: nothing read, but not EOF either - an empty-but-present
     * chunk lets InputDecoder.feed see zero bytes without the caller
     * mistaking this for EOF. */
    Some((buf, 0));
  };
};

/* Signal number for terminal resize (SIGWINCH).
 * Value is 28 on macOS and Linux.
 */
let sigwinch = 28;
