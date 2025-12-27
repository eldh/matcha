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
    Unix.c_vmin: 0,
    Unix.c_vtime: 1,
  };
  Unix.tcsetattr(Unix.stdin, Unix.TCSANOW, rawTermio);
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
  showCursor();
  print_newline();
};

/* Read a key press from the terminal.
 *
 * Non-blocking: returns None if no key is available.
 * Handles escape sequences for arrow keys, function keys, etc.
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

/* Signal number for terminal resize (SIGWINCH).
 * Value is 28 on macOS and Linux.
 */
let sigwinch = 28;
