/*
 * Key - Keyboard input handling and normalization
 *
 * This module provides types and parsing for keyboard input in terminal applications.
 * It normalizes various terminal-specific key codes into a consistent API, so
 * application code doesn't need to handle raw terminal escape sequences.
 *
 * Key Types:
 * - Arrow keys: Arrow_up, Arrow_down, Arrow_left, Arrow_right
 * - Special keys: Escape, Enter, Backspace, Tab, Delete
 * - Terminal shortcuts: KillLine (Ctrl+U), KillWord (Ctrl+W)
 * - Character input: Char(char) with optional modifiers
 *
 * Modifiers:
 * Key presses can have modifiers: ctrl, alt, shift.
 * For most special keys, modifiers are already normalized (e.g., Ctrl+H becomes Backspace).
 */

/* Keyboard key type.
 * Represents a normalized key press, abstracting over terminal-specific encodings.
 */
type t =
  | Arrow_up /* Up arrow key */
  | Arrow_down /* Down arrow key */
  | Arrow_left /* Left arrow key */
  | Arrow_right /* Right arrow key */
  | Char(char) /* Regular character key */
  | Escape /* Escape key */
  | Enter /* Enter/Return key */
  | Backspace /* Backspace key (normalized from code 8 or 127) */
  | Tab /* Tab key */
  | Delete /* Delete key (forward delete) */
  | KillLine /* Ctrl+U - kill/clear entire line */
  | KillWord /* Ctrl+W - kill/delete previous word */
  | Unknown; /* Unrecognized key or sequence */

/* Key modifiers record.
 * Indicates which modifier keys were held during the key press.
 */
type modifiers = {
  ctrl: bool, /* Control key was held */
  alt: bool, /* Alt/Option key was held */
  shift: bool /* Shift key was held */
};

/* Default modifiers with all flags set to false.
 * Used for keys that don't have any modifiers.
 */
let noModifiers = {
  ctrl: false,
  alt: false,
  shift: false,
};

/* Parse raw terminal input bytes into a normalized key and modifiers.
 *
 * This function handles:
 * - Single-byte characters and control codes
 * - ANSI escape sequences for arrow keys and special keys
 * - Alt+key combinations (ESC followed by character)
 * - Ctrl+key combinations (control codes 1-26)
 *
 * Normalization performed:
 * - Code 8 (Ctrl+H) and 127 (DEL) -> Backspace
 * - Code 9 (Ctrl+I) -> Tab
 * - Code 21 (Ctrl+U) -> KillLine
 * - Code 23 (Ctrl+W) -> KillWord
 * - ESC[A/B/C/D -> Arrow keys
 * - ESC[3~ -> Delete
 *
 * Returns: A tuple of (key, modifiers)
 */
let parse = (bytes: bytes, len: int): (t, modifiers) =>
  if (len == 0) {
    (Unknown, noModifiers);
  } else if (len == 1) {
    let c = Bytes.get(bytes, 0);
    let code = Char.code(c);

    /* Check special keys first, then Ctrl+key */
    switch (code) {
    | 27 => (Escape, noModifiers)
    | 10
    | 13 => (Enter, noModifiers) /* Both \n and \r */
    | 8 => (Backspace, noModifiers) /* Ctrl+H is traditionally backspace */
    | 127 => (Backspace, noModifiers) /* DEL key / backspace */
    | 9 => (Tab, noModifiers) /* Tab key (Ctrl+I) */
    | 21 => (KillLine, noModifiers) /* Ctrl+U - kill/clear line */
    | 23 => (KillWord, noModifiers) /* Ctrl+W - kill/delete word */
    | _ when code >= 1 && code <= 26 =>
      /* Ctrl+key (codes 1-26 map to Ctrl+A through Ctrl+Z) */
      /* Note: 8, 9, 21, 23 are handled above as special keys */
      let letter = Char.chr(code + 96); /* Convert to lowercase letter */
      (
        Char(letter),
        {
          ctrl: true,
          alt: false,
          shift: false,
        },
      );
    | _ => (Char(c), noModifiers)
    };
  } else if (len >= 3
             && Bytes.get(bytes, 0) == '\027'
             && Bytes.get(bytes, 1) == '[') {
    /* ANSI escape sequences: ESC [ <code> */
    switch (Bytes.get(bytes, 2)) {
    | 'A' => (Arrow_up, noModifiers)
    | 'B' => (Arrow_down, noModifiers)
    | 'C' => (Arrow_right, noModifiers)
    | 'D' => (Arrow_left, noModifiers)
    | '3' when len >= 4 && Bytes.get(bytes, 3) == '~' => (
        Delete,
        noModifiers,
      )
    | _ => (Unknown, noModifiers)
    };
  } else if (len >= 2 && Bytes.get(bytes, 0) == '\027') {
    /* Alt+key: ESC followed by character */
    let c = Bytes.get(bytes, 1);
    (
      Char(c),
      {
        ctrl: false,
        alt: true,
        shift: false,
      },
    );
  } else {
    (Unknown, noModifiers);
  };
