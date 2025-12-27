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
  shift: bool, /* Shift key was held */
  meta: bool, /* Meta/Command key was held (Cmd on macOS) */
};

/* Default modifiers with all flags set to false.
 * Used for keys that don't have any modifiers.
 */
let noModifiers = {
  ctrl: false,
  alt: false,
  shift: false,
  meta: false,
};

/* Parse modifier code from extended escape sequences.
 * Terminal modifier encoding: 1 + (shift?1:0) + (alt?2:0) + (ctrl?4:0) + (meta?8:0)
 * So modifier code 2 = Shift, 3 = Alt, 5 = Ctrl, 9 = Meta, etc.
 */
let parseModifierCode = (code: int): modifiers => {
  let adjusted = code - 1; /* Remove the +1 offset */
  {
    shift: adjusted land 1 != 0,
    alt: adjusted land 2 != 0,
    ctrl: adjusted land 4 != 0,
    meta: adjusted land 8 != 0,
  };
};

/* Debug flag - set to true to log raw key input to /tmp/matcha-keys.log */
let debugKeys = ref(false);

/* Log file for debugging */
let logFile = ref(None);

/* Get or create log file */
let getLogFile = (): out_channel => {
  switch (logFile^) {
  | Some(f) => f
  | None =>
    let f = open_out("/tmp/matcha-keys.log");
    logFile := Some(f);
    f
  };
};

/* Log raw bytes for debugging key input */
let logBytes = (bytes: bytes, len: int): unit => {
  if (debugKeys^) {
    let buf = Buffer.create(len * 4);
    Buffer.add_string(buf, "[Key] len=");
    Buffer.add_string(buf, string_of_int(len));
    Buffer.add_string(buf, " bytes=[");
    for (i in 0 to len - 1) {
      if (i > 0) { Buffer.add_string(buf, ", "); };
      let c = Bytes.get(bytes, i);
      let code = Char.code(c);
      if (code == 27) {
        Buffer.add_string(buf, "ESC");
      } else if (code >= 32 && code < 127) {
        Buffer.add_char(buf, '\'');
        Buffer.add_char(buf, c);
        Buffer.add_char(buf, '\'');
      } else {
        Buffer.add_string(buf, string_of_int(code));
      };
    };
    Buffer.add_string(buf, "]");
    let f = getLogFile();
    output_string(f, Buffer.contents(buf) ++ "\n");
    flush(f);
  };
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
let parse = (bytes: bytes, len: int): (t, modifiers) => {
  logBytes(bytes, len);
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
    | 1 => (Arrow_left, {ctrl: false, alt: false, shift: false, meta: true}) /* Ctrl+A - start of line */
    | 5 => (Arrow_right, {ctrl: false, alt: false, shift: false, meta: true}) /* Ctrl+E - end of line */
    | _ when code >= 2 && code <= 26 =>
      /* Ctrl+key (codes 2-26 map to Ctrl+B through Ctrl+Z) */
      /* Note: 1, 5, 8, 9, 21, 23 are handled above as special keys */
      let letter = Char.chr(code + 96); /* Convert to lowercase letter */
      (
        Char(letter),
        {
          ctrl: true,
          alt: false,
          shift: false,
          meta: false,
        },
      );
    | _ => (Char(c), noModifiers)
    };
  } else if (len >= 3
             && Bytes.get(bytes, 0) == '\027'
             && Bytes.get(bytes, 1) == '[') {
    /* ANSI escape sequences: ESC [ <code> */
    /* Check for extended format: ESC[1;{modifier}{key} */
    if (len >= 6
        && Bytes.get(bytes, 2) == '1'
        && Bytes.get(bytes, 3) == ';') {
      /* Extended arrow key with modifiers: ESC[1;{mod}{key} */
      /* Parse modifier digit(s) - can be 1 or 2 digits */
      let (modCode, keyIdx) =
        if (len >= 7
            && Bytes.get(bytes, 5) >= '0'
            && Bytes.get(bytes, 5) <= '9') {
          /* Two-digit modifier (e.g., 10 for Shift+Meta) */
          let tens = Char.code(Bytes.get(bytes, 4)) - 48;
          let ones = Char.code(Bytes.get(bytes, 5)) - 48;
          (tens * 10 + ones, 6);
        } else {
          /* Single-digit modifier */
          (Char.code(Bytes.get(bytes, 4)) - 48, 5);
        };
      let mods = parseModifierCode(modCode);
      if (keyIdx < len) {
        switch (Bytes.get(bytes, keyIdx)) {
        | 'A' => (Arrow_up, mods)
        | 'B' => (Arrow_down, mods)
        | 'C' => (Arrow_right, mods)
        | 'D' => (Arrow_left, mods)
        | 'H' => (Arrow_left, {...mods, meta: true}) /* Home - start of line */
        | 'F' => (Arrow_right, {...mods, meta: true}) /* End - end of line */
        | _ => (Unknown, mods)
        };
      } else {
        (Unknown, noModifiers);
      };
    } else {
      /* Simple arrow keys: ESC[A/B/C/D and special keys */
      let metaMods = {ctrl: false, alt: false, shift: false, meta: true};
      switch (Bytes.get(bytes, 2)) {
      | 'A' => (Arrow_up, noModifiers)
      | 'B' => (Arrow_down, noModifiers)
      | 'C' => (Arrow_right, noModifiers)
      | 'D' => (Arrow_left, noModifiers)
      | 'H' => (Arrow_left, metaMods) /* Home - start of line */
      | 'F' => (Arrow_right, metaMods) /* End - end of line */
      | '1' when len >= 4 && Bytes.get(bytes, 3) == '~' => (Arrow_left, metaMods) /* ESC[1~ = Home */
      | '4' when len >= 4 && Bytes.get(bytes, 3) == '~' => (Arrow_right, metaMods) /* ESC[4~ = End */
      | '3' when len >= 4 && Bytes.get(bytes, 3) == '~' => (Delete, noModifiers)
      | _ => (Unknown, noModifiers)
      };
    };
  } else if (len >= 3
             && Bytes.get(bytes, 0) == '\027'
             && Bytes.get(bytes, 1) == 'O') {
    /* Application mode keys: ESC O <key> */
    let metaMods = {ctrl: false, alt: false, shift: false, meta: true};
    switch (Bytes.get(bytes, 2)) {
    | 'H' => (Arrow_left, metaMods) /* Home - start of line */
    | 'F' => (Arrow_right, metaMods) /* End - end of line */
    | 'A' => (Arrow_up, noModifiers)
    | 'B' => (Arrow_down, noModifiers)
    | 'C' => (Arrow_right, noModifiers)
    | 'D' => (Arrow_left, noModifiers)
    | _ => (Unknown, noModifiers)
    };
  } else if (len >= 2 && Bytes.get(bytes, 0) == '\027') {
    /* Alt+key: ESC followed by character */
    /* Handle Emacs-style navigation (common on macOS terminals) */
    let c = Bytes.get(bytes, 1);
    let altMods = {ctrl: false, alt: true, shift: false, meta: false};
    let altShiftMods = {ctrl: false, alt: true, shift: true, meta: false};
    switch (c) {
    | 'b' => (Arrow_left, altMods) /* ESC b = Alt+Left (word backward) */
    | 'B' => (Arrow_left, altShiftMods) /* ESC B = Alt+Shift+Left (select word backward) */
    | 'f' => (Arrow_right, altMods) /* ESC f = Alt+Right (word forward) */
    | 'F' => (Arrow_right, altShiftMods) /* ESC F = Alt+Shift+Right (select word forward) */
    | '\127' => (Backspace, altMods) /* ESC DEL = Alt+Backspace (delete word) */
    | _ =>
      /* Detect shift from uppercase letters */
      let isUpper = c >= 'A' && c <= 'Z';
      let mods = if (isUpper) { altShiftMods } else { altMods };
      (Char(c), mods)
    };
  } else {
    (Unknown, noModifiers);
  };
};
