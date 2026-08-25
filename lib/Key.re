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
 * - Multi-byte input: Text(string) for one complete UTF-8 codepoint
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
  | Char(char) /* Regular single-byte (ASCII) character key */
  | Text(string) /* One complete multi-byte UTF-8 codepoint, e.g. "é" or "日" */
  | Escape /* Escape key */
  | Enter /* Enter/Return key */
  | Backspace /* Backspace key (normalized from code 8 or 127) */
  | Tab /* Tab key */
  | Delete /* Delete key (forward delete) */
  | KillLine /* Ctrl+U - kill/clear entire line */
  | KillWord /* Ctrl+W - kill/delete previous word */
  | Home /* Home key (line/document start, terminal-dependent) */
  | End /* End key (line/document end, terminal-dependent) */
  | Insert /* Insert key */
  | Page_up /* Page Up key */
  | Page_down /* Page Down key */
  | F(int) /* Function key F1-F12 */
  | Paste(string) /* Bracketed-paste payload (produced by InputDecoder, step B2/S6 - no parse branch here yet) */
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

/* Is bytes[start .. start + len - 1] exactly one complete multi-byte UTF-8
 * sequence? Requires a valid lead byte (0xC2-0xF4), a length that matches
 * what the lead byte announces, and valid continuation bytes (0x80-0xBF).
 */
let isUtf8Sequence = (bytes: bytes, start: int, len: int): bool =>
  len >= 2
  && Char.code(Bytes.get(bytes, start)) >= 0xC2
  && TextWidth.utf8ExpectedLen(Bytes.get(bytes, start)) == len
  && {
    let rec continuations = i =>
      if (i >= len) {
        true;
      } else {
        let b = Char.code(Bytes.get(bytes, start + i));
        b >= 0x80 && b <= 0xBF && continuations(i + 1);
      };
    continuations(1);
  };

/* Parse the parameter section of a CSI sequence (the bytes between
 * `ESC[` and the final byte). Scans forward from `start` for the first
 * byte in the final-byte range (0x40-0x7E); everything before it is the
 * parameter string, split on ';' into ints (an empty segment, including
 * a wholly-empty parameter string, is treated as 1 per terminal
 * convention for omitted params). Returns None if no final byte is
 * present within `len`. */
let parseCsiParams = (bytes: bytes, start: int, len: int): option((list(int), char)) => {
  let rec findFinal = i =>
    if (i >= len) {
      None;
    } else {
      let b = Char.code(Bytes.get(bytes, i));
      if (b >= 0x40 && b <= 0x7E) {
        Some(i);
      } else {
        findFinal(i + 1);
      };
    };
  switch (findFinal(start)) {
  | None => None
  | Some(finalIdx) =>
    let paramStr = Bytes.sub_string(bytes, start, finalIdx - start);
    let finalChar = Bytes.get(bytes, finalIdx);
    let parseSegment = s =>
      if (s == "") {
        1;
      } else {
        switch (int_of_string_opt(s)) {
        | Some(n) => n
        | None => 1
        };
      };
    let params =
      if (paramStr == "") {
        [];
      } else {
        String.split_on_char(';', paramStr) |> List.map(parseSegment);
      };
    Some((params, finalChar));
  };
};

/* Extract the modifier code (second CSI parameter, e.g. the `5` in
 * `ESC[1;5H`) and decode it. Sequences with no second parameter (plain
 * `ESC[H`, `ESC[A`, etc.) carry no modifiers. */
let modsFromParams = (params: list(int)): modifiers =>
  switch (params) {
  | [_, m, ..._] => parseModifierCode(m)
  | _ => noModifiers
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
    /* ANSI CSI sequences: ESC [ <params> <final>
     * Home = ESC[H, ESC[1~, ESC[7~, ESC[1;<m>H (ESCOH handled below);
     * End = ESC[F, ESC[4~, ESC[8~, ESC[1;<m>F (ESCOF handled below);
     * Insert = ESC[2~; Delete = ESC[3~; Page_up/down = ESC[5~/6~;
     * ESC[Z = backtab -> Shift+Tab; F1-F4 also reachable via
     * ESC[1;<m>P..S and legacy ESC[11~..14~; F5-F12 via ESC[15~,
     * 17~..21~, 23~, 24~ (16 and 22 are gaps); Linux console
     * ESC[[A..E -> F1-F5; kitty CSI-u ESC[<code>;<m>u. */
    if (len >= 4
        && Bytes.get(bytes, 2) == '['
        && Bytes.get(bytes, 3) >= 'A'
        && Bytes.get(bytes, 3) <= 'E') {
      /* Linux console function keys: ESC[[A..E -> F1..F5 */
      let n = Char.code(Bytes.get(bytes, 3)) - Char.code('A') + 1;
      (F(n), noModifiers);
    } else {
      switch (parseCsiParams(bytes, 2, len)) {
      | None => (Unknown, noModifiers)
      | Some((params, finalChar)) =>
        let m = modsFromParams(params);
        switch (finalChar) {
        | 'A' => (Arrow_up, m)
        | 'B' => (Arrow_down, m)
        | 'C' => (Arrow_right, m)
        | 'D' => (Arrow_left, m)
        | 'H' => (Home, m)
        | 'F' => (End, m)
        | 'P' => (F(1), m)
        | 'Q' => (F(2), m)
        | 'R' => (F(3), m)
        | 'S' => (F(4), m)
        | 'Z' =>
          switch (params) {
          | [] => (Tab, {ctrl: false, alt: false, shift: true, meta: false}) /* backtab */
          | _ => (Unknown, m)
          }
        | '~' =>
          switch (params) {
          | [] => (Unknown, noModifiers)
          | [code, ..._] =>
            switch (code) {
            | 1
            | 7 => (Home, m)
            | 2 => (Insert, m)
            | 3 => (Delete, m)
            | 4
            | 8 => (End, m)
            | 5 => (Page_up, m)
            | 6 => (Page_down, m)
            | 11 => (F(1), m)
            | 12 => (F(2), m)
            | 13 => (F(3), m)
            | 14 => (F(4), m)
            | 15 => (F(5), m)
            | 17 => (F(6), m)
            | 18 => (F(7), m)
            | 19 => (F(8), m)
            | 20 => (F(9), m)
            | 21 => (F(10), m)
            | 23 => (F(11), m)
            | 24 => (F(12), m)
            | _ => (Unknown, m)
            }
          }
        | 'u' =>
          /* Kitty disambiguate-only CSI-u: ESC[<code>;<mods>u */
          switch (params) {
          | [] => (Unknown, noModifiers)
          | [code, ..._] =>
            switch (code) {
            | 13 => (Enter, m)
            | 9 => (Tab, m)
            | 27 => (Escape, m)
            | 127 => (Backspace, m)
            | c when c >= 32 && c <= 126 => (Char(Char.chr(c)), m)
            | _ => (Unknown, m)
            }
          }
        | _ => (Unknown, m)
        };
      };
    };
  } else if (len >= 3
             && Bytes.get(bytes, 0) == '\027'
             && Bytes.get(bytes, 1) == 'O') {
    /* Application mode keys: ESC O <key> (SS3) */
    switch (Bytes.get(bytes, 2)) {
    | 'H' => (Home, noModifiers)
    | 'F' => (End, noModifiers)
    | 'A' => (Arrow_up, noModifiers)
    | 'B' => (Arrow_down, noModifiers)
    | 'C' => (Arrow_right, noModifiers)
    | 'D' => (Arrow_left, noModifiers)
    | 'P' => (F(1), noModifiers)
    | 'Q' => (F(2), noModifiers)
    | 'R' => (F(3), noModifiers)
    | 'S' => (F(4), noModifiers)
    | _ => (Unknown, noModifiers)
    };
  } else if (len >= 3
             && Bytes.get(bytes, 0) == '\027'
             && isUtf8Sequence(bytes, 1, len - 1)) {
    /* Alt + a multi-byte character: ESC followed by one UTF-8 codepoint */
    (
      Text(Bytes.sub_string(bytes, 1, len - 1)),
      {ctrl: false, alt: true, shift: false, meta: false},
    );
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
  } else if (isUtf8Sequence(bytes, 0, len)) {
    /* One complete multi-byte UTF-8 codepoint typed as-is */
    (Text(Bytes.sub_string(bytes, 0, len)), noModifiers);
  } else {
    (Unknown, noModifiers);
  };
};
