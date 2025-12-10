type t =
  | Arrow_up
  | Arrow_down
  | Arrow_left
  | Arrow_right
  | Char(char)
  | Escape
  | Enter
  | Backspace
  | Unknown;

type modifiers = {
  ctrl: bool,
  alt: bool,
  shift: bool,
};

let noModifiers = {
  ctrl: false,
  alt: false,
  shift: false,
};

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
    | 127 => (Backspace, noModifiers)
    | _ when code >= 1 && code <= 26 =>
      /* Ctrl+key (codes 1-26 map to Ctrl+A through Ctrl+Z) */
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
    /* Escape sequences */
    switch (Bytes.get(bytes, 2)) {
    | 'A' => (Arrow_up, noModifiers)
    | 'B' => (Arrow_down, noModifiers)
    | 'C' => (Arrow_right, noModifiers)
    | 'D' => (Arrow_left, noModifiers)
    | _ => (Unknown, noModifiers)
    };
  } else if (len >= 2 && Bytes.get(bytes, 0) == '\027') {
    /* Alt+key */
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
