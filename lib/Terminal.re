external getSize: unit => (int, int) = "caml_get_terminal_size";

let originalTermio = ref(None);

let clearScreen = () => print_string("\027[2J\027[H");

let hideCursor = () => print_string("\027[?25l");

let showCursor = () => print_string("\027[?25h");

let moveCursor = (row: int, col: int) => {
  Printf.printf("\027[%d;%dH", row, col);
};

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

let restoreTerminal = () => {
  switch (originalTermio^) {
  | Some(termio) => Unix.tcsetattr(Unix.stdin, Unix.TCSANOW, termio)
  | None => ()
  };
  showCursor();
  print_newline();
};

/* Read a key with support for escape sequences */
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

/* SIGWINCH = 28 on macOS/Linux */
let sigwinch = 28;
