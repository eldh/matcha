/*
 * Mouse - SGR (1006) mouse event types, decoding, and rect helpers.
 *
 * Terminals report mouse activity as an SGR-encoded CSI sequence:
 *   ESC [ < Cb ; Cx ; Cy M   (button press / motion / wheel)
 *   ESC [ < Cb ; Cx ; Cy m   (button release)
 * InputDecoder frames this sequence off the wire (recognizing the `ESC[<`
 * prefix and the `M`/`m` final byte) and hands the part after `<` - e.g.
 * "0;10;20M" - to parseSgr, which is the pure decode this module owns.
 *
 * This module is dependency-free (no TextWidth, no Element) so it can be
 * unit-tested in isolation and is safe for InputDecoder to depend on.
 */

/* Which button a Down/Up/Move event is about. NoButton covers plain
 * motion reports (no button held) and the "3" bit pattern wheel events
 * don't use. */
type button =
  | Left
  | Middle
  | Right
  | NoButton;

/* What kind of mouse activity occurred. */
type kind =
  | Down
  | Up
  | Move
  | ScrollUp
  | ScrollDown;

/* A single decoded mouse event. Coordinates are 0-based (the wire format
 * is 1-based) and, once B4 wires this up, relative to the live region -
 * not the raw terminal screen. */
type event = {
  kind,
  button,
  x: int,
  y: int,
  shift: bool,
  alt: bool,
  ctrl: bool,
};

/* An allocated layout rect: x/y of the top-left corner, width, height.
 * This is the CONSTRAINT box a component was given, not the ink it
 * actually painted - clicks that land in alignment padding still count
 * (documented in B4). */
type rect = {
  rx: int,
  ry: int,
  rw: int,
  rh: int,
};

/* Half-open containment test: x in [rx, rx+rw), y in [ry, ry+rh). */
let contains = (r: rect, x: int, y: int): bool =>
  x >= r.rx && x < r.rx + r.rw && y >= r.ry && y < r.ry + r.rh;

/* The overlapping region of two rects. Degenerates to a zero-size rect
 * (rw=0 or rh=0) when they don't overlap - still a valid rect, `contains`
 * simply never returns true for it. Used to clip a component's bounds to
 * its ancestors' (e.g. a future Viewport clipping its scrolled-out
 * children). */
let intersect = (a: rect, b: rect): rect => {
  let x1 = max(a.rx, b.rx);
  let y1 = max(a.ry, b.ry);
  let x2 = min(a.rx + a.rw, b.rx + b.rw);
  let y2 = min(a.ry + a.rh, b.ry + b.rh);
  {rx: x1, ry: y1, rw: max(0, x2 - x1), rh: max(0, y2 - y1)};
};

/* Decode the body of an SGR mouse sequence - everything between `ESC[<`
 * and the terminating `M`/`m`, INCLUDING that final byte, e.g. "0;10;20M".
 * Returns None for anything that doesn't parse as exactly three ';'
 * separated integers followed by 'M' or 'm', and for the horizontal-wheel
 * button codes (base land 3 == 2 or 3 when the wheel bit is set), which
 * this version doesn't support.
 *
 * Decode order matters: modifier bits are additive on top of the base
 * button/wheel code, so they must be masked off FIRST, before inspecting
 * the base value - otherwise e.g. Shift+wheel-up (Cb=68) would be
 * misread as some other base code entirely.
 */
let parseSgr = (s: string): option(event) => {
  let len = String.length(s);
  if (len == 0) {
    None;
  } else {
    let finalChar = s.[len - 1];
    if (finalChar != 'M' && finalChar != 'm') {
      None;
    } else {
      let paramStr = String.sub(s, 0, len - 1);
      switch (String.split_on_char(';', paramStr)) {
      | [bStr, xStr, yStr] =>
        switch (
          int_of_string_opt(bStr),
          int_of_string_opt(xStr),
          int_of_string_opt(yStr),
        ) {
        | (Some(b), Some(x1), Some(y1)) =>
          /* Mask the modifier bits off FIRST - they're additive on top of
           * whatever base code follows, so the base can only be read
           * correctly once they're stripped. */
          let shift = b land 4 != 0;
          let alt = b land 8 != 0;
          let ctrl = b land 16 != 0;
          let base = b land lnot(4 lor 8 lor 16);
          let x = x1 - 1;
          let y = y1 - 1;
          if (base land 64 != 0) {
            /* Wheel event. base land 3: 0 => up, 1 => down, 2/3 => the
             * horizontal wheel, which this decoder doesn't represent. */
            switch (base land 3) {
            | 0 =>
              Some({kind: ScrollUp, button: NoButton, x, y, shift, alt, ctrl})
            | 1 =>
              Some({
                kind: ScrollDown,
                button: NoButton,
                x,
                y,
                shift,
                alt,
                ctrl,
              })
            | _ => None
            };
          } else {
            let button =
              switch (base land 3) {
              | 0 => Left
              | 1 => Middle
              | 2 => Right
              | _ => NoButton
              };
            let kind =
              if (base land 32 != 0) {
                Move;
              } else if (finalChar == 'M') {
                Down;
              } else {
                Up;
              };
            Some({kind, button, x, y, shift, alt, ctrl});
          }
        | _ => None
        }
      | _ => None
      };
    };
  };
};

/* Re-encode an event back to the `Cb;Cx;CyM`/`m` body parseSgr consumes -
 * a test helper so mouse_parse_tests can round-trip every case through
 * parseSgr rather than hand-writing wire strings for the decode side too.
 * Not a byte-perfect inverse of an arbitrary wire sequence (a Down and a
 * "release with a stale button" both collapse through real terminals in
 * ways this doesn't try to reproduce) - it only needs to reproduce what
 * parseSgr itself would decode back out. */
let encodeSgr = (ev: event): string => {
  let (base, finalChar) =
    switch (ev.kind) {
    | ScrollUp => (64, 'M')
    | ScrollDown => (64 lor 1, 'M')
    | Down => (
        switch (ev.button) {
        | Left => 0
        | Middle => 1
        | Right => 2
        | NoButton => 3
        },
        'M',
      )
    | Up => (
        switch (ev.button) {
        | Left => 0
        | Middle => 1
        | Right => 2
        | NoButton => 3
        },
        'm',
      )
    | Move => (
        (
          switch (ev.button) {
          | Left => 0
          | Middle => 1
          | Right => 2
          | NoButton => 3
          }
        )
        lor 32,
        'M',
      )
    };
  let b =
    base
    lor (ev.shift ? 4 : 0)
    lor (ev.alt ? 8 : 0)
    lor (ev.ctrl ? 16 : 0);
  Printf.sprintf("%d;%d;%d%c", b, ev.x + 1, ev.y + 1, finalChar);
};
