/*
 * Vterm - a minimal VT/xterm screen model for tests.
 *
 * WHY THIS EXISTS
 * ---------------
 * Matcha's painters (lib/FrameDiff.re, lib/LiveRegion.re) are pure, so they
 * are easy to test byte for byte. That is also their trap: a byte-exact
 * expectation encodes a MODEL of what a terminal does with those bytes, and
 * if the model in the test author's head is wrong, the test happily pins the
 * wrong bytes forever. That is exactly how the "content ++ ESC[0m ESC[K"
 * ordering bug survived a green suite: the frame text was right, the byte
 * expectation matched the code, and only a human looking at a real terminal
 * noticed that every full-width row had lost its last cell.
 *
 * This module is the independent second opinion. It is a screen grid that
 * consumes raw bytes and answers "what would a terminal be DISPLAYING now".
 * Painter tests feed it real painter output and assert on the resulting
 * GRID, not on the bytes.
 *
 * INDEPENDENCE IS THE POINT
 * -------------------------
 * Everything here is written from xterm's documented behaviour, NOT from
 * reading Matcha's painters. If the model and a painter disagree, that
 * disagreement is a finding to investigate - never a reason to "fix" the
 * model until the painter's output looks right. Every place where xterm's
 * behaviour is ambiguous or where this model deliberately simplifies is
 * called out in a comment below.
 *
 * SCOPE
 * -----
 * The closed set of sequences Matcha's writers emit, plus printing and the
 * C0 controls needed to interpret them. Anything else is consumed and
 * RECORDED (see [unknownSeqs]) so a test can assert that the writers never
 * emit something the model does not understand - which catches drift in
 * both directions.
 *
 * WHAT IS NOT MODELLED
 * --------------------
 * Colors (SGR params are tracked as an opaque int list, never interpreted),
 * scroll regions (DECSTBM), origin mode, tab stops beyond every-8-columns,
 * character sets, insert mode, and reverse wraparound. None of them are
 * reachable from Matcha's output.
 */

/* One terminal cell.
 *
 * `glyph` holds the UTF-8 bytes that render in this cell: " " for an empty
 * cell, and "" for the CONTINUATION cell of a width-2 glyph (the wide glyph
 * itself lives in the cell to its left). Joining a row's glyphs therefore
 * reproduces the row's text with correct column alignment.
 *
 * `sgr` is the SGR parameter list that was active when the cell was written.
 * Erased cells keep the SGR that was active at erase time - see [eraseCell]. */
type cell = {
  glyph: string,
  sgr: list(int),
};

let blankCell = (sgr: list(int)): cell => {glyph: " ", sgr};

type t = {
  mutable width: int,
  mutable height: int,
  /* The two screen buffers. `altScreen` selects which one is live; the
   * other keeps its contents (xterm's 1049 saves and restores the primary
   * screen wholesale). */
  mutable primary: array(array(cell)),
  mutable alt: array(array(cell)),
  mutable altScreen: bool,
  /* Cursor, 0-based internally (the wire protocol is 1-based; conversion
   * happens at the CSI boundary and in [cursor]). */
  mutable row: int,
  mutable col: int,
  /* DEFERRED WRAP (xterm's `do_wrap`). Printing INTO the last column leaves
   * the cursor ON that column with this flag set; the wrap to the next row
   * happens when the NEXT printable arrives. Every cursor-positioning
   * operation clears it. This flag is the whole reason this module exists:
   * an EL issued in this state erases the last painted cell. */
  mutable pendingWrap: bool,
  mutable sgr: list(int),
  /* Cursor saved by ESC[?1049h, restored by ESC[?1049l. */
  mutable savedCursor: (int, int),
  /* Lines that scrolled off the top of the PRIMARY buffer, most recent
   * first. The alternate screen has no scrollback: rows scrolled off it are
   * discarded, exactly as on a real terminal. */
  mutable scrollbackRev: list(string),
  mutable cursorVisible: bool,
  mutable bracketedPaste: bool,
  mutable mouseReporting: bool,
  /* DSR (ESC[6n) requests seen since the last drain, most recent first,
   * each paired with the cursor position AT THE MOMENT OF THE QUERY. The
   * position has to be captured here rather than read off the model later:
   * a writer typically sends the query and then a whole frame in one write,
   * and a real terminal answers with the cursor where the query found it,
   * not where the frame left it. */
  mutable dsrRequestsRev: list((int, int)),
  /* Escape sequences the model does not implement, most recent first. */
  mutable unknownRev: list(string),
  /* Bytes of an escape sequence (or a UTF-8 character) that was cut in half
   * by a chunk boundary. Prepended to the next [feed]. */
  mutable partial: string,
};

let makeGrid = (~width: int, ~height: int): array(array(cell)) =>
  Array.init(height, _ => Array.make(width, blankCell([])));

let create = (~width: int, ~height: int): t => {
  let width = max(1, width);
  let height = max(1, height);
  {
    width,
    height,
    primary: makeGrid(~width, ~height),
    alt: makeGrid(~width, ~height),
    altScreen: false,
    row: 0,
    col: 0,
    pendingWrap: false,
    sgr: [],
    savedCursor: (0, 0),
    scrollbackRev: [],
    cursorVisible: true,
    bracketedPaste: false,
    mouseReporting: false,
    dsrRequestsRev: [],
    unknownRev: [],
    partial: "",
  };
};

/* The buffer currently being displayed. */
let active = (t: t): array(array(cell)) =>
  if (t.altScreen) {
    t.alt;
  } else {
    t.primary;
  };

/* ============================================================================
 * Grid primitives
 * ============================================================================ */

let rowText = (cells: array(cell)): string => {
  let buf = Buffer.create(Array.length(cells));
  Array.iter(c => Buffer.add_string(buf, c.glyph), cells);
  Buffer.contents(buf);
};

/* Erase one cell.
 *
 * SIMPLIFICATION (documented, per the module header): a real terminal fills
 * an erased cell with the current BACKGROUND colour only, dropping the
 * foreground/bold/etc. attributes. This model has no colour semantics at
 * all, so it stores the whole current SGR list on the erased cell. Tests
 * that care about erasure assert on the glyph; [cellSgr] on an erased cell
 * therefore tells you "what was active when this was erased", which is the
 * useful thing for the ESC[0m-before-ESC[K question. */
let eraseCell = (t: t, ~row: int, ~col: int): unit => {
  let grid = active(t);
  if (row >= 0 && row < t.height && col >= 0 && col < t.width) {
    grid[row][col] = blankCell(t.sgr);
  };
};

let eraseRowRange = (t: t, ~row: int, ~from: int, ~upto: int): unit =>
  for (c in max(0, from) to min(t.width - 1, upto)) {
    eraseCell(t, ~row, ~col=c);
  };

let eraseWholeRow = (t: t, ~row: int): unit =>
  eraseRowRange(t, ~row, ~from=0, ~upto=t.width - 1);

/* Scroll the active buffer up one line.
 *
 * The top row leaves the screen: on the PRIMARY buffer it is pushed into
 * scrollback; on the ALTERNATE buffer it is discarded (an alt screen has no
 * scrollback - that is one of the two things that make it "alternate"). */
let scrollUp = (t: t): unit => {
  let grid = active(t);
  let top = grid[0];
  if (!t.altScreen) {
    t.scrollbackRev = [rowText(top), ...t.scrollbackRev];
  };
  for (r in 0 to t.height - 2) {
    grid[r] = grid[r + 1];
  };
  grid[t.height - 1] = Array.make(t.width, blankCell(t.sgr));
};

/* Move down one row, scrolling at the bottom margin. Used by LF and by the
 * deferred wrap. */
let indexDown = (t: t): unit =>
  if (t.row >= t.height - 1) {
    t.row = t.height - 1;
    scrollUp(t);
  } else {
    t.row = t.row + 1;
  };

/* ============================================================================
 * Printing
 * ============================================================================ */

/* Fuse a zero-width codepoint (combining mark, variation selector) onto the
 * cell to the left, which is what a terminal does with it. If the cursor is
 * at column 0 there is nothing to fuse onto and the codepoint is dropped -
 * xterm's behaviour here is implementation-defined and nothing Matcha emits
 * depends on it. */
let fuseZeroWidth = (t: t, bytes: string): unit => {
  let grid = active(t);
  let rec target = c =>
    if (c < 0) {
      None;
    } else if (grid[t.row][c].glyph == "") {
      target(c - 1);
    } else {
      Some(c);
    };
  switch (target(t.col - 1)) {
  | None => ()
  | Some(c) =>
    let cur = grid[t.row][c];
    grid[t.row][c] = {glyph: cur.glyph ++ bytes, sgr: cur.sgr};
  };
};

/* Take the deferred wrap, if one is pending: move to column 0 of the next
 * row (scrolling at the bottom) and clear the flag. */
let takePendingWrap = (t: t): unit =>
  if (t.pendingWrap) {
    t.col = 0;
    indexDown(t);
    t.pendingWrap = false;
  };

let printGlyph = (t: t, bytes: string, w: int): unit =>
  if (w == 0) {
    fuseZeroWidth(t, bytes);
  } else {
    takePendingWrap(t);
    /* A width-2 glyph that does not fit in the remaining columns wraps
     * WHOLE rather than being split: the last column is left blank and the
     * glyph starts the next row. */
    if (w == 2 && t.col + 2 > t.width) {
      eraseCell(t, ~row=t.row, ~col=t.width - 1);
      t.col = 0;
      indexDown(t);
    };
    let grid = active(t);
    grid[t.row][t.col] = {glyph: bytes, sgr: t.sgr};
    if (w == 2 && t.col + 1 < t.width) {
      grid[t.row][t.col + 1] = {glyph: "", sgr: t.sgr};
    };
    let newCol = t.col + w;
    if (newCol >= t.width) {
      /* Deferred wrap: stay ON the last column, remember that the next
       * printable has to wrap first. */
      t.col = t.width - 1;
      t.pendingWrap = true;
    } else {
      t.col = newCol;
    };
  };

/* ============================================================================
 * CSI handling
 * ============================================================================ */

let recordUnknown = (t: t, seq: string): unit =>
  t.unknownRev = [seq, ...t.unknownRev];

/* Split a CSI parameter string into ints. An empty parameter means "use the
 * default", represented here as 0 - every caller applies its own default via
 * [param]. */
let parseParams = (s: string): array(int) =>
  if (s == "") {
    [||];
  } else {
    String.split_on_char(';', s)
    |> List.map(p =>
         switch (int_of_string_opt(p)) {
         | Some(n) => n
         | None => 0
         }
       )
    |> Array.of_list;
  };

/* Read parameter [i], applying [default] when it is missing or 0. This is
 * the standard VT rule for the movement finals (CUU/CUD/CUP/...): a 0
 * parameter means "use the default". */
let param = (ps: array(int), i: int, default: int): int =>
  if (i < Array.length(ps) && ps[i] != 0) {
    ps[i];
  } else {
    default;
  };

/* Reader for EL/ED, whose 0 parameter is a real selector ("erase from the
 * cursor forward") rather than a request for a default. */
let paramZ = (ps: array(int), i: int): int =>
  if (i < Array.length(ps)) {
    ps[i];
  } else {
    0;
  };

let setMode = (t: t, ~n: int, ~on: bool): unit =>
  switch (n) {
  | 25 => t.cursorVisible = on
  | 1002
  | 1003
  | 1006
  | 1015 =>
    /* Any of the mouse-reporting modes, in any ';'-combined form. Matcha
     * emits "?1002;1006h" / "?1002;1006l" as one sequence, so both params
     * pass through here. */
    t.mouseReporting = on
  | 2004 => t.bracketedPaste = on
  | 2026 =>
    /* Synchronized update. A terminal that supports it batches the enclosed
     * writes and applies them atomically; the visible end state is identical
     * either way, so the model just parses and ignores it. */
    ()
  | 1049 =>
    if (on && !t.altScreen) {
      /* xterm 1049h: save the cursor, switch to the alternate buffer, and
       * CLEAR it. The primary buffer's contents are untouched and come back
       * on 1049l. */
      t.savedCursor = (t.row, t.col);
      t.alt = makeGrid(~width=t.width, ~height=t.height);
      t.altScreen = true;
      t.pendingWrap = false;
    } else if (!on && t.altScreen) {
      t.altScreen = false;
      let (r, c) = t.savedCursor;
      t.row = min(r, t.height - 1);
      t.col = min(c, t.width - 1);
      t.pendingWrap = false;
    }
  | _ =>
    /* Unknown DEC private mode: parsed and ignored, per the module header.
     * Not recorded as "unknown" - a well-formed private mode the model
     * chooses not to implement is not writer/model drift. */
    ()
  };

let handleCsi = (t: t, ~priv: string, ~params: string, ~final: char, ~raw: string)
    : unit => {
  let ps = parseParams(params);
  switch (priv, final) {
  | ("?", 'h') => Array.iter(n => setMode(t, ~n, ~on=true), ps)
  | ("?", 'l') => Array.iter(n => setMode(t, ~n, ~on=false), ps)
  /* Kitty keyboard protocol push (ESC[>1u) and pop (ESC[<u). Parsed and
   * ignored: they change how the terminal ENCODES key input, which has no
   * effect on the screen this model renders. */
  | (">", 'u')
  | ("<", 'u') => ()
  | ("", 'H')
  | ("", 'f') =>
    let r = max(1, param(ps, 0, 1));
    let c = max(1, param(ps, 1, 1));
    t.row = min(t.height - 1, r - 1);
    t.col = min(t.width - 1, c - 1);
    t.pendingWrap = false;
  | ("", 'A') =>
    t.row = max(0, t.row - max(1, param(ps, 0, 1)));
    t.pendingWrap = false;
  | ("", 'B') =>
    t.row = min(t.height - 1, t.row + max(1, param(ps, 0, 1)));
    t.pendingWrap = false;
  | ("", 'C') =>
    t.col = min(t.width - 1, t.col + max(1, param(ps, 0, 1)));
    t.pendingWrap = false;
  | ("", 'D') =>
    t.col = max(0, t.col - max(1, param(ps, 0, 1)));
    t.pendingWrap = false;
  | ("", 'G') =>
    t.col = min(t.width - 1, max(1, param(ps, 0, 1)) - 1);
    t.pendingWrap = false;
  | ("", 'K') =>
    /* EL. The cursor does NOT move, and critically it is NOT "one past the
     * last column" in the pending-wrap state - it is ON the last column, so
     * ESC[K there erases the cell that was just painted. That is the bug
     * class this whole module exists for.
     *
     * xterm ties do_wrap to the cursor's column and EL does not move the
     * cursor, so the pending-wrap flag is left ALONE here. Nothing Matcha
     * emits depends on that choice (every EL it writes is followed by
     * content or by a cursor move), but it is a deliberate reading of
     * xterm's behaviour rather than an accident. */
    switch (paramZ(ps, 0)) {
    | 1 => eraseRowRange(t, ~row=t.row, ~from=0, ~upto=t.col)
    | 2 => eraseWholeRow(t, ~row=t.row)
    | _ => eraseRowRange(t, ~row=t.row, ~from=t.col, ~upto=t.width - 1)
    }
  | ("", 'J') =>
    switch (paramZ(ps, 0)) {
    | 1 =>
      eraseRowRange(t, ~row=t.row, ~from=0, ~upto=t.col);
      for (r in 0 to t.row - 1) {
        eraseWholeRow(t, ~row=r);
      };
    | 2 =>
      /* Whole screen, cursor UNMOVED (ESC[2J does not home the cursor -
       * that is why every writer follows it with an explicit ESC[H). */
      for (r in 0 to t.height - 1) {
        eraseWholeRow(t, ~row=r);
      }
    | 3 =>
      /* xterm extension: erase scrollback. */
      t.scrollbackRev = []
    | _ =>
      eraseRowRange(t, ~row=t.row, ~from=t.col, ~upto=t.width - 1);
      for (r in t.row + 1 to t.height - 1) {
        eraseWholeRow(t, ~row=r);
      };
    }
  | ("", 'm') =>
    /* SGR. No colour modeling: 0 (or an empty parameter list) resets to [],
     * anything else appends to the current list. */
    if (Array.length(ps) == 0) {
      t.sgr = [];
    } else {
      Array.iter(
        n =>
          if (n == 0) {
            t.sgr = [];
          } else {
            t.sgr = t.sgr @ [n];
          },
        ps,
      );
    }
  | ("", 'n') =>
    if (paramZ(ps, 0) == 6) {
      t.dsrRequestsRev = [(t.row, t.col), ...t.dsrRequestsRev];
    } else {
      recordUnknown(t, raw);
    }
  | _ => recordUnknown(t, raw)
  };
};

/* ============================================================================
 * feed
 * ============================================================================ */

let isCsiParamByte = (c: char): bool => {
  let b = Char.code(c);
  b >= 0x30 && b <= 0x3F;
};

let isCsiIntermediateByte = (c: char): bool => {
  let b = Char.code(c);
  b >= 0x20 && b <= 0x2F;
};

let isCsiFinalByte = (c: char): bool => {
  let b = Char.code(c);
  b >= 0x40 && b <= 0x7E;
};

/* Consume a chunk of output bytes. The chunk may be split anywhere: a
 * sequence (or a UTF-8 character) cut in half is stashed in `partial` and
 * completed by the next call. */
let feed = (t: t, chunk: string): unit => {
  let s = t.partial ++ chunk;
  t.partial = "";
  let len = String.length(s);
  /* Consume from byte [i] onward. A branch that cannot finish the sequence
   * it is looking at writes the rest of the chunk into `partial` and simply
   * returns, which ends the scan. */
  let rec step = (i: int): unit =>
    if (i < len) {
      let c = s.[i];
      switch (c) {
      | '\027' =>
        if (i + 1 >= len) {
          t.partial = String.sub(s, i, len - i);
        } else {
          switch (s.[i + 1]) {
          | '[' =>
            /* CSI: parameter bytes, then intermediates, then one final. */
            let j = ref(i + 2);
            while (j^ < len && isCsiParamByte(s.[j^])) {
              incr(j);
            };
            let paramEnd = j^;
            while (j^ < len && isCsiIntermediateByte(s.[j^])) {
              incr(j);
            };
            if (j^ >= len) {
              t.partial = String.sub(s, i, len - i);
            } else if (!isCsiFinalByte(s.[j^])) {
              /* Malformed: an out-of-range byte where the final should be.
               * Record what we scanned and resume at that byte. */
              recordUnknown(t, String.sub(s, i, j^ - i));
              step(j^);
            } else {
              let final = s.[j^];
              let rawSeq = String.sub(s, i, j^ - i + 1);
              let paramStr = String.sub(s, i + 2, paramEnd - (i + 2));
              /* A private-marker byte (< = > ?) leads the parameter string. */
              let (priv, paramStr) =
                if (String.length(paramStr) > 0) {
                  switch (paramStr.[0]) {
                  | '<' as p
                  | '=' as p
                  | '>' as p
                  | '?' as p => (
                      String.make(1, p),
                      String.sub(paramStr, 1, String.length(paramStr) - 1),
                    )
                  | _ => ("", paramStr)
                  };
                } else {
                  ("", paramStr);
                };
              handleCsi(t, ~priv, ~params=paramStr, ~final, ~raw=rawSeq);
              step(j^ + 1);
            };
          | ']' =>
            /* OSC: terminated by BEL or by ST (ESC \). Nothing Matcha emits
             * uses OSC, so it is consumed and recorded. */
            let rec findEnd = k =>
              if (k >= len) {
                (-1);
              } else if (s.[k] == '\007') {
                k + 1;
              } else if (s.[k] == '\027' && k + 1 < len && s.[k + 1] == '\\') {
                k + 2;
              } else if (s.[k] == '\027' && k + 1 >= len) {
                (-1);
              } else {
                findEnd(k + 1);
              };
            let e = findEnd(i + 2);
            if (e < 0) {
              t.partial = String.sub(s, i, len - i);
            } else {
              recordUnknown(t, String.sub(s, i, e - i));
              step(e);
            };
          | other =>
            /* Two-byte escape (ESC 7, ESC =, ...). None are emitted by
             * Matcha; consumed and recorded. */
            recordUnknown(t, "\027" ++ String.make(1, other));
            step(i + 2);
          };
        }
      | '\r' =>
        t.col = 0;
        t.pendingWrap = false;
        step(i + 1);
      | '\n' =>
        /* LF: down one row, SAME column (the terminal's OPOST/ONLCR turns a
         * program's "\n" into CR LF on the wire, so a bare LF reaching this
         * model really does keep the column). At the bottom row it scrolls. */
        indexDown(t);
        t.pendingWrap = false;
        step(i + 1);
      | '\b' =>
        t.col = max(0, t.col - 1);
        t.pendingWrap = false;
        step(i + 1);
      | '\t' =>
        t.col = min(t.width - 1, (t.col / 8 + 1) * 8);
        t.pendingWrap = false;
        step(i + 1);
      | c when Char.code(c) < 0x20 || Char.code(c) == 0x7F =>
        /* Other C0 controls (BEL, SO/SI, ...) and DEL: ignored, as on a
         * terminal that has nothing bound to them. */
        step(i + 1)
      | _ =>
        /* Printable. Take the whole UTF-8 sequence; if it is cut off by the
         * end of the chunk, stash it and wait for the rest. */
        let expected = Matcha.TextWidth.utf8ExpectedLen(c);
        if (i + expected > len) {
          t.partial = String.sub(s, i, len - i);
        } else {
          let (cp, consumed) = Matcha.TextWidth.decodeUtf8(s, i);
          let bytes = String.sub(s, i, consumed);
          printGlyph(t, bytes, Matcha.TextWidth.charWidth(cp));
          step(i + consumed);
        };
      };
    };
  step(0);
};

/* ============================================================================
 * Readers
 * ============================================================================ */

let row = (t: t, i: int): string =>
  if (i < 0 || i >= t.height) {
    "";
  } else {
    rowText(active(t)[i]);
  };

let snapshot = (t: t): array(string) => Array.init(t.height, i => row(t, i));

let rtrim = (s: string): string => {
  let len = String.length(s);
  let rec findEnd = i =>
    if (i <= 0) {
      0;
    } else {
      switch (s.[i - 1]) {
      | ' '
      | '\t'
      | '\r' => findEnd(i - 1)
      | _ => i
      };
    };
  String.sub(s, 0, findEnd(len));
};

let snapshotTrimmed = (t: t): array(string) =>
  Array.map(rtrim, snapshot(t));

/* The whole visible screen as one newline-joined string, right-trimmed. */
let text = (t: t): string =>
  String.concat("\n", Array.to_list(snapshotTrimmed(t)));

/* Lines that scrolled off the top of the primary buffer, OLDEST FIRST. */
let scrollback = (t: t): list(string) => List.rev(t.scrollbackRev);

let scrollbackText = (t: t): string =>
  String.concat("\n", List.map(rtrim, scrollback(t)));

/* Scrollback and screen together, in display order - "everything the user
 * could scroll back to and see". */
let fullText = (t: t): string => scrollbackText(t) ++ "\n" ++ text(t);

let cursor = (t: t): (int, int) => (t.row, t.col);
let pendingWrap = (t: t): bool => t.pendingWrap;
let inAltScreen = (t: t): bool => t.altScreen;
let cursorVisible = (t: t): bool => t.cursorVisible;
let bracketedPaste = (t: t): bool => t.bracketedPaste;
let mouseReporting = (t: t): bool => t.mouseReporting;
let size = (t: t): (int, int) => (t.width, t.height);

let cellSgr = (t: t, ~row: int, ~col: int): list(int) =>
  if (row < 0 || row >= t.height || col < 0 || col >= t.width) {
    [];
  } else {
    active(t)[row][col].sgr;
  };

let cellGlyph = (t: t, ~row: int, ~col: int): string =>
  if (row < 0 || row >= t.height || col < 0 || col >= t.width) {
    "";
  } else {
    active(t)[row][col].glyph;
  };

/* Number of ESC[6n requests seen since the last drain, and reset. */
let takeDsrRequests = (t: t): int => {
  let n = List.length(t.dsrRequestsRev);
  t.dsrRequestsRev = [];
  n;
};

/* The same queue, drained as the cursor positions (0-based) the queries
 * found - which is what a harness needs to answer them the way a real
 * terminal would. NOTE: this and [takeDsrRequests] drain the SAME queue;
 * call one or the other, not both. */
let takeDsrReplies = (t: t): list((int, int)) => {
  let l = List.rev(t.dsrRequestsRev);
  t.dsrRequestsRev = [];
  l;
};

/* Escape sequences the model does not implement, in the order they arrived.
 * A painter test asserting this is empty is asserting that the writer emits
 * nothing outside the model's vocabulary. */
let unknownSeqs = (t: t): list(string) => List.rev(t.unknownRev);

/* Resize the screen.
 *
 * CHOICE (documented per the module header): contents are preserved
 * TOP-LEFT anchored and cropped - rows and columns that no longer fit are
 * dropped, new rows/columns come in blank, and the cursor is clamped into
 * the new bounds. Real terminals reflow (or not) in ways that vary wildly
 * between implementations; Matcha repaints in full on SIGWINCH, so nothing
 * in the test suite depends on the reflow policy - only on the new size
 * being in force for what is painted NEXT. */
let resizeGrid =
    (old: array(array(cell)), ~width: int, ~height: int)
    : array(array(cell)) =>
  Array.init(height, r =>
    Array.init(width, c =>
      if (r < Array.length(old) && c < Array.length(old[r])) {
        old[r][c];
      } else {
        blankCell([]);
      }
    )
  );

let resize = (t: t, ~width: int, ~height: int): unit => {
  let width = max(1, width);
  let height = max(1, height);
  t.primary = resizeGrid(t.primary, ~width, ~height);
  t.alt = resizeGrid(t.alt, ~width, ~height);
  t.width = width;
  t.height = height;
  t.row = min(t.row, height - 1);
  t.col = min(t.col, width - 1);
  t.pendingWrap = false;
};
