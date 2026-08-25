/*
 * FrameDiff - compute the minimal terminal escape-sequence patch between
 * two rendered frames.
 *
 * THIS PAINTS FULLSCREEN MODE. Runtime.start(~screen=Fullscreen) owns the
 * whole viewport on the terminal's ALTERNATE screen, pads every frame to the
 * terminal height, and patches it through `diff` below.
 *
 * It is NOT what Inline mode uses (A4). Inline paints through
 * lib/LiveRegion.re, which addresses the screen RELATIVE to the cursor
 * instead of absolutely. The reason is `<Static>`: absolute rows (and the
 * ESC[2J clear this module's first frame emits) assume the application owns
 * the whole screen and that row N stays row N, both of which stop being true
 * as soon as committed output scrolls up into the scrollback. On the
 * alternate screen there is no scrollback and no committed output (Static
 * and useStdout raise there), so both assumptions hold and absolute
 * addressing is exactly right.
 *
 * The module is pure and has no dependencies on the rest of the runtime; it
 * is unit-tested byte for byte in test/framediff_tests.re.
 *
 * The interactive runtime used to repaint every frame with a full
 * "clear screen, then redraw everything" sequence. That causes a visible
 * blink on every re-render: the screen goes blank for one frame before the
 * new content is painted. Most lines in a typical re-render (e.g. moving a
 * cursor one row in a list) do not change at all, so clearing and repainting
 * them is wasted work and wasted visual noise.
 *
 * `diff` instead compares the previous frame to the next frame line by line
 * and emits cursor-positioning + redraw sequences only for the lines that
 * actually changed (plus a trailing clear-to-end-of-screen when the new
 * frame has fewer lines than the old one). Unchanged lines are left alone,
 * so the terminal never blanks.
 *
 * Contract:
 * - `prev = None` means "nothing has been painted yet" (first frame, or a
 *   forced full repaint e.g. after a resize): the whole screen is cleared
 *   and every line of `next` is painted.
 * - `prev = Some(p)`: for every row index `i` in `next`, if `i` is beyond
 *   the end of `p`, or `p[i]` differs from `next[i]` (plain structural
 *   string comparison - no attempt to diff within a line), that row is
 *   repainted. If `next` is shorter than `p`, everything from the new last
 *   row onward is cleared with `ESC[J` (clear from cursor to end of
 *   screen) so stale rows from the longer previous frame don't linger.
 * - If nothing changed at all, the result is the empty string "" - callers
 *   should skip writing/flushing entirely in that case, since even writing
 *   zero-effect escape codes is pointless I/O.
 * - Any non-empty result is wrapped in the "synchronized update" escape
 *   pair (ESC[?2026h ... ESC[?2026l). Terminals that support this mode
 *   buffer the enclosed writes and apply them atomically, avoiding
 *   intermediate partial-frame flicker; terminals that don't recognize the
 *   mode simply ignore the two guard sequences and render the writes
 *   as normal, so the wrapping is safe everywhere.
 *
 * Each repainted line is written as:
 *   ESC[<row>;1H  ++  <line content>  ++  ESC[0m ESC[K
 * - ESC[<row>;1H moves the cursor to the start of that row (rows are
 *   1-indexed in terminal coordinates, hence `i + 1`).
 * - ESC[0m resets text attributes *before* ESC[K. Without the reset, if the
 *   new line is shorter than what used to occupy that row, and the old
 *   content had left the terminal's "current attributes" in some state
 *   (e.g. a background color) - much less commonly, if the new line's own
 *   trailing style was carried into the erase - ESC[K would erase the
 *   remainder of the row *using whatever attributes are currently active*,
 *   which can leave a strip of stale-colored residue on a terminal that
 *   fills erased cells with the current SGR state rather than a neutral
 *   default. Resetting to default attributes immediately before erasing
 *   guarantees the erased remainder is always blank, never tinted.
 * - ESC[K clears from the cursor to the end of the line, removing any
 *   leftover characters from a longer previous line at that row.
 *
 * This module is intentionally pure (no I/O, no terminal reads) so its
 * output is fully determined by its inputs. That makes it trivial to unit
 * test byte-for-byte without a real terminal or PTY: given two frames, the
 * escape sequence emitted is always the same.
 */

let clearScreenHome = "\027[2J\027[H";
let syncStart = "\027[?2026h";
let syncEnd = "\027[?2026l";

let moveTo = (row: int): string => "\027[" ++ string_of_int(row) ++ ";1H";

/* Reset attributes, then clear to end of line - emitted at column 1 BEFORE
 * the line's content, never after it. After painting a line that spans the
 * terminal's full width the cursor is in the PENDING-WRAP state, logically
 * still on the last column, and an EL there erases the cell just written:
 * every full-width row would lose its last character (exactly how the
 * fullscreen input box lost its right border). Clearing first, right after
 * the absolute move to column 1, erases the row's stale tail and touches
 * nothing painted. */
let clearLinePrefix = "\027[0m\027[K";

/* Reset after the content, so a line ending mid-style cannot bleed into the
 * next row's move. */
let resetSuffix = "\027[0m";

let paintLine = (buf: Buffer.t, ~row: int, ~line: string): unit => {
  Buffer.add_string(buf, moveTo(row));
  Buffer.add_string(buf, clearLinePrefix);
  Buffer.add_string(buf, line);
  Buffer.add_string(buf, resetSuffix);
};

let diff = (~prev: option(array(string)), ~next: array(string)): string => {
  let buf = Buffer.create(256);
  switch (prev) {
  | None =>
    Buffer.add_string(buf, clearScreenHome);
    Array.iteri((i, line) => paintLine(buf, ~row=i + 1, ~line), next);
  | Some(p) =>
    let prevLen = Array.length(p);
    let nextLen = Array.length(next);
    for (i in 0 to nextLen - 1) {
      if (i >= prevLen || p[i] != next[i]) {
        paintLine(buf, ~row=i + 1, ~line=next[i]);
      };
    };
    if (nextLen < prevLen) {
      Buffer.add_string(buf, moveTo(nextLen + 1));
      Buffer.add_string(buf, "\027[J");
    };
  };

  if (Buffer.length(buf) == 0) {
    "";
  } else {
    syncStart ++ Buffer.contents(buf) ++ syncEnd;
  };
};
