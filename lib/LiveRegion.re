/*
 * LiveRegion - inline (non-alternate-screen) frame patching with RELATIVE
 * cursor addressing.
 *
 * WHY THIS EXISTS (and why it is not FrameDiff)
 * --------------------------------------------
 * FrameDiff paints a frame with ABSOLUTE addressing (ESC[<row>;1H) after an
 * initial ESC[2J clear: it owns the whole screen. That model cannot support
 * `<Static>`, whose whole point is that committed output scrolls up into the
 * terminal's scrollback and STAYS there, above a small live region that keeps
 * being repainted. Absolute rows break the moment the terminal scrolls - row 5
 * is a different line after a scroll than it was before it.
 *
 * LiveRegion therefore addresses the screen RELATIVE to where the cursor
 * already is. Matcha's interactive loop starts inline: no clear, no alternate
 * screen. Output flows at the cursor, exactly like an ordinary command's
 * output does, and everything the app commits stays in the transcript.
 *
 * THE LIVE REGION AND ITS CURSOR INVARIANT
 * ----------------------------------------
 * The "live region" is the block of lines the current frame occupies. It is
 * NEVER zero lines high: Element.splitLines("") is [""], so an empty frame is
 * the one-line array [|""|] (see `normalize`).
 *
 * INVARIANT: before and after every non-empty patch, the cursor sits at
 * COLUMN 1 of the LAST line of the live region. Every sequence below is
 * written assuming that position and restores it before returning. The
 * interactive loop must not move the cursor between patches.
 *
 * MOVEMENT PRIMITIVES
 * -------------------
 * - Up:   "\r" ++ ESC[{n}A   (carriage return first, so the column is known)
 * - Down: "\r\n" repeated    - a LINE FEED, deliberately NOT ESC[B.
 *   At the bottom margin ESC[B does nothing, while LF SCROLLS the screen.
 *   That scroll is the mechanism by which committed static content is pushed
 *   into scrollback when the live region grows at the bottom of the screen.
 *   ONLCR/opost is still on for output in Matcha's raw mode, so a "\r\n"
 *   after a bare "\r" is harmless (the extra CR is a no-op).
 * - Erase below cursor: ESC[0J
 * - Erase to end of line:  ESC[0m ESC[K  (the reset first, so that erased
 *   cells cannot inherit a background color - same reasoning as FrameDiff).
 *   Emitted BEFORE the line's content, at column 1 - never after it. After
 *   painting a line that spans the terminal's full width the cursor is in
 *   the PENDING-WRAP state, logically still on the last column, and an EL
 *   there erases the cell just written: every full-width row would lose its
 *   last character (a box border, a scrollbar). Clearing first, from column
 *   1, erases the whole row's stale tail and touches nothing painted.
 *
 * PURITY
 * ------
 * Like FrameDiff, this module is pure: no I/O, no terminal reads. Its output
 * is fully determined by its arguments, so it is unit-testable byte for byte
 * (test/liveregion_tests.re).
 */

/* Synchronized-update guards. Terminals that support DEC mode 2026 apply the
 * enclosed writes atomically (no partial-frame flicker); the rest ignore
 * both sequences. Only ever wrapped around a NON-EMPTY patch. */
let syncStart = "\027[?2026h";
let syncEnd = "\027[?2026l";

/* Reset attributes, then clear to end of line. Emitted at column 1 BEFORE
 * every painted line, static or live - see MOVEMENT PRIMITIVES for why it
 * must never follow the content. */
let clearLinePrefix = "\027[0m\027[K";

/* Reset attributes after a painted line, so a line that ends mid-style
 * cannot bleed into the movement that follows. */
let resetSuffix = "\027[0m";

/* Erase from the cursor to the end of the screen. */
let eraseBelow = "\027[0J";

/* Move the cursor up n lines (nothing at all for n <= 0). */
let cursorUp = (n: int): string =>
  if (n > 0) {
    "\027[" ++ string_of_int(n) ++ "A";
  } else {
    "";
  };

/* Move down one line, at column 1. See MOVEMENT PRIMITIVES above for why
 * this is a line feed rather than ESC[B. */
let lineDown = "\r\n";

/* Canonical form of a frame, applied at the top of `patch` (rules 0 and 1):
 *
 * - Rule 0: a frame is never zero lines. An empty render is [|""|], one blank
 *   row. A caller that somehow produces [||] gets the same treatment, so
 *   `prevLen >= 1` holds from the first paint onward and the "move up
 *   prevLen - 1 lines" arithmetic below is always well defined.
 * - Rule 1: a frame TALLER than the screen is TRUNCATED to its first
 *   termHeight lines. The live region has to fit on screen for relative
 *   addressing to work - if it did not, moving "up prevLen - 1" would walk
 *   off the top of the screen and land on the wrong line. Content below the
 *   cut is simply not shown (an app that needs more should scroll it itself;
 *   see ScrollView, B5).
 *
 * termHeight <= 0 (an unreadable terminal size) is treated as 1.
 */
let normalize = (~next: array(string), ~termHeight: int): array(string) => {
  let maxLines = max(1, termHeight);
  let len = Array.length(next);
  if (len == 0) {
    [|""|];
  } else if (len > maxLines) {
    Array.sub(next, 0, maxLines);
  } else {
    next;
  };
};

/* Erase a live region of `prevHeight` lines, leaving the cursor at column 1
 * of where its FIRST line was.
 *
 * Used on SIGWINCH (the old region's line breakdown is meaningless once the
 * width changed, so it is erased and fully repainted) and by
 * quit(ClearScreen), which erases the live region ONLY - everything already
 * committed above it stays in the scrollback, which is the entire point of
 * inline rendering.
 *
 * Returns "" when there is nothing painted yet (prevHeight <= 0), so quitting
 * before the first frame emits no escape codes at all.
 */
let erase = (~prevHeight: int, ~termHeight: int): string => {
  let h = min(prevHeight, max(1, termHeight));
  if (h <= 0) {
    "";
  } else {
    "\r" ++ cursorUp(h - 1) ++ eraseBelow;
  };
};

/* Paint static lines followed by a full live frame, starting from the cursor
 * (which must already be at column 1 of the first line to write).
 *
 * Static lines each end with a line feed, so they scroll up out of the live
 * region and become part of the transcript. Live lines end with a line feed
 * EXCEPT the last one, which ends with a bare "\r" - that is what leaves the
 * cursor at column 1 of the last live line (the invariant).
 */
let paintAll =
    (buf: Buffer.t, staticLines: list(string), next: array(string)): unit => {
  List.iter(
    line => {
      Buffer.add_string(buf, clearLinePrefix);
      Buffer.add_string(buf, line);
      Buffer.add_string(buf, resetSuffix);
      Buffer.add_string(buf, lineDown);
    },
    staticLines,
  );
  let nextLen = Array.length(next);
  for (i in 0 to nextLen - 1) {
    Buffer.add_string(buf, clearLinePrefix);
    Buffer.add_string(buf, next[i]);
    Buffer.add_string(buf, resetSuffix);
    if (i < nextLen - 1) {
      Buffer.add_string(buf, lineDown);
    } else {
      Buffer.add_string(buf, "\r");
    };
  };
};

/* Compute the escape sequence that turns the currently painted live region
 * (`prev`) into `next`, first committing `staticLines` above it.
 *
 * ~prev: the previous frame, ALREADY normalized (the caller stores what it
 *        painted; use `normalize` if in doubt). None means "nothing painted
 *        yet" - the first frame, or a forced repaint after a resize.
 * ~staticLines: lines to commit above the live region this frame, in order.
 *        They are printed once and never touched again.
 * ~next: this frame. Normalized here (rules 0 and 1).
 * ~termHeight: the terminal's height in rows, used for the clamp.
 *
 * The rules, in the order they are applied:
 *
 * 0. Normalize `next` ([||] -> [|""|]).
 * 1. Clamp `next` to termHeight lines.
 * 2. Nothing to commit and nothing changed -> "" (the caller skips the write
 *    entirely; even zero-effect escape codes are pointless I/O).
 * 3. Any non-empty result is wrapped in the synchronized-update guards.
 * 4. prev = None: "\r" to normalize the column, then paint statics and the
 *    whole frame (paintAll).
 * 5. prev = Some(p) WITH static lines: the static content has to be inserted
 *    ABOVE the live region, which means the region moves - so the region is
 *    rebuilt: go up to its first line, erase everything below, then paint
 *    statics + the whole frame as in rule 4.
 * 6. prev = Some(p) with NO static lines: an in-place line diff. Go up to the
 *    first line of the region, then walk DOWN through it with line feeds,
 *    repainting only rows that differ (or rows past the end of the old
 *    frame). If the new frame is SHORTER, erase from its new last row down
 *    and step back up onto it. Finally walk down to the last live line and
 *    return to column 1.
 *
 * Reserved for B5/Part B: an optional ~cursorAt: option((int, int)) argument
 * for placing the REAL terminal cursor at a focused caret inside the region
 * (the invariant position would become the caret's position instead).
 */
let patch =
    (
      ~prev: option(array(string)),
      ~staticLines: list(string),
      ~next: array(string),
      ~termHeight: int,
    )
    : string => {
  /* Rules 0 and 1 */
  let next = normalize(~next, ~termHeight);
  let nextLen = Array.length(next);
  let hasStatic = staticLines != [];

  let body =
    switch (prev) {
    /* Rule 2 */
    | Some(p) when !hasStatic && p == next => ""

    /* Rule 4 */
    | None =>
      let buf = Buffer.create(256);
      Buffer.add_string(buf, "\r");
      paintAll(buf, staticLines, next);
      Buffer.contents(buf);

    /* Rule 5 */
    | Some(p) when hasStatic =>
      let buf = Buffer.create(256);
      let prevLen = Array.length(p);
      Buffer.add_string(buf, "\r");
      Buffer.add_string(buf, cursorUp(prevLen - 1));
      Buffer.add_string(buf, eraseBelow);
      paintAll(buf, staticLines, next);
      Buffer.contents(buf);

    /* Rule 6 */
    | Some(p) =>
      let prevLen = Array.length(p);
      let buf = Buffer.create(256);
      Buffer.add_string(buf, "\r");
      Buffer.add_string(buf, cursorUp(prevLen - 1));
      /* Row the cursor currently sits on, 0-based within the region. */
      let cur = ref(0);
      let touched = ref(false);
      let moveTo = (row: int) =>
        for (_ in 1 to row - cur^) {
          Buffer.add_string(buf, lineDown);
        };
      for (i in 0 to nextLen - 1) {
        if (i >= prevLen || p[i] != next[i]) {
          moveTo(i);
          Buffer.add_string(buf, clearLinePrefix);
          Buffer.add_string(buf, next[i]);
          Buffer.add_string(buf, resetSuffix);
          cur := i;
          touched := true;
        };
      };
      if (nextLen < prevLen) {
        /* Step one row PAST the new last line, erase everything from there
         * down (that is the stale tail of the old, taller region), then step
         * back up onto the new last line. */
        moveTo(nextLen);
        Buffer.add_string(buf, eraseBelow);
        Buffer.add_string(buf, cursorUp(1));
        cur := nextLen - 1;
        touched := true;
      };
      /* Restore the invariant: column 1 of the last live line. */
      moveTo(nextLen - 1);
      Buffer.add_string(buf, "\r");
      if (touched^) {
        Buffer.contents(buf);
      } else {
        /* Unreachable in practice - rule 2 already caught "nothing changed" -
         * but a patch that only moves the cursor is not worth writing. */
        "";
      };
    };

  /* Rule 3 */
  if (body == "") {
    "";
  } else {
    syncStart ++ body ++ syncEnd;
  };
};
