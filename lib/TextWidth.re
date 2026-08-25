/*
 * TextWidth - UTF-8 decoding and terminal display width
 *
 * Terminal layout is measured in COLUMNS, not in bytes and not in
 * codepoints. A CJK ideograph occupies two columns, a combining accent
 * occupies none, and an ASCII letter occupies one. Every layout
 * calculation in Matcha (padding, truncation, stack sizing) must agree on
 * these numbers or boxes come out ragged.
 *
 * This module has no dependencies: it hand-rolls UTF-8 decoding and uses a
 * pragmatic wcwidth-style range table. The table is deliberately
 * conservative - box-drawing characters (U+2500..U+257F) are width 1, as
 * every terminal renders them.
 *
 * Malformed input never raises: an invalid byte decodes as U+FFFD and
 * consumes exactly one byte, so scanning always makes progress.
 */

/* ============================================================================
 * UTF-8 decoding
 * ============================================================================ */

/* Number of bytes in the UTF-8 sequence that starts with [c].
 *
 * 0x00-0x7F => 1 (ASCII)
 * 0xC2-0xDF => 2
 * 0xE0-0xEF => 3
 * 0xF0-0xF4 => 4
 * anything else (continuation bytes, overlong leads, out-of-range) => 1
 */
let utf8ExpectedLen = (c: char): int => {
  let b = Char.code(c);
  if (b < 0x80) {
    1;
  } else if (b >= 0xC2 && b <= 0xDF) {
    2;
  } else if (b >= 0xE0 && b <= 0xEF) {
    3;
  } else if (b >= 0xF0 && b <= 0xF4) {
    4;
  } else {
    1;
  };
};

/* Replacement character used for malformed input. */
let replacementChar = 0xFFFD;

/* Decode the UTF-8 sequence starting at byte offset [i] of [s].
 *
 * Returns (codepoint, bytesConsumed). Malformed or truncated sequences
 * yield (0xFFFD, 1) so that callers always advance by at least one byte.
 */
let decodeUtf8 = (s: string, i: int): (int, int) => {
  let len = String.length(s);
  if (i < 0 || i >= len) {
    (replacementChar, 1);
  } else {
    let b0 = Char.code(s.[i]);
    let expected = utf8ExpectedLen(s.[i]);
    /* Continuation byte at offset i + k, or -1 when missing/invalid. */
    let cont = k =>
      if (i + k >= len) {
        (-1);
      } else {
        let b = Char.code(s.[i + k]);
        if (b >= 0x80 && b <= 0xBF) {
          b land 0x3F;
        } else {
          (-1);
        };
      };
    switch (expected) {
    | 1 =>
      if (b0 < 0x80) {
        (b0, 1);
      } else {
        (replacementChar, 1);
      }
    | 2 =>
      let c1 = cont(1);
      if (c1 < 0) {
        (replacementChar, 1);
      } else {
        ((b0 land 0x1F) lsl 6 lor c1, 2);
      };
    | 3 =>
      let c1 = cont(1);
      let c2 = cont(2);
      if (c1 < 0 || c2 < 0) {
        (replacementChar, 1);
      } else {
        let cp = (b0 land 0x0F) lsl 12 lor c1 lsl 6 lor c2;
        /* Overlong and surrogate encodings are not valid text. */
        if (cp < 0x800 || cp >= 0xD800 && cp <= 0xDFFF) {
          (replacementChar, 1);
        } else {
          (cp, 3);
        };
      };
    | _ =>
      let c1 = cont(1);
      let c2 = cont(2);
      let c3 = cont(3);
      if (c1 < 0 || c2 < 0 || c3 < 0) {
        (replacementChar, 1);
      } else {
        let cp = (b0 land 0x07) lsl 18 lor c1 lsl 12 lor c2 lsl 6 lor c3;
        if (cp < 0x10000 || cp > 0x10FFFF) {
          (replacementChar, 1);
        } else {
          (cp, 4);
        };
      };
    };
  };
};

/* ============================================================================
 * Display width
 * ============================================================================ */

/* Codepoints that occupy no columns: control characters, combining marks,
 * variation selectors, zero-width space/joiners. Sorted, non-overlapping. */
let zeroRanges = [|
  (0x0000, 0x001F),
  (0x0300, 0x036F),
  (0x0483, 0x0489),
  (0x0591, 0x05C7),
  (0x0610, 0x061A),
  (0x064B, 0x065F),
  (0x0670, 0x0670),
  (0x06D6, 0x06DC),
  (0x0E31, 0x0E31),
  (0x0E34, 0x0E3A),
  (0x0E47, 0x0E4E),
  (0x1AB0, 0x1AFF),
  (0x1DC0, 0x1DFF),
  (0x200B, 0x200F),
  (0x20D0, 0x20FF),
  (0xFE00, 0xFE0F),
  (0xFE20, 0xFE2F),
  (0xFEFF, 0xFEFF),
|];

/* Codepoints that occupy two columns: CJK, Hangul, fullwidth forms and the
 * common emoji blocks. NOTE: box drawing (0x2500-0x257F) is deliberately
 * absent - it is width 1 and Matcha's rendering depends on that. */
let wideRanges = [|
  (0x1100, 0x115F),
  (0x2E80, 0x303E),
  (0x3041, 0x33FF),
  (0x3400, 0x4DBF),
  (0x4E00, 0x9FFF),
  (0xA000, 0xA4CF),
  (0xAC00, 0xD7A3),
  (0xF900, 0xFAFF),
  (0xFE30, 0xFE4F),
  (0xFF00, 0xFF60),
  (0xFFE0, 0xFFE6),
  (0x1F300, 0x1F64F),
  (0x1F680, 0x1F6FF),
  (0x1F900, 0x1FAFF),
  (0x20000, 0x3FFFD),
|];

/* Binary search a sorted array of inclusive (lo, hi) ranges. */
let inRanges = (ranges: array((int, int)), cp: int): bool => {
  let rec search = (lo, hi) =>
    if (lo > hi) {
      false;
    } else {
      let mid = (lo + hi) / 2;
      let (rlo, rhi) = ranges[mid];
      if (cp < rlo) {
        search(lo, mid - 1);
      } else if (cp > rhi) {
        search(mid + 1, hi);
      } else {
        true;
      };
    };
  search(0, Array.length(ranges) - 1);
};

/* Number of terminal columns a codepoint occupies: 0, 1 or 2. */
let charWidth = (cp: int): int =>
  if (inRanges(zeroRanges, cp)) {
    0;
  } else if (inRanges(wideRanges, cp)) {
    2;
  } else {
    1;
  };

/* Display width of a string in terminal columns.
 *
 * ANSI escape sequences are skipped (they occupy no columns), using the
 * same state machine as Element.stripAnsi: an ESC starts a sequence which
 * ends at the first ASCII letter.
 */
let stringWidth = (s: string): int => {
  let len = String.length(s);
  let rec loop = (i, width, inEscape) =>
    if (i >= len) {
      width;
    } else if (inEscape) {
      let c = Char.code(s.[i]);
      /* End of escape sequence when we hit a letter (A-Z or a-z) */
      let stillEscaping = !(c >= 65 && c <= 90 || c >= 97 && c <= 122);
      loop(i + 1, width, stillEscaping);
    } else if (Char.code(s.[i]) == 27) {
      /* ESC (0x1B) - start of escape sequence */
      loop(i + 1, width, true);
    } else {
      let (cp, consumed) = decodeUtf8(s, i);
      loop(i + consumed, width + charWidth(cp), false);
    };
  loop(0, 0, false);
};

/* ============================================================================
 * Cells
 * ============================================================================ */

/* One terminal cell: the bytes that render together and the columns they
 * take. Zero-width codepoints (combining marks, variation selectors) are
 * fused onto the preceding cell, so a cell can hold several codepoints. */
type cell = {
  bytes: string,
  width: int,
};

/* Split an ANSI-FREE string into cells.
 *
 * The caller must strip escape sequences first (see Element.stripAnsi);
 * escape bytes here would be treated as ordinary text.
 *
 * A leading combining mark (no base character to fuse onto) becomes its
 * own width-0 cell.
 */
let toCells = (s: string): array(cell) => {
  let len = String.length(s);
  let cells: ref(list(cell)) = ref([]);
  let rec loop = i =>
    if (i < len) {
      let (cp, consumed) = decodeUtf8(s, i);
      let bytes = String.sub(s, i, min(consumed, len - i));
      let w = charWidth(cp);
      switch (w, cells^) {
      | (0, [prev, ...rest]) =>
        cells := [{bytes: prev.bytes ++ bytes, width: prev.width}, ...rest]
      | _ => cells := [{bytes, width: w}, ...cells^]
      };
      loop(i + consumed);
    };
  loop(0);
  Array.of_list(List.rev(cells^));
};
