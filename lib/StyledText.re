/*
 * StyledText - ANSI-aware wrapping and truncation of already-rendered text
 *
 * Matcha renders styling eagerly: [Element.Styled] emits the SGR escape for
 * its style, renders its child, then emits a reset (see [Element.render] and
 * [Runtime.renderElement]). Because every style Matcha ever emits is one of a
 * small CLOSED set (bold/dim/italic/underline/inverted, a 256-color fg or bg,
 * and the plain reset - see [Element.styleToAnsi]), a baked string can be
 * PARSED back into exactly the styled spans that produced it. That is the
 * "parse-back" approach this module implements: wrapping and truncation are
 * pure string -> string transforms, built on top of parsing already-rendered
 * ANSI text into per-cell style information and re-serializing ("baking") it.
 *
 * This module has no dependency on layout - [Runtime] is the only caller that
 * knows a width; here everything is just [string -> string] (via the chunk
 * list representation) parameterized by an explicit width.
 */

/* ============================================================================
 * Chunks: one terminal cell plus the styles active when it was emitted
 * ============================================================================ */

/* An unordered-by-construction, insertion-ordered set of active styles. A
 * fresh style is appended; a repeated color style replaces the previous one
 * of the same kind (matching how [Element.Styled] nesting actually behaves:
 * the innermost/most-recent color wins). */
type styleSet = list(Element.style);

/* One terminal cell: the bytes that render together (a base codepoint plus
 * any zero-width codepoints fused onto it, exactly like [TextWidth.cell]),
 * its display width, and the styles active when it was parsed. */
type chunk = {
  bytes: string,
  width: int,
  styles: styleSet,
};

/* ============================================================================
 * parse: ANSI-styled string -> per-line chunk lists
 * ============================================================================ */

/* The 256-color codes [Element.colorToCode] can produce: 0-15 are the named
 * colors (in enum order), 16-231 are the 6x6x6 RGB cube. Matcha never emits
 * codes 232-255 (the grayscale ramp), so those are not decoded. */
let namedColors: array(Element.color) = [|
  Element.Black,
  Element.Red,
  Element.Green,
  Element.Yellow,
  Element.Blue,
  Element.Magenta,
  Element.Cyan,
  Element.White,
  Element.BrightBlack,
  Element.BrightRed,
  Element.BrightGreen,
  Element.BrightYellow,
  Element.BrightBlue,
  Element.BrightMagenta,
  Element.BrightCyan,
  Element.BrightWhite,
|];

let codeToColor = (n: int): Element.color =>
  if (n >= 0 && n < 16) {
    namedColors[n];
  } else if (n >= 16 && n <= 231) {
    let idx = n - 16;
    let r = idx / 36;
    let g = idx mod 36 / 6;
    let b = idx mod 6;
    Element.Rgb(r, g, b);
  } else {
    /* Not a code Matcha emits - fall back rather than crash. */
    Element.Black;
  };

/* Add a style to an active set, in emission order: a second color of the
 * same kind (fg or bg) replaces the first (matching nested [Styled] - the
 * later/inner escape is what is visibly in effect); any other style is only
 * added if not already present (so re-opening the same style on every chunk
 * does not pile up duplicates). */
let addStyle = (active: styleSet, style: Element.style): styleSet =>
  switch (style) {
  | Element.FgColor(_) =>
    List.filter(
      s =>
        switch (s) {
        | Element.FgColor(_) => false
        | _ => true
        },
      active,
    )
    @ [style]
  | Element.BgColor(_) =>
    List.filter(
      s =>
        switch (s) {
        | Element.BgColor(_) => false
        | _ => true
        },
      active,
    )
    @ [style]
  | _ => List.mem(style, active) ? active : active @ [style]
  };

/* Scan the CSI escape sequence starting at the ESC byte [s.[i]].
 * Returns (params, finalByte, nextIndex). [params] is [Some(ints)] only for
 * a well-formed "ESC[<params>m" (SGR) sequence - the only kind Matcha emits.
 * Anything else (not a CSI, or a CSI with a different final byte) yields
 * [None] and is dropped by the caller, per the "unknown escapes: skip to the
 * final letter" rule - this scan still advances [nextIndex] past it so
 * scanning always makes progress. */
let scanEscape = (s: string, i: int): (option(list(int)), char, int) => {
  let len = String.length(s);
  if (i + 1 >= len || s.[i + 1] != '[') {
    /* Lone ESC or a non-CSI escape: nothing sensible to skip to, so just
     * consume the ESC byte itself. */
    (None, ' ', i + 1);
  } else {
    let j = ref(i + 2);
    while (j^ < len
           && {
                let c = Char.code(s.[j^]);
                c >= 0x20 && c <= 0x3F;
              }) {
      j := j^ + 1;
    };
    if (j^ >= len) {
      /* Truncated sequence - consume what was scanned and stop. */
      (None, ' ', j^);
    } else {
      let final = s.[j^];
      let next = j^ + 1;
      if (final == 'm') {
        let paramStr = String.sub(s, i + 2, j^ - (i + 2));
        let parts = String.split_on_char(';', paramStr);
        let ints = List.map(p => p == "" ? 0 : int_of_string(p), parts);
        (Some(ints), final, next);
      } else {
        (None, final, next);
      };
    };
  };
};

/* Parse an ANSI-styled string (as [Element.render]/[Runtime.renderElement]
 * emit it) into a list of lines, each a list of [chunk]s.
 *
 * - '\n' ends a line; the active style set survives across lines (a
 *   continuation line's chunks still carry whatever style was open).
 * - Known SGR codes (the ones [Element.styleToAnsi] emits, plus the plain
 *   reset) update the active style set; every other escape sequence is
 *   skipped to its final byte and dropped, contributing no chunk.
 * - A zero-width codepoint (e.g. a combining mark) fuses its bytes onto the
 *   previous chunk on the line rather than starting a new cell, mirroring
 *   [TextWidth.toCells]. A leading zero-width codepoint (no previous chunk on
 *   the line) becomes its own width-0 chunk.
 */
let parse = (s: string): list(list(chunk)) => {
  let len = String.length(s);
  let lines: ref(list(list(chunk))) = ref([]);
  let curLine: ref(list(chunk)) = ref([]); /* reverse order */
  let active: ref(styleSet) = ref([]);

  let pushChar = (bytes: string, width: int) =>
    if (width == 0) {
      switch (curLine^) {
      | [] => curLine := [{bytes, width: 0, styles: active^}]
      | [prev, ...rest] =>
        curLine := [{...prev, bytes: prev.bytes ++ bytes}, ...rest]
      };
    } else {
      curLine := [{bytes, width, styles: active^}, ...curLine^];
    };

  let endLine = () => {
    lines := [List.rev(curLine^), ...lines^];
    curLine := [];
  };

  let applyEscape = (params: option(list(int)), final: char) =>
    switch (params, final) {
    | (Some([0]), 'm') => active := []
    | (Some([1]), 'm') => active := addStyle(active^, Element.Bold)
    | (Some([2]), 'm') => active := addStyle(active^, Element.Dim)
    | (Some([3]), 'm') => active := addStyle(active^, Element.Italic)
    | (Some([4]), 'm') => active := addStyle(active^, Element.Underline)
    | (Some([7]), 'm') => active := addStyle(active^, Element.Inverted)
    | (Some([38, 5, n]), 'm') =>
      active := addStyle(active^, Element.FgColor(codeToColor(n)))
    | (Some([48, 5, n]), 'm') =>
      active := addStyle(active^, Element.BgColor(codeToColor(n)))
    | _ => () /* unknown SGR (or non-SGR CSI) - drop */
    };

  let rec loop = i =>
    if (i >= len) {
      ();
    } else if (s.[i] == '\n') {
      endLine();
      loop(i + 1);
    } else if (Char.code(s.[i]) == 27) {
      let (params, final, next) = scanEscape(s, i);
      applyEscape(params, final);
      loop(next);
    } else {
      let (cp, consumed) = TextWidth.decodeUtf8(s, i);
      let bytes = String.sub(s, i, min(consumed, len - i));
      let w = TextWidth.charWidth(cp);
      pushChar(bytes, w);
      loop(i + consumed);
    };
  loop(0);
  endLine();
  List.rev(lines^);
};

/* ============================================================================
 * bake: chunk lines -> ANSI-styled string
 * ============================================================================ */

/* Re-serialize parsed lines back into an ANSI-styled string. Per line, the
 * active style set is re-opened whenever a chunk's styles differ from what
 * is currently emitted (reset first if anything was emitted), and the line
 * ends with a reset if it ended with any style open. Lines are joined with
 * '\n'. This is what lets a wrapped continuation line re-open a style that
 * was active when the wrap point fell in the middle of styled text. */
let bake = (lines: list(list(chunk))): string => {
  let bakeLine = (line: list(chunk)): string => {
    let buf = Buffer.create(64);
    let emitted: ref(styleSet) = ref([]);
    List.iter(
      (c: chunk) => {
        if (c.styles != emitted^) {
          if (emitted^ != []) {
            Buffer.add_string(buf, Element.resetAnsi);
          };
          List.iter(
            st => Buffer.add_string(buf, Element.styleToAnsi(st)),
            c.styles,
          );
          emitted := c.styles;
        };
        Buffer.add_string(buf, c.bytes);
      },
      line,
    );
    if (emitted^ != []) {
      Buffer.add_string(buf, Element.resetAnsi);
    };
    Buffer.contents(buf);
  };
  lines |> List.map(bakeLine) |> String.concat("\n");
};

/* ============================================================================
 * sliceLines: take a range of LINES out of an already-rendered string
 * ============================================================================ */

/* Keep the lines [from, from + count) of an ANSI-styled, possibly multi-line
 * string, and return them as a string again. Both the input and the output
 * are FULL multi-line strings (lines joined with '\n') - this is a
 * string -> string transform, like [wrapString].
 *
 * The point of routing this through parse/bake rather than
 * [String.split_on_char('\n')] is the style that was OPEN when the slice
 * begins. Matcha renders styling eagerly, so a styled block spanning several
 * lines emits its SGR escape once, on the first line; cutting the string at
 * a later line with plain string surgery would drop that escape and the
 * surviving lines would render unstyled. [parse] tracks the active style set
 * ACROSS newlines and [bake] re-opens it at the start of every line it
 * writes, so a slice that begins in the middle of a styled block keeps its
 * styling. (This is the same flaw VStack's height truncation still has;
 * migrating it here is a documented follow-up, not part of B5.)
 *
 * Clamping: [from] below 0 is treated as 0, [count] at or below 0 yields "".
 * A range running past the end is NOT padded - it simply yields fewer lines,
 * so the caller decides what a short slice should be filled with (Runtime's
 * Viewport pads with blank rows).
 */
let sliceLines = (lines: string, ~from: int, ~count: int): string =>
  if (count <= 0) {
    "";
  } else {
    let parsed = parse(lines);
    let total = List.length(parsed);
    let start = max(0, from);
    if (start >= total) {
      "";
    } else {
      let n = min(count, total - start);
      let kept =
        parsed
        |> List.filteri((i, _) => i >= start && i < start + n);
      bake(kept);
    };
  };

/* ============================================================================
 * wrapString: Wrap / Truncate / TruncateStart / TruncateMiddle
 * ============================================================================ */

let chunkListWidth = (cs: list(chunk)): int =>
  List.fold_left((acc, c: chunk) => acc + c.width, 0, cs);

let isSpaceChunk = (c: chunk): bool => c.bytes == " ";

/* Word/space tokenization for greedy line filling. A "word" is a maximal run
 * of non-space chunks; a "space" is a maximal run of space chunks. */
type token =
  | Word(list(chunk))
  | Space(list(chunk));

let tokenize = (line: list(chunk)): list(token) => {
  let rec go = (cs: list(chunk)): list(token) =>
    switch (cs) {
    | [] => []
    | [c, ..._] =>
      let isSp = isSpaceChunk(c);
      let rec span = (cs, acc) =>
        switch (cs) {
        | [c, ...rest] when isSpaceChunk(c) == isSp => span(rest, [c, ...acc])
        | _ => (List.rev(acc), cs)
        };
      let (grp, rest') = span(cs, []);
      let tok = isSp ? Space(grp) : Word(grp);
      [tok, ...go(rest')];
    };
  go(line);
};

/* Drop chunks classified as spaces off the end of a (forward-order) chunk
 * list - used whenever a wrapped line is flushed, so a wrap point never
 * leaves trailing whitespace on the line above it. */
let dropTrailingSpaces = (cs: list(chunk)): list(chunk) => {
  let rec stripRev = lst =>
    switch (lst) {
    | [c, ...rest] when isSpaceChunk(c) => stripRev(rest)
    | _ => lst
    };
  List.rev(stripRev(List.rev(cs)));
};

/* Pack a single unbreakable run of chunks into width-limited lines, at cell
 * granularity, never splitting a wide (width-2) cell across a line boundary
 * - if a wide cell would straddle the boundary, the line it would have
 * started on is simply left one column short. Always makes progress (each
 * line gets at least the one chunk that didn't fit on the previous line). */
let hardBreak = (chunks: list(chunk), width: int): list(list(chunk)) => {
  let lines = ref([]);
  let cur = ref([]);
  let curW = ref(0);
  List.iter(
    (c: chunk) =>
      if (curW^ + c.width > width && cur^ != []) {
        lines := lines^ @ [cur^];
        cur := [c];
        curW := c.width;
      } else {
        cur := cur^ @ [c];
        curW := curW^ + c.width;
      },
    chunks,
  );
  lines^ @ [cur^];
};

/* Greedy-fill word wrap of a single line to [width0] columns (>= 1; a
 * non-positive width is clamped to 1). An empty line wraps to itself (one
 * empty line, not zero lines) - joining zero lines back with "\n" would
 * silently swallow a genuinely blank line. */
let wrapLine = (line: list(chunk), width0: int): list(list(chunk)) => {
  let width = max(1, width0);
  if (line == []) {
    [[]];
  } else {
    let tokens = tokenize(line);
    let output = ref([]);
    let cur = ref([]);
    let curW = ref(0);

    let flush = () => {
      output := output^ @ [dropTrailingSpaces(cur^)];
      cur := [];
      curW := 0;
    };

    List.iter(
      tok =>
        switch (tok) {
        | Word(chunks) =>
          let w = chunkListWidth(chunks);
          if (curW^ + w <= width) {
            /* Fits on the current line as-is. */
            cur := cur^ @ chunks;
            curW := curW^ + w;
          } else if (w <= width) {
            /* Doesn't fit here, but fits alone on a fresh line. */
            flush();
            cur := chunks;
            curW := w;
          } else {
            /* Wider than the whole width: hard-break at cell granularity.
             * All but the last piece become complete output lines; the
             * last piece becomes the new current line, so later tokens
             * keep filling it as usual. */
            if (cur^ != []) {
              flush();
            };
            switch (List.rev(hardBreak(chunks, width))) {
            | [] => ()
            | [last, ...restRev] =>
              List.iter(p => output := output^ @ [p], List.rev(restRev));
              cur := last;
              curW := chunkListWidth(last);
            };
          };
        | Space(chunks) =>
          if (cur^ == []) {
            (); /* leading space at the start of a (continuation) line: drop */
          } else {
            let w = chunkListWidth(chunks);
            if (curW^ + w <= width) {
              cur := cur^ @ chunks;
              curW := curW^ + w;
            } else {
              /* Doesn't fit: flush what we have and drop the space itself. */
              flush();
            };
          }
        },
      tokens,
    );
    /* Final flush - but only add a line if there is content, or if nothing
     * was ever emitted (so a whitespace-only line still yields one empty
     * line rather than none). */
    if (cur^ != [] || output^ == []) {
      flush();
    };
    output^;
  };
};

let ellipsisChunk = (styles: styleSet): chunk => {
  bytes: "\xE2\x80\xA6" /* U+2026 HORIZONTAL ELLIPSIS */,
  width: 1,
  styles,
};

let lastStyles = (cs: list(chunk)): styleSet =>
  switch (List.rev(cs)) {
  | [] => []
  | [c, ..._] => c.styles
  };

let firstStyles = (cs: list(chunk)): styleSet =>
  switch (cs) {
  | [] => []
  | [c, ..._] => c.styles
  };

/* Longest prefix of [chunks] whose total width is <= [maxW] (>= 0), taken at
 * cell granularity - a chunk is either wholly included or wholly excluded,
 * so a wide cell is never split. */
let takeWidthPrefix = (chunks: list(chunk), maxW: int): list(chunk) => {
  let rec go = (cs, acc, accW) =>
    switch (cs) {
    | [] => List.rev(acc)
    | [c, ...rest] =>
      if (accW + c.width <= maxW) {
        go(rest, [c, ...acc], accW + c.width);
      } else {
        List.rev(acc);
      }
    };
  go(chunks, [], 0);
};

/* Longest suffix of [chunks] whose total width is <= [maxW], same rule. */
let takeWidthSuffix = (chunks: list(chunk), maxW: int): list(chunk) => {
  let rec go = (cs, acc, accW) =>
    switch (cs) {
    | [] => acc
    | [c, ...rest] =>
      if (accW + c.width <= maxW) {
        go(rest, [c, ...acc], accW + c.width);
      } else {
        acc;
      }
    };
  go(List.rev(chunks), [], 0);
};

/* Truncate = unchanged when the line already fits in w columns; otherwise
 * the longest prefix <= w-1, plus an ellipsis styled like the last kept
 * cell (or unstyled if nothing was kept, i.e. w <= 1). w <= 0 -> "". */
let truncateLine = (line: list(chunk), w: int): list(chunk) =>
  if (w <= 0) {
    [];
  } else if (chunkListWidth(line) <= w) {
    line;
  } else {
    let kept = takeWidthPrefix(line, w - 1);
    kept @ [ellipsisChunk(lastStyles(kept))];
  };

/* TruncateStart = unchanged when the line fits; otherwise an ellipsis
 * (styled like the first kept cell of the suffix, or unstyled) followed by
 * the longest suffix <= w-1. w <= 0 -> "". */
let truncateStartLine = (line: list(chunk), w: int): list(chunk) =>
  if (w <= 0) {
    [];
  } else if (chunkListWidth(line) <= w) {
    line;
  } else {
    let kept = takeWidthSuffix(line, w - 1);
    [ellipsisChunk(firstStyles(kept)), ...kept];
  };

/* TruncateMiddle: headW = ceil((w-1)/2) (== w/2 by integer division: this
 * identity holds for every integer w), tailW = (w-1) - headW. head and tail
 * are the longest prefix/suffix within those budgets; ellipsis styled like
 * the last head cell. At w<=1, headW=tailW=0, so head and tail are both
 * empty and the result is the ellipsis alone - no separate case needed. */
let truncateMiddleLine = (line: list(chunk), w: int): list(chunk) =>
  if (w <= 0) {
    [];
  } else if (chunkListWidth(line) <= w) {
    line;
  } else {
    let headW = w / 2;
    let tailW = w - 1 - headW;
    let head = takeWidthPrefix(line, headW);
    let tail = takeWidthSuffix(line, tailW);
    head @ [ellipsisChunk(lastStyles(head))] @ tail;
  };

/* Wrap or truncate an already-rendered (possibly ANSI-styled) string to
 * [width] columns, per [mode]. Each line of [s] (split on '\n') is handled
 * independently and the results joined with '\n'. This is the function
 * [Runtime.renderElement]'s [WrappedText] case calls once a layout width is
 * known. */
let wrapString = (~mode: Element.wrap, ~width: int, s: string): string => {
  let lines = parse(s);
  let resultLines =
    switch (mode) {
    | Element.Wrap => List.concat(List.map(line => wrapLine(line, width), lines))
    | Element.Truncate => List.map(line => truncateLine(line, width), lines)
    | Element.TruncateStart =>
      List.map(line => truncateStartLine(line, width), lines)
    | Element.TruncateMiddle =>
      List.map(line => truncateMiddleLine(line, width), lines)
    };
  bake(resultLines);
};
