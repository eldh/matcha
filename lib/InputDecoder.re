/*
 * InputDecoder - stateful byte-stream assembler for terminal input.
 *
 * Replaces the old `Unix.read(8) -> Key.parse` pipeline. A real terminal
 * hands input to a program in arbitrarily-sized chunks - a bracketed paste
 * can span many reads, a single UTF-8 codepoint can be split across two,
 * and even a plain multi-character read (several keys typed faster than
 * the event loop drains stdin) used to come back as one undifferentiated
 * `Unknown` under the old 8-byte-buffer-into-Key.parse scheme. This module
 * owns reassembly: `feed` is called with every chunk `Terminal.readBytes`
 * returns, and produces zero or more fully-formed `event`s from it, one
 * KeyEvent per plain byte, exactly one event per escape sequence, one
 * PasteEvent per complete bracketed paste, and one MouseEvent per SGR
 * mouse report - however those bytes happened to be split across reads.
 *
 * States (see `state` below):
 * - Ground: no sequence in progress. Also holds an auxiliary pending-UTF-8
 *   buffer (see `utf8Buf`) for a plain (non-Alt) multi-byte codepoint that
 *   arrived split across feeds.
 * - Esc(buf): bytes accumulated since an ESC byte, not yet resolved into
 *   an event (a CSI/SS3 sequence in progress, a lone ESC waiting to see if
 *   more follows, or an Alt+UTF-8 codepoint being collected).
 * - Paste(buf, matched): inside a bracketed-paste body. `matched` is how
 *   many bytes of the "\027[201~" terminator have been matched so far
 *   against the incoming stream (0..5 - a full 6-byte match ends the
 *   paste immediately, so 6 is never stored).
 *
 * Every terminal escape sequence Key.re already knows how to parse is
 * routed back through `Key.parse` once this module has found where it
 * ends - the decoder's job is exclusively finding sequence boundaries
 * across read() calls, not re-implementing per-key semantics.
 */

/* Events produced by `feed`/`flush`. CursorReport is the decoder's own
 * framing of a DSR cursor-position response (`ESC[row;colR`, exactly two
 * params) - Runtime consumes it for mouse coordinate bookkeeping (B4/A4)
 * and it is never dispatched to application key handlers. */
type event =
  | KeyEvent(Key.t, Key.modifiers)
  | PasteEvent(string)
  | MouseEvent(Mouse.event)
  | CursorReport(int, int);

/* Internal decoder state - see the module doc comment above. */
type state =
  | Ground
  | Esc(Buffer.t)
  | Paste(Buffer.t, int);

type t = {
  mutable state,
  /* Ground-mode pending multi-byte UTF-8 sequence, collected across feeds.
   * Only meaningful (non-empty) while state == Ground; a transition to
   * Esc or Paste can only happen when it's empty (an ESC byte or a
   * bracketed-paste terminator are both single ASCII bytes, never part of
   * a UTF-8 continuation). */
  utf8Buf: Buffer.t,
  maxPasteBytes: int,
};

/* Create a fresh decoder. maxPasteBytes caps how many BODY bytes of a
 * single paste are retained (before CRLF normalization) - once the cap is
 * reached, the decoder keeps scanning for the terminator (so the paste
 * still ends correctly and subsequent input is not corrupted) but stops
 * appending, silently truncating the delivered PasteEvent. */
let create = (~maxPasteBytes: int=1_000_000, ()): t => {
  state: Ground,
  utf8Buf: Buffer.create(8),
  maxPasteBytes,
};

/* Whether the decoder is mid-escape-sequence (Esc state) - Runtime uses
 * this to shorten the select() timeout to 25ms so a lone ESC (which
 * otherwise waits indefinitely to see if it's the start of a longer
 * sequence) still surfaces as a keypress at human-perceptible latency. */
let pendingEsc = (t: t): bool =>
  switch (t.state) {
  | Esc(_) => true
  | Ground
  | Paste(_, _) => false
  };

/* CRLF normalization, applied ONCE to the fully-assembled paste body (not
 * per byte / per feed) so a "\r\n" split across two feed() calls is never
 * mistaken for two separate lone-CR line endings: \r\n -> \n first, then
 * any remaining lone \r -> \n. */
let normalizePasteBody = (s: string): string => {
  let n = String.length(s);
  let buf = Buffer.create(n);
  let i = ref(0);
  while (i^ < n) {
    if (s.[i^] == '\r' && i^ + 1 < n && s.[i^ + 1] == '\n') {
      Buffer.add_char(buf, '\n');
      i := i^ + 2;
    } else if (s.[i^] == '\r') {
      Buffer.add_char(buf, '\n');
      i := i^ + 1;
    } else {
      Buffer.add_char(buf, s.[i^]);
      i := i^ + 1;
    };
  };
  Buffer.contents(buf);
};

/* Key.parse returns a (Key.t, Key.modifiers) tuple; KeyEvent takes the two
 * as separate constructor arguments. Every call site below just wants to
 * parse a fully-assembled byte buffer and emit the result, so funnel that
 * through one helper rather than destructuring at each of the dozen call
 * sites. */
let emitParsed = (emit: event => unit, bs: bytes): unit => {
  let (key, mods) = Key.parse(bs, Bytes.length(bs));
  emit(KeyEvent(key, mods));
};

/* Parse a complete CSI sequence's body (everything after `ESC[`,
 * INCLUDING the final byte) into a MouseEvent, CursorReport, or KeyEvent -
 * whichever it turns out to be - or start a bracketed paste. `body` never
 * includes the leading `ESC[`. */
let completeCsi = (t: t, body: string, emit: event => unit): unit => {
  let blen = String.length(body);
  if (body == "200~") {
    /* Bracketed-paste start: \027[200~. Nothing is emitted; the body that
     * follows is scanned for the \027[201~ terminator by the Paste state. */
    t.state = Paste(Buffer.create(64), 0);
  } else if (blen >= 2
             && body.[0] == '<'
             && (body.[blen - 1] == 'M' || body.[blen - 1] == 'm')) {
    /* SGR mouse report: ESC[< Cb;Cx;Cy M/m. Mouse.parseSgr takes the part
     * after '<', including the final M/m. */
    let sgrBody = String.sub(body, 1, blen - 1);
    switch (Mouse.parseSgr(sgrBody)) {
    | Some(ev) => emit(MouseEvent(ev))
    | None => () /* malformed SGR report - dropped, per spec */
    };
    t.state = Ground;
  } else if (blen >= 1 && body.[blen - 1] == 'R') {
    /* Possible DSR cursor-position report: ESC[row;colR. Only a report
     * with exactly two params is claimed as CursorReport; anything else
     * ending in 'R' (there is no such normal key) falls through to
     * Key.parse like any other CSI, which will call it Unknown. */
    let paramStr = String.sub(body, 0, blen - 1);
    let params =
      if (paramStr == "") {
        [];
      } else {
        String.split_on_char(';', paramStr)
        |> List.map(seg =>
             switch (int_of_string_opt(seg)) {
             | Some(n) => n
             | None => 0
             }
           );
      };
    switch (params) {
    | [row, col] =>
      emit(CursorReport(row, col));
      t.state = Ground;
    | _ =>
      let bs = Bytes.of_string("\027[" ++ body);
      emitParsed(emit, bs);
      t.state = Ground;
    };
  } else {
    let bs = Bytes.of_string("\027[" ++ body);
    emitParsed(emit, bs);
    t.state = Ground;
  };
};

/* Force-flush whatever is held in an Esc buffer through Key.parse and
 * return to Ground - used both for the 64-byte cap and by `flush` below. */
let flushEscBuffer = (t: t, buf: Buffer.t, emit: event => unit): unit => {
  let bs = Bytes.of_string("\027" ++ Buffer.contents(buf));
  emitParsed(emit, bs);
  t.state = Ground;
};

/* Handle one byte while state == Esc(buf). Mutually recursive with
 * handleGround, handlePasteByte and processByte below: the malformed-
 * sequence branches in handleEsc/handleGround need to reprocess a byte
 * they didn't consume through the top-level dispatcher. */
let rec handleEsc = (t: t, buf: Buffer.t, emit: event => unit, byte: char): unit => {
  let code = Char.code(byte);
  if (Buffer.length(buf) == 0) {
    /* First byte after the ESC decides what kind of sequence this is. */
    if (byte == '[') {
      Buffer.add_char(buf, byte);
    } else if (byte == 'O') {
      Buffer.add_char(buf, byte);
    } else if (code == 0x1B) {
      /* A second ESC before anything else resolved: the first one was a
       * lone ESC on its own - emit it as Escape - and stay in Esc holding
       * nothing, so this new ESC gets the same treatment as the first. */
      emit(KeyEvent(Key.Escape, Key.noModifiers));
    } else if (code < 0x80) {
      /* Alt+key resolves immediately - no need to wait for more bytes. */
      let bs = Bytes.of_string("\027" ++ String.make(1, byte));
      emitParsed(emit, bs);
      t.state = Ground;
    } else if (code >= 0xC2) {
      /* Start of an Alt+UTF-8 codepoint - keep collecting. */
      Buffer.add_char(buf, byte);
    } else {
      /* Malformed: a continuation byte or invalid lead right after ESC. */
      let bs = Bytes.of_string("\027" ++ String.make(1, byte));
      emitParsed(emit, bs);
      t.state = Ground;
    };
  } else {
    let first = Buffer.nth(buf, 0);
    if (first == '[') {
      Buffer.add_char(buf, byte);
      if (code >= 0x40 && code <= 0x7E) {
        /* Final byte reached - the CSI sequence is complete. */
        let body = Buffer.sub(buf, 1, Buffer.length(buf) - 1);
        completeCsi(t, body, emit);
      };
      /* else: parameter/intermediate byte, keep collecting. */
    } else if (first == 'O') {
      /* SS3: exactly one byte follows 'O'. */
      Buffer.add_char(buf, byte);
      let bs = Bytes.of_string("\027" ++ Buffer.contents(buf));
      emitParsed(emit, bs);
      t.state = Ground;
    } else {
      /* Collecting an Alt+UTF-8 codepoint; `first` is its lead byte. */
      let expected = TextWidth.utf8ExpectedLen(first);
      if (code >= 0x80 && code <= 0xBF) {
        Buffer.add_char(buf, byte);
        if (Buffer.length(buf) >= expected) {
          let bs = Bytes.of_string("\027" ++ Buffer.contents(buf));
          emitParsed(emit, bs);
          t.state = Ground;
        };
      } else {
        /* Malformed continuation - flush what's held (a single Key.parse
         * call over ESC+partial sequence, most likely Unknown) and let
         * this byte be reprocessed fresh, since it wasn't consumed. */
        flushEscBuffer(t, buf, emit);
        processByte(t, emit, byte);
      };
    };
  };
  /* Cap: an escape sequence that never resolves within 64 bytes is
   * force-flushed rather than accumulated forever. */
  switch (t.state) {
  | Esc(buf2) when Buffer.length(buf2) > 64 => flushEscBuffer(t, buf2, emit)
  | Ground
  | Esc(_)
  | Paste(_, _) => ()
  };
}

/* Handle one byte while state == Ground. */
and handleGround = (t: t, emit: event => unit, byte: char): unit => {
  let code = Char.code(byte);
  if (Buffer.length(t.utf8Buf) > 0) {
    /* Continuing a multi-byte UTF-8 sequence collected across feeds. */
    let lead = Buffer.nth(t.utf8Buf, 0);
    let expected = TextWidth.utf8ExpectedLen(lead);
    if (code >= 0x80 && code <= 0xBF) {
      Buffer.add_char(t.utf8Buf, byte);
      if (Buffer.length(t.utf8Buf) >= expected) {
        let bs = Bytes.of_string(Buffer.contents(t.utf8Buf));
        Buffer.clear(t.utf8Buf);
        emitParsed(emit, bs);
      };
    } else {
      /* Malformed / interrupted sequence: flush what's held and
       * reprocess this byte fresh, since it wasn't consumed. */
      let bs = Bytes.of_string(Buffer.contents(t.utf8Buf));
      Buffer.clear(t.utf8Buf);
      emitParsed(emit, bs);
      processByte(t, emit, byte);
    };
  } else if (code == 0x1B) {
    t.state = Esc(Buffer.create(8));
  } else if (code < 0x80) {
    /* One KeyEvent per plain byte - this is what fixes the old
     * multi-char-per-read -> Unknown bug: a fast typist's "abc" landing
     * in one read() used to be a single unparseable chunk. */
    let bs = Bytes.make(1, byte);
    emitParsed(emit, bs);
  } else if (code >= 0xC0) {
    /* Lead byte of a multi-byte UTF-8 codepoint - collect the whole
     * thing (possibly spanning more feeds) before calling Key.parse
     * exactly once, so it always arrives as one Text event. */
    Buffer.add_char(t.utf8Buf, byte);
    let expected = TextWidth.utf8ExpectedLen(byte);
    if (Buffer.length(t.utf8Buf) >= expected) {
      let bs = Bytes.of_string(Buffer.contents(t.utf8Buf));
      Buffer.clear(t.utf8Buf);
      emitParsed(emit, bs);
    };
  } else {
    /* Stray continuation byte with no lead in progress. */
    let bs = Bytes.make(1, byte);
    emitParsed(emit, bs);
  };
}

/* Handle one byte while state == Paste(buf, matched). Scans for the
 * "\027[201~" terminator with a cross-feed prefix counter: a byte that
 * doesn't extend the current partial match flushes the bytes matched so
 * far into the paste body (they were never a terminator after all) and
 * re-checks THIS byte against position 0 of the terminator, in case it
 * starts a fresh match. */
and handlePasteByte =
    (t: t, buf: Buffer.t, matched: int, emit: event => unit, byte: char)
    : unit => {
  let terminator = "\027[201~";
  let tlen = String.length(terminator);
  let appendToBody = c =>
    if (Buffer.length(buf) < t.maxPasteBytes) {
      Buffer.add_char(buf, c);
    };
  if (byte == terminator.[matched]) {
    let newMatched = matched + 1;
    if (newMatched == tlen) {
      let body = normalizePasteBody(Buffer.contents(buf));
      emit(PasteEvent(body));
      t.state = Ground;
    } else {
      t.state = Paste(buf, newMatched);
    };
  } else {
    /* Mismatch: the `matched` bytes tentatively held back as a partial
     * terminator match were actually just body bytes - flush them
     * literally, then recheck this byte as a possible fresh match start. */
    for (i in 0 to matched - 1) {
      appendToBody(terminator.[i]);
    };
    if (matched > 0 && byte == terminator.[0]) {
      t.state = Paste(buf, 1);
    } else {
      appendToBody(byte);
      t.state = Paste(buf, 0);
    };
  };
}

/* Dispatch one byte to the handler for the decoder's current state.
 * Mutually recursive with handleEsc/handleGround (via `and` above): their
 * malformed-sequence branches call straight back into this dispatcher to
 * reprocess a byte that turned out not to belong to the sequence they
 * were collecting. */
and processByte = (t: t, emit: event => unit, byte: char): unit =>
  switch (t.state) {
  | Ground => handleGround(t, emit, byte)
  | Esc(buf) => handleEsc(t, buf, emit, byte)
  | Paste(buf, matched) => handlePasteByte(t, buf, matched, emit, byte)
  };

/* Feed one chunk of raw bytes (as returned by Terminal.readBytes) into the
 * decoder, returning every event that chunk completed, in order. A
 * sequence that straddles this call and the next (a paste, a split UTF-8
 * codepoint, a mouse report cut mid-CSI) simply produces no event yet -
 * the next feed() (or, for a lone/partial escape sequence, `flush`)
 * finishes it. */
let feed = (t: t, bytes: bytes, len: int): list(event) => {
  let events = ref([]);
  let emit = (e: event) => events := [e, ...events^];
  for (i in 0 to len - 1) {
    processByte(t, emit, Bytes.get(bytes, i));
  };
  List.rev(events^);
};

/* Resolve whatever is held pending, without waiting for more bytes:
 * - Esc(buf): a lone ESC (buf empty) or a partial/unrecognized CSI/SS3/
 *   Alt+UTF-8 sequence (buf non-empty) - both go through Key.parse over
 *   whatever was collected, same as the 64-byte cap above.
 * - Paste: no-op. A paste can only end at its terminator; there is
 *   nothing sensible to emit from a partial one, so flush leaves it
 *   exactly as it was, waiting for more input.
 * - Ground with a dangling partial UTF-8 lead: best-effort Key.parse of
 *   whatever bytes were collected (this only matters if a program calls
 *   flush() directly; Runtime's own flush trigger - the pendingEsc-driven
 *   select timeout - only ever fires for the Esc case).
 */
let flush = (t: t): list(event) => {
  let events = ref([]);
  let emit = (e: event) => events := [e, ...events^];
  switch (t.state) {
  | Esc(buf) => flushEscBuffer(t, buf, emit)
  | Paste(_, _) => ()
  | Ground =>
    if (Buffer.length(t.utf8Buf) > 0) {
      let bs = Bytes.of_string(Buffer.contents(t.utf8Buf));
      Buffer.clear(t.utf8Buf);
      emitParsed(emit, bs);
    }
  };
  List.rev(events^);
};
