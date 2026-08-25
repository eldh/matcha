/*
 * Tests for InputDecoder - the stateful byte-stream assembler (B2/S6).
 *
 * Mostly pure decoder-level tests (feed/flush over explicit byte chunks,
 * no app needed), plus one headless test at the bottom confirming a
 * paste reaches an application as a single Key.Paste event.
 */
open Matcha;

let pasteStart = "\027[200~";
let pasteEnd = "\027[201~";

/* Feed a string into a FRESH decoder in one chunk, then flush it, and
 * return every event produced, in order. */
let decodeAll = (s: string): list(InputDecoder.event) => {
  let decoder = InputDecoder.create();
  let bytes = Bytes.of_string(s);
  let fed = InputDecoder.feed(decoder, bytes, Bytes.length(bytes));
  fed @ InputDecoder.flush(decoder);
};

/* Feed a string into a fresh decoder split into two pieces at byte offset
 * `at`, then flush, returning every event produced across both feeds. */
let decodeSplitAt = (s: string, at: int): list(InputDecoder.event) => {
  let decoder = InputDecoder.create(());
  let n = String.length(s);
  let first = Bytes.of_string(String.sub(s, 0, at));
  let second = Bytes.of_string(String.sub(s, at, n - at));
  let ev1 = InputDecoder.feed(decoder, first, Bytes.length(first));
  let ev2 = InputDecoder.feed(decoder, second, Bytes.length(second));
  ev1 @ ev2 @ InputDecoder.flush(decoder);
};

/* Feed a string byte-by-byte (the worst-case split) into a fresh decoder,
 * then flush, returning every event produced. */
let decodeByteByByte = (s: string): list(InputDecoder.event) => {
  let decoder = InputDecoder.create();
  let events = ref([]);
  String.iter(
    c => {
      let one = Bytes.make(1, c);
      events := events^ @ InputDecoder.feed(decoder, one, 1);
    },
    s,
  );
  events^ @ InputDecoder.flush(decoder);
};

/* -------------------------------------------------------------------- */
/* KeyRecorder - tiny headless app for the end-to-end sendPaste test.    */
/* -------------------------------------------------------------------- */
module PasteRecorder = {
  let recorded: ref(list((Key.t, Key.modifiers))) = ref([]);
  let reset = () => recorded := [];

  [@component]
  let make = () => {
    Event.useKeyDown((key, mods) => recorded := recorded^ @ [(key, mods)]);
    <Text> "recorder" </Text>;
  };
};

let run = () => {
  Test.group("InputDecoder - bracketed paste", () => {
    Test.run("one-chunk paste yields a single PasteEvent", () => {
      let events = decodeAll(pasteStart ++ "hello" ++ pasteEnd);
      Test.assertEqual(
        events,
        [InputDecoder.PasteEvent("hello")],
        "whole paste in one feed -> one PasteEvent(\"hello\")",
      );
    });

    Test.run("paste split at every byte boundary yields one PasteEvent", () => {
      let s = pasteStart ++ "hello" ++ pasteEnd;
      let n = String.length(s);
      for (at in 1 to n - 1) {
        let events = decodeSplitAt(s, at);
        Test.assertEqual(
          events,
          [InputDecoder.PasteEvent("hello")],
          "split at byte "
          ++ string_of_int(at)
          ++ " still yields one PasteEvent",
        );
      };
    });

    Test.run("paste split byte-by-byte (every feed is 1 byte) still works", () => {
      let s = pasteStart ++ "hello" ++ pasteEnd;
      Test.assertEqual(
        decodeByteByByte(s),
        [InputDecoder.PasteEvent("hello")],
        "byte-by-byte split -> one PasteEvent",
      );
    });

    Test.run("empty paste yields PasteEvent(\"\")", () => {
      let events = decodeAll(pasteStart ++ pasteEnd);
      Test.assertEqual(
        events,
        [InputDecoder.PasteEvent("")],
        "empty body -> PasteEvent(\"\")",
      );
    });

    Test.run("CRLF is normalized to LF, lone CR is normalized to LF", () => {
      let body = "line1\r\nline2\rline3\n";
      let events = decodeAll(pasteStart ++ body ++ pasteEnd);
      Test.assertEqual(
        events,
        [InputDecoder.PasteEvent("line1\nline2\nline3\n")],
        "\\r\\n and lone \\r both become \\n",
      );
    });

    Test.run("a lone ESC inside the paste body is preserved literally", () => {
      /* The ESC starts a tentative terminator match (it's terminator[0]),
       * but the very next byte ('l') isn't '[' (terminator[1]), so the
       * match is abandoned and the ESC is flushed back into the body. */
      let events = decodeAll(pasteStart ++ "he\027llo" ++ pasteEnd);
      Test.assertEqual(
        events,
        [InputDecoder.PasteEvent("he\027llo")],
        "ESC inside body survives as a literal byte",
      );
    });

    Test.run(
      "a partial terminator match that doesn't complete stays in the body",
      () => {
      /* "\027[201x" matches the first 5 bytes of the real terminator
       * ("\027[201") but diverges on the 6th ('x' vs '~') - it must be
       * flushed back into the body verbatim, not silently dropped. */
      let events =
        decodeAll(pasteStart ++ "abc\027[201xdef" ++ pasteEnd);
      Test.assertEqual(
        events,
        [InputDecoder.PasteEvent("abc\027[201xdef")],
        "near-miss terminator stays in the body as literal bytes",
      );
    });

    Test.run("truncation: body beyond maxPasteBytes is dropped, terminator still found", () => {
      let decoder = InputDecoder.create(~maxPasteBytes=8, ());
      let s = pasteStart ++ "0123456789" ++ pasteEnd;
      let bytes = Bytes.of_string(s);
      let events = InputDecoder.feed(decoder, bytes, Bytes.length(bytes));
      Test.assertEqual(
        events,
        [InputDecoder.PasteEvent("01234567")],
        "only the first 8 body bytes are kept, but the paste still ends correctly",
      );
    });
  });

  Test.group("InputDecoder - plain keys, Alt, UTF-8, flush", () => {
    Test.run("lone ESC produces nothing from feed, Escape from flush", () => {
      let decoder = InputDecoder.create();
      let bytes = Bytes.of_string("\027");
      let fed = InputDecoder.feed(decoder, bytes, 1);
      Test.assertEqual(fed, [], "lone ESC held, nothing emitted yet");
      Test.assertEqual(
        InputDecoder.flush(decoder),
        [InputDecoder.KeyEvent(Key.Escape, Key.noModifiers)],
        "flush resolves the held ESC as Escape",
      );
    });

    Test.run("a plain multi-char chunk \"abc\" is three separate KeyEvents", () => {
      Test.assertEqual(
        decodeAll("abc"),
        [
          InputDecoder.KeyEvent(Key.Char('a'), Key.noModifiers),
          InputDecoder.KeyEvent(Key.Char('b'), Key.noModifiers),
          InputDecoder.KeyEvent(Key.Char('c'), Key.noModifiers),
        ],
        "one KeyEvent per plain byte, even in a single read",
      );
    });

    Test.run("Alt+e-acute (ESC + 2-byte UTF-8) is exactly one KeyEvent", () => {
      let s = "\027\xC3\xA9";
      Test.assertEqual(
        decodeAll(s),
        [
          InputDecoder.KeyEvent(
            Key.Text("\xC3\xA9"),
            {ctrl: false, alt: true, shift: false, meta: false},
          ),
        ],
        "Alt+e-acute never splits into Escape + bytes",
      );
    });

    Test.run("Alt+e-acute split across feeds is still exactly one KeyEvent", () => {
      let s = "\027\xC3\xA9";
      /* Split right after the ESC, and again mid-codepoint. */
      Test.assertEqual(
        decodeSplitAt(s, 1),
        [
          InputDecoder.KeyEvent(
            Key.Text("\xC3\xA9"),
            {ctrl: false, alt: true, shift: false, meta: false},
          ),
        ],
        "split right after ESC -> still one Text event",
      );
      Test.assertEqual(
        decodeSplitAt(s, 2),
        [
          InputDecoder.KeyEvent(
            Key.Text("\xC3\xA9"),
            {ctrl: false, alt: true, shift: false, meta: false},
          ),
        ],
        "split mid-codepoint -> still one Text event",
      );
    });

    Test.run("a plain UTF-8 codepoint split across feeds is one KeyEvent", () => {
      let s = "\xE6\x97\xA5"; /* "日", 3 bytes */
      let decoder = InputDecoder.create();
      let b1 = Bytes.of_string(String.sub(s, 0, 1));
      let b2 = Bytes.of_string(String.sub(s, 1, 2));
      let ev1 = InputDecoder.feed(decoder, b1, 1);
      Test.assertEqual(ev1, [], "first byte alone yields nothing yet");
      let ev2 = InputDecoder.feed(decoder, b2, 2);
      Test.assertEqual(
        ev2,
        [InputDecoder.KeyEvent(Key.Text(s), Key.noModifiers)],
        "remaining bytes complete the codepoint as one Text event",
      );
    });
  });

  Test.group("InputDecoder - mouse sequence framing", () => {
    Test.run("a complete SGR mouse sequence yields one MouseEvent", () => {
      let events = decodeAll("\027[<0;10;20M");
      Test.assertEqual(
        events,
        [
          InputDecoder.MouseEvent({
            kind: Mouse.Down,
            button: Mouse.Left,
            x: 9,
            y: 19,
            shift: false,
            alt: false,
            ctrl: false,
          }),
        ],
        "ESC[<0;10;20M -> one MouseEvent(Down, Left, 9, 19)",
      );
    });

    Test.run("a mouse sequence split across feeds still yields one MouseEvent", () => {
      let s = "\027[<0;10;20M";
      let n = String.length(s);
      for (at in 1 to n - 1) {
        Test.assertEqual(
          decodeSplitAt(s, at),
          [
            InputDecoder.MouseEvent({
              kind: Mouse.Down,
              button: Mouse.Left,
              x: 9,
              y: 19,
              shift: false,
              alt: false,
              ctrl: false,
            }),
          ],
          "split at byte "
          ++ string_of_int(at)
          ++ " still yields one MouseEvent",
        );
      };
    });
  });

  Test.group("Headless paste end-to-end (handle.sendPaste)", () => {
    Test.run("sendPaste delivers multiline text as one Key.Paste event", () => {
      PasteRecorder.reset();
      let handle = Runtime.startHeadless((module PasteRecorder));
      handle.sendPaste("line1\nline2\nline3");
      Test.assertEqual(
        PasteRecorder.recorded^,
        [(Key.Paste("line1\nline2\nline3"), Key.noModifiers)],
        "one Key.Paste event carrying the whole multiline body",
      );
      handle.quit();
    });

    Test.run("sendPaste normalizes CRLF the same way the decoder does", () => {
      PasteRecorder.reset();
      let handle = Runtime.startHeadless((module PasteRecorder));
      handle.sendPaste("a\r\nb\rc");
      Test.assertEqual(
        PasteRecorder.recorded^,
        [(Key.Paste("a\nb\nc"), Key.noModifiers)],
        "sendPaste applies the same CRLF/lone-CR normalization as a real paste",
      );
      handle.quit();
    });
  });
};
