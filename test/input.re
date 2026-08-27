/*
 * Input - helpers for simulating end-to-end keyboard input against a
 * headless Matcha app.
 *
 * feedBytes() drives a FRESH InputDecoder (lib/InputDecoder.re) over the
 * given string, exactly like Runtime's loops do with a real read: feed
 * the whole string in one `InputDecoder.feed` call, then `flush` it (so a
 * trailing lone ESC or partial CSI - which a real loop would only resolve
 * after the 25ms deadline - still resolves for a test that doesn't want
 * to wait on a timer). Every KeyEvent is delivered via handle.sendKey,
 * every PasteEvent via handle.sendPaste, and every MouseEvent via
 * handle.sendMouse (B4 - a headless frame is the live
 * region, so the decoder's coordinates need no screen-row mapping).
 * CursorReport is ignored: it feeds Runtime's own cursor tracking in the
 * interactive loop and has no headless equivalent.
 *
 * This intentionally feeds the WHOLE string as a single chunk, unlike a
 * real terminal's possibly-many small reads: InputDecoder's job is to
 * produce the same events regardless of how the bytes were split, and
 * paste_tests.re is what exercises the split-at-every-byte-boundary case
 * directly against the decoder. feedBytes here only needs to reproduce
 * what applications observe end-to-end.
 */
open Matcha;

/* Feed a raw string of bytes into a headless app, running it through a
 * fresh InputDecoder and delivering every resulting event via the
 * matching handle method. */
let feedBytes = (handle: Runtime.headlessHandle, s: string): unit => {
  let decoder = InputDecoder.create();
  let deliver = (event: InputDecoder.event) =>
    switch (event) {
    | InputDecoder.KeyEvent(key, modifiers) => handle.sendKey(key, modifiers)
    | InputDecoder.PasteEvent(text) => handle.sendPaste(text)
    | InputDecoder.MouseEvent(ev) => handle.sendMouse(ev)
    | InputDecoder.CursorReport(_, _) => () /* interactive-loop only */
    /* An OSC 11 background reply is routed by the interactive loop too; the
     * handle's own setTerminalBackground is the headless equivalent. */
    | InputDecoder.OscReport(_, _) => ()
    };
  let bytes = Bytes.of_string(s);
  List.iter(deliver, InputDecoder.feed(decoder, bytes, Bytes.length(bytes)));
  List.iter(deliver, InputDecoder.flush(decoder));
};

/* Feed a bracketed-paste payload into a headless app via handle.sendPaste
 * directly - the headless-handle-level equivalent of a real paste, for
 * tests that don't need byte-level decoder simulation. */
let feedPaste = (handle: Runtime.headlessHandle, text: string): unit =>
  handle.sendPaste(text);

/* Feed a pre-parsed list of (key, modifiers) events directly - a
 * convenience for tests that don't need byte-level simulation. */
let feedKeys =
    (handle: Runtime.headlessHandle, keys: list((Key.t, Key.modifiers)))
    : unit =>
  List.iter(((key, modifiers)) => handle.sendKey(key, modifiers), keys);

/* Press Tab, no modifiers - a focus-cycle keypress (B1). */
let pressTab = (handle: Runtime.headlessHandle): unit =>
  handle.sendKey(Key.Tab, Key.noModifiers);

/* Click the left mouse button at (x, y) in LIVE-REGION coordinates
 * (0-based, (0, 0) = top-left of the frame) - the B4 equivalent of
 * pressTab: the one gesture most mouse tests need, without spelling out a
 * whole Mouse.event. Fires a button-DOWN event, which is what <Clickable>
 * reacts to. */
let clickAt = (handle: Runtime.headlessHandle, ~x: int, ~y: int): unit =>
  handle.sendMouse({
    Mouse.kind: Mouse.Down,
    button: Mouse.Left,
    x,
    y,
    shift: false,
    alt: false,
    ctrl: false,
  });

/* Press Shift+Tab (backtab) - cycles focus in the opposite direction (B1). */
let pressShiftTab = (handle: Runtime.headlessHandle): unit =>
  handle.sendKey(Key.Tab, {...Key.noModifiers, shift: true});
