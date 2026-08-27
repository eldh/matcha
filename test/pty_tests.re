/*
 * End-to-end tests on a REAL pseudo-terminal (test/pty.re + test/pty_stubs.c).
 *
 * This is the only layer of the suite that runs an example binary the way a
 * user runs it: on a TTY, in raw mode, with the terminal answering DSR and
 * the kernel delivering SIGWINCH. Everything else in the suite talks to
 * Matcha through a pipe or an in-process handle, and neither of those has a
 * termios, an alternate screen, or a window size to change.
 *
 * What only this layer can see:
 *   - Ctrl+C reaching the APPLICATION rather than the kernel. If raw mode
 *     ever leaves ISIG on again, the child dies by SIGINT before any key
 *     handler or the terminal restore runs, and the assertions on
 *     Exited(0) below go red. On a pipe there is no ISIG to get wrong.
 *   - The full restore sequence actually being written on exit.
 *   - Alternate-screen enter/exit, and that the primary screen comes back.
 *   - A real SIGWINCH resize, and the frame repainting at the new width.
 *   - Several keystrokes arriving in ONE read() (batched input).
 *   - Every escape byte the runtime writes, interpreted by an independent
 *     terminal model (test/vterm.re) instead of by the code under test.
 *
 * Determinism rules for anything added here: never assert after a fixed
 * sleep - use Pty.drain, which polls until the child has been quiet.
 * Always go through Pty.withSession so a failing assertion cannot leak a
 * child process that is sitting in raw mode on a pty.
 */

let chatBin = "examples/chat/main.exe";
let ccBin = "examples/claude-code/main.exe";

/* Index of the first screen row containing `needle`, or -1. */
let rowIndexContaining = (vt: Vterm.t, needle: string): int => {
  let rows = Vterm.snapshot(vt);
  let found = ref(-1);
  Array.iteri(
    (i, r) =>
      if (found^ < 0 && Test.contains(r, needle)) {
        found := i;
      },
    rows,
  );
  found^;
};

/* Assert that the glyph in the LAST column of the row holding `needle` is
 * `glyph`. This is the machine-checkable form of the bug a human had to
 * catch in a screenshot: a row that spans the full terminal width leaves
 * the cursor in the pending-wrap state ON the last column, so an EL emitted
 * after the content erases the cell just painted - a box border, a
 * scrollbar. See test/vterm.re's header. */
let assertLastColumn =
    (s: Pty.session, ~find: string, ~glyph: string, msg: string): unit => {
  let vt = Pty.vterm(s);
  let (width, _) = Vterm.size(vt);
  let row = rowIndexContaining(vt, find);
  Test.assertTrue(row >= 0, msg ++ ": no row containing " ++ find);
  Test.assertEqualStr(
    Vterm.cellGlyph(vt, ~row, ~col=width - 1),
    glyph,
    msg,
  );
};

/* The writers must emit nothing outside the model's vocabulary. This
 * catches drift in BOTH directions: a runtime that starts writing a new
 * sequence, and a model that quietly loses one. */
let assertNoUnknown = (s: Pty.session, where: string): unit => {
  let seqs = Vterm.unknownSeqs(Pty.vterm(s));
  Test.assertEqualStr(
    String.concat(" ", List.map(String.escaped, seqs)),
    "",
    where ++ ": the runtime emitted sequences the terminal model does not know",
  );
};

let assertExitedCleanly = (st: Pty.exitStatus, msg: string): unit =>
  switch (st) {
  | Pty.Exited(0) => ()
  | other =>
    Test.assertTrue(
      false,
      msg
      ++ ": expected Exited(0), got "
      ++ Pty.exitStatusToString(other)
      ++ ". A Signaled(...) here means the key never reached the "
      ++ "application - raw mode let the kernel have it (the ISIG bug "
      ++ "class), and the terminal restore never ran.",
    )
  };

let run = () => {
  Test.group("PTY: chat on a real terminal (inline)", () => {
    Test.run("startup, a round trip, and a clean Ctrl+C exit", () => {
      Pty.withSession(~width=80, ~height=24, chatBin, [], s => {
        Pty.drain(~quietMs=250, ~timeoutMs=5000, s);

        /* The first frame, as a terminal would show it. */
        Test.assertContains(
          Pty.screen(s),
          "Type a message... (Enter to send)",
          "the placeholder is on screen",
        );
        Test.assertContains(Pty.screen(s), "Context", "the context panel too");
        /* The INLINE instance of the full-width-row bug class: the context
           panel's scrollbar is painted in the terminal's last column. */
        assertLastColumn(
          s,
          ~find="lib/Runtime.re",
          ~glyph="\xe2\x96\x88",
          "the scrollbar thumb survives in the LAST column",
        );
        assertNoUnknown(s, "chat startup");

        /* Two separate writes: an ordinary interactive typing rhythm. */
        Pty.send(s, "hi");
        Pty.drain(~quietMs=200, ~timeoutMs=5000, s);
        Pty.send(s, "\r");
        Pty.drain(~quietMs=250, ~timeoutMs=5000, s);
        Test.assertContains(
          Pty.visible(s),
          "> hi",
          "the committed transcript entry is on the screen or in the "
          ++ "terminal's scrollback",
        );

        /* Ctrl+C must reach the APPLICATION. */
        Pty.send(s, "\003");
        let st = Pty.waitExit(~timeoutMs=5000, s);
        assertExitedCleanly(st, "Ctrl+C quit");

        let log = Pty.byteLog(s);
        Test.assertContains(log, "\027[<u", "kitty keyboard stack popped");
        Test.assertContains(log, "\027[?2004l", "bracketed paste turned off");
        Test.assertContains(log, "\027[?1002;1006l", "mouse mode turned off");
        Test.assertContains(log, "\027[?25h", "cursor shown again");
        /* The USER'S SHELL gets its keyboard back. Asserted on the terminal
           model, not on the byte log: a pop is only a pop if it lands on
           the screen that was pushed - see the fullscreen case below and
           Vterm's kittyMain/kittyAlt comment. */
        Test.assertEqual(
          Vterm.kittyDepthMain(Pty.vterm(s)),
          0,
          "the kitty keyboard stack is empty on the MAIN screen after exit",
        );
        /* An INLINE app must never take over the alternate screen.
           NOTE: the log DOES contain "?1049l" - Terminal.restoreTerminal
           sends it unconditionally on every exit path, on purpose, so a
           crashed fullscreen app cannot strand the user on a blank
           alternate buffer. Leaving one that was never entered is a no-op.
           What must never appear is the ENTER. */
        Test.assertTrue(
          !Test.contains(log, "\027[?1049h"),
          "inline rendering never enters the alternate screen",
        );
        Test.assertFalse(
          Vterm.inAltScreen(Pty.vterm(s)),
          "and the terminal is on its primary buffer throughout",
        );
        assertNoUnknown(s, "chat full lifecycle");
      })
    });

    Test.run("a whole line typed in ONE write commits exactly once", () => {
      /* Batched bytes: "hi\r" arrives in a single read() on the child side,
         so the runtime has to decode two events out of one chunk and render
         between them. A loop that renders only once per read - or one whose
         second handler closes over the pre-first-event state - either drops
         the Enter or commits the entry twice. */
      Pty.withSession(~width=80, ~height=24, chatBin, [], s => {
        Pty.drain(~quietMs=250, ~timeoutMs=5000, s);
        Pty.send(s, "hi\r");
        Pty.drain(~quietMs=300, ~timeoutMs=5000, s);
        Test.assertEqual(
          Pty.countOccurrences(Pty.visible(s), "> hi"),
          1,
          "the batched Enter committed the entry exactly once",
        );
        assertNoUnknown(s, "chat batched input");
        Pty.send(s, "\003");
        assertExitedCleanly(Pty.waitExit(~timeoutMs=5000, s), "batched exit");
      })
    });
  });

  Test.group("PTY: claude-code on a real terminal (fullscreen)", () => {
        Test.run("the alternate screen holds a full 24-row frame", () => {
          Pty.withSession(~width=80, ~height=24, ccBin, [], s => {
            Pty.drain(~quietMs=300, ~timeoutMs=6000, s);
            let vt = Pty.vterm(s);

            Test.assertTrue(
              Vterm.inAltScreen(vt),
              "a Fullscreen app takes over the alternate screen",
            );
            Test.assertContains(
              Vterm.row(vt, 0),
              "\xe2\x95\xad",
              "the banner's top border is the very first row",
            );
            Test.assertContains(
              Vterm.row(vt, 1),
              "Welcome to Claude Code mock",
              "banner text on row 2",
            );

            /* The input box is pinned to the bottom of all 24 rows: top
               border, one prompt row, bottom border, hint. */
            Test.assertContains(
              Vterm.row(vt, 23),
              "? for shortcuts",
              "the hint row is the LAST row",
            );
            Test.assertEqualStr(
              Vterm.cellGlyph(vt, ~row=20, ~col=0),
              "\xe2\x95\xad",
              "input box top-left corner",
            );
            Test.assertEqualStr(
              Vterm.cellGlyph(vt, ~row=22, ~col=0),
              "\xe2\x95\xb0",
              "input box bottom-left corner",
            );
            /* THE EXACT BUG A HUMAN CAUGHT IN A SCREENSHOT, now machine
               checked: the box spans the full 80 columns, so its right
               border lives in the pending-wrap column. With an EL emitted
               after the content these three assertions all go red and the
               box loses its whole right edge. */
            Test.assertEqualStr(
              Vterm.cellGlyph(vt, ~row=20, ~col=79),
              "\xe2\x95\xae",
              "input box top-RIGHT corner survives in the last column",
            );
            Test.assertEqualStr(
              Vterm.cellGlyph(vt, ~row=21, ~col=79),
              "\xe2\x94\x82",
              "the prompt row's RIGHT border survives in the last column",
            );
            Test.assertEqualStr(
              Vterm.cellGlyph(vt, ~row=22, ~col=79),
              "\xe2\x95\xaf",
              "input box bottom-RIGHT corner survives in the last column",
            );
            assertNoUnknown(s, "claude-code first frame");
          })
        });

        Test.run("the background is probed with OSC 11 exactly once", () => {
          /* The one new byte sequence an interactive session emits since
             theme detection landed. It must go out ONCE, at startup, in
             BOTH screen modes (this is the Fullscreen one - the chat case
             above covers Inline through the same byte log), and the
             terminal's reply must be framed out of the input stream rather
             than delivered to the app as keystrokes. */
          Pty.withSession(~width=100, ~height=30, ccBin, [], s => {
            Pty.drain(~quietMs=300, ~timeoutMs=6000, s);
            let log = Pty.byteLog(s);
            Test.assertEqual(
              Pty.countOccurrences(log, "\027]11;?\007"),
              1,
              "exactly one background query, at startup",
            );
            /* Pty.absorb answered it with rgb:1e1e/1e1e/1e1e. If the reply
               had reached the application as keys, claude-code's input box
               would be full of them. */
            Test.assertFalse(
              Test.contains(Vterm.text(Pty.vterm(s)), "rgb:"),
              "the OSC reply never reached the application as text",
            );
            assertNoUnknown(s, "claude-code background probe");
            Pty.send(s, "\003\003");
            assertExitedCleanly(
              Pty.waitExit(~timeoutMs=6000, s),
              "quit after the background probe",
            );
          })
        });

        Test.run("a prompt streams, Esc interrupts, and the exit restores", () => {
          Pty.withSession(~width=80, ~height=24, ccBin, [], s => {
            Pty.drain(~quietMs=300, ~timeoutMs=6000, s);

            Pty.send(s, "fix a bug");
            Pty.drain(~quietMs=200, ~timeoutMs=5000, s);
            Pty.send(s, "\r");
            /* The mock streams one canned step every 350ms and spins every
               120ms while it does, so a 500ms quiet window necessarily
               spans several ticks and ends only once the stream stops.
               No fixed sleep, no fixed tick count. */
            Pty.drain(~quietMs=500, ~timeoutMs=15000, s);
            Test.assertContains(
              Pty.screen(s),
              "> fix a bug",
              "the prompt is in the transcript",
            );
            Test.assertContains(
              Pty.screen(s),
              "\xe2\x8f\xba ",
              "and the stream produced a tool line",
            );
            Test.assertContains(
              Pty.screen(s),
              "Try \"fix a bug\" or / for commands",
              "the input cleared back to its placeholder",
            );

            /* Enter and Esc in ONE write. The runtime decodes both from the
               same read and renders a frame between them, so the Esc
               handler sees the stream the Enter just started - which makes
               "Esc interrupts" deterministic instead of a race. */
            Pty.send(s, "another one\r\027");
            Pty.drain(~quietMs=500, ~timeoutMs=15000, s);
            Test.assertContains(
              Pty.screen(s),
              "Interrupted by user",
              "Esc reached the app and stopped the stream",
            );
            assertNoUnknown(s, "claude-code interaction");

            /* Double Ctrl+C quits. Both bytes in one write, again batched. */
            Pty.send(s, "\003\003");
            assertExitedCleanly(
              Pty.waitExit(~timeoutMs=6000, s),
              "double Ctrl+C quit",
            );
            let vt = Pty.vterm(s);
            Test.assertFalse(
              Vterm.inAltScreen(vt),
              "the restore left the alternate screen",
            );
            Test.assertTrue(
              !Test.contains(Vterm.text(vt), "Welcome to Claude Code mock"),
              "and the alt buffer's contents are gone - the PRIMARY screen "
              ++ "is what the user is looking at again",
            );
            Test.assertContains(
              Pty.byteLog(s),
              "\027[?1049l",
              "the exit really did write the leave-alt-screen sequence",
            );
            /* THE USER-REPORTED BUG, machine-caught. kitty-protocol
               terminals keep the keyboard stack per SCREEN BUFFER, so a
               fullscreen app that pops before leaving the alternate screen
               pops the ALT screen's stack and leaves the main screen's push
               - the one setRawMode made - standing forever. The user then
               gets CSI-u garbage from Ctrl+C in their shell. The restore
               must pop the current screen, leave the alt screen, and pop
               again. */
            Test.assertEqual(
              Vterm.kittyDepthMain(vt),
              0,
              "the kitty keyboard stack is empty on the MAIN screen after a "
              ++ "FULLSCREEN app exits - the user's shell is not left "
              ++ "speaking CSI-u",
            );
            Test.assertContains(
              Pty.byteLog(s),
              "\027[<u\027[?1049l\027[<u",
              "and the restore wrote the three in the only order that "
              ++ "works: pop the alt screen, leave it, pop the main screen",
            );
          })
        });

        Test.run("SIGWINCH: the frame repaints at the new terminal size", () => {
          /* The first machine verification of resize handling in this repo.
             Pty.resize does a TIOCSWINSZ on the master, so the kernel both
             records the new size and delivers a real SIGWINCH to the
             child's foreground process group.

             NOTE the deliberately NON-DEFAULT target size. A resize test
             run at 80x24 would agree with every fallback and default in the
             codebase and could pass while nothing was recomputed at all -
             see "coincidence defaults" in CLAUDE.md. */
          Pty.withSession(~width=80, ~height=24, ccBin, [], s => {
            Pty.drain(~quietMs=300, ~timeoutMs=6000, s);
            Test.assertEqualStr(
              Vterm.cellGlyph(Pty.vterm(s), ~row=21, ~col=79),
              "\xe2\x94\x82",
              "precondition: the box spans 80 columns",
            );

            Pty.resize(s, ~width=100, ~height=30);
            Pty.drain(~quietMs=400, ~timeoutMs=8000, s);

            let vt = Pty.vterm(s);
            Test.assertEqual(Vterm.size(vt), (100, 30), "the model resized");
            Test.assertContains(
              Vterm.row(vt, 29),
              "? for shortcuts",
              "the hint row moved to row 30 - the app repainted 30 rows tall",
            );
            Test.assertEqualStr(
              Vterm.cellGlyph(vt, ~row=26, ~col=99),
              "\xe2\x95\xae",
              "the input box's top-right corner is now in column 100",
            );
            Test.assertEqualStr(
              Vterm.cellGlyph(vt, ~row=27, ~col=99),
              "\xe2\x94\x82",
              "and its right border spans the new width, last column intact",
            );
            Test.assertEqualStr(
              Vterm.cellGlyph(vt, ~row=28, ~col=99),
              "\xe2\x95\xaf",
              "bottom-right corner too",
            );
            assertNoUnknown(s, "claude-code after resize");

            Pty.send(s, "\003\003");
            assertExitedCleanly(
              Pty.waitExit(~timeoutMs=6000, s),
              "quit after resize",
            );
          })
        });

        Test.run("no session emits a sequence the terminal model rejects", () => {
          /* A whole-lifecycle audit, including the bytes written AFTER the
             last frame (the restore set), which no other case reaches
             before asserting. */
          let audit = (bin, label) =>
            Pty.withSession(~width=90, ~height=28, bin, [], s => {
              Pty.drain(~quietMs=300, ~timeoutMs=6000, s);
              Pty.resize(s, ~width=70, ~height=20);
              Pty.drain(~quietMs=300, ~timeoutMs=6000, s);
              Pty.send(s, "\003\003");
              ignore(Pty.waitExit(~timeoutMs=6000, s));
              assertNoUnknown(s, label ++ " (startup, resize, restore)");
            });
          audit(chatBin, "chat");
          audit(ccBin, "claude-code");
        });
  });
};
