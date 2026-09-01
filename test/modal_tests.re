/*
 * Overlays and modals (B2): compositing, input capture, focus containment
 * and mouse routing.
 *
 * Two layers of assertion, deliberately:
 *
 *   1. Frame text, through the headless handle - what the application sees.
 *   2. A TERMINAL GRID, through test/vterm.re - what a terminal would show.
 *      Compositing is cell surgery on already-rendered ANSI (splitting rows
 *      at a column, padding them, adding a style to somebody else's cells),
 *      and the failure modes are all of the "the columns to the right
 *      shifted by one" / "the shadow painted over the log" kind. Those are
 *      invisible in stripAnsi'd text and obvious on a grid.
 *
 * Every case runs at a NON-80x24 size: 80x24 is the constraints default, the
 * headless-config default and the getSize fallback all at once, so at that
 * size a stale value and a computed one are indistinguishable.
 */
open Matcha;

/* ============================================================================
 * Grid helper
 * ============================================================================ */

/* Feed a rendered frame into a fresh terminal model.
 *
 * Lines are rejoined with CR LF, not LF: Vterm models a BARE line feed the
 * way a terminal does - it moves down and KEEPS THE COLUMN - so feeding a
 * frame with plain "\n" would stagger every row by the length of the one
 * above it. A real pty turns the runtime's "\n" into CR LF through ONLCR;
 * here we do it explicitly. */
let gridOf = (~width: int, ~height: int, frame: string): Vterm.t => {
  let vt = Vterm.create(~width, ~height);
  Vterm.feed(vt, String.concat("\r\n", String.split_on_char('\n', frame)));
  vt;
};

let hasSgr = (vt: Vterm.t, ~row: int, ~col: int, code: int): bool =>
  List.mem(code, Vterm.cellSgr(vt, ~row, ~col));

/* ============================================================================
 * Fixture 1: a raw <Overlay> over a striped base, for the compositing cases
 * ============================================================================ */

type compositeCfg = {
  baseRow: string,
  baseRows: int,
  baseAlign: Element.align,
  ovWidth: Element.size,
  ovHeight: Element.size,
  ovAlign: Element.overlayAlign,
  ovShadow: bool,
  ovRows: list(string),
};

let defaultComposite = {
  baseRow: String.make(40, 'B'),
  baseRows: 8,
  baseAlign: Element.AlignStretch,
  ovWidth: Element.Chars(10),
  ovHeight: Element.Chars(4),
  ovAlign: Element.OverlayTop(2),
  ovShadow: false,
  ovRows: [
    String.make(10, 'M'),
    String.make(10, 'M'),
    String.make(10, 'M'),
    String.make(10, 'M'),
  ],
};

/* The root has to be a zero-argument component (Runtime.HooksComponent), so
 * the per-case configuration travels through this ref. Set it, then start a
 * fresh handle - never share a handle between cases. */
let compositeCfg = ref(defaultComposite);

module CompositeApp = {
  [@component]
  let make = () => {
    let c = compositeCfg^;
    <VStack align={c.baseAlign}>
      ...{
           List.init(c.baseRows, _ => <Text> {c.baseRow} </Text>)
           @ [
             <Overlay
               width={c.ovWidth}
               height={c.ovHeight}
               align={c.ovAlign}
               shadow={c.ovShadow}>
               <VStack>
                 ...{List.map(r => <Text> r </Text>, c.ovRows)}
               </VStack>
             </Overlay>,
           ]
         }
    </VStack>;
  };
};

let startComposite =
    (~width: int, ~height: int, c: compositeCfg): Runtime.headlessHandle => {
  compositeCfg := c;
  Runtime.startHeadless(~config={width, height}, (module CompositeApp));
};

/* ============================================================================
 * Fixture 2: a <Modal> over a base pane, for capture / focus / mouse
 * ============================================================================ */

let baseInputHits = ref(0);
let baseKeyHits = ref(0);
let ctrlCHits = ref(0);
let modalInputHits = ref(0);
let dismissHits = ref(0);
let baseClickHits = ref(0);
let modalClickHits = ref(0);
let rootMouseHits = ref(0);
let modalOpen = ref(true);
/* Which of the base pane's two focusables the case wants; "log" first. */
let modalHeight = ref(Element.Chars(5));

let resetCounters = () => {
  baseInputHits := 0;
  baseKeyHits := 0;
  ctrlCHits := 0;
  modalInputHits := 0;
  dismissHits := 0;
  baseClickHits := 0;
  modalClickHits := 0;
  rootMouseHits := 0;
  modalOpen := true;
  modalHeight := Element.Chars(5);
};

/* The base pane: focusable, keyboard-driven through useInput (so it must go
 * quiet under a modal), and clickable at a position the dialog covers. */
module BasePane = {
  [@component]
  let make = (~id: string) => {
    let {Hooks.isFocused: _} = Hooks.useFocus(~id, ());
    Hooks.useInput((_k, _m) => incr(baseInputHits));
    Hooks.useMouse(_ev => incr(baseClickHits));
    <Text> {"base:" ++ id} </Text>;
  };
};

/* The dialog's content. A MEMBER of the layer - it is rendered inside the
 * Overlay's child - so its useInput fires while the layer is topmost. */
module DialogBody = {
  [@component]
  let make = () => {
    let box = useContainerSize();
    let {Hooks.isFocused: _} =
      Hooks.useFocus(~autoFocus=true, ~id="dialog-input", ());
    Hooks.useInput((_k, _m) => incr(modalInputHits));
    Hooks.useMouse(_ev => incr(modalClickHits));
    <VStack>
      <Text>
        {"box:"
         ++ string_of_int(box.Runtime.availWidth)
         ++ "x"
         ++ string_of_int(box.Runtime.availHeight)}
      </Text>
      <Text> "DIALOGCONTENT" </Text>
    </VStack>;
  };
};

module ModalApp = {
  [@component]
  let make = () => {
    /* Global, and therefore useKeyDown: this is the binding that has to
       survive a modal, or a raw-mode app (no ISIG) cannot be quit. */
    Event.useKeyDown((key, mods) => {
      incr(baseKeyHits);
      switch (key, mods) {
      | (Key.Char('c'), {Key.ctrl: true, _}) => incr(ctrlCHits)
      | _ => ()
      };
    });
    Hooks.useMouse(_ev => incr(rootMouseHits));
    <VStack>
      /* Six rows tall, so the base pane's clickable box spans the rows the
         dialog floats over - which is what makes "a click inside hits the
         dialog, not the base at the same coordinates" a real question. */
      <Sized size={Element.Chars(6)}> <BasePane id="log" /> </Sized>
      <Sized size={Element.Chars(1)}> <BasePane id="side" /> </Sized>
      <Modal
        isOpen={modalOpen^}
        width={Element.Percent(50)}
        height={modalHeight^}
        align={Element.OverlayTop(1)}
        shadow=false
        onDismiss={() => incr(dismissHits)}>
        <DialogBody />
      </Modal>
      <Text> "footer" </Text>
    </VStack>;
  };
};

let startModal = (~width: int, ~height: int): Runtime.headlessHandle =>
  Runtime.startHeadless(~config={width, height}, (module ModalApp));

/* The same modal inside an HSTACK. Worth its own fixture because the two
 * stack cases reach their layout-invisible partition by different routes -
 * and because an HStack renders its children TWICE (a natural-height
 * pre-pass, then the real one), so a layer recorded from the wrong pass
 * would show up here as a duplicate or as nothing at all. */
module HStackModalApp = {
  [@component]
  let make = () =>
    <HStack>
      <Sized size={Element.Chars(20)}> <Text> "left column" </Text> </Sized>
      <Modal
        isOpen=true
        width={Element.Chars(16)}
        height={Element.Chars(3)}
        align={Element.OverlayTop(1)}
        shadow=false>
        <Text> "HSTACKDIALOG" </Text>
      </Modal>
      <Sized size={Element.Flex(1)}> <Text> "right column" </Text> </Sized>
    </HStack>;
};

/* An <Overlay> inside a <Static> item: never a committed pass, so it must
 * record nothing at all. */
module StaticOverlayApp = {
  [@component]
  let make = () =>
    <VStack>
      <Static
        items=[1]
        renderItem={(_item, _i) =>
          <Overlay width={Element.Chars(8)} height={Element.Chars(1)}>
            <Text> "SHOULDNOTAPPEAR" </Text>
          </Overlay>}
      />
      <Text> "live row" </Text>
    </VStack>;
};

let run = () => {
  /* ========================================================================
   * Compositing, on a terminal grid
   * ====================================================================== */
  Test.group("Overlay: compositing (Vterm grid)", () => {
    Test.run("the box lands at its columns and the base survives beside it", () => {
      /* 40x10 frame, a 10-wide box - so ox = (40-10)/2 = 15 - two rows down. */
      let handle = startComposite(~width=40, ~height=10, defaultComposite);
      let vt = gridOf(~width=40, ~height=10, handle.getOutput(false));
      Test.assertEqualStr(Vterm.cellGlyph(vt, ~row=2, ~col=15), "M", "first overlay column");
      Test.assertEqualStr(Vterm.cellGlyph(vt, ~row=2, ~col=24), "M", "last overlay column");
      Test.assertEqualStr(
        Vterm.cellGlyph(vt, ~row=2, ~col=14),
        "B",
        "the base cell immediately LEFT of the box is untouched",
      );
      Test.assertEqualStr(
        Vterm.cellGlyph(vt, ~row=2, ~col=25),
        "B",
        "and so is the one immediately right of it - the splice must not "
        ++ "shift the columns after the box",
      );
      Test.assertEqualStr(
        Vterm.row(vt, 1),
        String.make(40, 'B'),
        "the row ABOVE the box is the base frame, unmodified",
      );
      Test.assertEqualStr(
        Vterm.row(vt, 6),
        String.make(40, 'B'),
        "and so is the row below it",
      );
      handle.quit();
    });

    Test.run("the shadow DIMS the base cells instead of painting over them", () => {
      let handle =
        startComposite(
          ~width=40,
          ~height=10,
          {...defaultComposite, ovShadow: true},
        );
      let vt = gridOf(~width=40, ~height=10, handle.getOutput(false));
      /* Right strip: two columns at ofX + ofW = 25, rows ofY+1 .. ofY+ofH-1. */
      Test.assertTrue(hasSgr(vt, ~row=3, ~col=25, 2), "shadow cell carries SGR 2 (dim)");
      Test.assertEqualStr(
        Vterm.cellGlyph(vt, ~row=3, ~col=25),
        "B",
        "AND still holds the base glyph - the backdrop dims what is under "
        ++ "it, it does not paint over it",
      );
      Test.assertTrue(hasSgr(vt, ~row=3, ~col=26, 2), "the strip is two columns wide");
      Test.assertFalse(
        hasSgr(vt, ~row=3, ~col=27, 2),
        "a cell just outside the L is NOT dimmed",
      );
      Test.assertFalse(
        hasSgr(vt, ~row=2, ~col=25, 2),
        "the strip starts one row BELOW the box's top - the shadow is offset "
        ++ "by (+1, +1)",
      );
      /* Bottom strip: row ofY + ofH = 6, columns ofX+1 .. ofX+ofW+1. */
      Test.assertTrue(hasSgr(vt, ~row=6, ~col=16, 2), "bottom strip starts at ofX+1");
      Test.assertTrue(hasSgr(vt, ~row=6, ~col=26, 2), "and ends at ofX+ofW+1");
      Test.assertFalse(hasSgr(vt, ~row=6, ~col=15, 2), "not at ofX itself");
      Test.assertEqualStr(
        Vterm.cellGlyph(vt, ~row=6, ~col=16),
        "B",
        "the bottom strip keeps its base glyph too",
      );
      handle.quit();
    });

    Test.run("a wide-character base row keeps its columns", () => {
      /* 20 CJK cells = 40 columns. The box covers columns 15..24, which cuts
         THROUGH the wide cell occupying 14-15 and the one occupying 24-25:
         both must degrade to blanks rather than shift everything right. */
      let cjk = "\xE6\x97\xA5"; /* U+65E5, two columns */
      let handle =
        startComposite(
          ~width=40,
          ~height=10,
          {...defaultComposite, baseRow: Element.repeatString(cjk, 20)},
        );
      let out = handle.getOutput(false);
      let vt = gridOf(~width=40, ~height=10, out);
      Test.assertEqualStr(Vterm.cellGlyph(vt, ~row=2, ~col=15), "M", "the box still starts at column 15");
      Test.assertEqualStr(Vterm.cellGlyph(vt, ~row=2, ~col=24), "M", "and still ends at column 24");
      Test.assertEqualStr(
        Vterm.cellGlyph(vt, ~row=2, ~col=26),
        cjk,
        "the first WHOLE wide cell after the box is still at column 26 - "
        ++ "nothing shifted",
      );
      List.iter(
        line =>
          Test.assertTrue(
            Element.visibleLength(line) <= 40,
            "no spliced row grew past the frame width (which would wrap)",
          ),
        Element.splitLines(out),
      );
      handle.quit();
    });

    Test.run("a base row shorter than the box's column still gets the box", () => {
      /* AlignStart, so the base rows are their natural 2 columns and the
         box at column 15 starts past the end of every one of them. */
      let handle =
        startComposite(
          ~width=40,
          ~height=10,
          {
            ...defaultComposite,
            baseRow: "ab",
            baseAlign: Element.AlignStart,
          },
        );
      let vt = gridOf(~width=40, ~height=10, handle.getOutput(false));
      Test.assertEqualStr(Vterm.cellGlyph(vt, ~row=2, ~col=0), "a", "the short base row is intact");
      Test.assertEqualStr(
        Vterm.cellGlyph(vt, ~row=2, ~col=15),
        "M",
        "and the box is at column 15, on blanks the splice had to invent",
      );
      Test.assertEqualStr(Vterm.cellGlyph(vt, ~row=2, ~col=9), " ", "the gap between them is blank");
      handle.quit();
    });

    Test.run("an overlay past the last rendered line grows the frame", () => {
      /* Three base rows in a 12-row frame: the app renders 3 lines, and the
         box sits on rows 5..7, which do not exist yet. */
      let handle =
        startComposite(
          ~width=40,
          ~height=12,
          {
            ...defaultComposite,
            baseRows: 3,
            ovHeight: Element.Chars(3),
            ovAlign: Element.OverlayTop(5),
            ovRows: [String.make(10, 'M'), String.make(10, 'M'), String.make(10, 'M')],
          },
        );
      let lines = handle.getLines(true);
      Test.assertEqual(Array.length(lines), 8, "the frame grew from 3 rows to 8");
      Test.assertEqualStr(String.trim(lines[3]), "", "the invented rows are blank");
      Test.assertEqualStr(String.trim(lines[4]), "", "both of them");
      Test.assertEqualStr(String.trim(lines[5]), String.make(10, 'M'), "and the box is on row 5");
      handle.quit();
    });

    Test.run("overlay content wider than the box is clipped, not wrapped", () => {
      let handle =
        startComposite(
          ~width=40,
          ~height=10,
          {
            ...defaultComposite,
            ovRows: [
              String.make(30, 'M'),
              String.make(30, 'M'),
              String.make(30, 'M'),
              String.make(30, 'M'),
            ],
          },
        );
      let vt = gridOf(~width=40, ~height=10, handle.getOutput(false));
      Test.assertEqualStr(Vterm.cellGlyph(vt, ~row=2, ~col=24), "M", "the last column of the box");
      Test.assertEqualStr(
        Vterm.cellGlyph(vt, ~row=2, ~col=25),
        "B",
        "one column further right is the BASE again - the surplus content "
        ++ "was clipped, not spilled",
      );
      Test.assertEqualStr(
        Vterm.row(vt, 3),
        String.make(15, 'B') ++ String.make(10, 'M') ++ String.make(15, 'B'),
        "and it did not wrap onto the row below either",
      );
      handle.quit();
    });

    Test.run("a box wider than the frame is clamped to it", () => {
      let handle =
        startComposite(
          ~width=20,
          ~height=8,
          {
            ...defaultComposite,
            baseRow: String.make(20, 'B'),
            ovWidth: Element.Chars(30),
          },
        );
      let out = handle.getOutput(false);
      List.iter(
        line =>
          Test.assertTrue(
            Element.visibleLength(line) <= 20,
            "no row is wider than the 20-column frame",
          ),
        Element.splitLines(out),
      );
      let vt = gridOf(~width=20, ~height=8, out);
      Test.assertEqualStr(Vterm.row(vt, 2), String.make(10, 'M') ++ String.make(10, ' '),
        "the box fills what it can of the frame from column 0");
      handle.quit();
    });

    Test.run("rendering twice produces the identical frame", () => {
      let handle = startComposite(~width=40, ~height=10, defaultComposite);
      let first = handle.getOutput(false);
      let second = handle.render();
      Test.assertEqualStr(second, first, "compositing is idempotent - a second frame is not double-spliced");
      handle.quit();
    });
  });

  /* ========================================================================
   * Layout cost: a modal must be free
   * ====================================================================== */
  Test.group("Modal: costs the stack nothing", () => {
    Test.run("open or closed, the base rows stay where they are", () => {
      resetCounters();
      let open_ = startModal(~width=60, ~height=16);
      let openLines = open_.getLines(true);
      open_.quit();

      resetCounters();
      modalOpen := false;
      let closed = startModal(~width=60, ~height=16);
      let closedLines = closed.getLines(true);
      closed.quit();

      Test.assertEqual(
        Array.length(closedLines),
        8,
        "closed: 6 rows of log pane, 1 of side pane, 1 of footer - and no "
        ++ "blank row where the <Modal> sits",
      );
      Test.assertEqual(
        Array.length(openLines),
        Array.length(closedLines),
        "opening the modal added no row to the frame either (its box fits "
        ++ "inside the rows the app already renders)",
      );
      Test.assertEqualStr(String.trim(closedLines[0]), "base:log", "closed: log pane on row 0");
      Test.assertEqualStr(String.trim(closedLines[6]), "base:side", "closed: side pane on row 6");
      Test.assertEqualStr(String.trim(closedLines[7]), "footer", "closed: footer on row 7");
      /* Open: the dialog covers columns 15.. on rows 1..5, so everything to
         the LEFT of it must still read exactly as it did when closed. */
      let leftOf = (line: string) =>
        String.trim(String.sub(line, 0, min(15, String.length(line))));
      Test.assertEqualStr(leftOf(openLines[0]), "base:log", "open: log pane still on row 0");
      Test.assertEqualStr(leftOf(openLines[6]), "base:side", "open: side pane still on row 6");
      Test.assertEqualStr(leftOf(openLines[7]), "footer", "open: footer still on row 7");
      resetCounters();
    });

    Test.run("the dialog is composited exactly once", () => {
      resetCounters();
      let handle = startModal(~width=60, ~height=16);
      let out = handle.getOutput(true);
      let count = (hay, needle) => {
        let hlen = String.length(hay);
        let nlen = String.length(needle);
        let n = ref(0);
        for (i in 0 to hlen - nlen) {
          if (String.sub(hay, i, nlen) == needle) {
            incr(n);
          };
        };
        n^;
      };
      Test.assertEqual(count(out, "DIALOGCONTENT"), 1, "one copy of the dialog body, not two");
      handle.quit();
      resetCounters();
    });

    Test.run("in an HStack it composites once and takes no column", () => {
      let handle =
        Runtime.startHeadless(
          ~config={width: 44, height: 8},
          (module HStackModalApp),
        );
      let out = handle.getOutput(true);
      let count = {
        let needle = "HSTACKDIALOG";
        let nlen = String.length(needle);
        let n = ref(0);
        for (i in 0 to String.length(out) - nlen) {
          if (String.sub(out, i, nlen) == needle) {
            incr(n);
          };
        };
        n^;
      };
      Test.assertEqual(
        count,
        1,
        "one copy - an HStack renders its children twice (a natural-height "
        ++ "pre-pass, then the real one) and only the committed pass may "
        ++ "record a layer",
      );
      let lines = handle.getLines(true);
      Test.assertContains(lines[0], "left column", "the left column is still at column 0");
      Test.assertContains(
        lines[0],
        "right column",
        "and the right one is still beside it - the modal consumed no "
        ++ "horizontal slot, so the flex split is unchanged",
      );
      handle.quit();
    });

    Test.run("an <Overlay> inside a <Static> item records nothing", () => {
      let handle =
        Runtime.startHeadless(
          ~config={width: 50, height: 10},
          (module StaticOverlayApp),
        );
      Test.assertFalse(
        Test.contains(handle.getOutput(true), "SHOULDNOTAPPEAR"),
        "a Static item's render carries no origin, so the overlay never "
        ++ "reaches a committed pass and never floats",
      );
      Test.assertFalse(
        Test.contains(handle.getStaticOutput(true), "SHOULDNOTAPPEAR"),
        "and it is not committed above the live region either",
      );
      Test.assertContains(handle.getOutput(true), "live row", "the rest of the frame is normal");
      handle.quit();
    });
  });

  /* ========================================================================
   * Input capture
   * ====================================================================== */
  Test.group("Modal: input capture", () => {
    Test.run("useInput is captured by the layer; useKeyDown always fires", () => {
      resetCounters();
      let handle = startModal(~width=60, ~height=16);
      let before = baseKeyHits^;
      handle.sendKey(Key.Char('x'), Key.noModifiers);
      Test.assertEqual(
        modalInputHits^,
        1,
        "the DIALOG's useInput fired - it is a member of the layer",
      );
      Test.assertEqual(
        baseInputHits^,
        0,
        "the BASE pane's useInput did NOT - it is outside the layer, which "
        ++ "is what makes the dialog modal",
      );
      Test.assertEqual(
        baseKeyHits^ - before,
        1,
        "useKeyDown fired regardless - it is never captured",
      );
      handle.quit();
      resetCounters();
    });

    Test.run("Ctrl+C reaches the app while the modal is open", () => {
      /* THE UNQUITTABLE-APP GUARD. Raw mode disables ISIG, so Ctrl+C is an
         ordinary keypress that only the application can act on. If a modal
         could swallow it, an app showing one could not be exited at all. */
      resetCounters();
      let handle = startModal(~width=60, ~height=16);
      Input.feedBytes(handle, "\003");
      Test.assertEqual(ctrlCHits^, 1, "Ctrl+C reached the root's useKeyDown handler");
      handle.quit();
      resetCounters();
    });

    Test.run("with the modal closed, the base pane hears keys again", () => {
      resetCounters();
      modalOpen := false;
      let handle = startModal(~width=60, ~height=16);
      handle.sendKey(Key.Char('x'), Key.noModifiers);
      Test.assertEqual(baseInputHits^, 2, "both base panes' useInput fired - nothing is open");
      Test.assertEqual(modalInputHits^, 0, "and the dialog is not rendered at all");
      handle.quit();
      resetCounters();
    });

    Test.run("Esc closes the dialog through the modal's own captured useInput", () => {
      resetCounters();
      let handle = startModal(~width=60, ~height=16);
      handle.sendKey(Key.Escape, Key.noModifiers);
      Test.assertEqual(dismissHits^, 1, "onDismiss ran once");
      handle.quit();
      resetCounters();
    });
  });

  /* ========================================================================
   * Container queries inside the dialog
   * ====================================================================== */
  Test.group("Modal: the dialog is its own container", () => {
    Test.run("useContainerSize() inside reports the MODAL box, not the frame", () => {
      resetCounters();
      /* 60 columns, Percent(50) -> a 30-column box; Chars(5) -> 5 rows. */
      let handle = startModal(~width=60, ~height=16);
      Test.assertContains(
        handle.getOutput(true),
        "box:30x5",
        "the dialog's content queries the dialog - 30x5 - and not the 60x16 "
        ++ "window it is floating over",
      );
      handle.quit();
      resetCounters();
    });

    Test.run("an Auto-height modal is as tall as its content plus the border", () => {
      resetCounters();
      modalHeight := Element.Auto;
      let handle = startModal(~width=60, ~height=16);
      /* DialogBody renders two rows, so the box is 4 rows: border, two
         content rows, border. The measure pass sees container height 0 and
         therefore draws no side bars, which is what stops it answering "as
         tall as the box I am about to become". */
      Test.assertContains(
        handle.getOutput(true),
        "box:30x4",
        "Auto resolved to content (2 rows) + the two border rows",
      );
      handle.quit();
      resetCounters();
    });
  });

  /* ========================================================================
   * Focus
   * ====================================================================== */
  Test.group("Modal: focus containment and restore", () => {
    Test.run("focus moves into the dialog and Tab cannot leave it", () => {
      resetCounters();
      let handle = startModal(~width=60, ~height=16);
      Test.assertEqual(
        handle.getFocusedId(),
        Some("dialog-input"),
        "the layer's only focusable took focus",
      );
      Input.pressTab(handle);
      Test.assertEqual(
        handle.getFocusedId(),
        Some("dialog-input"),
        "Tab wrapped inside the layer's ring - the base panes are not in it",
      );
      Input.pressShiftTab(handle);
      Test.assertEqual(
        handle.getFocusedId(),
        Some("dialog-input"),
        "and neither does Shift+Tab escape",
      );
      handle.quit();
      resetCounters();
    });

    Test.run("closing restores the id that had focus, not the ring's first", () => {
      resetCounters();
      modalOpen := false;
      let handle = startModal(~width=60, ~height=16);
      /* Put focus on the SECOND base focusable, so "restored" and "first in
         the ring" are different answers. */
      Input.pressTab(handle);
      let restored = handle.getFocusedId();
      Test.assertEqual(restored, Some("side"), "the second pane holds focus");

      modalOpen := true;
      ignore(handle.render());
      Test.assertEqual(
        handle.getFocusedId(),
        Some("dialog-input"),
        "opening the modal contains focus inside it",
      );

      modalOpen := false;
      ignore(handle.render());
      Test.assertEqual(
        handle.getFocusedId(),
        Some("side"),
        "closing it puts focus back where it was - NOT on \"log\", which is "
        ++ "what the ring's own successor rule would have picked",
      );
      handle.quit();
      resetCounters();
    });
  });

  /* ========================================================================
   * Mouse
   * ====================================================================== */
  Test.group("Modal: mouse routing", () => {
    Test.run("a click inside hits the dialog, not the base underneath it", () => {
      resetCounters();
      let handle = startModal(~width=60, ~height=16);
      /* The box is 30 columns wide at ox = 15 and 5 rows tall at oy = 1, so
         its border is on rows 1 and 5 and the dialog's CONTENT occupies
         rows 2..4, columns 17..42. The log pane's own box is rows 0..5
         across the full 60 columns, so (20, 3) lands in both - which is
         exactly the ambiguity the layer has to resolve. */
      Input.clickAt(handle, ~x=20, ~y=3);
      Test.assertEqual(modalClickHits^, 1, "the dialog got the click");
      Test.assertEqual(baseClickHits^, 0, "the base component at the same coordinates did not");
      Test.assertEqual(dismissHits^, 0, "a click inside is not a dismissal");
      handle.quit();
      resetCounters();
    });

    Test.run("a click outside dismisses and is swallowed whole", () => {
      resetCounters();
      let handle = startModal(~width=60, ~height=16);
      Input.clickAt(handle, ~x=2, ~y=0);
      Test.assertEqual(dismissHits^, 1, "onDismiss ran");
      Test.assertEqual(baseClickHits^, 0, "the base component under the pointer did NOT get the click");
      Test.assertEqual(
        rootMouseHits^,
        0,
        "and neither did the root's useMouse - the dismissing click is "
        ++ "swallowed including the root fan-out, the one deliberate "
        ++ "exception to \"root handlers always run\"",
      );
      handle.quit();
      resetCounters();
    });

    Test.run("a wheel notch outside the dialog does not dismiss it", () => {
      resetCounters();
      let handle = startModal(~width=60, ~height=16);
      handle.sendMouse({
        Mouse.kind: Mouse.ScrollDown,
        button: Mouse.NoButton,
        x: 2,
        y: 0,
        shift: false,
        alt: false,
        ctrl: false,
      });
      Test.assertEqual(dismissHits^, 0, "only a Down outside is a dismissal gesture");
      Test.assertEqual(baseClickHits^, 0, "the base is still not reachable through the layer");
      handle.quit();
      resetCounters();
    });
  });
};
