/*
 * command-menu - a live log viewer with a Ctrl+K command palette.
 *
 * The hardest case for an overlay, on purpose. The log underneath keeps
 * streaming (a useInterval appends a line every 200ms) while the palette is
 * open, so the dialog has to composite over a MOVING frame, and the timers
 * that move it have to keep firing while a modal owns the keyboard. If
 * either of those were wrong the app would visibly freeze the moment the
 * palette appeared.
 *
 * WHAT IT DEMONSTRATES
 *
 * - <Modal> written DIRECTLY IN THE STACK, costing that stack no row in
 *   either state. Nothing about the layout below changes when it opens.
 * - Globals with useKeyDown, layer keys with useInput. Ctrl+C quits from
 *   anywhere, including with the palette open - raw mode disables ISIG, so
 *   an app that cannot receive Ctrl+C under a modal cannot be exited at all.
 *   The palette's own arrows and Enter use useInput, so they are captured by
 *   the layer and never reach the log pane's ScrollView underneath.
 * - <Container> around the log pane (Part A): the status line inside it
 *   sizes itself against the PANE, not against the terminal.
 * - <ScrollView rows> for the log: pre-baked, style-self-contained rows, so
 *   a frame costs O(viewport) however long the log gets.
 *
 * KEYS
 *   Ctrl+K / Cmd+K   open the command palette
 *   type             filter the commands
 *   up / down        move the selection (captured - the log does not scroll)
 *   Enter            run the selected command
 *   Esc              close the palette
 *   up / down        scroll the log, while the palette is CLOSED
 *   Ctrl+C           quit, palette open or not
 *
 * The component lives here, in its own library, with main.re a one-line
 * launcher, so test/commandmenu_tests.re can drive the very same component
 * headlessly - the same split as examples/chat.
 */
open Matcha;

/* ============================================================================
 * The log
 * ============================================================================ */

type level =
  | Info
  | Warn
  | Error;

let levelName = (l: level): string =>
  switch (l) {
  | Info => "info"
  | Warn => "warn"
  | Error => "error"
  };

let levelColor = (l: level): Element.color =>
  switch (l) {
  | Info => Element.Cyan
  | Warn => Element.Yellow
  | Error => Element.Red
  };

type entry = {
  seq: int,
  level,
  text: string,
};

/* Deterministic generation - a golden has to be reproducible, and "random"
 * log lines would make the example untestable. Everything below is a pure
 * function of the sequence number. */
let messages = [|
  "GET /api/session 200",
  "cache warm: 412 entries",
  "worker 3 picked up job",
  "flush: 18 rows committed",
  "GET /api/user 200",
  "retry scheduled in 250ms",
  "connection pool at 60%",
  "checkpoint written",
|];

let levelFor = (seq: int): level =>
  switch (seq mod 7) {
  | 3 => Warn
  | 6 => Error
  | _ => Info
  };

let entryAt = (seq: int): entry => {
  seq,
  level: levelFor(seq),
  text: messages[seq mod Array.length(messages)],
};

/* Seeded with enough lines that the very first frame has content - the
 * golden and `checkExample` only ever see one frame. */
let seedCount = 40;
let seedEntries: list(entry) = List.init(seedCount, entryAt);

/* One pre-baked row for <ScrollView rows>.
 *
 * SELF-CONTAINED, per that mode's contract: every row opens the styles it
 * needs and closes them, assuming nothing is left open by the row above.
 * That independence is what lets the runtime paint row N without reading
 * rows 0..N-1. */
let bakeRow = (~dim: bool, e: entry): string => {
  let tag = Printf.sprintf("%04d", e.seq);
  let lvl = Printf.sprintf("%-5s", levelName(e.level));
  (dim ? Element.styleToAnsi(Element.Dim) : "")
  ++ Element.styleToAnsi(Element.FgColor(Element.BrightBlack))
  ++ tag
  ++ Element.resetAnsi
  ++ (dim ? Element.styleToAnsi(Element.Dim) : "")
  ++ " "
  ++ Element.styleToAnsi(Element.FgColor(levelColor(e.level)))
  ++ lvl
  ++ Element.resetAnsi
  ++ (dim ? Element.styleToAnsi(Element.Dim) : "")
  ++ " "
  ++ e.text
  ++ Element.resetAnsi;
};

/* ============================================================================
 * Commands
 * ============================================================================ */

type command = {
  cid: string,
  label: string,
};

let allCommands: list(command) = [
  {cid: "toggle-run", label: "Pause / resume the stream"},
  {cid: "filter-all", label: "Show all levels"},
  {cid: "filter-warn", label: "Filter: warnings only"},
  {cid: "filter-error", label: "Filter: errors only"},
  {cid: "clear", label: "Clear the log"},
  {cid: "top", label: "Jump to the top"},
  {cid: "bottom", label: "Jump to the bottom"},
  {cid: "theme", label: "Toggle dim theme"},
  {cid: "quit", label: "Quit"},
];

/* A ten-line case-insensitive substring match, deliberately. An example
 * should demonstrate the modal, not ship a fuzzy-matching library. */
let lowercase = String.lowercase_ascii;

let matches = (~query: string, c: command): bool => {
  let q = lowercase(String.trim(query));
  if (q == "") {
    true;
  } else {
    let hay = lowercase(c.label ++ " " ++ c.cid);
    let hl = String.length(hay);
    let ql = String.length(q);
    let rec scan = i =>
      if (i + ql > hl) {
        false;
      } else if (String.sub(hay, i, ql) == q) {
        true;
      } else {
        scan(i + 1);
      };
    scan(0);
  };
};

/* ============================================================================
 * The palette - the Modal's CHILD, and therefore a member of the layer
 * ============================================================================ */

module Palette = {
  [@component]
  let make = (~onRun: string => unit) => {
    /* The dialog's own box, not the window: an overlay pushes its box as a
       container, so this is responsive to the palette. */
    let box = useContainerSize();
    let listRows = max(1, box.Runtime.availHeight - 4);

    let (query, setQuery) = Hooks.useState("");
    let (cursor, setCursor) = Hooks.useState((0, 0));
    let (selection, setSelection) = Hooks.useState(None);
    let (selected, setSelected) = Hooks.useState(0);

    let (cursorRow, cursorCol) = cursor;
    let visible = List.filter(c => matches(~query, c), allCommands);
    let count = List.length(visible);
    let sel = count == 0 ? 0 : min(selected, count - 1);

    let {Hooks.isFocused: focused} =
      Hooks.useFocus(~autoFocus=true, ~id="cmd-input", ());

    /* useInput, not useKeyDown: these keys belong to the layer. Arrow_up and
       Arrow_down are CLAIMED here so they move the selection rather than
       reaching TextArea's cursor - the intercept-then-delegate idiom
       examples/claude-code already establishes. Esc is deliberately NOT
       handled: <Modal> owns it. */
    Hooks.useInput(~isActive=focused, (key, mods) =>
      switch (key, mods) {
      | (Key.Arrow_up, _) =>
        setSelected(count == 0 ? 0 : (sel - 1 + count) mod count)
      | (Key.Arrow_down, _) =>
        setSelected(count == 0 ? 0 : (sel + 1) mod count)
      | (Key.Enter, _) =>
        switch (List.nth_opt(visible, sel)) {
        | Some(c) => onRun(c.cid)
        | None => ()
        }
      | _ =>
        TextArea.handleKeyDown(
          key,
          mods,
          query,
          q => {
            setQuery(q);
            /* A new query renumbers the list, so start from the top. */
            setSelected(0);
          },
          None,
          cursorRow,
          cursorCol,
          setCursor,
          selection,
          setSelection,
        )
      }
    );

    let rows =
      visible
      |> List.filteri((i, _) => i < listRows)
      |> List.mapi((i, c) =>
           <Sized size={Chars(1)}>
             <Text
               bold={i == sel}
               color={i == sel ? Element.BrightCyan : Element.White}>
               {(i == sel ? "> " : "  ") ++ c.label}
             </Text>
           </Sized>
         );

    <VStack>
      <Sized size={Chars(1)}>
        {/* The framework's own single-line input. blink=false: no timer, so
            the palette cannot destabilise a golden. */
         <TextArea
           value=query
           onChange={q => setQuery(q)}
           placeholder="Type a command..."
           minHeight=1
           maxHeight=1
           blink=false
           cursorRow
           cursorCol
           setCursor
           selection
           setSelection
         />}
      </Sized>
      <Sized size={Chars(1)}>
        <Text dim=true>
          {Element.repeatString(
             Element.BoxChars.horizontal,
             max(0, box.Runtime.availWidth - 4),
           )}
        </Text>
      </Sized>
      <Sized size={Flex(1)}>
        <VStack>
          ...{
               count == 0
                 ? [<Text dim=true> "no matching command" </Text>] : rows
             }
        </VStack>
      </Sized>
    </VStack>;
  };
};

/* ============================================================================
 * The application
 * ============================================================================ */

[@component]
let make = () => {
  let quit = Event.useQuit();

  let (entries, setEntries) = Hooks.useState(seedEntries);
  let (nextSeq, setNextSeq) = Hooks.useState(seedCount);
  let (running, setRunning) = Hooks.useState(true);
  let (filter, setFilter) = Hooks.useState(None);
  let (dimTheme, setDimTheme) = Hooks.useState(false);
  let (paletteOpen, setPaletteOpen) = Hooks.useState(false);
  /* None means "stick to the bottom" - the tail-following behaviour a log
     viewer wants; a number means the user has scrolled somewhere. */
  let (pinned, setPinned) = Hooks.useState(None);

  /* The stream. ms=0 disables the interval outright (the Ink `delay={null}`
     idiom), which is what "pause" actually does - no timer at all rather
     than a timer whose callback does nothing. This keeps ticking while the
     palette is open: a modal owns the KEYBOARD, not the clock. */
  Hooks.useInterval(
    () => {
      setEntries(entries @ [entryAt(nextSeq)]);
      setNextSeq(nextSeq + 1);
    },
    ~ms=running ? 200 : 0,
  );

  let shown =
    switch (filter) {
    | None => entries
    | Some(l) => List.filter(e => e.level == l, entries)
    };
  let rows =
    shown |> List.map(e => bakeRow(~dim=dimTheme, e)) |> Array.of_list;
  let total = Array.length(rows);

  let closePalette = () => setPaletteOpen(false);

  let run = (cid: string): unit => {
    closePalette();
    switch (cid) {
    | "toggle-run" => setRunning(!running)
    | "filter-all" => setFilter(None)
    | "filter-warn" => setFilter(Some(Warn))
    | "filter-error" => setFilter(Some(Error))
    | "clear" =>
      setEntries([]);
      setPinned(Some(0));
    | "top" => setPinned(Some(0))
    | "bottom" => setPinned(None)
    | "theme" => setDimTheme(!dimTheme)
    | "quit" => quit(ClearScreen)
    | _ => ()
    };
  };

  /* GLOBALS - useKeyDown, so they fire whether or not a layer is open.
     Ctrl+C in particular: see this file's header. Ctrl+K is byte 11, which
     no terminal claims; Cmd+K arrives as meta on any terminal that speaks
     CSI-u. */
  Event.useKeyDown((key, mods) =>
    switch (key, mods) {
    | (Key.Char('c'), {Key.ctrl: true, _}) => quit(ClearScreen)
    | (Key.Char('k'), {Key.ctrl: true, _})
    | (Key.Char('k'), {Key.meta: true, _}) => setPaletteOpen(true)
    | _ => ()
    }
  );

  let statusText =
    (running ? "RUNNING" : "PAUSED")
    ++ "  ·  "
    ++ (
      switch (filter) {
      | None => "all levels"
      | Some(l) => levelName(l) ++ " only"
      }
    )
    ++ "  ·  "
    ++ string_of_int(total)
    ++ " lines  ·  ctrl+k commands";

  <VStack>
    <Sized size={Flex(1)}>
      {/* A container-query boundary (Part A): everything inside the log pane
          is responsive to the PANE. */
       <Container>
         <VStack>
           <Sized size={Chars(1)}>
             <Text bold=true color=Element.BrightWhite> " log stream" </Text>
           </Sized>
           <Sized size={Flex(1)}>
             <ScrollView
               id="log"
               rows
               offset={
                 switch (pinned) {
                 | Some(o) => o
                 | None => max(0, total)
                 }
               }
               onScroll={o => setPinned(Some(o))}
             />
           </Sized>
         </VStack>
       </Container>}
    </Sized>
    /* Directly in the stack, and free: no row in either state. */
    <Modal
      isOpen=paletteOpen
      title="Commands"
      width={Percent(60)}
      /* 13 rows: two borders, the query row, the rule, and nine commands -
         the whole list, so nothing is hidden before the user has typed. */
      height={Chars(13)}
      align={Element.OverlayTop(2)}
      onDismiss=closePalette>
      <Palette onRun=run />
    </Modal>
    <Sized size={Chars(1)}>
      <Text dim=true> statusText </Text>
    </Sized>
  </VStack>;
};

module App = {
  let make = make;
};
