/*
 * claude-code - a mock of the Claude Code CLI, and Matcha's FULLSCREEN
 * showcase. main.re starts it with
 *
 *     Matcha.Runtime.start(~screen=Fullscreen, (module ClaudeCodeApp.App));
 *
 * so the app runs on the terminal's ALTERNATE screen, exactly like vim or
 * htop: it fills the viewport from the first frame, the prompt is pinned to
 * the bottom rows, the terminal's scrollback cannot be scrolled away to, and
 * on exit the previous terminal contents come straight back.
 *
 * WHY THERE IS NO <Static> HERE
 * -----------------------------
 * The alternate screen has NO scrollback, so "commit this above the live
 * region" has no meaning: there is nothing above it and nothing that
 * survives. Matcha does not paper over that - <Static> and useStdout RAISE
 * under ~screen=Fullscreen (see the screenMode doc in lib/Runtime.re), so
 * this file cannot contain either. A fullscreen app owns its transcript
 * instead, and this one keeps it in state and renders it in a <ScrollView>
 * that takes all the spare height, controlled with a stick-to-bottom offset
 * so new output snaps the view down the way the real CLI does.
 *
 * For the INLINE model - a live region with a committed <Static> transcript
 * scrolling into the terminal's real scrollback - read examples/chat and
 * examples/static-demo instead. Those are the default; this is the opt-in.
 *
 * Everything else it exercises, unchanged: a timer-driven stream on a
 * virtual clock, a one-row live status with a useInterval spinner, a
 * controlled single-row <TextArea> prompt, and a slash-command palette built
 * from a second CONTROLLED <ScrollView> full of <Clickable> rows.
 *
 * Nothing here talks to anything: the "agent" is a deterministic canned
 * script (see cannedScript below) played back one step every 350ms, so the
 * whole app is reproducible for goldens and tests. It mocks the UI, not the
 * product.
 *
 * THE INPUT BOX GROWS
 * -------------------
 * <TextArea> soft-wraps its content at ~maxWidth and reports the height it
 * will paint through TextArea.measure. This app asks it that question every
 * frame (inputRows below) and sizes the bordered box - and both gutters -
 * around the answer, so a long prompt wraps onto new rows and the box grows
 * downward to five rows instead of overflowing its slot and blowing the right
 * border away. Past five rows the TextArea scrolls internally, keeping the
 * cursor in view. That is the recipe for any container around a growing
 * input: measure, then size.
 *
 * The component lives here, in its own little library (see the dune file),
 * with main.re a one-line launcher - so that test/claudecode_tests.re can
 * start THE SAME component in-process through Runtime.startHeadless. That
 * split is itself part of the recipe: structure an app as `library with App
 * + thin executable` and every test can drive the real thing. (startHeadless
 * is screen-agnostic: the tests drive this component with no screen at all.)
 *
 * Keys:
 *   type + Enter   submit a prompt; the mock agent streams its canned steps
 *                  into the transcript (submits while streaming are ignored).
 *                  A long prompt soft-wraps and the box grows, up to 5 rows
 *   wheel          over the transcript scrolls back through history; new
 *                  output snaps the view back to the bottom
 *   /              open the slash-command palette; type to filter
 *   up/down        move the palette selection (while the palette is open)
 *   Enter          run the selected command (while the palette is open)
 *   click          run the command row under the pointer
 *   wheel          scroll the palette (it is wheel-driven, never focused)
 *   Esc            interrupt the stream, or clear the input when idle
 *   Shift+Tab      cycle the permission mode (default / auto-accept / plan)
 *   Ctrl+C         once arms the exit, twice quits
 *
 * WHY THERE IS NO useFocus ANYWHERE IN THIS APP
 * ---------------------------------------------
 * This app registers NO useFocus anywhere (and BOTH ScrollViews opt out with
 * focusable=false). With zero focusables, the framework does NOT consume
 * Tab/Shift+Tab for focus cycling, so Shift+Tab reaches the app - which lets
 * the mock use it authentically to cycle permission modes like the real CLI.
 * The input is therefore always live: Hooks.useInput is registered without an
 * ~isActive gate, because there is nothing else that could own the keyboard.
 */
open Matcha;

/* ============================================================================
 * The transcript
 * ============================================================================ */

/* One line-group in the transcript. In a fullscreen app the transcript is
 * ordinary state and ordinary content: these items are re-rendered on every
 * frame inside the history <ScrollView>, not committed anywhere. They are
 * still deliberately inert - no timer, no subscription, nothing ongoing -
 * because a transcript entry is a record of what happened, and everything
 * ongoing (the spinner, the stream, the input) lives in the root component
 * below.
 *
 * Vertical rhythm is expressed with explicit Blank items in the stream rather
 * than padding inside each item's renderer - ONE approach, used consistently,
 * so "what does the transcript look like" is answerable by reading the list. */
type item =
  | Banner
  | UserPrompt(string) /* "> fix a bug" */
  | ToolCall(string) /* "Read(lib/Hooks.re)" */
  | ToolResult(string) /* "Read 1631 lines" */
  | AssistantText(string)
  | Notice(string) /* "Interrupted by user", slash-command output */
  | Blank;

/* The welcome box is fixed-width; padToWidth keeps its right edge honest. */
let bannerInner = 31;
let bannerRow = (s: string): string => "│" ++ padToWidth(s, bannerInner) ++ "│";

module TranscriptItem = {
  [@component]
  let make = (~it: item) =>
    switch (it) {
    | Banner =>
      <VStack>
        <Text color=Magenta>
          {"╭" ++ repeatString("─", bannerInner) ++ "╮"}
        </Text>
        <Text color=Magenta>
          {bannerRow(" ✻ Welcome to Claude Code mock")}
        </Text>
        <Text color=Magenta> {bannerRow("")} </Text>
        <Text color=Magenta> {bannerRow("   /help for help")} </Text>
        <Text color=Magenta> {bannerRow("   cwd: examples/claude-code")} </Text>
        <Text color=Magenta>
          {"╰" ++ repeatString("─", bannerInner) ++ "╯"}
        </Text>
        <Text> "" </Text>
      </VStack>
    | UserPrompt(p) => <Text dim=true> {"> " ++ p} </Text>
    | ToolCall(s) => <Text color=Green> {"⏺ " ++ s} </Text>
    | ToolResult(s) => <Text dim=true> {"  ⎿  " ++ s} </Text>
    | AssistantText(s) => <Text wrap=Wrap> {"● " ++ s} </Text>
    | Notice(s) => <Text color=Yellow> {"⏺ " ++ s} </Text>
    | Blank => <Text> "" </Text>
    };
};

/* ============================================================================
 * The mock agent
 * ============================================================================ */

/* Every submit plays back exactly this, one item per 350ms tick. Canned and
 * deterministic on purpose: the golden and test/claudecode_tests.re both
 * depend on the stream being identical every run. */
let cannedScript = (prompt: string): list(item) => [
  ToolCall("Read(lib/Hooks.re)"),
  ToolResult("Read 1631 lines"),
  ToolCall("Grep(\"useInterval\")"),
  ToolResult("Found 12 matches"),
  ToolCall("Edit(lib/Hooks.re)"),
  ToolResult("Updated lib/Hooks.re with 4 additions"),
  ToolCall("Bash(dune runtest)"),
  ToolResult("All tests passed"),
  /* The "(canned reply ...)" note sits deliberately near the FRONT of this
     sentence: the line renders with wrap=Wrap at the terminal's width, and a
     note at the end would be split across the wrap for a long prompt, which
     would make it unassertable in a test. */
  AssistantText(
    "Done. (canned reply - this example mocks the UI only.) I looked into \""
    ++ prompt
    ++ "\" and made the change; every test is green.",
  ),
];

let stepMs = 350;
let spinnerMs = 120;

let spinnerFrames = [|"·", "✢", "✳", "✻", "✽", "✻", "✳", "✢"|];

let thinkingVerbs = [|"Thinking", "Scheming", "Percolating", "Brewing"|];

let modeLabels = [|"default", "auto-accept edits on", "plan mode on"|];

/* Deterministic token bump per step, so the counter is reproducible. */
let tokensPerStep = 137;

/* "1.2k" above 999, the raw count below it. */
let formatTokens = (n: int): string =>
  if (n > 999) {
    Printf.sprintf("%.1fk", float_of_int(n) /. 1000.0);
  } else {
    string_of_int(n);
  };

/* ============================================================================
 * The slash-command palette
 * ============================================================================ */

let commands = [|
  "/clear",
  "/compact",
  "/config",
  "/cost",
  "/doctor",
  "/exit",
  "/help",
  "/init",
  "/memory",
  "/model",
  "/review",
  "/status",
|];

/* How many command rows the palette window shows at once. */
let menuRows = 5;

let startsWith = (s: string, prefix: string): bool =>
  String.length(s) >= String.length(prefix)
  && String.sub(s, 0, String.length(prefix)) == prefix;

/* Drop the leading "/" from a command, or from the typed text. */
let afterSlash = (s: string): string =>
  if (String.length(s) == 0) {
    "";
  } else {
    String.sub(s, 1, String.length(s) - 1);
  };

/* One palette row. Its own [@component] under a <Clickable> so each row is a
 * distinct instance with its own mouse registration. <Clickable> WITHOUT
 * ~onMouseDown is deliberately wheel-transparent, so a notch over a row still
 * reaches the <ScrollView> underneath - that is framework behavior this app
 * relies on rather than works around. */
module CommandRow = {
  [@component]
  let make = (~cmd: string, ~selected: bool, ~onRun: unit => unit) =>
    <Clickable onClick=onRun>
      <Text inverted=selected> {" " ++ cmd} </Text>
    </Clickable>;
};

/* ============================================================================
 * The app
 * ============================================================================ */

[@component]
let make = () => {
  let quit = Event.useQuit();
  let {Runtime.availWidth: width, _} = useLayout();

  /* The transcript - append only, and entirely ours: it is state that the
     history ScrollView below renders as ordinary content. The banner is the
     first item, so it is simply the top of the history and scrolls with it
     (a fullscreen app has no scrollback to commit it into). */
  let (items, setItems) = Hooks.useState([Banner]);

  /* Where the history window sits.
     None = stick to the bottom: a huge offset clamps to maxOffset, so the
     newest line is always visible. Some(o) = the user wheeled back. */
  let (histOffset, setHistOffset) = Hooks.useState(None);

  /* The mock agent's remaining steps, and whether it is mid-stream. */
  let (pending, setPending) = Hooks.useState([]);
  let (isStreaming, setIsStreaming) = Hooks.useState(false);
  let (ticks, setTicks) = Hooks.useState(0);
  let (tokens, setTokens) = Hooks.useState(0);
  let (spin, setSpin) = Hooks.useState(0);
  let (submitCount, setSubmitCount) = Hooks.useState(0);

  /* The prompt. TextArea is a controlled component: value, cursor and
     selection all live here, same shape as examples/chat. */
  let (inputText, setInputText) = Hooks.useState("");
  let (cursorRow, cursorCol, setCursor) = {
    let (pos, setPos) = Hooks.useState((0, 0));
    let (row, col) = pos;
    (row, col, setPos);
  };
  let (selection, setSelection) = Hooks.useState(None);

  /* Palette selection, permission mode, and the armed-exit flag. */
  let (sel, setSel) = Hooks.useState(0);
  let (modeIdx, setModeIdx) = Hooks.useState(0);
  let (confirmExit, setConfirmExit) = Hooks.useState(false);

  /* ---- derived palette state --------------------------------------------- */

  let menuOpen =
    String.length(inputText) > 0
    && inputText.[0] == '/'
    && !String.contains(inputText, '\n');

  let filtered =
    if (menuOpen) {
      let typed = afterSlash(inputText);
      commands
      |> Array.to_list
      |> List.filter(cmd => startsWith(afterSlash(cmd), typed));
    } else {
      [];
    };
  let filteredCount = List.length(filtered);

  /* `sel` is clamped at every use site rather than kept clamped in state:
     `filtered` shrinks as you type, and clamping here means the selection
     follows along without a second state write during render. */
  let selClamped = min(max(0, sel), max(0, filteredCount - 1));
  let menuOffset =
    min(max(0, selClamped - 2), max(0, filteredCount - menuRows));

  /* ---- actions ------------------------------------------------------------ */

  let clearInput = (): unit => {
    setInputText("");
    setCursor((0, 0));
    setSelection(None);
  };

  /* Snap the history back to the newest line. Called from every place that
     appends a transcript item, which mirrors the real CLI: new output pulls
     the view down, however far back you had wheeled. */
  let stickToBottom = (): unit => setHistOffset(None);

  let runCommand = (cmd: string): unit =>
    if (cmd == "/exit") {
      quit(ClearScreen);
    } else {
      setItems(
        items
        @ [
          Blank,
          Notice(
            "Ran "
            ++ cmd
            ++ " (mocked - this example demonstrates the palette, not the command)",
          ),
        ],
      );
      stickToBottom();
      clearInput();
    };

  let submit = (): unit => {
    let p = String.trim(inputText);
    /* A real CLI queues a prompt typed mid-stream; the mock simply ignores
       it, which keeps the canned script the only thing ever in flight. */
    if (p != "" && !isStreaming) {
      setItems(items @ [Blank, UserPrompt(p)]);
      setPending(cannedScript(p));
      setIsStreaming(true);
      setTicks(0);
      setTokens(0);
      setSpin(0);
      setSubmitCount(submitCount + 1);
      stickToBottom();
      clearInput();
    };
  };

  let interrupt = (): unit => {
    setPending([]);
    setIsStreaming(false);
    setItems(items @ [Blank, Notice("Interrupted by user")]);
    stickToBottom();
  };

  /* ---- the clock ---------------------------------------------------------- */

  /* One canned step per tick. Both intervals are registered unconditionally
     and disabled with ms=0 while idle (the Ink `delay=null` idiom) - flipping
     isStreaming changes ms, which is what registers/cancels the timer. The
     callback closes over THIS render's state values and useInterval always
     invokes the latest closure, so `items`/`pending` below are current. */
  Hooks.useInterval(
    () =>
      switch (pending) {
      | [] => setIsStreaming(false)
      | [next, ...rest] =>
        setItems(items @ [next]);
        stickToBottom();
        setPending(rest);
        setTokens(tokens + tokensPerStep);
        setTicks(ticks + 1);
        switch (rest) {
        | [] => setIsStreaming(false)
        | _ => ()
        };
      },
    ~ms=isStreaming ? stepMs : 0,
  );

  Hooks.useInterval(
    () => setSpin((spin + 1) mod Array.length(spinnerFrames)),
    ~ms=isStreaming ? spinnerMs : 0,
  );

  /* The armed exit disarms itself after 1.5s. */
  Hooks.useTimeout(() => setConfirmExit(false), ~ms=confirmExit ? 1500 : 0);

  /* ---- keys --------------------------------------------------------------- */

  /* Global keys. Ctrl+C is the one key that always reaches here, and Esc and
     Shift+Tab are free to be app-level precisely because nothing is focusable
     (see the header comment). */
  Event.useKeyDown((key, mods) =>
    switch (key, mods) {
    | (Key.Char('c'), {Key.ctrl: true, _}) =>
      if (confirmExit) {
        /* Fullscreen makes both quit behaviors identical: leaving the
           alternate screen restores whatever the terminal showed before the
           app started, transcript and all. */
        quit(ClearScreen);
      } else {
        setConfirmExit(true);
      }
    | (Key.Tab, {Key.shift: true, _}) =>
      setModeIdx((modeIdx + 1) mod Array.length(modeLabels))
    | (Key.Escape, _) =>
      if (isStreaming) {
        interrupt();
      } else {
        clearInput();
      }
    | _ => ()
    }
  );

  /* The prompt's keys. No ~isActive gate: with nothing focusable, the input
     is the only thing that could want a keystroke. */
  Hooks.useInput((key, mods) =>
    switch (key, mods) {
    | (Key.Enter, m) when !m.meta && !m.ctrl =>
      if (menuOpen) {
        switch (List.nth_opt(filtered, selClamped)) {
        | Some(cmd) => runCommand(cmd)
        | None => ()
        };
      } else {
        submit();
      }
    /* Arrows drive the palette while it is open, and must NOT fall through:
       TextArea would move the cursor instead. */
    | (Key.Arrow_up, _) when menuOpen => setSel(max(0, selClamped - 1))
    | (Key.Arrow_down, _) when menuOpen =>
      setSel(min(max(0, filteredCount - 1), selClamped + 1))
    /* Key.Paste has no arm in TextArea.handleKeyDown at all - it is inserted
       here instead, the same way TextArea inserts a keypress internally. */
    | (Key.Paste(text), _) =>
      let (newText, newRow, newCol) =
        TextArea.insertAt(inputText, cursorRow, cursorCol, text);
      setInputText(newText);
      setCursor((newRow, newCol));
    /* Both are handled by the global useKeyDown above; they are claimed here
       so they cannot fall through into TextArea (whose Tab arm would insert
       two spaces) and so Esc is not handled twice. */
    | (Key.Escape, _) => ()
    | (Key.Tab, m) when m.shift => ()
    | _ =>
      TextArea.handleKeyDown(
        key,
        mods,
        inputText,
        setInputText,
        None,
        cursorRow,
        cursorCol,
        setCursor,
        selection,
        setSelection,
      )
    }
  );

  /* ---- the frame ----------------------------------------------------------- */

  let inner = max(1, width - 2);
  let topBorder = "╭" ++ repeatString("─", inner) ++ "╮";
  let bottomBorder = "╰" ++ repeatString("─", inner) ++ "╯";

  /* How tall the prompt is this frame. TextArea.measure answers with exactly
     the number of rows the TextArea below will paint for this value at this
     width, so the box, both gutters and the TextArea agree by construction -
     no guessing, and no chance of the borders drifting off the text. The
     TextArea's own ~minHeight/~maxHeight must be the ones passed here. */
  let inputWidth = max(1, width - 6);
  let inputRows =
    TextArea.measure(
      ~value=inputText,
      ~maxWidth=inputWidth,
      ~minHeight=1,
      ~maxHeight=5,
      (),
    );

  /* The gutters are as many lines as the input: "> " only on the first row,
     the way a shell prompt continues. */
  let leftGutter =
    String.concat(
      "\n",
      List.init(inputRows, i => i == 0 ? "│ > " : "│   "),
    );
  let rightGutter = String.concat("\n", List.init(inputRows, _ => " │"));

  let statusText =
    spinnerFrames[spin]
    ++ " "
    ++ thinkingVerbs[max(0, submitCount - 1) mod Array.length(thinkingVerbs)]
    ++ "… ("
    ++ string_of_int(ticks * stepMs / 1000)
    ++ "s · ↑ "
    ++ formatTokens(tokens)
    ++ " tokens · esc to interrupt)";

  <VStack>
    /* The history. Flex(1) is what makes the frame span the full terminal
       height from the very first frame: it absorbs every row the fixed-size
       rows below it do not want, which pins the status/input/hint block to
       the bottom of the viewport. focusable=false keeps this app's
       no-focusables invariant (see the header), so the wheel is the only
       thing that moves it - and CONTROLLED, so appending an item can snap
       the window back to the newest line. */
    <Sized size={Flex(1)}>
      <ScrollView
        focusable=false
        showScrollbar=true
        offset={
          switch (histOffset) {
          | None => 1_000_000
          | Some(o) => o
          }
        }
        onScroll={o => setHistOffset(Some(o))}>
        <VStack>
          ...{items |> List.map(it => <TranscriptItem it />)}
        </VStack>
      </ScrollView>
    </Sized>
    <Sized size={Chars(1)}>
      {isStreaming ? <Text color=Cyan> statusText </Text> : <Text> "" </Text>}
    </Sized>
    <Sized size={Chars(inputRows + 2)}>
      <VStack>
        <Text dim=true> topBorder </Text>
        <HStack>
          <Text dim=true> leftGutter </Text>
          <Sized size={Flex(1)}>
            <TextArea
              value=inputText
              onChange=setInputText
              placeholder="Try \"fix a bug\" or / for commands"
              maxWidth=inputWidth
              minHeight=1
              maxHeight=5
              cursorRow
              cursorCol
              setCursor
              selection
              setSelection
            />
          </Sized>
          <Text dim=true> rightGutter </Text>
        </HStack>
        <Text dim=true> bottomBorder </Text>
      </VStack>
    </Sized>
    {menuOpen
       ? <Sized size={Chars(menuRows)}>
           <ScrollView
             focusable=false
             offset=menuOffset
             onScroll={o =>
               setSel(min(max(0, filteredCount - 1), max(0, o + 2)))
             }>
             <VStack>
               ...{
                 filtered
                 |> List.mapi((i, cmd) =>
                      <CommandRow
                        key={string_of_int(i)}
                        cmd
                        selected={i == selClamped}
                        onRun={() => runCommand(cmd)}
                      />
                    )
               }
             </VStack>
           </ScrollView>
         </Sized>
       : Element.Empty}
    <Sized size={Chars(1)}>
      {confirmExit
         ? <Text color=Yellow> "Press ctrl-c again to exit" </Text>
         : <Text dim=true>
             {"? for shortcuts · "
              ++ modeLabels[modeIdx]
              ++ " (shift+tab to cycle)"}
           </Text>}
    </Sized>
  </VStack>;
};

module App = {
  let make = make;
};
