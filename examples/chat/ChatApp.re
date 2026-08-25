/*
 * Chat - a Claude Code-style chat layout, exercising most of Matcha's
 * capabilities in one app: an append-only <Static> transcript, a live
 * status row with a useInterval spinner, a focused <TextArea> input row
 * (Enter submits, paste inserts), and a <ScrollView> "context" side panel
 * with clickable rows.
 *
 * This is the reference app the docs point to for "how do I test an app
 * built on these capabilities" - see test/chat_tests.re, which drives every
 * one of them headlessly. Its golden (test/goldens/example-chat.txt) pins
 * the very first frame below.
 *
 * The component lives here, in its own little library (see the dune file),
 * with main.re a one-line launcher - so that test/chat_tests.re can start
 * THE SAME component in-process through Runtime.startHeadless. That split
 * is itself part of the recipe: structure an app as `library with App +
 * thin executable` and every test can drive the real thing.
 *
 * Keys:
 *   Enter        submit the input as a new message (canned echo reply)
 *   Tab          move focus between the input and the context panel
 *   arrows       scroll the context panel while it holds focus
 *   wheel        scroll the context panel regardless of focus
 *   click        pin/unpin a context row
 *   Ctrl+C       quit
 *
 * There is deliberately no "q to quit": the input owns every printable key
 * while it is focused (that's the point of a chat box), so binding a letter
 * to quit would make it untypeable. Ctrl+C is the one key that always
 * quits, focused input or not - see the global useKeyDown below.
 */
open Matcha;

type message = {
  prompt: string,
  reply: string,
};

/* Deterministic, so the transcript is reproducible for goldens/tests - a
   real app would put whatever it actually received here instead. */
let cannedReply = (prompt: string): string =>
  "Echo: "
  ++ prompt
  ++ ". (canned reply - examples/chat is a UI demo, not a real assistant)";

let spinnerFrames = [|"⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"|];

let contextFiles = [|
  "lib/Runtime.re",
  "lib/Hooks.re",
  "lib/Element.re",
  "lib/Key.re",
  "lib/ScrollView.re",
  "lib/StyledText.re",
  "test/chat_tests.re",
  "README.md",
|];

/* One committed transcript entry. Mounts on the frame that commits it and
   unmounts on the next one (see examples/static-demo's Entry for the full
   explanation) - which is why it must not own anything ongoing itself; the
   spinner and input state below all live in the root component instead. */
module Entry = {
  [@component]
  let make = (~msg: message) =>
    <VStack>
      <Text bold=true color=Green> {"> " ++ msg.prompt} </Text>
      <Text wrap=Wrap> msg.reply </Text>
      <Text> "" </Text>
    </VStack>;
};

/* One row of the context panel: a file path, starred when pinned. Its own
   [@component] (rather than inlined in the map below) so each row is a
   distinct instance - keyed by index - with its own useMouse registration
   underneath <Clickable>. */
module ContextRow = {
  [@component]
  let make = (~path: string, ~pinned: bool, ~onToggle: unit => unit) =>
    <Clickable onClick=onToggle>
      <Text color={pinned ? Yellow : White}>
        {(pinned ? "* " : "  ") ++ path}
      </Text>
    </Clickable>;
};

[@component]
let make = () => {
  let quit = Event.useQuit();

  /* The transcript - APPEND ONLY, per <Static>'s contract (see
     examples/static-demo's header comment for the full rule). */
  let (messages, setMessages) = Hooks.useState([]);

  /* The input row. TextArea is a controlled component: value, cursor and
     selection all live here, same shape as examples/textarea-demo. */
  let (inputText, setInputText) = Hooks.useState("");
  let (cursorRow, cursorCol, setCursor) = {
    let (pos, setPos) = Hooks.useState((0, 0));
    let (row, col) = pos;
    (row, col, setPos);
  };
  let (selection, setSelection) = Hooks.useState(None);

  /* "Thinking" window: true for ~1.5s after every submit. The spinner
     interval only ticks while this is true - ms=0 disables useInterval
     entirely (the Ink `delay=null` idiom), and flipping isThinking changes
     ms, which is what re-registers/cancels the timer; see useInterval's doc
     comment in lib/Hooks.re. */
  let (isThinking, setIsThinking) = Hooks.useState(false);
  let (frame, setFrame) = Hooks.useState(0);
  Hooks.useInterval(
    () => setFrame((frame + 1) mod Array.length(spinnerFrames)),
    ~ms=isThinking ? 120 : 0,
  );
  Hooks.useTimeout(() => setIsThinking(false), ~ms=isThinking ? 1500 : 0);

  /* Context panel - which rows are pinned, toggled by a click. */
  let (pinned, setPinned) =
    Hooks.useState(Array.make(Array.length(contextFiles), false));
  let togglePin = (i: int): unit => {
    let next = Array.copy(pinned);
    next[i] = !next[i];
    setPinned(next);
  };

  let {Hooks.isFocused: inputFocused} =
    Hooks.useFocus(~autoFocus=true, ~id="chat-input", ());

  let submit = (): unit =>
    if (String.trim(inputText) != "") {
      setMessages(
        messages @ [{prompt: inputText, reply: cannedReply(inputText)}],
      );
      setInputText("");
      setCursor((0, 0));
      setSelection(None);
      setIsThinking(true);
      setFrame(0);
    };

  /* Ctrl+C quits from anywhere - focused input or not. */
  Event.useKeyDown((key, mods) =>
    switch (key, mods) {
    | (Key.Char('c'), {Key.ctrl: true, _}) => quit(ClearScreen)
    | _ => ()
    }
  );

  /* The input's own keys, active only while it holds focus (the
     useFocus/useInput(~isActive=isFocused) idiom - see test/focus_tests.re).
     Enter submits rather than inserting a newline: TextArea.handleKeyDown's
     plain-Enter arm inserts one (Cmd+Enter is its own submit path), but a
     chat box wants plain Enter to send, so it is intercepted here before
     falling through to TextArea for everything else. Key.Paste has no arm
     in TextArea.handleKeyDown at all - it is inserted here instead, the
     same way TextArea inserts a Key.Text/Char keypress internally. */
  Hooks.useInput(~isActive=inputFocused, (key, mods) =>
    switch (key, mods) {
    | (Key.Enter, m) when !m.meta && !m.ctrl => submit()
    | (Key.Paste(text), _) =>
      let (newText, newRow, newCol) =
        TextArea.insertAt(inputText, cursorRow, cursorCol, text);
      setInputText(newText);
      setCursor((newRow, newCol));
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

  <VStack>
    <Static items=messages renderItem={(msg, _i) => <Entry msg />} />
    <Sized size={Chars(1)}>
      <Text color={isThinking ? Cyan : Green}>
        {isThinking ? spinnerFrames[frame] ++ " thinking..." : "* ready"}
      </Text>
    </Sized>
    <Sized size={Chars(6)}>
      <HStack gap=1>
        <Sized size={Flex(1)}>
          <TextArea
            value=inputText
            onChange=setInputText
            placeholder="Type a message... (Enter to send)"
            maxWidth=54
            minHeight=5
            maxHeight=5
            cursorRow
            cursorCol
            setCursor
            selection
            setSelection
          />
        </Sized>
        <Sized size={Chars(22)}>
          <VStack>
            <Text bold=true color=Cyan> "Context" </Text>
            <Sized size={Flex(1)}>
              <ScrollView id="context">
                <VStack>
                  ...{
                    contextFiles
                    |> Array.mapi((i, path) =>
                         <ContextRow
                           key={string_of_int(i)}
                           path
                           pinned={pinned[i]}
                           onToggle={() => togglePin(i)}
                         />
                       )
                    |> Array.to_list
                  }
                </VStack>
              </ScrollView>
            </Sized>
          </VStack>
        </Sized>
      </HStack>
    </Sized>
    <Sized size={Chars(1)}>
      <Text dim=true>
        "Enter: send   Tab: focus context   click: pin   Ctrl+C: quit"
      </Text>
    </Sized>
  </VStack>;
};

module App = {
  let make = make;
};
