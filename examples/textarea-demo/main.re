open Matcha;

/* Demo of the TextArea multi-line editor component */

[@component]
let make = () => {
  let quit = Event.useQuit();
  let (text, setText) = Component.useState("Hello, world!\nThis is a multi-line text editor.\n\nTry these shortcuts:\n- Arrow keys to move\n- Cmd+Arrow to jump to line/doc boundaries\n- Alt+Arrow to move by word\n- Shift+Arrow to select\n- Cmd+Enter to submit\n- Enter for new line");
  let (cursorRow, cursorCol, setCursor) = {
    let (pos, setPos) = Component.useState((0, 0));
    let (row, col) = pos;
    (row, col, setPos);
  };
  let (selection, setSelection) = Component.useState(None);
  let (submitted, setSubmitted) = Component.useState(false);

  /* Handle keyboard events for the text area */
  Event.useKeyDown((key, modifiers) => {
    /* Check for quit first */
    switch (key) {
    | Key.Escape => quit(ClearScreen)
    | _ =>
      TextArea.handleKeyDown(
        key,
        modifiers,
        text,
        setText,
        Some(() => setSubmitted(true)),
        cursorRow,
        cursorCol,
        setCursor,
        selection,
        setSelection,
      )
    };
  });

  /* Get layout constraints */
  let {Runtime.availWidth: width, _} = useLayout();
  let editorWidth = min(60, width - 4);

  let header = "TextArea Demo - Press ESC to quit";
  let instructions =
    if (submitted) {
      "Submitted! (Cmd+Enter pressed)";
    } else {
      "Cmd+Enter to submit";
    };

  <VStack gap=1>
    <Text bold=true color=Cyan> header </Text>
    <Text dim=true> instructions </Text>
    <Text> "" </Text>
    <TextArea
      value=text
      onChange=setText
      onSubmit={() => setSubmitted(true)}
      placeholder="Type something..."
      maxWidth=editorWidth
      minHeight=10
      maxHeight=20
      cursorRow
      cursorCol
      setCursor
      selection
      setSelection
    />
    <Text> "" </Text>
    <Text dim=true>
      {
        "Cursor: ("
        ++ string_of_int(cursorRow)
        ++ ", "
        ++ string_of_int(cursorCol)
        ++ ")"
      }
    </Text>
  </VStack>;
};

module App = {
  let make = make;
};

let () = Runtime.start((module App));

