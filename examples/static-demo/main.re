/*
 * Static demo - a chat-style transcript built with <Static>.
 *
 * The transcript is APPEND-ONLY output: every message is rendered exactly
 * once, printed above the live region, and from then on it belongs to the
 * terminal's scrollback - it is never repainted, so the transcript can grow
 * without bound while the app keeps diffing only the three live rows at the
 * bottom.
 *
 * Keys:
 *   a / Enter  append the next message to the transcript
 *   w          write a raw line above the region with useStdout (the escape
 *              hatch for text that isn't a rendered item)
 *   q          quit
 *
 * Run it in a real terminal to see the point of inline rendering: the screen
 * is never cleared, the messages scroll away like ordinary command output,
 * and what is left after quitting is the whole transcript.
 */
open Matcha;

type message = {
  prompt: string,
  reply: string,
};

/* A fixed script, so the demo is deterministic (its headless output is a
 * golden test). A real app would append whatever it just received. */
let script = [|
  {prompt: "what is matcha?", reply: "A React-like framework for terminal UIs, in ReasonML."},
  {prompt: "how do I keep a transcript?", reply: "Put it in <Static>: every item is committed once and never repainted."},
  {prompt: "what stays live?", reply: "Everything below the Static node - the status row, the input row, a spinner."},
  {prompt: "and when it scrolls?", reply: "Committed lines scroll into the scrollback, exactly like command output."},
|];

/* One committed transcript entry. It renders on the frame that commits it
 * and is unmounted on the next one, so its mount effect runs exactly once -
 * which is why an item must not own anything ongoing (a timer, a
 * subscription); those belong in the live part of the tree. */
module Entry = {
  [@component]
  let make = (~msg: message, ~index: int) =>
    <VStack>
      <Text bold=true color=Green>
        {"> " ++ msg.prompt}
      </Text>
      <Text wrap=Wrap> {string_of_int(index + 1) ++ ". " ++ msg.reply} </Text>
      <Text> "" </Text>
    </VStack>;
};

[@component]
let make = () => {
  let quit = Event.useQuit();
  let stdout = Hooks.useStdout();
  /* Starts with the first message already in the transcript, so the very
     first frame commits something (and the headless golden shows what a
     static commit looks like ahead of the frame). */
  let (messages, setMessages) = Hooks.useState([script[0]]);
  let (writes, setWrites) = Hooks.useState(0);

  let sent = List.length(messages);

  Event.useKeyDown((key, _mods) =>
    switch (key) {
    | Key.Char('q')
    | Key.Char('Q') => quit(PreserveScreen)
    | Key.Char('a')
    | Key.Enter =>
      if (sent < Array.length(script)) {
        /* Append only: earlier items are already committed, and mutating
           them would have no effect on what is already on screen. */
        setMessages(messages @ [script[sent]]);
      }
    | Key.Char('w') =>
      stdout.write("[log] raw line #" ++ string_of_int(writes + 1));
      setWrites(writes + 1);
    | _ => ()
    }
  );

  <VStack>
    /* The transcript. Zero layout space: the live rows below are laid out
       as though this node were not there at all. */
    <Static
      items=messages
      renderItem={(msg, i) => <Entry msg index=i />}
    />
    <Text bold=true> "Matcha static demo" </Text>
    <Text>
      {"committed: "
       ++ string_of_int(sent)
       ++ "/"
       ++ string_of_int(Array.length(script))
       ++ "   raw writes: "
       ++ string_of_int(writes)}
    </Text>
    <Text dim=true> "a/Enter: append   w: write   q: quit" </Text>
  </VStack>;
};

module App = {
  let make = make;
};

let () = Runtime.start((module App));
