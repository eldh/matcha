open Matcha;

[@component]
let make = () => {
  let quit = Event.useQuit();

  Event.useKeyDown((key, _modifiers) => {
    switch (key) {
    | Key.Char('q')
    | Key.Char('Q') => quit(ClearScreen)
    | _ => ()
    }
  });

  <Column>
    <Text bold=true> "Hello, World!" </Text>
    <Text>
      "\nWelcome to Rere - a React-like terminal UI library for ReasonML."
    </Text>
    <Text dim=true> "\n\nPress Q to quit." </Text>
  </Column>;
};

module App = {
  let make = make;
};

let () = Runtime.start((module App));
