open Matcha;

[@component]
let make = () => {
  let quit = Event.useQuit();

  Event.useKeyDown((key, _modifiers) => {
    switch (key) {
    | Key.Char('q') | Key.Char('Q') => quit()
    | _ => ()
    }
  });

  <Column>
    <Bold> <Text> "Hello, World!" </Text> </Bold>
    <Text> "\nWelcome to Rere - a React-like terminal UI library for ReasonML." </Text>
    <Dim> <Text> "\n\nPress Q to quit." </Text> </Dim>
  </Column>;
};

module App = {
  let make = make;
};

let () = Runtime.start((module App));
