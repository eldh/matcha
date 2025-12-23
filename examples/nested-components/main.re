open Matcha;

/* A simple component with no arguments */
module Header = {
  [@component]
  let make = () => {
    <Text bold=true> "=== Nested Components Example ===" </Text>;
  };
};

/* Another no-argument component */
module Footer = {
  [@component]
  let make = () => {
    <Text dim=true> "\n\nPress Q to quit." </Text>;
  };
};

/* A component with arguments */
module Greeting = {
  [@component]
  let make = (~name: string) => {
    let {TerminalContext.width, height} = TerminalContext.use();
    <Text>
      {"Hello, "
       ++ name
       ++ "! "
       ++ string_of_int(width)
       ++ "x"
       ++ string_of_int(height)}
    </Text>;
  };
};

module WrappedApp = {
  /* Main app using all the components */
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
      <Header />
      <Text> "\n" </Text>
      <Greeting name="World" />
      <Text> "\n" </Text>
      <Greeting name="ReasonML" />
      <Footer />
    </Column>;
  };
};

[@component]
let make = () => {
  <TerminalContext.Provider> <WrappedApp /> </TerminalContext.Provider>;
};

module App = {
  let make = make;
};

let () = Runtime.start((module App));
