open Matcha;

module Profile = {
  [@component]
  let make = (~name: string, ~start: int) => {
    let (count, setCount) = Component.useState(start);

    /* Local key handlers only affect the active profile */
    Event.useKeyDown((key, _mods) =>
      switch (key) {
      | Key.Char('+') => setCount(count + 1)
      | Key.Char('-') => setCount(count - 1)
      | _ => ()
      }
    );

    <Column>
      <Bold> <Text> name </Text> </Bold>
      <Text> {"Count: " ++ string_of_int(count)} </Text>
      <Dim> <Text> "Use +/- to change this profile's count." </Text> </Dim>
    </Column>;
  };
};

[@component]
let make = () => {
  let quit = Event.useQuit();
  let (useFirst, setUseFirst) = Component.useState(true);

  /* Global controls for toggling and quitting */
  Event.useKeyDown((key, _mods) =>
    switch (key) {
    | Key.Char('q') => quit(ClearScreen)
    | Key.Char('t') => setUseFirst(!useFirst)
    | _ => ()
    }
  );

  /* Active profile changes tree ownership of the same component type */
  let (name, start, keyValue) =
    useFirst ? ("Alice", 1, "alice") : ("Bob", 10, "bob");

  <Column>
    <Bold> <Text> "Keyed component switch demo" </Text> </Bold>
    <Text> "Press t to toggle between Alice and Bob. q to quit." </Text>
    <Text> {"Active profile: " ++ name ++ " (key: " ++ keyValue ++ ")"} </Text>
    <Dim>
      <Text>
        "Each profile uses the same component type. With correct key handling, switching shows its own initial count instead of reusing the previous profile's state."
      </Text>
    </Dim>
    <Dim>
      <Text> "Press +/- to adjust the active profile's counter." </Text>
    </Dim>
    <Profile key=keyValue name start />
  </Column>;
};

module App = {
  let make = make;
};

let () = Runtime.start((module App));
