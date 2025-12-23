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
      <Text bold=true> name </Text>
      <Text> {"Count: " ++ string_of_int(count)} </Text>
      <Text dim=true> "Use +/- to change this profile's count." </Text>
    </Column>;
  };
};

/* Wrapper that simply renders Profile.
 * Used to validate that a different render path (Wrapper -> Profile)
 * does not reuse state from the direct Profile render.
 */
module Wrapper = {
  [@component]
  let make = (~name: string, ~start: int) => {
    <Profile name start />;
  };
};

/* Toggle mode used by the example */
type mode =
  | Direct
  | Wrapped;

[@component]
let make = () => {
  let quit = Event.useQuit();
  let (mode, setMode) = Component.useState(Direct);

  /* Global controls for toggling and quitting */
  Event.useKeyDown((key, _mods) =>
    switch (key) {
    | Key.Char('q') => quit(ClearScreen)
    | Key.Char('t') =>
      setMode(
        switch (mode) {
        | Direct => Wrapped
        | Wrapped => Direct
        },
      )
    | _ => ()
    }
  );

  /* Choose which render path to use (no keys passed) */
  let (_name, renderedProfile) =
    switch (mode) {
    | Direct =>
      let name = "Direct Profile";
      let start = 1;
      (name, <Profile name start />);
    | Wrapped =>
      let name = "Wrapped Profile";
      let start = 10;
      (name, <Wrapper name start />);
    };
  renderedProfile;
};

module App = {
  let make = make;
};

let () = Runtime.start((module App));
