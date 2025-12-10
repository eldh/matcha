open Rere;

let people: array(Types.person) = [|
  {
    name: "Alice",
    age: 28,
  },
  {
    name: "Bob",
    age: 34,
  },
  {
    name: "Charlie",
    age: 22,
  },
  {
    name: "Diana",
    age: 45,
  },
  {
    name: "Eve",
    age: 31,
  },
|];

[@component]
let make = () => {
  let quit = Event.useQuit();

  Event.useKeyDown((key, modifiers) => {
    switch (key, modifiers) {
    | (Key.Char('q'), _)
    | (Key.Char('Q'), _)
    | (Key.Char('c'), {Key.ctrl: true, _}) => quit()
    | _ => ()
    }
  });

  <TerminalContext.Provider> <PeopleList people /> </TerminalContext.Provider>;
};
