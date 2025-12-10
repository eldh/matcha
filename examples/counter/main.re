open Matcha;

[@component]
let make = () => {
  let quit = Event.useQuit();
  let (count, setCount) = Component.useState(0);

  Event.useKeyDown((key, modifiers) => {
    switch (key, modifiers) {
    | (Key.Char('q'), _)
    | (Key.Char('Q'), _) => quit(ClearScreen)
    | (Key.Char('c'), {Key.ctrl: true, _}) => quit(ClearScreen)
    | (Key.Arrow_up, _)
    | (Key.Char('k'), _) => setCount(count + 1)
    | (Key.Arrow_down, _)
    | (Key.Char('j'), _) => setCount(max(0, count - 1))
    | (Key.Char('r'), _) => setCount(0)
    | _ => ()
    }
  });

  let countText = string_of_int(count);
  let barWidth = min(count, 50);
  let bar = String.make(barWidth, '#');

  <Column>
    <Bold> <Text> "Counter Example" </Text> </Bold>
    <Text> "\n" </Text>
    <Row>
      <Text> "Count: " </Text>
      <Bold> <Text> countText </Text> </Bold>
    </Row>
    <Text> "\n" </Text>
    <Dim>
      <Text> {"[" ++ bar ++ String.make(50 - barWidth, ' ') ++ "]"} </Text>
    </Dim>
    <Text> "\n\n" </Text>
    <Dim>
      <Text> "↑/k: Increment  ↓/j: Decrement  r: Reset  q: Quit" </Text>
    </Dim>
  </Column>;
};

module App = {
  let make = make;
};

let () = Runtime.start((module App));
