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

  /* Memoize derived strings so we don't rebuild them unnecessarily */
  let (countText, bar) =
    Hooks.useMemo(
      () => {
        let barWidth = min(count, 50);
        (
          string_of_int(count),
          String.make(barWidth, '#') ++ String.make(50 - barWidth, ' '),
        );
      },
      [|count|],
    );

  <Column>
    <Text bold=true> "Counter Example" </Text>
    <Text> "\n" </Text>
    <Row>
      <Text> "Count: " </Text>
      <Text bold=true> countText </Text>
    </Row>
    <Text> "\n" </Text>
    <Text dim=true> {"[" ++ bar ++ "]"} </Text>
    <Text> "\n\n" </Text>
    <Text dim=true>
      "↑/k: Increment  ↓/j: Decrement  r: Reset  q: Quit"
    </Text>
  </Column>;
};

module App = {
  let make = make;
};

let () = Runtime.start((module App));
