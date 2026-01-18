/**
 * Test that useMemo works with functional values after the fix
 */
open Matcha;

module App = {
  [@component]
  let make = () => {
    let (count, setCount) = Component.useState(0);
    let quit = Event.useQuit();

    /* Create a function - this is a functional value */
    let transformer = (x) => x * 2;

    /* useMemo with functional value in deps */
    let result = Hooks.useMemo(
      () => {
        Printf.eprintf("Computing result... count=%d\n%!", count);
        transformer(count);
      },
      [|(Obj.repr(transformer): Obj.t), (Obj.repr(count): Obj.t)|],
    );

    Event.useKeyDown((key, _) => {
      switch (key) {
      | Key.Arrow_down =>
        Printf.eprintf("Arrow down - incrementing count\n%!");
        setCount(count + 1) /* THIS SHOULD NOT CRASH ANYMORE */
      | Key.Arrow_up =>
        if (count > 0) {
          setCount(count - 1)
        }
      | Key.Char('q')
      | Key.Escape => quit(ClearScreen)
      | _ => ()
      }
    });

    <VStack>
      <Text> "\n  useMemo Fix Test\n\n" </Text>
      <Text> {"  Count: " ++ string_of_int(count) ++ "\n"} </Text>
      <Text> {"  Result (count * 2): " ++ string_of_int(result) ++ "\n\n"} </Text>
      <Text dim=true> "  Press Arrow Down to increment (should not crash!)\n" </Text>
      <Text dim=true> "  Press Arrow Up to decrement\n" </Text>
      <Text dim=true> "  Press 'q' or Escape to quit\n" </Text>
    </VStack>;
  };
};

let () = Runtime.start((module App));
