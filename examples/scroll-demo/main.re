/*
 * Scroll demo - a thirty-row list inside a window that shows a handful of
 * rows at a time (B5).
 *
 * The point of the layout below is the SIZING rule: a ScrollView's natural
 * size is its content, so it only scrolls once something caps its height.
 * Here the title and the hint row take one line each and the ScrollView sits
 * in the Flex(1) slot between them, which is what turns thirty rows into a
 * window plus a scrollbar.
 *
 * Keys:
 *   up / down          scroll one row
 *   pgup / pgdn        scroll one window, minus a row of overlap
 *   home / end         jump to the top / bottom
 *   q                  quit
 *
 * The list is the only focusable thing here, so it holds the focus from the
 * first frame and the arrows work immediately.
 *
 * The mouse wheel scrolls it too, without focus - run it in a real terminal
 * and roll the wheel over the list.
 */
open Matcha;

/* A fixed list, so the demo is deterministic (its headless first frame is a
 * golden test). */
let rows =
  List.init(30, i =>
    Printf.sprintf("%2d. row number %d of thirty", i + 1, i + 1)
  );

[@component]
let make = () => {
  let quit = Event.useQuit();

  Event.useKeyDown((key, _modifiers) =>
    switch (key) {
    | Key.Char('q')
    | Key.Char('Q') => quit(ClearScreen)
    | _ => ()
    }
  );

  <VStack>
    <Sized size={Chars(1)}>
      <Text bold=true color=Cyan> "Scroll demo - 30 rows, one small window" </Text>
    </Sized>
    <Sized size={Flex(1)}>
      <ScrollView id="list">
        <VStack>
          ...{rows |> List.map(row => <Text> row </Text>)}
        </VStack>
      </ScrollView>
    </Sized>
    <Sized size={Chars(1)}>
      <Text dim=true>
        "up/down  pgup/pgdn  home/end  wheel   q: Quit"
      </Text>
    </Sized>
  </VStack>;
};

module App = {
  let make = make;
};

let () = Runtime.start(~screen=Fullscreen, (module App));
