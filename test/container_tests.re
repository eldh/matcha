/*
 * Container queries (A1).
 *
 * <Container> is a QUERY boundary and nothing else: useContainerSize()
 * reports the nearest enclosing container's box, or the whole frame when
 * there is none. Two properties are worth separate proof:
 *
 *   1. the query resolves to the right box (nesting, siblings, no container
 *      at all, resize), and
 *   2. the wrapper is LAYOUT-TRANSPARENT - a frame rendered with containers
 *      is byte-identical to the same frame rendered without them.
 *
 * Every case runs at a NON-80x24 size on purpose: 80x24 is the constraints
 * default, the headless-config default and the getSize fallback all at once,
 * so at that size a stale value and a computed one are indistinguishable.
 */
open Matcha;

/* Echoes whatever useContainerSize() reports, tagged so several of them can
 * share a frame. */
module Echo = {
  [@component]
  let make = (~tag: string) => {
    let c = useContainerSize();
    <Text>
      {tag
       ++ ":"
       ++ string_of_int(c.Runtime.availWidth)
       ++ "x"
       ++ string_of_int(c.Runtime.availHeight)}
    </Text>;
  };
};

/* Responsive-by-container: the same component says something different
 * depending on the region it was placed in, at the SAME terminal size. */
module Responsive = {
  [@component]
  let make = (~tag: string) => {
    let c = useContainerSize();
    <Text> {tag ++ ":" ++ (c.Runtime.availWidth >= 40 ? "wide" : "narrow")} </Text>;
  };
};

/* No <Container> anywhere: the query has to fall back to the frame, even
 * though the VStack slot this component sits in is one row tall. */
module NoContainer = {
  [@component]
  let make = () =>
    <VStack>
      <Sized size={Chars(1)}> <Echo tag="root" /> </Sized>
      <Sized size={Chars(1)}> <Text> "filler" </Text> </Sized>
    </VStack>;
};

/* Two panes, each its own container. Same component, different answers. */
module TwoPanes = {
  [@component]
  let make = () =>
    <HStack>
      <Sized size={Chars(20)}>
        <Container> <Echo tag="left" /> </Container>
      </Sized>
      <Sized size={Flex(1)}>
        <Container> <Echo tag="right" /> </Container>
      </Sized>
    </HStack>;
};

/* A container inside a container: the INNERMOST one wins, and a sibling
 * outside it still sees the outer pane. */
module Nested = {
  [@component]
  let make = () =>
    <HStack>
      <Sized size={Chars(30)}>
        <Container>
          <VStack>
            <Sized size={Chars(2)}>
              <Container> <Echo tag="inner" /> </Container>
            </Sized>
            <Sized size={Flex(1)}> <Echo tag="outer" /> </Sized>
          </VStack>
        </Container>
      </Sized>
      <Sized size={Flex(1)}> <Text> "pad" </Text> </Sized>
    </HStack>;
};

/* A width threshold decided by the container, not by the terminal: at 100
 * columns the narrow pane must still say "narrow". */
module Thresholds = {
  [@component]
  let make = () =>
    <HStack>
      <Sized size={Chars(30)}>
        <Container> <Responsive tag="pane" /> </Container>
      </Sized>
      <Sized size={Flex(1)}> <Responsive tag="frame" /> </Sized>
    </HStack>;
};

/* ---------------------------------------------------------------------
 * Transparency fixtures.
 *
 * The same tree twice, once with <Container> wrappers at three different
 * places (around a Sized child, around a plain child, and around an Empty).
 * Neither fixture queries anything - the point is purely what the renderer
 * puts on screen.
 * ------------------------------------------------------------------- */

let transparentBody = (wrap: Element.t => Element.t) =>
  <VStack gap=1>
    <HStack gap=2>
      {wrap(<Sized size={Chars(6)}> <Text> "abcdefghij" </Text> </Sized>)}
      <Sized size={Flex(1)}> <Text> "right" </Text> </Sized>
    </HStack>
    {wrap(Element.empty)}
    {wrap(<Text wrap=Wrap> "a fairly long line that has to wrap somewhere" </Text>)}
    <Text> "tail" </Text>
  </VStack>;

module Plain = {
  [@component]
  let make = () => transparentBody(el => el);
};

module Wrapped = {
  [@component]
  let make = () => transparentBody(el => <Container> el </Container>);
};

/* Hook identity must survive a Container appearing above a component: the
 * wrapper renders its child at the SAME tree path, so the counter keeps its
 * state rather than remounting. Two apps that differ only by the wrapper
 * must therefore agree after the same key presses. */
module Counter = {
  [@component]
  let make = () => {
    let (n, setN) = Hooks.useState(0);
    Event.useKeyDown((key, _) =>
      switch (key) {
      | Key.Arrow_up => setN(n + 1)
      | _ => ()
      }
    );
    <Text> {"n=" ++ string_of_int(n)} </Text>;
  };
};

module CounterPlain = {
  [@component]
  let make = () => <VStack> <Counter /> <Text> "tail" </Text> </VStack>;
};

module CounterWrapped = {
  [@component]
  let make = () =>
    <VStack>
      <Container> <Counter /> </Container>
      <Text> "tail" </Text>
    </VStack>;
};

let run = () => {
  Test.group("Container queries", () => {
    Test.run("with no <Container> anywhere, the query is the frame", () => {
      let config: Runtime.headlessConfig = {width: 57, height: 9};
      let handle = Runtime.startHeadless(~config, (module NoContainer));
      Test.assertContains(
        handle.getOutput(true),
        "root:57x9",
        "a component in a 1-row slot still reports the whole frame",
      );
      handle.quit();
    });

    Test.run("a <Container> in a pane reports the PANE's box", () => {
      let config: Runtime.headlessConfig = {width: 50, height: 6};
      let handle = Runtime.startHeadless(~config, (module TwoPanes));
      let out = handle.getOutput(true);
      Test.assertContains(
        out,
        "left:20x6",
        "the Chars(20) pane, not the 50-column frame",
      );
      Test.assertContains(
        out,
        "right:30x6",
        "sibling containers report their own boxes",
      );
      handle.quit();
    });

    Test.run("nested containers resolve to the innermost", () => {
      let config: Runtime.headlessConfig = {width: 44, height: 7};
      let handle = Runtime.startHeadless(~config, (module Nested));
      let out = handle.getOutput(true);
      Test.assertContains(
        out,
        "inner:30x2",
        "the inner Chars(2) container wins over the Chars(30) one around it",
      );
      Test.assertContains(
        out,
        "outer:30x7",
        "a sibling outside the inner container still sees the outer pane",
      );
      handle.quit();
    });

    Test.run("a resize reaches container queries", () => {
      let config: Runtime.headlessConfig = {width: 50, height: 6};
      let handle = Runtime.startHeadless(~config, (module TwoPanes));
      Test.assertContains(handle.getOutput(true), "right:30x6", "before");
      handle.resize(70, 11);
      let out = handle.getOutput(true);
      Test.assertContains(
        out,
        "left:20x11",
        "the fixed pane keeps its width and grows in height",
      );
      Test.assertContains(
        out,
        "right:50x11",
        "the flex pane absorbs the extra columns",
      );
      handle.quit();
    });

    Test.run("a width threshold flips at the container, not the terminal", () => {
      /* 100 columns wide, so "narrow" can only come from the container. */
      let config: Runtime.headlessConfig = {width: 100, height: 5};
      let handle = Runtime.startHeadless(~config, (module Thresholds));
      let out = handle.getOutput(true);
      Test.assertContains(
        out,
        "frame:wide",
        "outside every container the frame's 100 columns decide",
      );
      Test.assertContains(
        out,
        "pane:narrow",
        "inside a Chars(30) container the pane decides - the whole point",
      );
      handle.quit();
    });
  });

  Test.group("Container transparency", () => {
    Test.run("wrapping in <Container> renders byte-identical frames", () => {
      let config: Runtime.headlessConfig = {width: 34, height: 12};
      let plain = Runtime.startHeadless(~config, (module Plain));
      let wrapped = Runtime.startHeadless(~config, (module Wrapped));
      Test.assertEqualStr(
        wrapped.getOutput(false),
        plain.getOutput(false),
        "a Container changes no cell: size hints, Empty's zero-space rule "
        ++ "and wrapping all see straight through it",
      );
      plain.quit();
      wrapped.quit();
    });

    Test.run("a <Container> does not disturb component identity", () => {
      let config: Runtime.headlessConfig = {width: 21, height: 4};
      let plain = Runtime.startHeadless(~config, (module CounterPlain));
      let wrapped = Runtime.startHeadless(~config, (module CounterWrapped));
      plain.sendKey(Key.Arrow_up, Key.noModifiers);
      plain.sendKey(Key.Arrow_up, Key.noModifiers);
      wrapped.sendKey(Key.Arrow_up, Key.noModifiers);
      wrapped.sendKey(Key.Arrow_up, Key.noModifiers);
      Test.assertEqualStr(
        wrapped.getOutput(true),
        plain.getOutput(true),
        "the child renders at the same tree path with or without the "
        ++ "wrapper, so its hooks context is the same one",
      );
      Test.assertContains(wrapped.getOutput(true), "n=2", "state advanced");
      plain.quit();
      wrapped.quit();
    });
  });
};
