/*
 * Tests for mouse dispatch (B4/S9): the bounds registry that origin
 * threading fills in, and the single-target hit test that reads it.
 *
 * The wire-level decode (Mouse.parseSgr/encodeSgr) is covered by
 * mouse_parse_tests.re; nothing here re-tests it. What these tests pin down
 * is everything ABOVE the decoder:
 *
 *   - which component a click at (x, y) is delivered to,
 *   - in which coordinate space that component sees it,
 *   - and which components correctly see nothing.
 *
 * They drive real applications through Runtime.startHeadless, because a
 * component's box is a property of the whole frame - layout decides it -
 * and handle.sendMouse (via Input.clickAt) is the headless equivalent of a
 * real terminal's SGR report: a headless frame IS the live region, so its
 * coordinates need no screen-row mapping.
 *
 * Every spy below is a plain ref, so a click changes no state and forces no
 * re-render: what is being asserted is the dispatch, not a repaint.
 */
open Matcha;

/* A left button-down event is what Input.clickAt sends; these build the
 * other kinds a test needs. */
let wheelUpAt = (~x: int, ~y: int): Mouse.event => {
  Mouse.kind: Mouse.ScrollUp,
  button: Mouse.NoButton,
  x,
  y,
  shift: false,
  alt: false,
  ctrl: false,
};

let releaseAt = (~x: int, ~y: int): Mouse.event => {
  Mouse.kind: Mouse.Up,
  button: Mouse.Left,
  x,
  y,
  shift: false,
  alt: false,
  ctrl: false,
};

/* ============================================================================
 * 1. Row targeting: two <Clickable> rows, one line each
 * ========================================================================== */

let rowClicks = [|0, 0|];

module TwoRowsApp = {
  [@component]
  let make = () =>
    <VStack>
      <Sized size={Chars(1)}>
        <Clickable onClick={() => rowClicks[0] = rowClicks[0] + 1}>
          <Text> "row zero" </Text>
        </Clickable>
      </Sized>
      <Sized size={Chars(1)}>
        <Clickable onClick={() => rowClicks[1] = rowClicks[1] + 1}>
          <Text> "row one" </Text>
        </Clickable>
      </Sized>
    </VStack>;
};

/* ============================================================================
 * 2. Rebasing: a spy component that records the coordinates it is given
 * ========================================================================== */

let spyEvents: ref(list(Mouse.event)) = ref([]);

module CoordSpy = {
  [@component]
  let make = () => {
    Hooks.useMouse(ev => spyEvents := [ev, ...spyEvents^]);
    <Text> "spy" </Text>;
  };
};

/* Two rows of filler above the spy, so the spy's box starts at y=2 and the
 * rebased coordinates differ from the absolute ones. */
module OffsetSpyApp = {
  [@component]
  let make = () =>
    <VStack>
      <Sized size={Chars(2)}> <Text> "filler" </Text> </Sized>
      <Sized size={Chars(3)}> <CoordSpy /> </Sized>
    </VStack>;
};

/* ============================================================================
 * 3. Nesting: a Clickable wrapping a stack that contains another Clickable
 * ========================================================================== */

let innerClicks = ref(0);
let outerClicks = ref(0);

module NestedApp = {
  [@component]
  let make = () =>
    <Clickable onClick={() => outerClicks := outerClicks^ + 1}>
      <VStack>
        <Sized size={Chars(1)}>
          <Clickable onClick={() => innerClicks := innerClicks^ + 1}>
            <Text> "inner" </Text>
          </Clickable>
        </Sized>
        <Sized size={Chars(1)}> <Text> "plain" </Text> </Sized>
      </VStack>
    </Clickable>;
};

/* ============================================================================
 * 4/5. Root-level useMouse: the global escape hatch
 *
 * The root application component's hooks run in the ROOT context (the loops
 * call C.make() with it current), which is what makes this handler the
 * always-runs one - and the one that keeps ABSOLUTE coordinates.
 * ========================================================================== */

let rootEvents: ref(list(Mouse.event)) = ref([]);
let targetClicks = ref(0);

module RootHandlerApp = {
  [@component]
  let make = () => {
    Hooks.useMouse(ev => rootEvents := [ev, ...rootEvents^]);
    <VStack>
      <Sized size={Chars(1)}>
        <Clickable onClick={() => targetClicks := targetClicks^ + 1}>
          <Text> "target" </Text>
        </Clickable>
      </Sized>
      <Sized size={Chars(1)}> <Text> "not a target" </Text> </Sized>
    </VStack>;
  };
};

/* ============================================================================
 * 6. HStack targeting: two components side by side
 * ========================================================================== */

let halfClicks = [|0, 0|];

module TwoColumnsApp = {
  [@component]
  let make = () =>
    <HStack>
      <Sized size={Chars(10)}>
        <Clickable onClick={() => halfClicks[0] = halfClicks[0] + 1}>
          <Text> "left" </Text>
        </Clickable>
      </Sized>
      <Sized size={Chars(10)}>
        <Clickable onClick={() => halfClicks[1] = halfClicks[1] + 1}>
          <Text> "right" </Text>
        </Clickable>
      </Sized>
    </HStack>;
};

/* An HStack that centers a one-line child in a five-line container: the
 * child's box is shifted DOWN by the alignment, and only the shifted rows
 * are clickable. */
module CenteredColumnApp = {
  [@component]
  let make = () =>
    <HStack align=AlignCenter>
      <Sized size={Chars(10)}> <CoordSpy /> </Sized>
      <Sized size={Chars(10)}> <Text> "pad" </Text> </Sized>
    </HStack>;
};

/* ============================================================================
 * 7. Static exclusion: a <Static> sibling takes no layout space, so it must
 *    not push the clickable row down either
 * ========================================================================== */

let staticRowClicks = ref(0);

module StaticSiblingApp = {
  [@component]
  let make = () =>
    <VStack>
      <Static
        items=["committed"]
        renderItem={(t, _i) => <Text> t </Text>}
      />
      <Sized size={Chars(1)}>
        <Clickable onClick={() => staticRowClicks := staticRowClicks^ + 1}>
          <Text> "clickable row" </Text>
        </Clickable>
      </Sized>
    </VStack>;
};

/* An application with no useMouse anywhere: what every application written
 * before B4 looks like, and what must never switch the terminal into mouse
 * reporting mode. */
module NoMouseApp = {
  [@component]
  let make = () => <Text> "nothing clickable here" </Text>;
};

/* ============================================================================
 * Tests
 * ========================================================================== */

let run = () =>
  Test.group("Mouse dispatch", () => {
    Test.run("a click hits the row whose box contains it", () => {
      rowClicks[0] = 0;
      rowClicks[1] = 0;
      let handle = Runtime.startHeadless((module TwoRowsApp));

      Input.clickAt(handle, ~x=0, ~y=0);
      Test.assertEqual(rowClicks[0], 1, "the first row fired");
      Test.assertEqual(rowClicks[1], 0, "and only the first row");

      Input.clickAt(handle, ~x=3, ~y=1);
      Test.assertEqual(rowClicks[1], 1, "the second row fired");
      Test.assertEqual(rowClicks[0], 1, "and the first one did not fire again");

      handle.quit();
    });

    Test.run("a click below every row hits nothing", () => {
      rowClicks[0] = 0;
      rowClicks[1] = 0;
      let handle = Runtime.startHeadless((module TwoRowsApp));
      Input.clickAt(handle, ~x=0, ~y=5);
      Test.assertEqual(rowClicks[0], 0, "row zero is not a catch-all");
      Test.assertEqual(rowClicks[1], 0, "neither is row one");
      handle.quit();
    });

    Test.run("the target sees coordinates rebased to its own box", () => {
      spyEvents := [];
      let handle = Runtime.startHeadless((module OffsetSpyApp));

      /* The spy's box is rows 2..4; (5, 3) is its second row, fifth column. */
      Input.clickAt(handle, ~x=5, ~y=3);
      switch (spyEvents^) {
      | [ev] =>
        Test.assertEqual(ev.Mouse.x, 5, "x is unchanged (the box starts at column 0)");
        Test.assertEqual(ev.Mouse.y, 1, "y is rebased against the box's top row");
        Test.assertTrue(ev.Mouse.kind == Mouse.Down, "the kind survives rebasing");
        Test.assertTrue(ev.Mouse.button == Mouse.Left, "so does the button");
      | other =>
        Test.assertEqual(
          List.length(other),
          1,
          "the spy received exactly one event",
        )
      };

      /* One row above its box: not the spy's event. */
      Input.clickAt(handle, ~x=5, ~y=1);
      Test.assertEqual(
        List.length(spyEvents^),
        1,
        "a click above the box does not reach the spy",
      );
      handle.quit();
    });

    Test.run("nested clickables: the innermost one wins", () => {
      innerClicks := 0;
      outerClicks := 0;
      let handle = Runtime.startHeadless((module NestedApp));

      Input.clickAt(handle, ~x=2, ~y=0);
      Test.assertEqual(innerClicks^, 1, "the inner clickable fired");
      Test.assertEqual(outerClicks^, 0, "the outer one stayed silent");

      /* Second row: inside the outer box, outside the inner one. */
      Input.clickAt(handle, ~x=2, ~y=1);
      Test.assertEqual(outerClicks^, 1, "the outer clickable fired there");
      Test.assertEqual(innerClicks^, 1, "and the inner one did not");

      handle.quit();
    });

    Test.run("a root useMouse handler sees every event, absolute", () => {
      rootEvents := [];
      targetClicks := 0;
      let handle = Runtime.startHeadless((module RootHandlerApp));

      Input.clickAt(handle, ~x=4, ~y=0);
      Test.assertEqual(targetClicks^, 1, "the clickable row fired");
      switch (rootEvents^) {
      | [ev, ..._] =>
        Test.assertEqual(ev.Mouse.x, 4, "the root sees the absolute x");
        Test.assertEqual(ev.Mouse.y, 0, "and the absolute y");
      | [] => Test.assertTrue(false, "the root handler ran")
      };

      /* A row with no handler on it: only the root hears about it. */
      Input.clickAt(handle, ~x=4, ~y=1);
      Test.assertEqual(targetClicks^, 1, "no component consumed it");
      Test.assertEqual(List.length(rootEvents^), 2, "the root still saw it");
      switch (rootEvents^) {
      | [ev, ..._] =>
        Test.assertEqual(ev.Mouse.y, 1, "with unrebased coordinates")
      | [] => Test.assertTrue(false, "the root handler ran")
      };

      /* Every kind, not just clicks. */
      handle.sendMouse(releaseAt(~x=4, ~y=0));
      handle.sendMouse(wheelUpAt(~x=4, ~y=1));
      Test.assertEqual(
        List.length(rootEvents^),
        4,
        "release and wheel reach the root too",
      );
      Test.assertEqual(
        targetClicks^,
        1,
        "and a release over the target is not a click",
      );

      handle.quit();
    });

    Test.run("wheel events reach the component under the cursor", () => {
      spyEvents := [];
      let handle = Runtime.startHeadless((module OffsetSpyApp));
      handle.sendMouse(wheelUpAt(~x=1, ~y=4));
      switch (spyEvents^) {
      | [ev] =>
        Test.assertTrue(ev.Mouse.kind == Mouse.ScrollUp, "the wheel event arrived");
        Test.assertEqual(ev.Mouse.y, 2, "rebased like any other event");
      | other =>
        Test.assertEqual(List.length(other), 1, "exactly one event arrived")
      };
      handle.quit();
    });

    Test.run("an HStack routes each column to its own component", () => {
      halfClicks[0] = 0;
      halfClicks[1] = 0;
      let config: Runtime.headlessConfig = {width: 20, height: 3};
      let handle = Runtime.startHeadless(~config, (module TwoColumnsApp));

      Input.clickAt(handle, ~x=3, ~y=1);
      Test.assertEqual(halfClicks[0], 1, "the left column fired");
      Test.assertEqual(halfClicks[1], 0, "and only the left one");

      Input.clickAt(handle, ~x=13, ~y=1);
      Test.assertEqual(halfClicks[1], 1, "the right column fired");
      Test.assertEqual(halfClicks[0], 1, "and the left one did not fire again");

      handle.quit();
    });

    Test.run("cross-axis alignment moves the recorded box", () => {
      spyEvents := [];
      let config: Runtime.headlessConfig = {width: 20, height: 5};
      let handle = Runtime.startHeadless(~config, (module CenteredColumnApp));

      /* One line centered in five: the box is row 2, and nothing else. */
      Input.clickAt(handle, ~x=1, ~y=0);
      Test.assertEqual(
        List.length(spyEvents^),
        0,
        "the top row is alignment padding, not the child",
      );

      Input.clickAt(handle, ~x=1, ~y=2);
      switch (spyEvents^) {
      | [ev] =>
        Test.assertEqual(ev.Mouse.y, 0, "the centered row IS the child's row 0")
      | other =>
        Test.assertEqual(List.length(other), 1, "the centered row hit the child")
      };
      handle.quit();
    });

    /* The one piece of the pipeline the headless handle bypasses: a real
     * terminal reports SCREEN rows, and Runtime.deliverAll subtracts the
     * live region's top row before dispatching. Driven directly here (with
     * the root context the running handle installed) because a live region
     * that does NOT start at the top of the screen only ever happens in the
     * interactive loop. */
    Test.run("screen rows are mapped into the live region", () => {
      rootEvents := [];
      targetClicks := 0;
      let handle = Runtime.startHeadless((module RootHandlerApp));
      switch (Hooks.instance().rootContext^) {
      | None => Test.assertTrue(false, "the app installed a root context")
      | Some(ctx) =>
        /* Region top at screen row 3, i.e. 0-based row 2. A report on
           0-based screen row 2 is the region's first row. */
        let liveTop = () => 3;
        Runtime.deliverAll(
          ~liveTop,
          ctx,
          [InputDecoder.MouseEvent(releaseAt(~x=4, ~y=2))],
        );
        switch (rootEvents^) {
        | [ev, ..._] =>
          Test.assertEqual(ev.Mouse.y, 0, "screen row 2 is frame row 0");
          Test.assertEqual(ev.Mouse.x, 4, "columns are not shifted")
        | [] => Test.assertTrue(false, "the mapped event was dispatched")
        };

        /* Above the region - a click in the scrollback or in committed
           <Static> output. It belongs to the transcript, not to the app. */
        Runtime.deliverAll(
          ~liveTop,
          ctx,
          [InputDecoder.MouseEvent(releaseAt(~x=4, ~y=1))],
        );
        Test.assertEqual(
          List.length(rootEvents^),
          1,
          "an event above the live region is dropped",
        );
      };
      handle.quit();
    });

    /* The predicate behind auto-enable: the interactive loop calls this
     * after every commit and flips the terminal's mouse mode on the
     * transition (Terminal.enableMouse/disableMouse). Asserting it here is
     * what makes the interactive-only escape emission a two-line
     * consequence of a tested decision. */
    Test.run("interest tracking: only apps with handlers want mouse", () => {
      let plain = Runtime.startHeadless((module NoMouseApp));
      ignore(plain.render());
      Test.assertFalse(
        Hooks.hasMouseHandlers(),
        "an app with no useMouse never asks for mouse reporting",
      );
      plain.quit();

      let clickable = Runtime.startHeadless((module TwoRowsApp));
      ignore(clickable.render());
      Test.assertTrue(
        Hooks.hasMouseHandlers(),
        "a <Clickable> in the tree does",
      );
      clickable.quit();

      let rootOnly = Runtime.startHeadless((module RootHandlerApp));
      ignore(rootOnly.render());
      Test.assertTrue(
        Hooks.hasMouseHandlers(),
        "and so does a root-level useMouse",
      );
      rootOnly.quit();
    });

    Test.run("a Static sibling does not shift a clickable's box", () => {
      staticRowClicks := 0;
      let handle = Runtime.startHeadless((module StaticSiblingApp));
      Test.assertContains(
        handle.getStaticOutput(true),
        "committed",
        "the Static item was committed above the live region",
      );
      Input.clickAt(handle, ~x=0, ~y=0);
      Test.assertEqual(
        staticRowClicks^,
        1,
        "the clickable row still starts at the top of the frame",
      );
      handle.quit();
    });
  });
