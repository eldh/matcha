/*
 * Tests for <Static>, the static drain and useStdout (A4).
 *
 * These drive real applications through Runtime.startHeadless, because the
 * behavior under test is a property of the WHOLE frame cycle - render,
 * drain, advance the watermark - not of any one function. The two handle
 * methods used throughout say it all:
 *
 *   handle.getStaticOutput(true)  everything committed above the live
 *                                 region so far, accumulated across frames
 *   handle.getOutput(true)        the current live frame, and nothing else
 *
 * The central claim being tested is "exactly once, ever": an item committed
 * on one frame must appear in the transcript once, must never be committed
 * again however many frames follow, and must never appear in the live frame.
 */
open Matcha;

/* Number of non-overlapping occurrences of [needle] in [haystack]. */
let countOccurrences = (haystack: string, needle: string): int => {
  let hlen = String.length(haystack);
  let nlen = String.length(needle);
  if (nlen == 0 || nlen > hlen) {
    0;
  } else {
    let count = ref(0);
    let i = ref(0);
    while (i^ <= hlen - nlen) {
      if (String.sub(haystack, i^, nlen) == needle) {
        count := count^ + 1;
        i := i^ + nlen;
      } else {
        i := i^ + 1;
      };
    };
    count^;
  };
};

let assertCount =
    (haystack: string, needle: string, expected: int, msg: string): unit => {
  let actual = countOccurrences(haystack, needle);
  Test.assertTrue(
    actual == expected,
    msg
    ++ " (expected "
    ++ string_of_int(expected)
    ++ " occurrence(s) of \""
    ++ needle
    ++ "\", found "
    ++ string_of_int(actual)
    ++ ")",
  );
};

/* ============================================================================
 * A chat-log application: Enter appends a message to the transcript, space
 * ticks a live counter without touching the transcript, 'w' writes a raw
 * line through useStdout.
 * ========================================================================== */

/* Mount counter for the transcript entries. Module-level so the test can
 * read it after the app has moved on; reset at the start of each case that
 * uses it. */
let entryMounts = ref(0);

module Entry = {
  [@component]
  let make = (~text: string) => {
    Hooks.useEffect(
      () => {
        entryMounts := entryMounts^ + 1;
        None;
      },
      [||],
    );
    <Text> text </Text>;
  };
};

module ChatApp = {
  [@component]
  let make = () => {
    let stdout = Hooks.useStdout();
    let (messages, setMessages) = Hooks.useState([]);
    let (ticks, setTicks) = Hooks.useState(0);
    let (writes, setWrites) = Hooks.useState(0);

    Event.useKeyDown((key, _mods) =>
      switch (key) {
      | Key.Enter =>
        let n = List.length(messages) + 1;
        setMessages(messages @ ["message-" ++ string_of_int(n)]);
      | Key.Char(' ') => setTicks(ticks + 1)
      | Key.Char('w') =>
        stdout.write("raw-" ++ string_of_int(writes + 1));
        setWrites(writes + 1);
      | _ => ()
      }
    );

    <VStack>
      <Static items=messages renderItem={(m, _i) => <Entry text=m />} />
      <Text> {"spinner-" ++ string_of_int(ticks)} </Text>
    </VStack>;
  };
};

/* Two Static nodes in one tree, to pin down commit ORDER. */
module TwoStaticsApp = {
  [@component]
  let make = () => {
    let (n, setN) = Hooks.useState(0);
    Event.useKeyDown((key, _mods) =>
      switch (key) {
      | Key.Enter => setN(n + 1)
      | _ => ()
      }
    );
    let items = prefix => List.init(n, i => prefix ++ string_of_int(i));
    <VStack>
      <Static
        items={items("top-")}
        renderItem={(t, _i) => <Text> t </Text>}
      />
      <Static
        items={items("bottom-")}
        renderItem={(t, _i) => <Text> t </Text>}
      />
      <Text> "live" </Text>
    </VStack>;
  };
};

/* A Static node that can be swapped out for something else at the SAME tree
 * position, to check that the watermark survives unmount/remount. */
module ToggleApp = {
  [@component]
  let make = () => {
    let (visible, setVisible) = Hooks.useState(true);
    Event.useKeyDown((key, _mods) =>
      switch (key) {
      | Key.Char('h') => setVisible(false)
      | Key.Char('s') => setVisible(true)
      | _ => ()
      }
    );
    <VStack>
      {visible
         ? <Static
             items=["kept-1", "kept-2"]
             renderItem={(t, _i) => <Text> t </Text>}
           />
         : <Text> "hidden" </Text>}
      <Text> "live" </Text>
    </VStack>;
  };
};

/* A Static node one level below an HStack. This is the double-visit case:
 * an HStack renders each child ONCE to measure its natural height and again
 * for real, both times in real mode - so this VStack, and the Static inside
 * it, are walked twice in a single frame. The item must still be committed
 * exactly once. */
module NestedInHStackApp = {
  [@component]
  let make = () => {
    let (n, setN) = Hooks.useState(1);
    Event.useKeyDown((key, _mods) =>
      switch (key) {
      | Key.Enter => setN(n + 1)
      | _ => ()
      }
    );
    <HStack>
      <VStack>
        <Static
          items={List.init(n, i => "nested-" ++ string_of_int(i))}
          renderItem={(t, _i) => <Text> t </Text>}
        />
        <Text> "col-left" </Text>
      </VStack>
      <Text> "col-right" </Text>
    </HStack>;
  };
};

/* Layout fixtures: the same stack with and without a Static child. The
 * frames must be IDENTICAL - a Static node takes no line, no gap slot and no
 * share of the justify spacing. */
module StackWithStatic = {
  let make = () =>
    <VStack gap=1 justify=JustifySpaceBetween>
      <Static items=["committed"] renderItem={(t, _i) => <Text> t </Text>} />
      <Sized size={Chars(1)}> <Text> "A" </Text> </Sized>
      <Sized size={Chars(1)}> <Text> "B" </Text> </Sized>
    </VStack>;
};

module StackWithoutStatic = {
  let make = () =>
    <VStack gap=1 justify=JustifySpaceBetween>
      <Sized size={Chars(1)}> <Text> "A" </Text> </Sized>
      <Sized size={Chars(1)}> <Text> "B" </Text> </Sized>
    </VStack>;
};

module HStackWithStatic = {
  let make = () =>
    <HStack gap=2 justify=JustifySpaceBetween>
      <Static items=["committed"] renderItem={(t, _i) => <Text> t </Text>} />
      <Sized size={Chars(3)}> <Text> "AAA" </Text> </Sized>
      <Sized size={Chars(3)}> <Text> "BBB" </Text> </Sized>
    </HStack>;
};

module HStackWithoutStatic = {
  let make = () =>
    <HStack gap=2 justify=JustifySpaceBetween>
      <Sized size={Chars(3)}> <Text> "AAA" </Text> </Sized>
      <Sized size={Chars(3)}> <Text> "BBB" </Text> </Sized>
    </HStack>;
};

/* ============================================================================
 * Fixtures for the FULLSCREEN guard (see the group at the bottom).
 * ========================================================================== */

/* The useStdout handle the app below hands out, so a test can write with it
 * from outside a render. A module-level ref is fine here: this fixture is
 * only ever driven by one handle at a time. */
let capturedStdout: ref(option(Hooks.stdoutHandle)) = ref(None);

module StdoutApp = {
  let make = () => {
    let out = Hooks.useStdout();
    capturedStdout := Some(out);
    <Text> "live" </Text>;
  };
};

let run = () => {
  Test.group("Static output", () => {
    Test.run("appended messages are committed exactly once each", () => {
      let handle = Runtime.startHeadless((module ChatApp));
      handle.sendKey(Key.Enter, Key.noModifiers);
      handle.sendKey(Key.Enter, Key.noModifiers);
      let static = handle.getStaticOutput(true);
      assertCount(static, "message-1", 1, "first message committed once");
      assertCount(static, "message-2", 1, "second message committed once");
      handle.quit();
    });

    Test.run("a key that appends nothing re-commits nothing", () => {
      let handle = Runtime.startHeadless((module ChatApp));
      handle.sendKey(Key.Enter, Key.noModifiers);
      handle.sendKey(Key.Enter, Key.noModifiers);
      /* A live-only update: several more frames render, and every one of
         them visits the same Static node with the same two items. */
      handle.sendKey(Key.Char(' '), Key.noModifiers);
      handle.sendKey(Key.Char(' '), Key.noModifiers);
      let static = handle.getStaticOutput(true);
      assertCount(static, "message-1", 1, "still exactly one copy");
      assertCount(static, "message-2", 1, "still exactly one copy");
      handle.quit();
    });

    Test.run("committed messages never appear in the live frame", () => {
      let handle = Runtime.startHeadless((module ChatApp));
      handle.sendKey(Key.Enter, Key.noModifiers);
      let output = handle.getOutput(true);
      Test.assertTrue(
        !Test.contains(output, "message-1"),
        "the frame holds the live rows only - the transcript is not in it",
      );
      Test.assertContains(output, "spinner-0", "the live row is in the frame");
      handle.quit();
    });

    Test.run("the live region keeps updating while the transcript sits still", () => {
      let handle = Runtime.startHeadless((module ChatApp));
      handle.sendKey(Key.Enter, Key.noModifiers);
      let staticAfterAppend = handle.getStaticOutput(true);
      handle.sendKey(Key.Char(' '), Key.noModifiers);
      Test.assertContains(
        handle.getOutput(true),
        "spinner-1",
        "the live row re-rendered",
      );
      handle.sendKey(Key.Char(' '), Key.noModifiers);
      Test.assertContains(
        handle.getOutput(true),
        "spinner-2",
        "and again",
      );
      Test.assertEqualStr(
        handle.getStaticOutput(true),
        staticAfterAppend,
        "live re-renders add nothing to the transcript",
      );
      handle.quit();
    });

    Test.run("useStdout writes land in the transcript, in order", () => {
      let handle = Runtime.startHeadless((module ChatApp));
      handle.sendKey(Key.Char('w'), Key.noModifiers);
      handle.sendKey(Key.Enter, Key.noModifiers);
      handle.sendKey(Key.Char('w'), Key.noModifiers);
      let static = handle.getStaticOutput(true);
      assertCount(static, "raw-1", 1, "first write committed once");
      assertCount(static, "raw-2", 1, "second write committed once");
      assertCount(static, "message-1", 1, "the Static item too");
      let idx = (needle: string) => {
        let rec find = i =>
          if (i > String.length(static) - String.length(needle)) {
            (-1);
          } else if (String.sub(static, i, String.length(needle)) == needle) {
            i;
          } else {
            find(i + 1);
          };
        find(0);
      };
      Test.assertTrue(
        idx("raw-1") < idx("message-1") && idx("message-1") < idx("raw-2"),
        "writes and commits keep the order they were made in",
      );
      Test.assertTrue(
        !Test.contains(handle.getOutput(true), "raw-1"),
        "a raw write is not part of the live frame either",
      );
      handle.quit();
    });

    Test.run("two Static nodes commit in tree order", () => {
      let handle = Runtime.startHeadless((module TwoStaticsApp));
      handle.sendKey(Key.Enter, Key.noModifiers);
      let static = handle.getStaticOutput(true);
      assertCount(static, "top-0", 1, "first node committed its item");
      assertCount(static, "bottom-0", 1, "second node committed its item");
      let lines = String.split_on_char('\n', static);
      let rec position = (lines, needle, i) =>
        switch (lines) {
        | [] => (-1)
        | [line, ...rest] =>
          if (Test.contains(line, needle)) {
            i;
          } else {
            position(rest, needle, i + 1);
          }
        };
      Test.assertTrue(
        position(lines, "top-0", 0) < position(lines, "bottom-0", 0),
        "the node higher up the tree commits first",
      );
      handle.quit();
    });

    Test.run("a static item mounts exactly once", () => {
      entryMounts := 0;
      let handle = Runtime.startHeadless((module ChatApp));
      handle.sendKey(Key.Enter, Key.noModifiers);
      Test.assertEqual(
        entryMounts^,
        1,
        "the item's mount effect ran on the frame that committed it",
      );
      /* Several more frames, none of which re-render the item. */
      handle.sendKey(Key.Char(' '), Key.noModifiers);
      handle.sendKey(Key.Char(' '), Key.noModifiers);
      handle.sendKey(Key.Char(' '), Key.noModifiers);
      Test.assertEqual(
        entryMounts^,
        1,
        "and never again - the item is committed output, not live state",
      );
      handle.quit();
    });

    Test.run("the watermark survives unmounting and remounting the node", () => {
      let handle = Runtime.startHeadless((module ToggleApp));
      let afterFirst = handle.getStaticOutput(true);
      assertCount(afterFirst, "kept-1", 1, "committed on the first frame");
      handle.sendKey(Key.Char('h'), Key.noModifiers); /* Static unmounts */
      handle.sendKey(Key.Char('s'), Key.noModifiers); /* and comes back */
      let after = handle.getStaticOutput(true);
      assertCount(after, "kept-1", 1, "not re-emitted on remount");
      assertCount(after, "kept-2", 1, "nor the second item");
      Test.assertEqualStr(
        after,
        afterFirst,
        "a remount at the same path commits nothing new at all",
      );
      handle.quit();
    });

    Test.run("a subtree rendered twice in one frame commits once", () => {
      let handle = Runtime.startHeadless((module NestedInHStackApp));
      assertCount(
        handle.getStaticOutput(true),
        "nested-0",
        1,
        "the HStack's measure pass and its real pass commit one item, not two",
      );
      handle.sendKey(Key.Enter, Key.noModifiers);
      let static = handle.getStaticOutput(true);
      assertCount(static, "nested-0", 1, "and the first item stays committed once");
      assertCount(static, "nested-1", 1, "the appended one likewise");
      handle.quit();
    });

    Test.run("a Static child is invisible to VStack layout", () => {
      let config: Runtime.headlessConfig = {width: 20, height: 6};
      let withStatic = Runtime.startHeadless(~config, (module StackWithStatic));
      let without =
        Runtime.startHeadless(~config, (module StackWithoutStatic));
      Test.assertEqualStr(
        withStatic.getOutput(false),
        without.getOutput(false),
        "no line, no gap slot, no share of the justify spacing",
      );
      Test.assertContains(
        withStatic.getStaticOutput(true),
        "committed",
        "the item was still committed, it just did not take any space",
      );
      withStatic.quit();
      without.quit();
    });

    Test.run("a Static child is invisible to HStack layout", () => {
      let config: Runtime.headlessConfig = {width: 20, height: 3};
      let withStatic =
        Runtime.startHeadless(~config, (module HStackWithStatic));
      let without =
        Runtime.startHeadless(~config, (module HStackWithoutStatic));
      Test.assertEqualStr(
        withStatic.getOutput(false),
        without.getOutput(false),
        "the gap between the two live columns is unaffected",
      );
      Test.assertContains(
        withStatic.getStaticOutput(true),
        "committed",
        "and the item was committed once, from inside an HStack",
      );
      withStatic.quit();
      without.quit();
    });

    Test.run("an app with no Static and no useStdout commits nothing", () => {
      let handle = Runtime.startHeadless((module StackWithoutStatic));
      Test.assertEqualStr(
        handle.getStaticOutput(false),
        "",
        "the drain is empty for every application that predates A4",
      );
      handle.quit();
    });
  });

  /* ==========================================================================
   * Committing above the live region is meaningless on the ALTERNATE screen:
   * there is no scrollback there to commit into. Rather than absorb output
   * that can never appear, Matcha REJECTS the combination - <Static> and
   * useStdout both raise under Runtime.start(~screen=Fullscreen).
   *
   * The switch is one flag on Hooks.instanceState (staticAllowed), which
   * Runtime.start turns off for Fullscreen right after installing the fresh
   * instance. That is also what makes it testable without a TTY: flip the
   * same flag the same way and drive the app headlessly.
   * ======================================================================== */
  Test.group("Static output: the Fullscreen guard", () => {
    /* Assert that [thunk] raises Invalid_argument naming the screen mode. */
    let expectFullscreenRaise = (thunk: unit => unit, what: string): unit =>
      switch (thunk()) {
      | () =>
        Test.assertTrue(
          false,
          what ++ " should have raised Invalid_argument, but returned",
        )
      | exception (Invalid_argument(msg)) =>
        Test.assertContains(
          msg,
          "Fullscreen",
          what ++ " raises, and the message says which screen mode is to blame",
        );
        Test.assertContains(
          msg,
          "ScrollView",
          "and points at what to do instead",
        );
      };

    Test.run("headless is screen-agnostic: <Static> works normally", () => {
      /* The baseline this guard must not break. startHeadless never touches
         the flag - it has no terminal to own - so every existing test, every
         golden and every headless stream keeps committing as before. */
      let handle = Runtime.startHeadless((module StackWithStatic));
      Test.assertContains(
        handle.getStaticOutput(true),
        "committed",
        "a headless app commits exactly as it always has",
      );
      ignore(handle.render());
      Test.assertContains(
        handle.getStaticOutput(true),
        "committed",
        "and more frames are fine",
      );
      handle.quit();
    });

    Test.run("<Static> raises once the instance says fullscreen", () => {
      let handle = Runtime.startHeadless((module StackWithStatic));
      /* Exactly what Runtime.start does for ~screen=Fullscreen, on exactly
         the instance this handle owns (startHeadless leaves it installed). */
      Hooks.setStaticAllowed(Hooks.instance(), false);
      expectFullscreenRaise(() => ignore(handle.render()), "<Static>");
      handle.quit();
    });

    Test.run("useStdout().write raises the same way", () => {
      capturedStdout := None;
      let handle = Runtime.startHeadless((module StdoutApp));
      let out =
        switch (capturedStdout^) {
        | Some(o) => o
        | None => failwith("the app under test never called useStdout")
        };
      /* Allowed first, so the failure below is the flag and nothing else. */
      out.write("inline is fine");
      ignore(handle.render());
      Test.assertContains(
        handle.getStaticOutput(true),
        "inline is fine",
        "the write landed while committing was still allowed",
      );

      Hooks.setStaticAllowed(Hooks.instance(), false);
      expectFullscreenRaise(() => out.write("nowhere to go"), "useStdout");
      handle.quit();
    });
  });
};
