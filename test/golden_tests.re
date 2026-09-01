/*
 * Golden frame tests.
 *
 * These render a handful of small, stateless in-process components plus
 * every example binary headlessly, and compare the resulting frame against
 * a saved golden file under test/goldens/. Regenerate goldens after an
 * intentional rendering change with:
 *
 *   UPDATE_GOLDENS=1 dune exec test/run_tests.exe
 */
open Matcha;

/* ============================================================================
 * In-process components
 * ============================================================================ */

/* VStack with a gap and JustifySpaceBetween, exercising fixed-size children
 * distributed along the main (vertical) axis. */
module VStackGapJustify = {
  let make = () =>
    <VStack gap=1 justify=JustifySpaceBetween>
      <Sized size={Chars(3)}>
        <Text bold=true color=Cyan> "Header" </Text>
      </Sized>
      <Sized size={Chars(3)}>
        <Text> "Body content line" </Text>
      </Sized>
      <Sized size={Chars(3)}>
        <Text dim=true> "Footer" </Text>
      </Sized>
    </VStack>;
};

/* HStack with a mix of Flex and Chars sized children, and an align variant
 * (AlignCenter vs AlignStart) applied to children of differing natural
 * height, showing cross-axis alignment. */
module HStackBox = {
  let box = (~label: string, ~height: int) => {
    let top = "+------+";
    let mid = "|" ++ Element.padToWidth(label, 6) ++ "|";
    let empty = "|      |";
    let middleRows = max(0, height - 2);
    let midIndex = middleRows / 2;
    let lines =
      [top]
      @ List.init(middleRows, i => i == midIndex ? mid : empty)
      @ [top];
    <Text> {String.concat("\n", lines)} </Text>;
  };
};

module HStackAlignStart = {
  let make = () =>
    <Sized size={Chars(8)}>
      <HStack gap=2 align=AlignStart>
        <Sized size={Chars(8)}> {HStackBox.box(~label="A", ~height=3)} </Sized>
        <Sized size={Chars(8)}> {HStackBox.box(~label="BB", ~height=6)} </Sized>
        <Sized size={Flex(1)}> {HStackBox.box(~label="CCC", ~height=4)} </Sized>
      </HStack>
    </Sized>;
};

module HStackAlignCenter = {
  let make = () =>
    <Sized size={Chars(8)}>
      <HStack gap=2 align=AlignCenter>
        <Sized size={Chars(8)}> {HStackBox.box(~label="A", ~height=3)} </Sized>
        <Sized size={Chars(8)}> {HStackBox.box(~label="BB", ~height=6)} </Sized>
        <Sized size={Flex(1)}> {HStackBox.box(~label="CCC", ~height=4)} </Sized>
      </HStack>
    </Sized>;
};

/* A styled Text composition: nested bold/italic/underline/color/bgColor
 * combinations, exercising the Styled wrapper ordering. */
module StyledTextDemo = {
  let make = () =>
    <VStack gap=0>
      <Text bold=true color=Yellow> "Bold yellow" </Text>
      <Text italic=true dim=true> "Italic dim" </Text>
      <Text underline=true color=Green bgColor=Black> "Underlined green on black" </Text>
      <Text inverted=true color=Red> "Inverted red" </Text>
      <Text bold=true italic=true underline=true color=Magenta>
        "Bold italic underline magenta"
      </Text>
    </VStack>;
};

/* <Text wrap>: a Wrap column (bold+cyan) next to a Truncate column
 * (dim+red), both styled - exercises StyledText.wrapString wrapping and
 * truncating already-styled text side by side. Sized(Chars(n)) inside this
 * HStack fixes each column's WIDTH (the sized axis for an HStack), which is
 * what makes the wrap/truncate widths deterministic regardless of the
 * golden's own ~width. */
module WrapStyledDemo = {
  let make = () =>
    <HStack gap=2>
      <Sized size={Chars(16)}>
        <Text wrap=Wrap bold=true color=Cyan>
          "hello wonderful world of styled wrapping"
        </Text>
      </Sized>
      <Sized size={Chars(10)}>
        <Text wrap=Truncate dim=true color=Red>
          "a very long line that will not fit"
        </Text>
      </Sized>
    </HStack>;
};

/* A <Modal> over live content (B2): the whole composite in one frame -
 * border, title, the shadow's L to the lower right, and the base rows
 * surviving to the left of the box and above and below it. Auto height, so
 * the box is exactly the dialog's content plus its two border rows. */
module ModalDemo = {
  let make = () =>
    <VStack>
      ...{
           List.init(10, i =>
             <Text> {"row " ++ string_of_int(i) ++ " of the application"} </Text>
           )
           @ [
             <Modal isOpen=true title="Commands" align={Element.OverlayTop(2)}>
               <VStack>
                 <Text bold=true> "Pick an action" </Text>
                 <Text> "> pause" </Text>
                 <Text dim=true> "  clear" </Text>
               </VStack>
             </Modal>,
           ]
         }
    </VStack>;
};

let run = () =>
  Test.group("Golden Frames", () => {
    /* In-process component goldens */
    Test.run("vstack gap+justify layout", () =>
      Golden.checkComponent(
        "vstack-gap-justify",
        ~width=40,
        ~height=16,
        (module VStackGapJustify),
      )
    );

    Test.run("hstack align=start", () =>
      Golden.checkComponent(
        "hstack-align-start",
        ~width=40,
        ~height=10,
        (module HStackAlignStart),
      )
    );

    Test.run("hstack align=center", () =>
      Golden.checkComponent(
        "hstack-align-center",
        ~width=40,
        ~height=10,
        (module HStackAlignCenter),
      )
    );

    Test.run("styled text composition", () =>
      Golden.checkComponent(
        "styled-text",
        ~width=40,
        ~height=10,
        (module StyledTextDemo),
      )
    );

    Test.run("wrap-styled text (<Text wrap>)", () =>
      Golden.checkComponent(
        "wrap-styled",
        ~width=40,
        ~height=8,
        (module WrapStyledDemo),
      )
    );

    Test.run("modal composited over live content", () =>
      Golden.checkComponent("modal-over-content", ~width=50, ~height=14, (module ModalDemo))
    );

    /* Example goldens - run each built example binary headlessly and
     * compare its first rendered frame. */
    Test.run("example: hello-world", () => Golden.checkExample("hello-world"));
    Test.run("example: counter", () => Golden.checkExample("counter"));
    Test.run("example: keyed-switch", () => Golden.checkExample("keyed-switch"));
    Test.run("example: layout-alignment", () =>
      Golden.checkExample("layout-alignment")
    );
    Test.run("example: textarea-demo", () => Golden.checkExample("textarea-demo"));
    Test.run("example: optional-params", () =>
      Golden.checkExample("optional-params")
    );
    /* static-demo's golden is the whole headless STREAM, which is what makes
     * it interesting: the message its <Static> commits on the first frame
     * appears above the frame, in commit order, exactly where the
     * interactive loop would print it. */
    Test.run("example: static-demo", () => Golden.checkExample("static-demo"));
    Test.run("example: scroll-demo", () => Golden.checkExample("scroll-demo"));
    /* chat is the capstone: this golden pins its first frame (status row,
     * placeholder input, context panel with scrollbar, hint row) - the same
     * frame test/chat_tests.re starts from in-process. */
    Test.run("example: chat", () => Golden.checkExample("chat"));
    /* claude-code is the FULLSCREEN example, and its golden is a single
     * full-viewport frame: nothing is committed above it (an alt-screen app
     * has no scrollback, and <Static>/useStdout raise there), and the
     * Flex(1) transcript ScrollView absorbs every spare row - so the banner
     * sits at the top, blank fill runs down the middle, and the status,
     * input box and hint rows are pinned to the bottom of all 24 rows. */
    Test.run("example: claude-code", () => Golden.checkExample("claude-code"));
    /* command-menu's golden is the CLOSED state: `checkExample` sees one
     * frame (stdin is /dev/null, so the app reaches EOF before any key can
     * arrive), and that frame must contain no border at all - a <Modal> that
     * is not open costs its stack nothing and paints nothing. The composited
     * state is pinned by the "modal-over-content" component golden above and
     * exercised end to end in test/commandmenu_tests.re. */
    Test.run("example: command-menu", () => Golden.checkExample("command-menu"));

    /* nested-components and layout-demo call Terminal.getSize() directly
     * (bypassing the headless-aware MATCHA_WIDTH/MATCHA_HEIGHT constraints)
     * to display and lay out against the RAW terminal size.
     *
     * These two used to be substring assertions rather than goldens, for a
     * real reason: caml_get_terminal_size in lib/terminal_stubs.c called
     * ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) without checking its return
     * value, so with stdout on a pipe (which is how these run - we capture
     * the child's output) the ioctl failed and `w` was read back
     * UNINITIALIZED. The reported "terminal size" was stack garbage that
     * differed run to run, which made both frames nondeterministic and an
     * exact golden impossible.
     *
     * The stub now checks the ioctl and falls back to a standard 80x24, so
     * these frames are byte-for-byte reproducible and they are exact
     * goldens like every other example. Verified by running each binary
     * three times and comparing checksums before converting them.
     *
     * If one of these goldens ever starts flapping again, suspect the stub
     * first: a size read that is not deterministic under a pipe is the
     * failure mode this comment is a monument to. */
    Test.run("example: nested-components", () =>
      Golden.checkExample("nested-components")
    );
    Test.run("example: layout-demo", () => Golden.checkExample("layout-demo"));

    /* people-list also goes through TerminalContext -> Terminal.getSize(),
     * and its width/height feed directly into SplitView's layout math (line
     * widths and row counts) rather than into a text label - so back when
     * the stub returned garbage, a bad reading could blow the frame up to
     * hundreds of MB of padding (Golden.runExample's byte cap exists for
     * that). It is deterministic now too, but its assertions are left as
     * stable substrings deliberately: what this case is for is that the
     * split view populates and selects a row, not the exact padding of a
     * wide frame. */
    Test.run("example: people-list (stable text)", () => {
      let output = Golden.runExample("people-list");
      Test.assertContains(
        output,
        "People (",
        "left pane header renders",
      );
      Test.assertContains(
        output,
        "> Alice (28 years old)",
        "first person is rendered and selected by default",
      );
    });

    /* async-fetch's output depends on background-thread timing once a fetch
     * is triggered (via the 'f' key), so we don't golden its frame. We never
     * send any keys here, so the process only ever renders its initial Idle
     * state before stdin EOF closes it - that text is deterministic, so we
     * assert on stable substrings instead of an exact golden match. */
    Test.run("example: async-fetch (stable idle text)", () => {
      let output = Golden.runExample("async-fetch");
      Test.assertContains(output, "Async Fetch Example", "title renders");
      Test.assertContains(
        output,
        "No data fetched yet. Press 'f' to fetch.",
        "idle state renders before any key is sent",
      );
      Test.assertContains(
        output,
        "Total successful fetches: 0",
        "fetch count starts at 0",
      );
    });
  });
