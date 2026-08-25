/*
 * Tests for Key.parse - raw terminal byte -> normalized (Key.t, modifiers)
 *
 * Every expectation here is derived from reading lib/Key.re's parse
 * implementation directly (not from what "should" happen), per the task
 * instructions. Where the parser's behavior looked surprising rather than
 * a deliberate design choice, the current behavior is still asserted, with
 * a `/* NOTE: possibly buggy */` comment explaining why.
 */
open Matcha;

/* Parse a full string as one buffer (like handing all its bytes to
 * Key.parse in a single call). */
let parseStr = (s: string): (Key.t, Key.modifiers) => {
  let b = Bytes.of_string(s);
  Key.parse(b, Bytes.length(b));
};

/* Parse only the first `len` bytes of a possibly-longer buffer, to test
 * that trailing bytes beyond `len` are ignored. */
let parseWithLen = (s: string, len: int): (Key.t, Key.modifiers) => {
  let b = Bytes.of_string(s);
  Key.parse(b, len);
};

let mods = (~ctrl=false, ~alt=false, ~shift=false, ~meta=false, ()): Key.modifiers => {
  ctrl,
  alt,
  shift,
  meta,
};

let assertKey =
    (actual: (Key.t, Key.modifiers), expected: (Key.t, Key.modifiers), msg: string) =>
  Test.assertEqual(actual, expected, msg);

/* ============================================================================
 * KeyRecorder - tiny headless app used by the feedBytes/feedKeys tests at
 * the bottom of this file. Records every key event it receives via
 * Event.useKeyDown into a module-level ref, following the pattern in
 * test/headless_tests.re.
 * ============================================================================ */
module KeyRecorder = {
  let recorded: ref(list((Key.t, Key.modifiers))) = ref([]);
  let reset = () => recorded := [];

  [@component]
  let make = () => {
    Event.useKeyDown((key, keyMods) => recorded := recorded^ @ [(key, keyMods)]);
    <Text> "recorder" </Text>;
  };
};

let run = () => {
  Test.group("Key Parsing", () => {
    /* -------------------------------------------------------------- */
    /* Edge cases: empty / too-short input                             */
    /* -------------------------------------------------------------- */
    Test.run("empty input (len=0) returns Unknown", () => {
      let result = Key.parse(Bytes.create(0), 0);
      assertKey(result, (Key.Unknown, Key.noModifiers), "len=0 -> Unknown");
    });

    Test.run("lone ESC returns Escape", () => {
      let result = parseStr("\027");
      assertKey(result, (Key.Escape, Key.noModifiers), "lone ESC -> Escape");
    });

    Test.run("truncated escape sequence (ESC[1; with no key) is Unknown", () => {
      /* len=5, fails the `len >= 6` guard for the extended-modifier form,
       * and doesn't match any of the simple ESC[<key> patterns either. */
      let result = parseStr("\027[1;5");
      assertKey(
        result,
        (Key.Unknown, Key.noModifiers),
        "truncated extended sequence -> Unknown",
      );
    });

    Test.run("truncated ESC[ (only 2 bytes) falls back to Alt+'[' ", () => {
      /* Only len=2 bytes are available, so the `len >= 3` guards for the
       * ESC[ and ESC O branches don't apply; it falls through to the
       * generic Alt+key handling, treating '[' as the alt-ed character. */
      let result = parseStr("\027[");
      assertKey(
        result,
        (Key.Char('['), mods(~alt=true, ())),
        "truncated ESC[ with only 2 bytes -> Alt+'['",
      );
    });

    Test.run("bytes beyond n are ignored (plain char)", () => {
      let result = parseWithLen("aXXXX", 1);
      assertKey(result, (Key.Char('a'), Key.noModifiers), "only first byte used");
    });

    Test.run("bytes beyond n are ignored (escape sequence)", () => {
      let result = parseWithLen("\027[AXXXXX", 3);
      assertKey(result, (Key.Arrow_up, Key.noModifiers), "only first 3 bytes used");
    });

    /* -------------------------------------------------------------- */
    /* Plain ASCII characters                                         */
    /* -------------------------------------------------------------- */
    Test.run("plain lowercase char", () => {
      let result = parseStr("a");
      assertKey(result, (Key.Char('a'), Key.noModifiers), "'a' -> Char('a')");
    });

    Test.run("plain digit char", () => {
      let result = parseStr("5");
      assertKey(result, (Key.Char('5'), Key.noModifiers), "'5' -> Char('5')");
    });

    Test.run("uppercase char (shift) carries no explicit shift modifier", () => {
      /* Terminals send the uppercase byte directly; there's no separate
       * shift flag for plain characters - this matches normal raw-mode
       * terminal behavior, not a bug. */
      let result = parseStr("A");
      assertKey(result, (Key.Char('A'), Key.noModifiers), "'A' -> Char('A'), no modifiers");
    });

    Test.run("space char", () => {
      let result = parseStr(" ");
      assertKey(result, (Key.Char(' '), Key.noModifiers), "' ' -> Char(' ')");
    });

    /* -------------------------------------------------------------- */
    /* Special single-byte keys                                       */
    /* -------------------------------------------------------------- */
    Test.run("Enter via \\r (13)", () => {
      let result = parseStr("\r");
      assertKey(result, (Key.Enter, Key.noModifiers), "\\r -> Enter");
    });

    Test.run("Enter via \\n (10)", () => {
      let result = parseStr("\n");
      assertKey(result, (Key.Enter, Key.noModifiers), "\\n -> Enter");
    });

    Test.run("Tab (Ctrl+I, code 9)", () => {
      let result = parseStr("\t");
      assertKey(result, (Key.Tab, Key.noModifiers), "\\t -> Tab");
    });

    Test.run("Backspace via code 8 (Ctrl+H)", () => {
      let result = parseStr(String.make(1, Char.chr(8)));
      assertKey(result, (Key.Backspace, Key.noModifiers), "code 8 -> Backspace");
    });

    Test.run("Backspace via code 127 (DEL)", () => {
      let result = parseStr(String.make(1, Char.chr(127)));
      assertKey(result, (Key.Backspace, Key.noModifiers), "code 127 -> Backspace");
    });

    Test.run("KillLine (Ctrl+U, code 21)", () => {
      let result = parseStr(String.make(1, Char.chr(21)));
      assertKey(result, (Key.KillLine, Key.noModifiers), "code 21 -> KillLine");
    });

    Test.run("KillWord (Ctrl+W, code 23)", () => {
      let result = parseStr(String.make(1, Char.chr(23)));
      assertKey(result, (Key.KillWord, Key.noModifiers), "code 23 -> KillWord");
    });

    /* -------------------------------------------------------------- */
    /* Ctrl+letter control codes                                      */
    /* -------------------------------------------------------------- */
    Test.run("Ctrl+A (code 1) maps to Arrow_left+meta (start of line)", () => {
      /* Deliberate normalization: Ctrl+A / Home both become Arrow_left
       * with meta:true (see the 'H' case for ESC[H below), not ctrl:true.
       * Consistent with the rest of the parser's Home/End convention. */
      let result = parseStr(String.make(1, Char.chr(1)));
      assertKey(
        result,
        (Key.Arrow_left, mods(~meta=true, ())),
        "code 1 -> Arrow_left with meta (Home-style)",
      );
    });

    Test.run("Ctrl+E (code 5) maps to Arrow_right+meta (end of line)", () => {
      let result = parseStr(String.make(1, Char.chr(5)));
      assertKey(
        result,
        (Key.Arrow_right, mods(~meta=true, ())),
        "code 5 -> Arrow_right with meta (End-style)",
      );
    });

    Test.run("Ctrl+B (code 2)", () => {
      let result = parseStr(String.make(1, Char.chr(2)));
      assertKey(
        result,
        (Key.Char('b'), mods(~ctrl=true, ())),
        "code 2 -> Char('b') with ctrl",
      );
    });

    Test.run("Ctrl+C (code 3)", () => {
      let result = parseStr(String.make(1, Char.chr(3)));
      assertKey(
        result,
        (Key.Char('c'), mods(~ctrl=true, ())),
        "code 3 -> Char('c') with ctrl",
      );
    });

    Test.run("Ctrl+T (code 20)", () => {
      let result = parseStr(String.make(1, Char.chr(20)));
      assertKey(
        result,
        (Key.Char('t'), mods(~ctrl=true, ())),
        "code 20 -> Char('t') with ctrl",
      );
    });

    Test.run("Ctrl+Z (code 26)", () => {
      let result = parseStr(String.make(1, Char.chr(26)));
      assertKey(
        result,
        (Key.Char('z'), mods(~ctrl=true, ())),
        "code 26 -> Char('z') with ctrl",
      );
    });

    /* -------------------------------------------------------------- */
    /* Arrow keys: ESC [ A/B/C/D                                       */
    /* -------------------------------------------------------------- */
    Test.run("Arrow up: ESC[A", () => {
      let result = parseStr("\027[A");
      assertKey(result, (Key.Arrow_up, Key.noModifiers), "ESC[A -> Arrow_up");
    });

    Test.run("Arrow down: ESC[B", () => {
      let result = parseStr("\027[B");
      assertKey(result, (Key.Arrow_down, Key.noModifiers), "ESC[B -> Arrow_down");
    });

    Test.run("Arrow right: ESC[C", () => {
      let result = parseStr("\027[C");
      assertKey(result, (Key.Arrow_right, Key.noModifiers), "ESC[C -> Arrow_right");
    });

    Test.run("Arrow left: ESC[D", () => {
      let result = parseStr("\027[D");
      assertKey(result, (Key.Arrow_left, Key.noModifiers), "ESC[D -> Arrow_left");
    });

    Test.run("Backtab: ESC[Z -> Shift+Tab", () => {
      /* Deliberate remap (was Unknown): ESC[Z is the standard xterm
       * backtab sequence sent for Shift+Tab, and B1 (focus) needs it to
       * drive focusPrevious. */
      let result = parseStr("\027[Z");
      assertKey(result, (Key.Tab, mods(~shift=true, ())), "ESC[Z -> Tab+shift (backtab)");
    });

    /* -------------------------------------------------------------- */
    /* Home / End / PageUp / PageDown / Delete                        */
    /* -------------------------------------------------------------- */
    Test.run("Home: ESC[H", () => {
      /* Deliberate breaking change: ESC[H used to alias to Arrow_left+meta;
       * it is now its own Key.Home variant with no modifiers pre-forced. */
      let result = parseStr("\027[H");
      assertKey(result, (Key.Home, Key.noModifiers), "ESC[H -> Home");
    });

    Test.run("End: ESC[F", () => {
      let result = parseStr("\027[F");
      assertKey(result, (Key.End, Key.noModifiers), "ESC[F -> End");
    });

    Test.run("Home (alt form): ESC[1~", () => {
      let result = parseStr("\027[1~");
      assertKey(result, (Key.Home, Key.noModifiers), "ESC[1~ -> Home");
    });

    Test.run("Home (alt form 2): ESC[7~", () => {
      let result = parseStr("\027[7~");
      assertKey(result, (Key.Home, Key.noModifiers), "ESC[7~ -> Home");
    });

    Test.run("End (alt form): ESC[4~", () => {
      let result = parseStr("\027[4~");
      assertKey(result, (Key.End, Key.noModifiers), "ESC[4~ -> End");
    });

    Test.run("End (alt form 2): ESC[8~", () => {
      let result = parseStr("\027[8~");
      assertKey(result, (Key.End, Key.noModifiers), "ESC[8~ -> End");
    });

    Test.run("Delete: ESC[3~", () => {
      let result = parseStr("\027[3~");
      assertKey(result, (Key.Delete, Key.noModifiers), "ESC[3~ -> Delete");
    });

    Test.run("Insert: ESC[2~", () => {
      let result = parseStr("\027[2~");
      assertKey(result, (Key.Insert, Key.noModifiers), "ESC[2~ -> Insert");
    });

    Test.run("Insert with modifier: ESC[2;5~ (Ctrl+Insert)", () => {
      let result = parseStr("\027[2;5~");
      assertKey(
        result,
        (Key.Insert, mods(~ctrl=true, ())),
        "ESC[2;5~ -> Insert with ctrl",
      );
    });

    Test.run("Delete with modifier: ESC[3;2~ (Shift+Delete)", () => {
      let result = parseStr("\027[3;2~");
      assertKey(
        result,
        (Key.Delete, mods(~shift=true, ())),
        "ESC[3;2~ -> Delete with shift",
      );
    });

    Test.run("PageUp: ESC[5~", () => {
      /* Deliberate remap (was Unknown, no Key.t variant existed): PageUp
       * now has a dedicated Page_up variant. */
      let result = parseStr("\027[5~");
      assertKey(result, (Key.Page_up, Key.noModifiers), "ESC[5~ -> Page_up");
    });

    Test.run("PageDown: ESC[6~", () => {
      let result = parseStr("\027[6~");
      assertKey(result, (Key.Page_down, Key.noModifiers), "ESC[6~ -> Page_down");
    });

    Test.run("PageUp with modifier: ESC[5;5~ (Ctrl+PageUp)", () => {
      let result = parseStr("\027[5;5~");
      assertKey(
        result,
        (Key.Page_up, mods(~ctrl=true, ())),
        "ESC[5;5~ -> Page_up with ctrl",
      );
    });

    Test.run("PageDown with modifier: ESC[6;2~ (Shift+PageDown)", () => {
      let result = parseStr("\027[6;2~");
      assertKey(
        result,
        (Key.Page_down, mods(~shift=true, ())),
        "ESC[6;2~ -> Page_down with shift",
      );
    });

    /* -------------------------------------------------------------- */
    /* Modified arrows: ESC[1;<mod>{A,B,C,D,H,F}                       */
    /* -------------------------------------------------------------- */
    Test.run("Ctrl+Up: ESC[1;5A", () => {
      let result = parseStr("\027[1;5A");
      assertKey(
        result,
        (Key.Arrow_up, mods(~ctrl=true, ())),
        "ESC[1;5A -> Arrow_up with ctrl",
      );
    });

    Test.run("Shift+Right: ESC[1;2C", () => {
      let result = parseStr("\027[1;2C");
      assertKey(
        result,
        (Key.Arrow_right, mods(~shift=true, ())),
        "ESC[1;2C -> Arrow_right with shift",
      );
    });

    Test.run("Alt+Down: ESC[1;3B", () => {
      let result = parseStr("\027[1;3B");
      assertKey(
        result,
        (Key.Arrow_down, mods(~alt=true, ())),
        "ESC[1;3B -> Arrow_down with alt",
      );
    });

    Test.run("no-modifier extended form: ESC[1;1A", () => {
      /* Modifier code 1 = "1 + no bits set" per parseModifierCode. */
      let result = parseStr("\027[1;1A");
      assertKey(
        result,
        (Key.Arrow_up, Key.noModifiers),
        "ESC[1;1A -> Arrow_up, no modifiers",
      );
    });

    Test.run("two-digit modifier: ESC[1;10A (Shift+Meta)", () => {
      let result = parseStr("\027[1;10A");
      assertKey(
        result,
        (Key.Arrow_up, mods(~shift=true, ~meta=true, ())),
        "ESC[1;10A -> Arrow_up with shift+meta",
      );
    });

    Test.run("Ctrl+Home: ESC[1;5H", () => {
      /* Deliberate breaking change: Home no longer forces meta:true on
       * top of the decoded modifier - it's its own Key.Home variant now. */
      let result = parseStr("\027[1;5H");
      assertKey(
        result,
        (Key.Home, mods(~ctrl=true, ())),
        "ESC[1;5H -> Home with ctrl",
      );
    });

    Test.run("Ctrl+End: ESC[1;5F", () => {
      let result = parseStr("\027[1;5F");
      assertKey(
        result,
        (Key.End, mods(~ctrl=true, ())),
        "ESC[1;5F -> End with ctrl",
      );
    });

    Test.run("two-digit modifier on Home: ESC[1;10H (Shift+Meta+Home)", () => {
      let result = parseStr("\027[1;10H");
      assertKey(
        result,
        (Key.Home, mods(~shift=true, ~meta=true, ())),
        "ESC[1;10H -> Home with shift+meta",
      );
    });

    Test.run("modified but unrecognized key char keeps parsed modifiers", () => {
      /* Unlike the plain ESC[Z case, this Unknown carries the parsed
       * modifiers instead of noModifiers - the fallback at lib/Key.re:200
       * ("_" => (Unknown, mods)) differs from the ESC[ simple-form
       * fallback at lib/Key.re:218 ("_" => (Unknown, noModifiers)). Both
       * are plausible; documenting the current (inconsistent) behavior. */
      let result = parseStr("\027[1;5Z");
      assertKey(
        result,
        (Key.Unknown, mods(~ctrl=true, ())),
        "ESC[1;5Z -> Unknown but keeps ctrl modifier",
      );
    });

    /* -------------------------------------------------------------- */
    /* Application mode: ESC O <key>                                  */
    /* -------------------------------------------------------------- */
    Test.run("ESC O A (application-mode arrow up)", () => {
      let result = parseStr("\027OA");
      assertKey(result, (Key.Arrow_up, Key.noModifiers), "ESC O A -> Arrow_up");
    });

    Test.run("ESC O D (application-mode arrow left)", () => {
      let result = parseStr("\027OD");
      assertKey(result, (Key.Arrow_left, Key.noModifiers), "ESC O D -> Arrow_left");
    });

    Test.run("ESC O H (application-mode Home)", () => {
      /* Deliberate breaking change: no longer forced to meta. */
      let result = parseStr("\027OH");
      assertKey(result, (Key.Home, Key.noModifiers), "ESC O H -> Home");
    });

    Test.run("ESC O F (application-mode End)", () => {
      let result = parseStr("\027OF");
      assertKey(result, (Key.End, Key.noModifiers), "ESC O F -> End");
    });

    Test.run("ESC O Z is unrecognized", () => {
      let result = parseStr("\027OZ");
      assertKey(result, (Key.Unknown, Key.noModifiers), "ESC O Z -> Unknown");
    });

    /* -------------------------------------------------------------- */
    /* Function keys: F1-F12 (SS3, legacy ~, extended ESC[1;<m>P..S,   */
    /* Linux console)                                                  */
    /* -------------------------------------------------------------- */
    Test.run("F1-F4 via SS3: ESC O P/Q/R/S", () => {
      assertKey(parseStr("\027OP"), (Key.F(1), Key.noModifiers), "ESC O P -> F1");
      assertKey(parseStr("\027OQ"), (Key.F(2), Key.noModifiers), "ESC O Q -> F2");
      assertKey(parseStr("\027OR"), (Key.F(3), Key.noModifiers), "ESC O R -> F3");
      assertKey(parseStr("\027OS"), (Key.F(4), Key.noModifiers), "ESC O S -> F4");
    });

    Test.run("F1-F4 via extended CSI: ESC[1;<m>P..S", () => {
      assertKey(
        parseStr("\027[1;2P"),
        (Key.F(1), mods(~shift=true, ())),
        "ESC[1;2P -> F1 with shift",
      );
      assertKey(
        parseStr("\027[1;5Q"),
        (Key.F(2), mods(~ctrl=true, ())),
        "ESC[1;5Q -> F2 with ctrl",
      );
      assertKey(
        parseStr("\027[1;3R"),
        (Key.F(3), mods(~alt=true, ())),
        "ESC[1;3R -> F3 with alt",
      );
      assertKey(parseStr("\027[1;1S"), (Key.F(4), Key.noModifiers), "ESC[1;1S -> F4, no mods");
    });

    Test.run("F1-F4 legacy tilde form: ESC[11~..14~", () => {
      assertKey(parseStr("\027[11~"), (Key.F(1), Key.noModifiers), "ESC[11~ -> F1");
      assertKey(parseStr("\027[12~"), (Key.F(2), Key.noModifiers), "ESC[12~ -> F2");
      assertKey(parseStr("\027[13~"), (Key.F(3), Key.noModifiers), "ESC[13~ -> F3");
      assertKey(parseStr("\027[14~"), (Key.F(4), Key.noModifiers), "ESC[14~ -> F4");
    });

    Test.run("F5-F12 tilde form (gaps at 16, 22)", () => {
      assertKey(parseStr("\027[15~"), (Key.F(5), Key.noModifiers), "ESC[15~ -> F5");
      assertKey(parseStr("\027[17~"), (Key.F(6), Key.noModifiers), "ESC[17~ -> F6");
      assertKey(parseStr("\027[18~"), (Key.F(7), Key.noModifiers), "ESC[18~ -> F7");
      assertKey(parseStr("\027[19~"), (Key.F(8), Key.noModifiers), "ESC[19~ -> F8");
      assertKey(parseStr("\027[20~"), (Key.F(9), Key.noModifiers), "ESC[20~ -> F9");
      assertKey(parseStr("\027[21~"), (Key.F(10), Key.noModifiers), "ESC[21~ -> F10");
      assertKey(parseStr("\027[23~"), (Key.F(11), Key.noModifiers), "ESC[23~ -> F11");
      assertKey(parseStr("\027[24~"), (Key.F(12), Key.noModifiers), "ESC[24~ -> F12");
      /* 16 and 22 are gaps in the legacy table - not mapped to any F key. */
      assertKey(parseStr("\027[16~"), (Key.Unknown, Key.noModifiers), "ESC[16~ -> Unknown (gap)");
      assertKey(parseStr("\027[22~"), (Key.Unknown, Key.noModifiers), "ESC[22~ -> Unknown (gap)");
    });

    Test.run("F1-F5 via Linux console: ESC[[A..E", () => {
      assertKey(parseStr("\027[[A"), (Key.F(1), Key.noModifiers), "ESC[[A -> F1");
      assertKey(parseStr("\027[[B"), (Key.F(2), Key.noModifiers), "ESC[[B -> F2");
      assertKey(parseStr("\027[[C"), (Key.F(3), Key.noModifiers), "ESC[[C -> F3");
      assertKey(parseStr("\027[[D"), (Key.F(4), Key.noModifiers), "ESC[[D -> F4");
      assertKey(parseStr("\027[[E"), (Key.F(5), Key.noModifiers), "ESC[[E -> F5");
    });

    /* -------------------------------------------------------------- */
    /* Kitty disambiguate-only CSI-u: ESC[<code>;<mods>u               */
    /* -------------------------------------------------------------- */
    Test.run("CSI-u: Enter (13), Tab (9), Escape (27), Backspace (127)", () => {
      assertKey(parseStr("\027[13u"), (Key.Enter, Key.noModifiers), "ESC[13u -> Enter");
      assertKey(parseStr("\027[9u"), (Key.Tab, Key.noModifiers), "ESC[9u -> Tab");
      assertKey(parseStr("\027[27u"), (Key.Escape, Key.noModifiers), "ESC[27u -> Escape");
      assertKey(parseStr("\027[127u"), (Key.Backspace, Key.noModifiers), "ESC[127u -> Backspace");
    });

    Test.run("CSI-u: Shift+Enter (from the plan's worked example)", () => {
      /* ESC[13;2u = 1B 5B 31 33 3B 32 75 */
      let result = parseStr("\027[13;2u");
      assertKey(result, (Key.Enter, mods(~shift=true, ())), "ESC[13;2u -> Enter+shift");
    });

    Test.run("CSI-u: printable range 32..126 -> Char", () => {
      assertKey(parseStr("\027[32u"), (Key.Char(' '), Key.noModifiers), "ESC[32u -> Char(' ')");
      assertKey(parseStr("\027[97u"), (Key.Char('a'), Key.noModifiers), "ESC[97u -> Char('a')");
      assertKey(parseStr("\027[126u"), (Key.Char('~'), Key.noModifiers), "ESC[126u -> Char('~')");
    });

    Test.run("CSI-u: printable char with modifier", () => {
      let result = parseStr("\027[97;5u"); /* Ctrl+a via CSI-u */
      assertKey(result, (Key.Char('a'), mods(~ctrl=true, ())), "ESC[97;5u -> Char('a') with ctrl");
    });

    Test.run("CSI-u: code outside mapped ranges -> Unknown", () => {
      let result = parseStr("\027[200u");
      assertKey(result, (Key.Unknown, Key.noModifiers), "ESC[200u -> Unknown");
    });

    Test.run("truncated CSI sequences (no final byte) stay Unknown", () => {
      assertKey(parseStr("\027[1;5"), (Key.Unknown, Key.noModifiers), "ESC[1;5 (no final) -> Unknown");
      assertKey(parseStr("\027[1;"), (Key.Unknown, Key.noModifiers), "ESC[1; (no final) -> Unknown");
      assertKey(parseStr("\027[9"), (Key.Unknown, Key.noModifiers), "ESC[9 (no final) -> Unknown");
    });

    /* -------------------------------------------------------------- */
    /* Alt/meta sequences: ESC <char>                                  */
    /* -------------------------------------------------------------- */
    Test.run("Alt+b (word backward)", () => {
      let result = parseStr("\027b");
      assertKey(
        result,
        (Key.Arrow_left, mods(~alt=true, ())),
        "ESC b -> Arrow_left+alt (word backward)",
      );
    });

    Test.run("Alt+Shift+B (select word backward)", () => {
      let result = parseStr("\027B");
      assertKey(
        result,
        (Key.Arrow_left, mods(~alt=true, ~shift=true, ())),
        "ESC B -> Arrow_left+alt+shift",
      );
    });

    Test.run("Alt+f (word forward)", () => {
      let result = parseStr("\027f");
      assertKey(
        result,
        (Key.Arrow_right, mods(~alt=true, ())),
        "ESC f -> Arrow_right+alt (word forward)",
      );
    });

    Test.run("Alt+Shift+F (select word forward)", () => {
      let result = parseStr("\027F");
      assertKey(
        result,
        (Key.Arrow_right, mods(~alt=true, ~shift=true, ())),
        "ESC F -> Arrow_right+alt+shift",
      );
    });

    Test.run("Alt+Backspace (delete word): ESC DEL", () => {
      let result = parseStr("\027" ++ String.make(1, Char.chr(127)));
      assertKey(
        result,
        (Key.Backspace, mods(~alt=true, ())),
        "ESC DEL -> Backspace+alt",
      );
    });

    Test.run("Alt+lowercase letter (not b/f)", () => {
      let result = parseStr("\027x");
      assertKey(result, (Key.Char('x'), mods(~alt=true, ())), "ESC x -> Char('x')+alt");
    });

    Test.run("Alt+uppercase letter implies alt+shift", () => {
      let result = parseStr("\027X");
      assertKey(
        result,
        (Key.Char('X'), mods(~alt=true, ~shift=true, ())),
        "ESC X -> Char('X')+alt+shift",
      );
    });

    /* -------------------------------------------------------------- */
    /* Multi-byte UTF-8 input -> Key.Text                              */
    /* -------------------------------------------------------------- */
    Test.run("2-byte UTF-8 char returns Text", () => {
      let result = parseStr("\xC3\xA9"); /* e-acute */
      assertKey(result, (Key.Text("\xC3\xA9"), Key.noModifiers), "e-acute -> Text");
    });

    Test.run("3-byte UTF-8 char returns Text", () => {
      let result = parseStr("\xE6\x97\xA5"); /* CJK */
      assertKey(
        result,
        (Key.Text("\xE6\x97\xA5"), Key.noModifiers),
        "CJK char -> Text",
      );
    });

    Test.run("4-byte UTF-8 char returns Text", () => {
      let result = parseStr("\xF0\x9F\x98\x80"); /* emoji */
      assertKey(
        result,
        (Key.Text("\xF0\x9F\x98\x80"), Key.noModifiers),
        "emoji -> Text",
      );
    });

    Test.run("ESC + multi-byte UTF-8 char is Alt+Text", () => {
      let result = parseStr("\027\xC3\xA9");
      assertKey(
        result,
        (Key.Text("\xC3\xA9"), mods(~alt=true, ())),
        "ESC e-acute -> Text+alt",
      );
    });

    Test.run("ESC + 3-byte UTF-8 char is Alt+Text", () => {
      let result = parseStr("\027\xE6\x97\xA5");
      assertKey(
        result,
        (Key.Text("\xE6\x97\xA5"), mods(~alt=true, ())),
        "ESC CJK -> Text+alt",
      );
    });

    Test.run("incomplete or malformed UTF-8 stays Unknown", () => {
      assertKey(
        parseStr("\xC3"),
        (Key.Char('\xC3'), Key.noModifiers),
        "a single lead byte is still a 1-byte Char",
      );
      assertKey(
        parseStr("\xC3\x41"),
        (Key.Unknown, Key.noModifiers),
        "bad continuation byte -> Unknown",
      );
      assertKey(
        parseStr("\xE6\x97"),
        (Key.Unknown, Key.noModifiers),
        "truncated 3-byte sequence -> Unknown",
      );
      assertKey(
        parseStr("\xE6\x97\xA5\xE6\x9C\xAC"),
        (Key.Unknown, Key.noModifiers),
        "two chars in one read -> Unknown (paste is handled elsewhere)",
      );
    });

    Test.run("two pasted ASCII chars are still Unknown", () => {
      /* Documents that Text is only for one complete multi-byte codepoint;
       * multi-char reads remain the InputDecoder's job. */
      assertKey(
        parseStr("ab"),
        (Key.Unknown, Key.noModifiers),
        "'ab' in one read -> Unknown",
      );
    });

    /* -------------------------------------------------------------- */
    /* feedBytes / feedKeys (test/input.re) end-to-end simulation      */
    /* -------------------------------------------------------------- */
    Test.run("feedBytes delivers plain chars one at a time", () => {
      KeyRecorder.reset();
      let handle = Runtime.startHeadless((module KeyRecorder));
      Input.feedBytes(handle, "abc");
      Test.assertEqual(
        KeyRecorder.recorded^,
        [
          (Key.Char('a'), Key.noModifiers),
          (Key.Char('b'), Key.noModifiers),
          (Key.Char('c'), Key.noModifiers),
        ],
        "abc -> three separate Char events",
      );
      handle.quit();
    });

    Test.run("feedBytes delivers an escape sequence as one event", () => {
      KeyRecorder.reset();
      let handle = Runtime.startHeadless((module KeyRecorder));
      Input.feedBytes(handle, "\027[A");
      Test.assertEqual(
        KeyRecorder.recorded^,
        [(Key.Arrow_up, Key.noModifiers)],
        "ESC[A -> single Arrow_up event",
      );
      handle.quit();
    });

    Test.run("feedBytes mixes plain chars and an escape sequence", () => {
      KeyRecorder.reset();
      let handle = Runtime.startHeadless((module KeyRecorder));
      Input.feedBytes(handle, "ab\027[Bc");
      Test.assertEqual(
        KeyRecorder.recorded^,
        [
          (Key.Char('a'), Key.noModifiers),
          (Key.Char('b'), Key.noModifiers),
          (Key.Arrow_down, Key.noModifiers),
          (Key.Char('c'), Key.noModifiers),
        ],
        "chars and escape sequence correctly split into 4 events",
      );
      handle.quit();
    });

    Test.run("feedKeys delivers pre-parsed keys directly", () => {
      KeyRecorder.reset();
      let handle = Runtime.startHeadless((module KeyRecorder));
      Input.feedKeys(
        handle,
        [(Key.Enter, Key.noModifiers), (Key.Tab, Key.noModifiers)],
      );
      Test.assertEqual(
        KeyRecorder.recorded^,
        [(Key.Enter, Key.noModifiers), (Key.Tab, Key.noModifiers)],
        "feedKeys forwards keys unchanged",
      );
      handle.quit();
    });
  });
};
