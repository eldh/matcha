/*
 * TextArea - the <TextArea> applications actually use: Element.TextArea's
 * pure renderer wrapped in a real component that owns the cursor blink.
 *
 * WHY THE SPLIT. Element.TextArea is a pure element builder - it turns a
 * value, a cursor position and a selection into a [Text] node and nothing
 * else. It cannot blink on its own, because a blink needs a timer, a timer
 * needs Hooks, and Hooks depends on Element: the arrow only points one way.
 * So the pure renderer takes a [~cursorVisible] flag and this module - which
 * sits above Hooks and may use them - is the component that drives it.
 *
 * THE BLINK. A [useInterval] flips a [useState] bool every [blinkMs] (530ms,
 * the usual terminal cadence), and the flag is handed to the renderer. Both
 * hooks take slots, so they are called unconditionally on every render. The
 * blink state is per instance: two TextAreas on screen blink on their own
 * clocks, and a keyed one that remounts starts over.
 *
 * The cursor shows on an EMPTY value too: with no text to sit in, the block
 * borrows the first cell of the placeholder, which keeps its character and
 * only gains a background. A frame compared with ANSI stripped is therefore
 * unchanged whether the cursor is on, off, or absent.
 *
 * [~blink=false] opts out - a steady, always-visible cursor, and no timer is
 * registered at all.
 *
 * [~key] is supported here and was not on Element.TextArea.createElement:
 * this is a real component, so it takes part in the usual identity rules
 * (tree path + type + key), and a changed key gives a fresh blink phase.
 *
 * Element.TextArea.createElement is still there and still builds a plain
 * [Lazy] node with a steady cursor - the non-blinking fallback for anyone
 * using the element module directly. Everything reached through
 * [Matcha.TextArea] comes through here.
 */

/* Everything pure - insertAt, handleKeyDown, the selection type, make -
 * comes straight from the element-level module; only createElement is
 * shadowed below. */
include Element.TextArea;

/* Component type ID: part of a component's identity (path + type + key), so
 * it must be stable across renders and distinct from every other component
 * type. */
let componentTypeId = "Matcha.TextArea";

/* Half-period of the blink, in milliseconds. */
let blinkMs = 530;

/* MATCHA_HEADLESS=1 stream mode prints a frame per re-render to stdout; a
 * wall-clock cosmetic blink would spam that stream (and make example
 * goldens timing-dependent), so the blink is disabled there - the cursor
 * shows steady. Handle-based Runtime.startHeadless is NOT this mode: its
 * clock is virtual (advanceTime), so tests still exercise the blink. Env
 * check duplicated from Runtime.isHeadless to avoid a dependency cycle -
 * keep them in sync. */
let isStreamHeadless = Sys.getenv_opt("MATCHA_HEADLESS") == Some("1");

let createElement =
    (
      ~key: option(string)=?,
      ~value: string,
      ~onChange: string => unit,
      ~onSubmit: option(unit => unit)=?,
      ~placeholder: option(string)=?,
      ~maxWidth: option(int)=?,
      ~maxHeight: option(int)=?,
      ~minHeight: option(int)=?,
      ~cursorColor: option(Element.color)=?,
      ~selectionColor: option(Element.color)=?,
      ~cursorRow: int,
      ~cursorCol: int,
      ~setCursor: ((int, int)) => unit,
      ~selection: option(selection),
      ~setSelection: option(selection) => unit,
      ~blink: bool=true,
      (),
    )
    : Element.t =>
  Element.createComponent(~key?, ~typeId=componentTypeId, (), () => {
    let (cursorOn, setCursorOn) = Hooks.useState(true);
    let blinkActive = blink && !isStreamHeadless;
    /* ms=0 disables the interval outright (the Ink `delay={null}` idiom), so
     * a steady cursor registers no timer. */
    Hooks.useInterval(() => setCursorOn(!cursorOn), ~ms=blinkActive ? blinkMs : 0);
    make(
      ~cursorVisible=blinkActive ? cursorOn : true,
      ~value,
      ~onChange,
      ~onSubmit?,
      ~placeholder?,
      ~maxWidth?,
      ~maxHeight?,
      ~minHeight?,
      ~cursorColor?,
      ~selectionColor?,
      ~cursorRow,
      ~cursorCol,
      ~setCursor,
      ~selection,
      ~setSelection,
      (),
    );
  });
