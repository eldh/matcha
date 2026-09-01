/*
 * Modal - a bordered dialog floating over the frame (B2).
 *
 *   <Modal isOpen title="Commands" onDismiss={() => setOpen(false)}>
 *     <Palette />
 *   </Modal>
 *
 * Put it DIRECTLY IN A STACK, next to the rest of your app:
 *
 *   <VStack>
 *     <Sized size={Flex(1)}> <LogPane /> </Sized>
 *     <Modal isOpen ...> <Palette /> </Modal>
 *     <Sized size={Chars(1)}> <StatusBar /> </Sized>
 *   </VStack>
 *
 * It costs that stack NOTHING - no row, no gap slot, no justify share - in
 * either state.
 *
 * ============================================================================
 * WHY THIS IS NOT A COMPONENT
 * ============================================================================
 *
 * [createElement] returns [Element.Lazy(() => isOpen ? Overlay(...) : Empty)],
 * NOT a component that returns an Overlay. Two independent reasons, and both
 * are load-bearing:
 *
 * 1. LAYOUT. [Runtime.isInvisibleToLayout] deliberately does not look through
 *    [Component] - finding out what a component renders means RUNNING it,
 *    which cannot happen during a stack's partition. It DOES look through
 *    [Lazy]. So a component-wrapped modal would occupy a layout slot - one
 *    blank row in every VStack that holds it, open or closed - while this
 *    shape occupies none.
 *
 * 2. INPUT. A layer's MEMBERS are exactly what is rendered inside the
 *    Overlay's child. A component that merely *returns* an Overlay renders
 *    OUTSIDE it, so its own hooks would not be members - and its own modal
 *    would suppress them. The Esc binding below would stop working the moment
 *    the dialog it belongs to opened. Here the hooks live in the inner
 *    component, which the Overlay renders as its child, so they are members.
 *
 * The inner component is where everything else lives: it reads its own box
 * from [useContainerSize()] (an overlay pushes its box as a container, so no
 * escape hatch is needed and no other API had to change), draws the border,
 * owns Esc through [useInput] - captured, so Esc closes the TOP dialog and
 * not every open one - and saves/restores focus across its own lifetime.
 *
 * ============================================================================
 * SIZING
 * ============================================================================
 *
 * ~width and ~height resolve against the FRAME, never against the slot the
 * <Modal> was written in (see [Element.overlayOptions]). The defaults are
 * [Percent(60)] wide and [Auto] tall - as tall as the content plus the two
 * border rows.
 *
 * [~height=Auto] works because the overlay measures its child with the
 * container's height set to 0 (see Runtime's Overlay case): [innerH] below is
 * then 0, the modal measures as "content + 2", and the real pass gets exactly
 * that box back. [~width=Auto] is supported but measures the CONTENT's width
 * only - the border and padding are not added - so prefer an explicit width.
 *
 * ============================================================================
 * INLINE APPS: PREFER ~align={OverlayTop(n)}
 * ============================================================================
 *
 * The default [OverlayCenter] centres the dialog in the whole frame, and in
 * Inline mode the frame is the terminal - so a centred modal grows the live
 * region to the full terminal height even when the app itself is six rows
 * tall. [OverlayTop(1)] keeps it compact.
 */

type props = {
  children: Element.t,
  title: option(string),
  onDismiss: option(unit => unit),
};

/* Component type ID: part of a component's identity (path + type + key), so
 * it must be stable across renders and distinct from every other component
 * type. */
let componentTypeId = "Matcha.Modal";

/* "┌─ Title ────────┐", or "┌────────────────┐" without one, at exactly
 * [width] columns. A title too long for the box is dropped rather than
 * allowed to push the corners out of place. */
let topBorder = (~width: int, ~title: option(string)): string =>
  if (width <= 0) {
    "";
  } else if (width < 2) {
    Element.repeatString(Element.BoxChars.horizontal, width);
  } else {
    let inner = width - 2;
    let fill =
      switch (title) {
      | Some(t) when String.trim(t) != "" =>
        let label = Element.BoxChars.horizontal ++ " " ++ String.trim(t) ++ " ";
        let used = Element.visibleLength(label);
        used <= inner
          ? label ++ Element.repeatString(Element.BoxChars.horizontal, inner - used)
          : Element.repeatString(Element.BoxChars.horizontal, inner);
      | _ => Element.repeatString(Element.BoxChars.horizontal, inner)
      };
    Element.BoxChars.topLeft ++ fill ++ Element.BoxChars.topRight;
  };

let bottomBorder = (~width: int): string =>
  if (width <= 0) {
    "";
  } else if (width < 2) {
    Element.repeatString(Element.BoxChars.horizontal, width);
  } else {
    Element.BoxChars.bottomLeft
    ++ Element.repeatString(Element.BoxChars.horizontal, width - 2)
    ++ Element.BoxChars.bottomRight;
  };

/* A column of [n] copies of [s] - the left or right border, one row per
 * content row. n = 0 (the Auto-height MEASURE pass, where the container
 * reports height 0) yields a VStack with no children, which measures to no
 * rows and no columns and therefore lets the CONTENT decide how tall the
 * dialog is. That is what keeps ~height=Auto from being circular. */
let barColumn = (s: string, n: int): Element.t =>
  Element.VStack(
    List.init(max(0, n), _ => Element.Text(s)),
    {gap: 0, align: Element.AlignStretch, justify: Element.JustifyStart},
  );

let make = (props: props): Element.t => {
  /* The overlay's own box - an Overlay pushes it as a container while its
   * child renders, so this is the dialog, not the window and not the slot
   * the <Modal> was written in. */
  let box = Runtime.getContainerSize();
  let w = box.Runtime.availWidth;
  let h = box.Runtime.availHeight;

  /* ---- Focus save and restore -------------------------------------------
   * Containment is free (Hooks.commitFocus filters the ring to the topmost
   * layer's members), but RESTORE is not: when the layer closes, the ring is
   * whole again and commitFocus's "successor at the same index" rule would
   * hand focus to the first entry, not to whatever had it before.
   *
   * useMemo with no deps runs exactly once per instance, on the first render
   * - which is before this frame's commitFocus has moved focus into the
   * dialog, so it captures the PRE-MODAL id (possibly None, which is why
   * focusManager gained setFocused). The cleanup runs on unmount, and both
   * unmount paths - the effect cleanup in commitEffects and the context reap
   * in cleanupUnmountedComponents - run strictly before commitFocus, so the
   * restored id is still in the ring by the time it is checked. */
  let fm = Hooks.useFocusManager();
  let previouslyFocused =
    Hooks.useMemo(() => Hooks.instance().Hooks.focus.Hooks.focusedId, [||]);
  Hooks.useEffect(
    () => Some(() => fm.Hooks.setFocused(previouslyFocused)),
    [||],
  );

  /* Esc closes. useInput, not useKeyDown: captured, so with two modals open
   * Esc closes the top one only. (A global binding - Ctrl+C - must use
   * useKeyDown instead, or the app becomes unquittable under a modal; see
   * Hooks.useInput.) */
  let dismiss = props.onDismiss;
  Hooks.useInput((key, _mods) =>
    switch (key) {
    | Key.Escape =>
      switch (dismiss) {
      | Some(f) => f()
      | None => ()
      }
    | _ => ()
    }
  );

  /* Guarded on WIDTH only, deliberately. h == 0 is not a degenerate box, it
   * is the Auto-height MEASURE pass (Runtime's Overlay case zeroes the axis
   * it is about to determine): innerH falls out at 0, no side bars are
   * drawn, and the modal measures as "content + 2 border rows" - which is
   * the whole point. Bailing to Empty here would make an Auto-height modal
   * measure as nothing and collapse to a single border row. A genuinely
   * zero-height box never reaches this function at all; the Overlay case
   * skips rendering the child when boxH <= 0. */
  if (w <= 0) {
    Element.Empty;
  } else {
    /* One column of padding inside each border, when there is room for it. */
    let (leftBar, rightBar, barW) =
      w >= 6
        ? (
          Element.BoxChars.vertical ++ " ",
          " " ++ Element.BoxChars.vertical,
          2,
        )
        : (Element.BoxChars.vertical, Element.BoxChars.vertical, 1);
    let innerH = max(0, h - 2);
    let middle =
      if (w < 2) {
        props.children;
      } else {
        Element.HStack(
          [
            Element.Sized(barColumn(leftBar, innerH), Element.Chars(barW)),
            Element.Sized(props.children, Element.Flex(1)),
            Element.Sized(barColumn(rightBar, innerH), Element.Chars(barW)),
          ],
          {gap: 0, align: Element.AlignStretch, justify: Element.JustifyStart},
        );
      };
    Element.VStack(
      [
        Element.Sized(
          Element.Text(topBorder(~width=w, ~title=props.title)),
          Element.Chars(1),
        ),
        Element.Sized(middle, Element.Chars(innerH)),
        Element.Sized(
          Element.Text(bottomBorder(~width=w)),
          Element.Chars(1),
        ),
      ],
      {gap: 0, align: Element.AlignStretch, justify: Element.JustifyStart},
    );
  };
};

let createElement =
    (
      ~key: option(string)=?,
      ~isOpen: bool,
      ~width: Element.size=Element.Percent(60),
      ~height: Element.size=Element.Auto,
      ~align: Element.overlayAlign=Element.OverlayCenter,
      ~title: option(string)=?,
      ~shadow: bool=true,
      ~onDismiss: option(unit => unit)=?,
      ~children: Element.t,
      (),
    )
    : Element.t => {
  let props = {children, title, onDismiss};
  /* Lazy, never a component - see this file's header for the two reasons.
   * Closed is Empty, which is equally invisible to layout, so toggling
   * ~isOpen never moves a row of the app underneath. */
  Element.Lazy(() =>
    if (isOpen) {
      Element.Overlay(
        Element.createComponent(~key?, ~typeId=componentTypeId, props, () =>
          make(props)
        ),
        {
          ovWidth: width,
          ovHeight: height,
          ovAlign: align,
          ovShadow: shadow,
          ovOnDismiss: onDismiss,
        },
      );
    } else {
      Element.Empty;
    }
  );
};
