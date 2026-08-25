/*
 * Clickable - wrap any element so that clicking it runs a callback (B4).
 *
 *   <Clickable onClick={() => select(i)}>
 *     <Text> {"Row " ++ string_of_int(i)} </Text>
 *   </Clickable>
 *
 * The click target is the BOX the parent stack allocated to this component,
 * not the ink its child painted: a click in the padding a short row leaves
 * behind still counts. Nesting works the way it looks - the innermost
 * Clickable under the pointer fires and the outer ones stay silent (see
 * Hooks.dispatchMouse; there is no bubbling in this version).
 *
 * onClick fires on BUTTON DOWN, not on release. That is the snappiest thing
 * a terminal can do and there is no drag affordance to reconsider a press
 * mid-gesture; a caller that wants release semantics (or wheel, or motion)
 * can take ~onMouseDown, which receives every OTHER event that lands on this
 * component, with coordinates rebased to its own box.
 *
 * The wheel is special: a Clickable WITHOUT ~onMouseDown is transparent to
 * it (useMouse's ~wheel=false), so a notch over a clickable row inside a
 * <ScrollView> scrolls the list rather than vanishing into the row.
 *
 * This is a hand-written component rather than a [@component] one because
 * lib/ is compiled without the ppx; the shape below - props record, a
 * componentTypeId, a make that takes the record, and a createElement that
 * calls Element.createComponent - is exactly what the ppx emits.
 */

type props = {
  onClick: unit => unit,
  onMouseDown: option(Mouse.event => unit),
  children: Element.t,
};

/* Component type ID: part of a component's identity (path + type + key), so
 * it must be stable across renders and distinct from every other component
 * type. The ppx derives one from the source location; a hand-written
 * component in lib/ names itself. */
let componentTypeId = "Matcha.Clickable";

let make = (props: props): Element.t => {
  /* Without ~onMouseDown this component has no use for wheel events, so it
   * opts OUT of wheel interest - the notch passes through it to an
   * enclosing <ScrollView> instead of dying here. With ~onMouseDown the
   * caller asked for every event, wheel included, and gets them. */
  let wantsWheel =
    switch (props.onMouseDown) {
    | Some(_) => true
    | None => false
    };
  Hooks.useMouse(~wheel=wantsWheel, ev =>
    switch (ev.Mouse.kind, ev.Mouse.button) {
    | (Mouse.Down, Mouse.Left) => props.onClick()
    | _ =>
      switch (props.onMouseDown) {
      | Some(handler) => handler(ev)
      | None => ()
      }
    }
  );
  props.children;
};

let createElement =
    (
      ~key: option(string)=?,
      ~onClick: unit => unit,
      ~onMouseDown: option(Mouse.event => unit)=?,
      ~children: Element.t,
      (),
    )
    : Element.t => {
  let props = {onClick, onMouseDown, children};
  Element.createComponent(~key?, ~typeId=componentTypeId, props, () =>
    make(props)
  );
};
