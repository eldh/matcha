open Matcha;

type dimensions = {
  width: int,
  height: int,
};

include Context.Make({
  type t = dimensions;
  let default = {
    width: 80,
    height: 24,
  };
});

/* Provider component that reads the size of its container and provides it.
 *
 * It is mounted at the root, with no <Container> above it, so the query
 * answers with the whole frame - the terminal size, honouring
 * MATCHA_WIDTH/MATCHA_HEIGHT (the raw Terminal.getSize() this replaced did
 * not). Move it inside a <Container> and the dimensions it provides become
 * that region's, which is usually what a consumer actually wants. */
module Provider = {
  [@component]
  let make = (~children: Element.t) => {
    let {Runtime.availWidth: width, availHeight: height} = useContainerSize();
    provide(
      {
        width,
        height,
      },
      children,
    );
  };
};
