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

/* Provider component that reads terminal size and provides it */
module Provider = {
  [@component]
  let make = (~children: Element.t) => {
    let (height, width) = Terminal.getSize();
    provide(
      {
        width,
        height,
      },
      children,
    );
  };
};
