open Matcha;

[@component]
let make = (~name: string, ~age: int, ~selected: bool) => {
  let prefix = selected ? "> " : "  ";
  let text = Printf.sprintf("%s%s (%d years old)", prefix, name, age);

  if (selected) {
    Element.bold(Element.text(text));
  } else {
    Element.text(text);
  };
};
