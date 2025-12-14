open Matcha;

module OptionalParams = {
  [@component]
  let make = (~first: string, ~second: string=?) => {
    let secondValue = Option.value(~default="no second", second);
    <Column> <Text> first </Text> <Text> secondValue </Text> </Column>;
  };
};

[@component]
let make = () => {
  <Column>
    <OptionalParams first="First" second="Second" />
    <OptionalParams first="First" />
  </Column>;
};

module App = {
  let make = make;
};

let () = Runtime.start((module App));
