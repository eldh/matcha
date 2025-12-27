open Matcha;

module OptionalParams = {
  [@component]
  let make = (~first: string, ~second: string=?) => {
    let secondValue = Option.value(~default="no second", second);
    <VStack> <Text> first </Text> <Text> secondValue </Text> </VStack>;
  };
};

[@component]
let make = () => {
  <VStack>
    <OptionalParams first="First" second="Second" />
    <OptionalParams first="First" />
  </VStack>;
};

module App = {
  let make = make;
};

let () = Runtime.start((module App));
