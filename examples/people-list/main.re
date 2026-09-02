open Matcha;

let () = {
  Runtime.start(~screen=Fullscreen, (module App));
};
