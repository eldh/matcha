/* Component signature for hooks-based components */
module type HooksComponent = {
  let make: unit => Element.t;
};

let start = (module C: HooksComponent) => {
  let running = ref(true);

  let quit = () => {
    running := false;
  };

  /* Create hooks context for this component */
  let ctx = Hooks.createContext(quit);

  /* Signal handler for resize */
  let handleResize = _ => {
    ctx.needsRerender = true;
  };

  Sys.set_signal(Terminal.sigwinch, Sys.Signal_handle(handleResize));

  /* Set up terminal */
  Terminal.setRawMode();
  Terminal.hideCursor();
  at_exit(() => {
    /* Run effect cleanups before exit */
    Hooks.runCleanups(ctx);
    Terminal.restoreTerminal();
  });

  /* Main loop */
  while (running^) {
    if (ctx.needsRerender) {
      ctx.needsRerender = false;
      Terminal.clearScreen();

      /* Set current context and render */
      Hooks.currentContext := Some(ctx);
      Hooks.beginRender(ctx);

      let element = C.make();
      let output = Element.render(element);
      print_string(output);
      flush(stdout);

      /* Run effects after render */
      Hooks.runEffects(ctx);

      Hooks.currentContext := None;
    };

    /* Handle input */
    switch (Terminal.readKey()) {
    | Some((key, modifiers)) =>
      List.iter(handler => handler(key, modifiers), ctx.keyHandlers)
    | None => ()
    };
  };
};
