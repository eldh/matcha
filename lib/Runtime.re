/*
 * Runtime - Main event loop and application lifecycle
 *
 * This module provides the entry point for running a Matcha terminal
 * application. It manages:
 * - Terminal setup and cleanup
 * - The render loop
 * - Keyboard input dispatch
 * - Signal handling (terminal resize)
 * - Effect lifecycle
 *
 * Usage:
 *   Runtime.start((module MyApp));
 */

/* Module signature for a top-level application component.
 * The component must have a make function that takes unit
 * and returns an Element.t.
 */
module type HooksComponent = {
  let make: unit => Element.t;
};

/* Start the application with the given root component.
 *
 * This function:
 * 1. Sets up raw terminal mode (no echo, no line buffering)
 * 2. Hides the cursor
 * 3. Installs a SIGWINCH handler for terminal resize
 * 4. Runs the main loop (renders, dispatches events, runs effects)
 * 5. Restores terminal on exit (via at_exit handler)
 *
 * The loop continues until quit is called (via useQuit hook).
 */
let start = (module C: HooksComponent) => {
  let running = ref(true);

  let quit = (behavior: Hooks.quitBehavior) => {
    running := false;
    switch (behavior) {
    | ClearScreen => Terminal.clearScreen()
    | PreserveScreen => ()
    };
  };

  /* Create hooks context for this component */
  let ctx = Hooks.createContext(quit);

  /* Signal handler for resize - triggers re-render */
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

    /* Handle input - dispatch to all registered handlers */
    switch (Terminal.readKey()) {
    | Some((key, modifiers)) =>
      List.iter(handler => handler(key, modifiers), ctx.keyHandlers)
    | None => ()
    };
  };
};
