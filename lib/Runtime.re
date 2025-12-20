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

/* Component instance counter for stable IDs based on tree position */
let componentCounter = ref(0);

/* Track which component IDs were rendered in the current pass */
let renderedComponentIds: ref(list(Element.componentId)) = ref([]);

/* Registry entries keyed by render position */
type componentEntry = {
  key: option(string),
  renderFnPtr: nativeint,
  stableId: Element.componentId,
};

/* Registry mapping render position to the last seen component identity */
let componentIdRegistry: Hashtbl.t(int, componentEntry) =
  Hashtbl.create(100);

/* Reset component tracking at start of render */
let resetComponentTracking = (): unit => {
  componentCounter := 0;
  renderedComponentIds := [];
  /* Don't clear registry - keep it persistent so component IDs are stable across renders */
};

/* Generate stable component ID based on position in tree, optional key, and renderFn identity */
let generateStableComponentId =
    (key: option(string), renderFn: unit => Element.t): Element.componentId => {
  let position = componentCounter^;
  componentCounter := position + 1;

  let renderFnPtr: nativeint = Obj.magic(renderFn);
  /* Look up existing entry for this position */
  switch (Hashtbl.find_opt(componentIdRegistry, position)) {
  | Some({ key: prevKey, renderFnPtr: prevPtr, stableId })
      when prevKey == key && prevPtr == renderFnPtr => stableId
  | _ =>
    let newId = Element.generateComponentId();
    Hashtbl.replace(
      componentIdRegistry,
      position,
      {
        key,
        renderFnPtr,
        stableId: newId,
      },
    );
    newId;
  };
};

/* Track a component ID as rendered during the current pass */
let recordRenderedComponent = (componentId: Element.componentId): unit => {
  renderedComponentIds := [componentId, ...renderedComponentIds^];
};

/* Render an element tree with selective component re-rendering.
 * Components are only re-rendered if their context needs updates.
 */
let rec renderElement = (el: Element.t, rootCtx: Hooks.renderContext): string => {
  switch (el) {
  | Element.Empty => ""
  | Element.Text(s) => s
  | Element.Styled(style, child) =>
    Element.styleToAnsi(style)
    ++ renderElement(child, rootCtx)
    ++ Element.resetAnsi
  | Element.Column(children) =>
    children
    |> List.map(child => renderElement(child, rootCtx))
    |> String.concat("\n")
  | Element.Row(children) =>
    children
    |> List.map(child => renderElement(child, rootCtx))
    |> String.concat("")
  | Element.Lazy(f) => renderElement(f(), rootCtx)
  | Element.Component(_id, key, props, renderFn, cachedOutput, stableIdRef) =>
    /* Always generate stable ID based on current position in tree */
    /* This ensures components at the same position get the same ID across renders */
    let stableId = generateStableComponentId(key, renderFn);
    /* Save for debugging/tracking, but position is the source of truth */
    stableIdRef := Some(stableId);
    /* Record that this component was visited this render */
    recordRenderedComponent(stableId);

    /* Check if this component needs re-rendering (props changed OR state changed) */
    Hooks.shouldRerenderComponent(stableId, props)
      /* Component needs re-render - set up its context and evaluate */
      ? {
        let componentCtx =
          switch (Hooks.getComponentContext(stableId)) {
          | Some(ctx) => ctx
          | None => Hooks.createComponentContext(stableId, rootCtx.quit)
          };

        /* Update stored props */
        Hooks.updateComponentProps(stableId, props);

        /* Set this component's context as current */
        let previousContext = Hooks.currentContext^;
        let previousComponentId = Hooks.currentComponentId^;
        Hooks.currentContext := Some(componentCtx);
        Hooks.currentComponentId := Some(stableId);
        Hooks.beginRender(componentCtx);

        /* Render the component */
        let result = renderElement(renderFn(), rootCtx);

        /* Run effects */
        Hooks.runEffects(componentCtx);

        /* Mark as rendered (clears needsRerender flag) */
        Hooks.markComponentRendered(stableId);

        /* Restore previous context */
        Hooks.currentContext := previousContext;
        Hooks.currentComponentId := previousComponentId;

        /* Cache the result */
        cachedOutput := Some(result);
        result;
      }
      /* Props haven't changed and state hasn't changed - use cached output */
      /* Ensure stableIdRef is set even when using cache */
      : {
        stableIdRef := Some(stableId);
        /* Note: componentCounter was already incremented when we generated stableId */
        switch (cachedOutput^) {
        | Some(cached) => cached
        | None =>
          /* No cache yet - render and cache (shouldn't happen, but handle it) */
          let componentCtx =
            switch (Hooks.getComponentContext(stableId)) {
            | Some(ctx) => ctx
            | None => Hooks.createComponentContext(stableId, rootCtx.quit)
            };

          Hooks.updateComponentProps(stableId, props);

          let previousContext = Hooks.currentContext^;
          let previousComponentId = Hooks.currentComponentId^;
          Hooks.currentContext := Some(componentCtx);
          Hooks.currentComponentId := Some(stableId);
          Hooks.beginRender(componentCtx);

          let result = renderElement(renderFn(), rootCtx);
          Hooks.runEffects(componentCtx);
          Hooks.markComponentRendered(stableId);

          Hooks.currentContext := previousContext;
          Hooks.currentComponentId := previousComponentId;

          cachedOutput := Some(result);
          result;
        };
      };
  | Element.WithContext(setup, teardown, children) =>
    setup();
    let result = renderElement(children, rootCtx);
    teardown();
    result;
  | Element.Box(content, width, height, padding) =>
    /* Calculate inner dimensions */
    let innerWidth = width - padding * 2;
    let innerHeight = height - padding * 2;

    /* Render content and split into lines */
    let contentLines = Element.splitLines(renderElement(content, rootCtx));

    /* Pad lines list to fill height */
    let padLines = (lines, targetHeight) => {
      let len = List.length(lines);
      if (len >= targetHeight) {
        let rec take = (n, lst) =>
          switch (n, lst) {
          | (0, _) => []
          | (_, []) => []
          | (n, [h, ...t]) => [h, ...take(n - 1, t)]
          };
        take(targetHeight, lines);
      } else {
        lines @ List.init(targetHeight - len, _ => "");
      };
    };

    let paddedLines = padLines(contentLines, innerHeight);

    /* Add horizontal padding and constrain width */
    let hPad = String.make(padding, ' ');
    let formattedLines =
      paddedLines
      |> List.map(line =>
           hPad
           ++ Element.padToWidth(line, innerWidth)
           ++ Element.resetAnsi
           ++ hPad
         );

    /* Add vertical padding */
    let emptyLine = String.make(width, ' ');
    let vPadLines = List.init(padding, _ => emptyLine);

    String.concat("\n", vPadLines @ formattedLines @ vPadLines);
  };
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

  /* Set root context reference so components can trigger re-renders */
  Hooks.rootContext := Some(ctx);

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
    /* Also clean up any component contexts */
    Hooks.cleanupUnmountedComponents([]);
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

      /* Reset component tracking for this render */
      resetComponentTracking();

      let element = C.make();
      let output = renderElement(element, ctx);
      print_string(output);
      flush(stdout);

      /* Run effects after render */
      Hooks.runEffects(ctx);

      /* Remove any component contexts that were not rendered this pass */
      Hooks.cleanupUnmountedComponents(renderedComponentIds^);

      /* Collect key handlers from all component contexts into root context */
      Hooks.collectKeyHandlers(ctx);

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
