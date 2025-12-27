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

/* ============================================================================
 * Layout Constraints
 * ============================================================================ */

/* Layout constraints passed down during rendering */
type constraints = {
  availWidth: int,
  availHeight: int,
};

/* Current layout constraints - accessible by components during render */
let currentConstraints: ref(constraints) =
  ref({
    availWidth: 80,
    availHeight: 24,
  });

/* Get the current layout constraints (available width/height for this component) */
let getConstraints = (): constraints => currentConstraints^;

/* Extract size hint from an element (looks for Sized wrapper) */
let rec getSizeHint = (el: Element.t): option(Element.size) => {
  switch (el) {
  | Element.Sized(_, size) => Some(size)
  | Element.Lazy(f) => getSizeHint(f())
  | _ => None
  };
};

/* Unwrap Sized wrapper to get inner element */
let rec unwrapSized = (el: Element.t): Element.t => {
  switch (el) {
  | Element.Sized(child, _) => unwrapSized(child)
  | Element.Lazy(f) => unwrapSized(f())
  | _ => el
  };
};

/* Calculate sizes for Stack children based on available space.
 * Returns list of (element, allocatedSize) pairs.
 *
 * Algorithm:
 * 1. Subtract gap space from available
 * 2. Allocate absolute (Chars) sizes
 * 3. Allocate percentage sizes from original available
 * 4. Distribute remaining space to flex children by ratio
 */
let calculateChildSizes =
    (children: list(Element.t), available: int, gap: int)
    : list((Element.t, int)) => {
  let numChildren = List.length(children);
  if (numChildren == 0) {
    [];
  } else {
    /* Subtract gap space */
    let totalGap = gap * (numChildren - 1);
    let availableForContent = max(0, available - totalGap);

    /* Extract size hints (default to Flex(1)) */
    let childrenWithHints =
      children
      |> List.map(child => {
           let hint =
             switch (getSizeHint(child)) {
             | Some(s) => s
             | None => Element.Flex(1)
             };
           (child, hint);
         });

    /* First pass: calculate absolute and percentage sizes, sum flex units */
    let (absTotal, pctTotal, flexTotal) =
      List.fold_left(
        ((abs, pct, flex), (_, hint)) =>
          switch (hint) {
          | Element.Chars(n) => (abs + n, pct, flex)
          | Element.Percent(p) => (
              abs,
              pct + availableForContent * p / 100,
              flex,
            )
          | Element.Flex(f) => (abs, pct, flex + f)
          },
        (0, 0, 0),
        childrenWithHints,
      );

    /* Remaining space for flex children */
    let remainingForFlex = max(0, availableForContent - absTotal - pctTotal);

    /* Second pass: allocate actual sizes */
    childrenWithHints
    |> List.map(((child, hint)) => {
         let size =
           switch (hint) {
           | Element.Chars(n) => n
           | Element.Percent(p) => availableForContent * p / 100
           | Element.Flex(f) =>
             if (flexTotal > 0) {
               remainingForFlex * f / flexTotal;
             } else {
               0;
             }
           };
         (child, max(0, size));
       });
  };
};

/* ============================================================================
 * Rendering with Layout
 * ============================================================================ */

/* Render an element tree with layout constraints.
 * Stack components distribute space among children based on size hints.
 */
let rec renderElement =
        (
          el: Element.t,
          rootCtx: Hooks.renderContext,
          constraints: constraints,
        )
        : string => {
  /* Update current constraints so components can access them */
  let prevConstraints = currentConstraints^;
  currentConstraints := constraints;

  let result =
    switch (el) {
    | Element.Empty => ""
    | Element.Text(s) => s
    | Element.Styled(style, child) =>
      Element.styleToAnsi(style)
      ++ renderElement(child, rootCtx, constraints)
      ++ Element.resetAnsi

    | Element.VStack(children, gap) =>
      /* Calculate height for each child */
      let childSizes =
        calculateChildSizes(children, constraints.availHeight, gap);

      /* Render each child with its allocated height */
      let renderedChildren =
        childSizes
        |> List.map(((child, height)) => {
             let childConstraints = {
               availWidth: constraints.availWidth,
               availHeight: height,
             };
             let unwrapped = unwrapSized(child);
             let rendered =
               renderElement(unwrapped, rootCtx, childConstraints);

             /* Pad or truncate to exact height */
             let lines = Element.splitLines(rendered);
             let paddedLines =
               if (List.length(lines) >= height) {
                 let rec take = (n, lst) =>
                   switch (n, lst) {
                   | (0, _) => []
                   | (_, []) => []
                   | (n, [h, ...t]) => [h, ...take(n - 1, t)]
                   };
                 take(height, lines);
               } else {
                 lines @ List.init(height - List.length(lines), _ => "");
               };
             String.concat("\n", paddedLines);
           });

      /* Join with gap lines */
      let gapStr = String.concat("\n", List.init(gap, _ => ""));
      String.concat(
        gapStr == "" ? "\n" : "\n" ++ gapStr ++ "\n",
        renderedChildren,
      );

    | Element.HStack(children, gap) =>
      /* Calculate width for each child */
      let childSizes =
        calculateChildSizes(children, constraints.availWidth, gap);

      /* Render each child with its allocated width */
      let renderedChildren =
        childSizes
        |> List.map(((child, width)) => {
             let childConstraints = {
               availWidth: width,
               availHeight: constraints.availHeight,
             };
             let unwrapped = unwrapSized(child);
             let rendered =
               renderElement(unwrapped, rootCtx, childConstraints);

             /* Split into lines and pad each line to width */
             let lines = Element.splitLines(rendered);
             lines |> List.map(line => Element.padToWidth(line, width));
           });

      /* Combine horizontally - zip lines together with gap */
      let gapStr = String.make(gap, ' ');
      let maxLines =
        List.fold_left(
          (m, lines) => max(m, List.length(lines)),
          0,
          renderedChildren,
        );

      /* Pad all children to same number of lines */
      let paddedChildren =
        childSizes
        |> List.mapi((i, (_, width)) => {
             let lines = List.nth(renderedChildren, i);
             let emptyLine = String.make(width, ' ');
             if (List.length(lines) >= maxLines) {
               lines;
             } else {
               lines
               @ List.init(maxLines - List.length(lines), _ => emptyLine);
             };
           });

      /* Zip lines together */
      let rec zipLines = (lineNum: int, acc: list(string)): list(string) =>
        if (lineNum >= maxLines) {
          List.rev(acc);
        } else {
          let line =
            paddedChildren
            |> List.map(lines => List.nth(lines, lineNum))
            |> String.concat(gapStr);
          zipLines(lineNum + 1, [line, ...acc]);
        };

      String.concat("\n", zipLines(0, []));

    | Element.Sized(child, _size) =>
      /* Size hint is used by parent Stack; here we just render the child */
      renderElement(child, rootCtx, constraints)

    | Element.Lazy(f) => renderElement(f(), rootCtx, constraints)

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
          let result = renderElement(renderFn(), rootCtx, constraints);

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

            let result = renderElement(renderFn(), rootCtx, constraints);
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
      let res = renderElement(children, rootCtx, constraints);
      teardown();
      res;
    };

  /* Restore previous constraints */
  currentConstraints := prevConstraints;
  result;
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

      /* Get terminal dimensions for layout constraints */
      let (termWidth, termHeight) = Terminal.getSize();
      let constraints = {
        availWidth: termWidth,
        availHeight: termHeight,
      };

      let element = C.make();
      let output = renderElement(element, ctx, constraints);
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
