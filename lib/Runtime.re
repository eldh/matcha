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

/* ============================================================================
 * Wake Pipe for Background Thread State Updates
 * ============================================================================ */

/* Self-pipe for waking the main loop when state changes from background threads */
let wakePipe: ref(option((Unix.file_descr, Unix.file_descr))) = ref(None);

/* Initialize the wake pipe */
let initWakePipe = (): unit => {
  let (readFd, writeFd) = Unix.pipe();
  Unix.set_nonblock(readFd);
  Unix.set_nonblock(writeFd);
  wakePipe := Some((readFd, writeFd));
};

/* Drain any pending bytes from the wake pipe */
let drainWakePipe = (): unit => {
  switch (wakePipe^) {
  | Some((readFd, _)) =>
    let buf = Bytes.create(64);
    let rec drain = () => {
      try({
        let _ = Unix.read(readFd, buf, 0, 64);
        drain(); /* Keep draining until empty */
      }) {
      | Unix.Unix_error(Unix.EAGAIN, _, _) => () /* No more data */
      | Unix.Unix_error(Unix.EWOULDBLOCK, _, _) => () /* No more data */
      | _ => ()
      };
    };
    drain();
  | None => ()
  };
};

/* Wake the main loop by writing to the pipe */
let wakeMainLoop = (): unit => {
  switch (wakePipe^) {
  | Some((_, writeFd)) =>
    try(ignore(Unix.write(writeFd, Bytes.of_string("w"), 0, 1))) {
    | _ => () /* Ignore errors (pipe full, etc.) */
    }
  | None => ()
  };
};

/* Register the wake function with Hooks */
let () = Hooks.setWakeMainLoop(wakeMainLoop);

/* ============================================================================
 * Component ID Management
 * ============================================================================ */

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

/* Measure the content size of an element (simple heuristic).
 * For width: returns the maximum line length
 * For height: returns the number of lines
 */
let measureContentSize = (el: Element.t, measureWidth: bool): int => {
  /* Render element to string without constraints to measure natural size */
  let content = Element.render(el);
  let lines = Element.splitLines(content);
  if (measureWidth) {
    /* Return max visible width of any line */
    List.fold_left(
      (maxW, line) => max(maxW, Element.visibleLength(line)),
      0,
      lines,
    );
  } else {
    /* Return number of lines */
    List.length(lines);
  };
};

/* Calculate sizes for Stack children based on available space.
 * Returns list of (element, allocatedSize) pairs.
 *
 * Algorithm:
 * 1. Subtract gap space from available
 * 2. Measure Auto-sized children content
 * 3. Allocate absolute (Chars) and percentage sizes
 * 4. Distribute remaining space to flex children by ratio
 *
 * measureWidth: true for HStack (measuring widths), false for VStack (measuring heights)
 */
let calculateChildSizes =
    (
      children: list(Element.t),
      available: int,
      gap: int,
      measureWidth: bool,
    )
    : list((Element.t, int)) => {
  let numChildren = List.length(children);
  if (numChildren == 0) {
    [];
  } else {
    /* Subtract gap space */
    let totalGap = gap * (numChildren - 1);
    let availableForContent = max(0, available - totalGap);

    /* Extract size hints (default to Auto) */
    let childrenWithHints =
      children
      |> List.map(child => {
           let hint =
             switch (getSizeHint(child)) {
             | Some(s) => s
             | None => Element.Auto
             };
           (child, hint);
         });

    /* First pass: measure Auto children and calculate fixed sizes */
    let childrenWithMeasured =
      childrenWithHints
      |> List.map(((child, hint)) => {
           let measured =
             switch (hint) {
             | Element.Auto =>
               let unwrapped = unwrapSized(child);
               Some(measureContentSize(unwrapped, measureWidth));
             | _ => None
             };
           (child, hint, measured);
         });

    /* Calculate totals for each size type */
    let (autoTotal, absTotal, pctTotal, flexTotal) =
      List.fold_left(
        ((auto, abs, pct, flex), (_, hint, measured)) =>
          switch (hint) {
          | Element.Auto =>
            switch (measured) {
            | Some(size) => (auto + size, abs, pct, flex)
            | None => (auto, abs, pct, flex)
            }
          | Element.Chars(n) => (auto, abs + n, pct, flex)
          | Element.Percent(p) => (
              auto,
              abs,
              pct + availableForContent * p / 100,
              flex,
            )
          | Element.Flex(f) => (auto, abs, pct, flex + f)
          },
        (0, 0, 0, 0),
        childrenWithMeasured,
      );

    /* Remaining space for flex children */
    let remainingForFlex =
      max(0, availableForContent - autoTotal - absTotal - pctTotal);

    /* Second pass: allocate actual sizes */
    childrenWithMeasured
    |> List.map(((child, hint, measured)) => {
         let size =
           switch (hint) {
           | Element.Auto =>
             switch (measured) {
             | Some(s) => s
             | None => 0
             }
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

    | Element.VStack(children, options) =>
      let {gap, align, justify}: Element.stackOptions = options;

      /* Calculate height for each child (measureWidth=false for VStack) */
      let childSizes =
        calculateChildSizes(children, constraints.availHeight, gap, false);

      /* Calculate total content height and remaining space for justify */
      let totalContentHeight =
        List.fold_left((acc, (_, h)) => acc + h, 0, childSizes);
      let totalGapHeight = gap * max(0, List.length(children) - 1);
      let usedHeight = totalContentHeight + totalGapHeight;
      let remainingSpace = max(0, constraints.availHeight - usedHeight);

      /* Calculate spacing based on justify */
      let numChildren = List.length(children);
      let (spaceBefore, spaceBetween, _spaceAfter) =
        switch (justify) {
        | Element.JustifyStart => (0, gap, 0)
        | Element.JustifyEnd => (remainingSpace, gap, 0)
        | Element.JustifyCenter => (remainingSpace / 2, gap, remainingSpace / 2)
        | Element.JustifySpaceBetween =>
          if (numChildren <= 1) {
            (0, gap, 0);
          } else {
            (0, gap + remainingSpace / (numChildren - 1), 0);
          }
        | Element.JustifySpaceAround =>
          if (numChildren == 0) {
            (0, gap, 0);
          } else {
            let space = remainingSpace / (numChildren * 2);
            (space, gap + space * 2, space);
          }
        | Element.JustifySpaceEvenly =>
          if (numChildren == 0) {
            (0, gap, 0);
          } else {
            let space = remainingSpace / (numChildren + 1);
            (space, gap + space, space);
          }
        };

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

             /* Split into lines */
             let lines = Element.splitLines(rendered);

             /* Apply horizontal alignment (cross-axis for VStack) */
             let alignedLines =
               switch (align) {
               | Element.AlignStretch =>
                 /* Pad each line to full width */
                 lines |> List.map(line => Element.padToWidth(line, constraints.availWidth))
               | Element.AlignStart =>
                 /* Left-align (no change needed, just ensure no extra padding) */
                 lines
               | Element.AlignEnd =>
                 /* Right-align each line */
                 lines
                 |> List.map(line => {
                      let lineWidth = Element.visibleLength(line);
                      let padding = max(0, constraints.availWidth - lineWidth);
                      String.make(padding, ' ') ++ line;
                    })
               | Element.AlignCenter =>
                 /* Center each line */
                 lines
                 |> List.map(line => {
                      let lineWidth = Element.visibleLength(line);
                      let padding = max(0, constraints.availWidth - lineWidth);
                      let leftPad = padding / 2;
                      String.make(leftPad, ' ') ++ line;
                    })
               };

             /* Pad or truncate to exact height */
             let paddedLines =
               if (List.length(alignedLines) >= height) {
                 let rec take = (n, lst) =>
                   switch (n, lst) {
                   | (0, _) => []
                   | (_, []) => []
                   | (n, [h, ...t]) => [h, ...take(n - 1, t)]
                   };
                 take(height, alignedLines);
               } else {
                 alignedLines @ List.init(height - List.length(alignedLines), _ => "");
               };
             String.concat("\n", paddedLines);
           });

      /* Build output with justify spacing */
      let beforeStr =
        if (spaceBefore > 0) {
          String.concat("\n", List.init(spaceBefore, _ => ""));
        } else {
          "";
        };
      let betweenStr =
        if (spaceBetween > 0) {
          String.concat("\n", List.init(spaceBetween, _ => ""));
        } else {
          "";
        };

      let content =
        String.concat(
          betweenStr == "" ? "\n" : "\n" ++ betweenStr ++ "\n",
          renderedChildren,
        );

      (spaceBefore > 0 ? beforeStr ++ "\n" : "") ++ content;

    | Element.HStack(children, options) =>
      let {gap, align, justify}: Element.stackOptions = options;

      /* Calculate width for each child (measureWidth=true for HStack) */
      let childSizes =
        calculateChildSizes(children, constraints.availWidth, gap, true);

      /* Calculate total content width and remaining space for justify */
      let totalContentWidth =
        List.fold_left((acc, (_, w)) => acc + w, 0, childSizes);
      let totalGapWidth = gap * max(0, List.length(children) - 1);
      let usedWidth = totalContentWidth + totalGapWidth;
      let remainingSpace = max(0, constraints.availWidth - usedWidth);

      /* Calculate spacing based on justify */
      let numChildren = List.length(children);
      let (spaceBefore, spaceBetween, _spaceAfter) =
        switch (justify) {
        | Element.JustifyStart => (0, gap, 0)
        | Element.JustifyEnd => (remainingSpace, gap, 0)
        | Element.JustifyCenter => (remainingSpace / 2, gap, remainingSpace / 2)
        | Element.JustifySpaceBetween =>
          if (numChildren <= 1) {
            (0, gap, 0);
          } else {
            (0, gap + remainingSpace / (numChildren - 1), 0);
          }
        | Element.JustifySpaceAround =>
          if (numChildren == 0) {
            (0, gap, 0);
          } else {
            let space = remainingSpace / (numChildren * 2);
            (space, gap + space * 2, space);
          }
        | Element.JustifySpaceEvenly =>
          if (numChildren == 0) {
            (0, gap, 0);
          } else {
            let space = remainingSpace / (numChildren + 1);
            (space, gap + space, space);
          }
        };

      /* Save component counter before measurement pass */
      let savedCounter = componentCounter^;

      /* First pass: render children with minimal height constraint to measure natural size */
      let measured =
        childSizes
        |> List.map(((child, width)) => {
             let childConstraints = {
               availWidth: width,
               availHeight: 0, /* Minimal height - let children use their natural size */
             };
             let unwrapped = unwrapSized(child);
             let rendered =
               renderElement(unwrapped, rootCtx, childConstraints);
             let lines = Element.splitLines(rendered);
             (child, width, List.length(lines));
           });

      /* Use the container height for cross-axis alignment */
      let containerHeight = max(0, constraints.availHeight);

      /* For AlignStretch, re-render with container height; otherwise use measured results */
      /* Restore counter so later rendering uses consistent IDs */
      componentCounter := savedCounter;
      /* Re-render children with appropriate height constraint based on alignment */
      let renderedChildren =
        measured
        |> List.map(((child, width, naturalHeight)) => {
             let childHeight =
               switch (align) {
               | Element.AlignStretch => containerHeight
               | _ => naturalHeight
               };
             let childConstraints = {
               availWidth: width,
               availHeight: childHeight,
             };
             let unwrapped = unwrapSized(child);
             let rendered =
               renderElement(unwrapped, rootCtx, childConstraints);
             let lines = Element.splitLines(rendered);
             lines |> List.map(line => Element.padToWidth(line, width));
           });

      let maxLines = containerHeight;

      /* Combine horizontally - zip lines together with gap */
      let gapStr = String.make(spaceBetween, ' ');

      /* Pad all children to same number of lines with vertical alignment */
      let paddedChildren =
        childSizes
        |> List.mapi((i, (_, width)) => {
             let lines = List.nth(renderedChildren, i);
             let numLines = List.length(lines);
             let emptyLine = String.make(width, ' ');

             if (numLines >= maxLines) {
               lines;
             } else {
               let linesToAdd = maxLines - numLines;
               switch (align) {
               | Element.AlignStretch =>
                 /* For stretch, pad at bottom (content should already fill height) */
                 lines @ List.init(linesToAdd, _ => emptyLine)
               | Element.AlignStart =>
                 /* Add empty lines at bottom */
                 lines @ List.init(linesToAdd, _ => emptyLine)
               | Element.AlignEnd =>
                 /* Add empty lines at top */
                 List.init(linesToAdd, _ => emptyLine) @ lines
               | Element.AlignCenter =>
                 /* Add empty lines evenly top and bottom */
                 let topPad = linesToAdd / 2;
                 let bottomPad = linesToAdd - topPad;
                 List.init(topPad, _ => emptyLine)
                 @ lines
                 @ List.init(bottomPad, _ => emptyLine);
               };
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

      /* Add horizontal padding for justify */
      let beforePad = String.make(spaceBefore, ' ');
      let lines = zipLines(0, []);
      let paddedLines =
        if (spaceBefore > 0) {
          lines |> List.map(line => beforePad ++ line);
        } else {
          lines;
        };

      String.concat("\n", paddedLines);

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

      /* Check if this component needs re-rendering (props, state, OR constraints changed) */
      Hooks.shouldRerenderComponent(
        stableId,
        props,
        constraints.availWidth,
        constraints.availHeight,
      )
        /* Component needs re-render - set up its context and evaluate */
        ? {
          let componentCtx =
            switch (Hooks.getComponentContext(stableId)) {
            | Some(ctx) => ctx
            | None => Hooks.createComponentContext(stableId, rootCtx.quit)
            };

          /* Update stored props and constraints */
          Hooks.updateComponentProps(stableId, props);
          Hooks.updateComponentConstraints(
            stableId,
            constraints.availWidth,
            constraints.availHeight,
          );

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
        /* Props, state, and constraints haven't changed - use cached output */
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
            Hooks.updateComponentConstraints(
              stableId,
              constraints.availWidth,
              constraints.availHeight,
            );

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

  /* Initialize wake pipe for background thread state updates */
  initWakePipe();

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

    /* Wait for input on stdin or wake pipe using select */
    let readFds =
      switch (wakePipe^) {
      | Some((readFd, _)) => [Unix.stdin, readFd]
      | None => [Unix.stdin]
      };

    let (ready, _, _) =
      try(Unix.select(readFds, [], [], 0.1)) {
      | Unix.Unix_error(Unix.EINTR, _, _) => ([], [], []) /* Interrupted by signal */
      };

    /* Drain wake pipe if it was signaled */
    let hasWakePipe =
      switch (wakePipe^) {
      | Some((readFd, _)) => List.mem(readFd, ready)
      | None => false
      };
    if (hasWakePipe) {
      drainWakePipe();
    };

    /* Handle stdin input if ready */
    if (List.mem(Unix.stdin, ready)) {
      switch (Terminal.readKey()) {
      | Some((key, modifiers)) =>
        List.iter(handler => handler(key, modifiers), ctx.keyHandlers)
      | None => ()
      };
    };
  };
};
