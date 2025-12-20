/*
 * Hooks - React-style hooks for stateful terminal components
 *
 * This module provides hooks that allow components to have state and
 * side effects while remaining functional. Hooks must be called in the
 * same order on every render.
 *
 * Available hooks:
 * - useState: Local component state
 * - useEffect: Side effects with dependency tracking
 * - useEffectAlways: Side effects that run every render
 * - useMemo: Memoize values based on dependencies
 * - useKeyDown: Register keyboard event handlers
 * - useQuit: Get a function to quit the application
 */

/* Internal hook value storage.
 * Each hook slot stores either state or effect data.
 */
type hookValue =
  | StateHook(ref(Obj.t))
  | EffectHook(ref(option(array(Obj.t))), ref(option(unit => unit)))
  | MemoHook(ref(option((Obj.t, array(Obj.t))))); /* cached value + deps */
/* EffectHook stores: (previous deps for comparison, cleanup function) */

/* Pending effect to be run after render (internal) */
type pendingEffect = {
  effect: unit => option(unit => unit),
  cleanup: ref(option(unit => unit)),
};

/* Behavior when quitting the application.
 * Controls whether the screen is cleared on exit.
 */
type quitBehavior =
  | ClearScreen /* Clear the terminal before exiting */
  | PreserveScreen; /* Keep terminal content visible after exit */

/* Render context containing all hook state for a component.
 * Managed by the runtime - not for direct use.
 */
type renderContext = {
  mutable hookIndex: int, /* Current hook slot */
  mutable hooks: array(hookValue), /* Stored hook values */
  mutable keyHandlers: list((Key.t, Key.modifiers) => unit), /* Active handlers */
  mutable pendingEffects: list(pendingEffect), /* Effects to run post-render */
  mutable needsRerender: bool, /* Whether to re-render */
  componentId: option(Element.componentId), /* Component instance ID if this is a component context */
  quit: quitBehavior => unit /* Quit callback */
};

/* Component context registry - maps component IDs to their render contexts */
let componentContexts: Hashtbl.t(Element.componentId, renderContext) =
  Hashtbl.create(100);

/* Component props registry - maps component IDs to their previous props for comparison */
let componentProps: Hashtbl.t(Element.componentId, Obj.t) =
  Hashtbl.create(100);

/* Generate a new unique component ID - delegates to Element to avoid circular dependency */
let generateComponentId = (): Element.componentId => {
  Element.generateComponentId();
};

/* Global current context (set during render) */
let currentContext: ref(option(renderContext)) = ref(None);

/* Global current component ID (set during component rendering) */
let currentComponentId: ref(option(Element.componentId)) = ref(None);

/* Global root context reference (set when Runtime.start is called) */
let rootContext: ref(option(renderContext)) = ref(None);

/* Get the current render context (internal - fails if not in render) */
let getContext = () => {
  switch (currentContext^) {
  | None => failwith("Hook called outside of render context")
  | Some(ctx) => ctx
  };
};

/* Create local state for a component.
 *
 * Returns a tuple of (currentValue, setValue). Calling setValue will
 * trigger a re-render with the new value.
 *
 * Rules:
 * - Must be called in the same order every render
 * - Don't call inside conditionals or loops
 * - The initial value is only used on first render
 *
 * Example:
 *   let (count, setCount) = Hooks.useState(0);
 *   setCount(count + 1);  // Triggers re-render
 */
let useState = (initial: 'a): ('a, 'a => unit) => {
  let ctx = getContext();
  let idx = ctx.hookIndex;
  ctx.hookIndex = idx + 1;

  if (idx >= Array.length(ctx.hooks)) {
    /* First render - initialize state */
    let stateRef = ref(Obj.repr(initial));
    ctx.hooks = Array.append(ctx.hooks, [|StateHook(stateRef)|]);

    let setState = (newValue: 'a) => {
      stateRef := Obj.repr(newValue);
      /* Only mark this component's context as needing re-render */
      switch (ctx.componentId) {
      | Some(_id) =>
        /* Component context - mark only this component */
        ctx.needsRerender = true;
        /* Also mark root context to trigger render loop */
        switch (rootContext^) {
        | Some(rootCtx) => rootCtx.needsRerender = true
        | None => ()
        };
      | None =>
        /* Root context - mark for full re-render */
        ctx.needsRerender = true
      };
    };
    (initial, setState);
  } else {
    /* Subsequent render - return existing state */
    switch (ctx.hooks[idx]) {
    | StateHook(stateRef) =>
      let setState = (newValue: 'a) => {
        stateRef := Obj.repr(newValue);
        /* Only mark this component's context as needing re-render */
        switch (ctx.componentId) {
        | Some(_id) =>
          /* Component context - mark only this component */
          ctx.needsRerender = true;
          /* Also mark root context to trigger render loop */
          switch (rootContext^) {
          | Some(rootCtx) => rootCtx.needsRerender = true
          | None => ()
          };
        | None =>
          /* Root context - mark for full re-render */
          ctx.needsRerender = true
        };
      };
      (Obj.magic(stateRef^), setState);
    | EffectHook(_, _) => failwith("Hook type mismatch: expected StateHook")
    | MemoHook(_) => failwith("Hook type mismatch: expected StateHook")
    };
  };
};

/** Compare dependency arrays for changes (internal) */
let depsEqual = (prev: option(array(Obj.t)), curr: array(Obj.t)): bool => {
  switch (prev) {
  | None => false /* First run, deps don't exist yet */
  | Some(prevDeps) =>
    if (Array.length(prevDeps) != Array.length(curr)) {
      false;
    } else {
      let equal = ref(true);
      for (i in 0 to Array.length(prevDeps) - 1) {
        if (prevDeps[i] != curr[i]) {
          equal := false;
        };
      };
      equal^;
    }
  };
};

/* Memoize a computed value until dependencies change.
 *
 * The function runs during render when dependencies differ from the
 * previous render. The returned value is cached in the hook state and
 * reused while dependencies are equal (physical equality via Obj.repr).
 *
 * Example:
 *   let expensive = Hooks.useMemo(() => doWork(data), [|data|]);
 */
let useMemo = (compute: unit => 'a, deps: array('b)): 'a => {
  let ctx = getContext();
  let idx = ctx.hookIndex;
  ctx.hookIndex = idx + 1;

  let depsObj = Array.map(Obj.repr, deps);

  let recalc = (memoRef: ref(option((Obj.t, array(Obj.t))))) => {
    let value = compute();
    memoRef := Some((Obj.repr(value), depsObj));
    value;
  };

  if (idx >= Array.length(ctx.hooks)) {
    /* First render - compute and store */
    let memoRef = ref(None);
    let value = recalc(memoRef);
    ctx.hooks = Array.append(ctx.hooks, [|MemoHook(memoRef)|]);
    value;
  } else {
    switch (ctx.hooks[idx]) {
    | MemoHook(memoRef) =>
      switch (memoRef^) {
      | Some((cachedValue, prevDeps)) =>
        if (depsEqual(Some(prevDeps), depsObj)) {
          Obj.magic(cachedValue);
        } else {
          recalc(memoRef);
        }
      | None => recalc(memoRef)
      }
    | StateHook(_) => failwith("Hook type mismatch: expected MemoHook")
    | EffectHook(_, _) => failwith("Hook type mismatch: expected MemoHook")
    };
  };
};

/* Run a side effect when dependencies change.
 *
 * The effect runs after render completes. If it returns a cleanup function,
 * that function is called before the next effect run or on unmount.
 *
 * The effect only runs when one of the dependencies changes (compared by
 * physical equality).
 *
 * Example:
 *   Hooks.useEffect(() => ..., [|id|]);
 *   Return Some(cleanupFn) to run cleanup before next effect.
 */
let useEffect =
    (effect: unit => option(unit => unit), deps: array('a)): unit => {
  let ctx = getContext();
  let idx = ctx.hookIndex;
  ctx.hookIndex = idx + 1;

  let depsObj = Array.map(Obj.repr, deps);

  if (idx >= Array.length(ctx.hooks)) {
    /* First render - create effect hook and schedule effect */
    let prevDepsRef = ref(None);
    let cleanupRef = ref(None);
    ctx.hooks =
      Array.append(ctx.hooks, [|EffectHook(prevDepsRef, cleanupRef)|]);

    ctx.pendingEffects = [
      {
        effect,
        cleanup: cleanupRef,
      },
      ...ctx.pendingEffects,
    ];

    prevDepsRef := Some(depsObj);
  } else {
    /* Subsequent render - check if deps changed */
    switch (ctx.hooks[idx]) {
    | EffectHook(prevDepsRef, cleanupRef) =>
      if (!depsEqual(prevDepsRef^, depsObj)) {
        ctx.pendingEffects = [
          {
            effect,
            cleanup: cleanupRef,
          },
          ...ctx.pendingEffects,
        ];
        prevDepsRef := Some(depsObj);
      }
    | StateHook(_) => failwith("Hook type mismatch: expected EffectHook")
    | MemoHook(_) => failwith("Hook type mismatch: expected EffectHook")
    };
  };
};

/* Run a side effect on every render.
 *
 * Unlike useEffect, this runs after every render regardless of any
 * dependencies. Use sparingly as it can impact performance.
 */
let useEffectAlways = (effect: unit => option(unit => unit)): unit => {
  let ctx = getContext();
  let idx = ctx.hookIndex;
  ctx.hookIndex = idx + 1;

  if (idx >= Array.length(ctx.hooks)) {
    let prevDepsRef = ref(None);
    let cleanupRef = ref(None);
    ctx.hooks =
      Array.append(ctx.hooks, [|EffectHook(prevDepsRef, cleanupRef)|]);

    ctx.pendingEffects = [
      {
        effect,
        cleanup: cleanupRef,
      },
      ...ctx.pendingEffects,
    ];
  } else {
    switch (ctx.hooks[idx]) {
    | EffectHook(_, cleanupRef) =>
      ctx.pendingEffects = [
        {
          effect,
          cleanup: cleanupRef,
        },
        ...ctx.pendingEffects,
      ]
    | StateHook(_) => failwith("Hook type mismatch: expected EffectHook")
    | MemoHook(_) => failwith("Hook type mismatch: expected EffectHook")
    };
  };
};

/* Register a keyboard event handler.
 *
 * The handler is called whenever a key is pressed while the app is running.
 * Handlers are cleared and re-registered on each render.
 *
 * Example:
 *   Hooks.useKeyDown((key, modifiers) =>
 *     switch (key) ...
 *   );
 */
let useKeyDown = (handler: (Key.t, Key.modifiers) => unit): unit => {
  let ctx = getContext();
  ctx.keyHandlers = [handler, ...ctx.keyHandlers];
};

/* Get a function to quit the application.
 *
 * Returns a function that, when called, will stop the main loop.
 * The quitBehavior argument controls whether the terminal is cleared.
 *
 * Example:
 *   let quit = Hooks.useQuit();
 *   quit(ClearScreen);    // Exit and clear terminal
 *   quit(PreserveScreen); // Exit but keep output visible
 */
let useQuit = (): (quitBehavior => unit) => {
  let ctx = getContext();
  ctx.quit;
};

/* ============================================================================
 * Internal Runtime Functions
 * These are used by Runtime.re to manage the render lifecycle.
 * ============================================================================ */

/* Create a fresh render context for a component.
 * Called once when the app starts. (internal)
 */
let createContext = (quit: quitBehavior => unit): renderContext => {
  {
    hookIndex: 0,
    hooks: [||],
    keyHandlers: [],
    pendingEffects: [],
    needsRerender: true,
    componentId: None, /* Root context has no component ID */
    quit,
  };
};

/* Create a render context for a component instance.
 * Called when a component is first rendered. (internal)
 */
let createComponentContext =
    (componentId: Element.componentId, quit: quitBehavior => unit)
    : renderContext => {
  let ctx = {
    hookIndex: 0,
    hooks: [||],
    keyHandlers: [],
    pendingEffects: [],
    needsRerender: true,
    componentId: Some(componentId),
    quit,
  };
  Hashtbl.add(componentContexts, componentId, ctx);
  ctx;
};

/* Get the render context for a component instance. (internal) */
let getComponentContext =
    (componentId: Element.componentId): option(renderContext) =>
  try(Some(Hashtbl.find(componentContexts, componentId))) {
  | Not_found => None
  };

/* Check if a component needs re-rendering based on state changes. (internal) */
let componentNeedsRerender = (componentId: Element.componentId): bool => {
  switch (getComponentContext(componentId)) {
  | Some(ctx) => ctx.needsRerender
  | None => true /* Component not rendered yet - needs initial render */
  };
};

/* Check if props have changed by comparing with previous props. (internal) */
let propsChanged = (componentId: Element.componentId, newProps: Obj.t): bool =>
  try({
    let prevProps = Hashtbl.find(componentProps, componentId);
    /* Use physical pointer comparison via Obj.magic to avoid comparing function values */
    /* Convert Obj.t to nativeint (pointer) and compare - safe even with functions */
    let prevPtr: nativeint = Obj.magic(prevProps);
    let newPtr: nativeint = Obj.magic(newProps);
    prevPtr != newPtr;
  }) {
  | Not_found => true /* No previous props - treat as changed */
  };

/* Update stored props for a component. (internal) */
let updateComponentProps =
    (componentId: Element.componentId, props: Obj.t): unit => {
  Hashtbl.replace(componentProps, componentId, props);
};

/* Check if a component needs re-rendering (props changed OR state changed). (internal) */
let shouldRerenderComponent =
    (componentId: Element.componentId, newProps: Obj.t): bool => {
  let stateChanged = componentNeedsRerender(componentId);
  let propsChanged = propsChanged(componentId, newProps);
  stateChanged || propsChanged;
};

/* Mark a component as rendered (clears needsRerender flag). (internal) */
let markComponentRendered = (componentId: Element.componentId): unit => {
  switch (getComponentContext(componentId)) {
  | Some(ctx) => ctx.needsRerender = false
  | None => ()
  };
};

/* Prepare context for a new render pass.
 * Resets hook index and clears transient state. (internal)
 */
let beginRender = (ctx: renderContext): unit => {
  ctx.hookIndex = 0;
  ctx.keyHandlers = [];
  ctx.pendingEffects = [];
};

/* Collect key handlers from all component contexts into the root context.
 * This ensures handlers registered in components are available for event dispatch.
 */
let collectKeyHandlers = (rootCtx: renderContext): unit => {
  /* Collect handlers from all component contexts */
  Hashtbl.iter(
    (_componentId, componentCtx) => {
      rootCtx.keyHandlers =
        List.rev_append(componentCtx.keyHandlers, rootCtx.keyHandlers)
    },
    componentContexts,
  );
  /* Reverse to maintain registration order (most recent first, but we want first registered first) */
  rootCtx.keyHandlers = List.rev(rootCtx.keyHandlers);
};

/* Run pending effects after render completes.
 * Effects run in declaration order. Cleanup from previous
 * render is called before running the new effect. (internal)
 */
let runEffects = (ctx: renderContext): unit => {
  let effects = List.rev(ctx.pendingEffects);
  List.iter(
    ({ effect, cleanup }) => {
      switch (cleanup^) {
      | Some(cleanupFn) => cleanupFn()
      | None => ()
      };
      cleanup := effect();
    },
    effects,
  );
  ctx.pendingEffects = [];
};

/* Run all cleanup functions for component unmount.
 * Called when the app is exiting. (internal)
 */
let runCleanups = (ctx: renderContext): unit => {
  Array.iter(
    hook => {
      switch (hook) {
      | EffectHook(_, cleanupRef) =>
        switch (cleanupRef^) {
        | Some(cleanup) => cleanup()
        | None => ()
        }
      | StateHook(_) => ()
      | MemoHook(_) => ()
      }
    },
    ctx.hooks,
  );
};

/* Remove component contexts that were not rendered in the latest pass.
 * Runs cleanups for those contexts and drops their stored props to
 * prevent key handlers or effects from leaking after unmount.
 */
let cleanupUnmountedComponents =
    (activeComponentIds: list(Element.componentId)): unit => {
  /* Track currently active IDs for quick membership checks */
  let activeSet: Hashtbl.t(Element.componentId, unit) =
    Hashtbl.create(Hashtbl.length(componentContexts) + 10);
  List.iter(id => Hashtbl.replace(activeSet, id, ()), activeComponentIds);

  Hashtbl.iter(
    (componentId, ctx) =>
      if (!Hashtbl.mem(activeSet, componentId)) {
        runCleanups(ctx);
        Hashtbl.remove(componentContexts, componentId);
        Hashtbl.remove(componentProps, componentId);
      },
    componentContexts,
  );
};
