/* Hook state storage */
type hookValue =
  | StateHook(ref(Obj.t))
  | EffectHook(ref(option(array(Obj.t))), ref(option(unit => unit)));
/* EffectHook: (previous deps, cleanup function) */

type pendingEffect = {
  effect: unit => option(unit => unit),
  cleanup: ref(option(unit => unit)),
};

type renderContext = {
  mutable hookIndex: int,
  mutable hooks: array(hookValue),
  mutable keyHandlers: list((Key.t, Key.modifiers) => unit),
  mutable pendingEffects: list(pendingEffect),
  mutable needsRerender: bool,
  quit: unit => unit,
};

let currentContext: ref(option(renderContext)) = ref(None);

let getContext = () => {
  switch (currentContext^) {
  | None => failwith("Hook called outside of render context")
  | Some(ctx) => ctx
  };
};

/* useState hook */
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
      ctx.needsRerender = true;
    };
    (initial, setState);
  } else {
    /* Subsequent render - return existing state */
    switch (ctx.hooks[idx]) {
    | StateHook(stateRef) =>
      let setState = (newValue: 'a) => {
        stateRef := Obj.repr(newValue);
        ctx.needsRerender = true;
      };
      (Obj.magic(stateRef^), setState);
    | EffectHook(_, _) => failwith("Hook type mismatch: expected StateHook")
    };
  };
};

/* Compare dependency arrays */
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

/* useEffect hook */
let useEffect =
    (effect: unit => option(unit => unit), deps: array('a)): unit => {
  let ctx = getContext();
  let idx = ctx.hookIndex;
  ctx.hookIndex = idx + 1;

  /* Convert deps to Obj.t array for comparison */
  let depsObj = Array.map(Obj.repr, deps);

  if (idx >= Array.length(ctx.hooks)) {
    /* First render - create effect hook and schedule effect */
    let prevDepsRef = ref(None);
    let cleanupRef = ref(None);
    ctx.hooks =
      Array.append(ctx.hooks, [|EffectHook(prevDepsRef, cleanupRef)|]);

    /* Schedule effect to run after render */
    ctx.pendingEffects = [
      {
        effect,
        cleanup: cleanupRef,
      },
      ...ctx.pendingEffects,
    ];

    /* Store deps for next render */
    prevDepsRef := Some(depsObj);
  } else {
    /* Subsequent render - check if deps changed */
    switch (ctx.hooks[idx]) {
    | EffectHook(prevDepsRef, cleanupRef) =>
      if (!depsEqual(prevDepsRef^, depsObj)) {
        /* Deps changed - schedule effect */
        ctx.pendingEffects = [
          {
            effect,
            cleanup: cleanupRef,
          },
          ...ctx.pendingEffects,
        ];

        /* Update deps */
        prevDepsRef := Some(depsObj);
      }
    | StateHook(_) => failwith("Hook type mismatch: expected EffectHook")
    };
  };
};

/* useEffect with no deps - runs every render */
let useEffectAlways = (effect: unit => option(unit => unit)): unit => {
  let ctx = getContext();
  let idx = ctx.hookIndex;
  ctx.hookIndex = idx + 1;

  if (idx >= Array.length(ctx.hooks)) {
    /* First render */
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
    };
  };
};

/* useKeyDown hook - registers a key handler for this render */
let useKeyDown = (handler: (Key.t, Key.modifiers) => unit): unit => {
  let ctx = getContext();
  ctx.keyHandlers = [handler, ...ctx.keyHandlers];
};

/* useQuit hook - returns a function to quit the app */
let useQuit = (): (unit => unit) => {
  let ctx = getContext();
  ctx.quit;
};

/* Internal: create a fresh context for a component */
let createContext = (quit: unit => unit): renderContext => {
  {
    hookIndex: 0,
    hooks: [||],
    keyHandlers: [],
    pendingEffects: [],
    needsRerender: true,
    quit,
  };
};

/* Internal: prepare context for a render pass */
let beginRender = (ctx: renderContext): unit => {
  ctx.hookIndex = 0;
  ctx.keyHandlers = [];
  ctx.pendingEffects = [];
};

/* Internal: run pending effects after render */
let runEffects = (ctx: renderContext): unit => {
  /* Run effects in reverse order (so they run in declaration order) */
  let effects = List.rev(ctx.pendingEffects);
  List.iter(
    ({effect, cleanup}) => {
      /* Run cleanup from previous render if exists */
      switch (cleanup^) {
      | Some(cleanupFn) => cleanupFn()
      | None => ()
      };

      /* Run effect and store new cleanup */
      cleanup := effect();
    },
    effects,
  );
  ctx.pendingEffects = [];
};

/* Internal: run all cleanup functions (for unmount) */
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
      }
    },
    ctx.hooks,
  );
};
