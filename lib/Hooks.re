/* Hook state storage */
type hookValue =
  | StateHook(ref(Obj.t));

type renderContext = {
  mutable hookIndex: int,
  mutable hooks: array(hookValue),
  mutable keyHandlers: list((Key.t, Key.modifiers) => unit),
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
    needsRerender: true,
    quit,
  };
};

/* Internal: prepare context for a render pass */
let beginRender = (ctx: renderContext): unit => {
  ctx.hookIndex = 0;
  ctx.keyHandlers = [];
};
