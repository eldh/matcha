/* Context ID for type-safe context lookup */
type contextId = int;

let nextContextId = ref(0);

let generateId = (): contextId => {
  let id = nextContextId^;
  nextContextId := id + 1;
  id;
};

/* A context handle that components use to read values */
type t('a) = {
  id: contextId,
  mutable currentValue: 'a,
  defaultValue: 'a,
};

/* Create a new context with a default value */
let create = (defaultValue: 'a): t('a) => {
  {
    id: generateId(),
    currentValue: defaultValue,
    defaultValue,
  };
};

/* Provider element - wraps children with a context value */
type provider('a) = {
  context: t('a),
  value: 'a,
  children: Element.t,
};

let provide = (context: t('a), value: 'a, children: Element.t): Element.t => {
  /* Store the value and render children */
  let previousValue = context.currentValue;
  context.currentValue = value;

  /* Wrap in a lazy element so the value is set when rendered */
  Element.createElement(() => {
    let result =
      switch (children) {
      | Element.Lazy(f) => f()
      | other => other
      };
    /* Restore previous value after children render */
    context.currentValue = previousValue;
    result;
  });
};

/* Read the current value of a context */
let use = (context: t('a)): 'a => {
  context.currentValue;
};

/* Functor for creating typed context modules */
module type Config = {
  type t;
  let default: t;
};

module Make = (C: Config) => {
  let context = create(C.default);

  let provide = (value: C.t, children: Element.t): Element.t =>
    provide(context, value, children);

  let use = (): C.t => use(context);
};
