/*
 * Context - React-style context for passing data through the component tree
 *
 * Context provides a way to pass data through the component tree without
 * passing props manually at every level. Useful for "global" data like
 * terminal dimensions, themes, or user preferences.
 *
 * Usage:
 * - Create: let ctx = Context.create(defaultValue);
 * - Provide: Context.provide(ctx, value, children)
 * - Use: let value = Context.use(ctx);
 *
 * For cleaner APIs, use the Make functor to create typed context modules.
 */

/* Unique identifier for context instances */
type contextId = int;

/* Counter for generating unique context IDs */
let nextContextId = ref(0);

/* Generate a new unique context ID (internal) */
let generateId = (): contextId => {
  let id = nextContextId^;
  nextContextId := id + 1;
  id;
};

/* Context handle for reading and providing values.
 * Type param 'a is the type of value stored in this context.
 */
type t('a) = {
  id: contextId, /* Unique identifier */
  mutable currentValue: 'a, /* Current value from nearest provider */
  defaultValue: 'a /* Default when no provider exists */
};

/* Create a new context with a default value.
 *
 * The default is used when components read the context but no
 * provider exists in the ancestor tree.
 */
let create = (defaultValue: 'a): t('a) => {
  {
    id: generateId(),
    currentValue: defaultValue,
    defaultValue,
  };
};

/* Provider element descriptor (internal) */
type provider('a) = {
  context: t('a),
  value: 'a,
  children: Element.t,
};

/* Provide a context value to a subtree.
 *
 * All descendants will read this value when calling Context.use(context).
 * The value is properly scoped - when rendering exits the subtree,
 * the previous value is restored.
 */
let provide = (context: t('a), value: 'a, children: Element.t): Element.t => {
  let previousValue = ref(context.currentValue);

  let setup = () => {
    previousValue := context.currentValue;
    context.currentValue = value;
  };

  let teardown = () => {
    context.currentValue = previousValue^;
  };

  Element.WithContext(setup, teardown, children);
};

/* Read the current value of a context.
 *
 * Returns the value from the nearest ancestor provider, or the default
 * value if no provider exists. Call this during render (inside make).
 */
let use = (context: t('a)): 'a => {
  context.currentValue;
};

/* Module signature for typed context configuration.
 * Used with the Make functor to create context modules.
 */
module type Config = {
  /* The type of value stored in the context */
  type t;
  /* The default value when no provider is present */
  let default: t;
};

/* Functor for creating typed context modules.
 *
 * Creates a module with provide and use functions that don't
 * require passing the context handle explicitly.
 *
 * Example:
 *   module Theme = Context.Make(...)
 *   Theme.provide("dark", children);
 *   let theme = Theme.use();
 */
module Make = (C: Config) => {
  /* The shared context instance for this module */
  let context = create(C.default);

  /* Provide a value to all descendants */
  let provide = (value: C.t, children: Element.t): Element.t =>
    provide(context, value, children);

  /* Read the current context value */
  let use = (): C.t => use(context);
};
