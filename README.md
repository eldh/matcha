# Matcha

A React-like terminal UI library for ReasonML/OCaml. Build interactive command-line applications with a familiar component-based architecture, hooks, and JSX syntax.

## Features

- **React-like API**: Components, hooks (`useState`, `useEffect`), and JSX syntax
- **Declarative UI**: Describe your UI as a tree of elements
- **Built-in hooks**: `useState`, `useEffect`, `useMemo`, `useKeyDown`, `useQuit`
- **Context API**: Share state across components without prop drilling
- **Layout primitives**: `Column`, `Row`, `Box`, `SplitView`
- **Styling**: Bold, dim, italic, underline, inverted text
- **PPX support**: `[@component]` decorator for cleaner component definitions

## Installation

Add to your `dune-project`:

```dune
(package
 (name your-app)
 (depends
  (matcha (>= 0.1.0))))
```

Add to your `dune` file:

```dune
(executable
 (name main)
 (libraries matcha)
 (preprocess (pps ppx_component)))
```

## Quick Start

```reason
open Matcha;

[@component]
let make = () => {
  let quit = Event.useQuit();
  let (count, setCount) = Component.useState(0);

  Event.useKeyDown((key, _) => {
    switch (key) {
    | Key.Arrow_up => setCount(count + 1)
    | Key.Arrow_down => setCount(max(0, count - 1))
    | Key.Char('q') => quit()
    | _ => ()
    }
  });

  <Column>
    <Bold> <Text> "Counter" </Text> </Bold>
    <Text> {"\nCount: " ++ string_of_int(count)} </Text>
    <Dim> <Text> "\n\nPress ↑/↓ to change, Q to quit" </Text> </Dim>
  </Column>;
};

module App = { let make = make; };
let () = Runtime.start((module App));
```

## Core Concepts

### Components

Components are defined using the `[@component]` decorator:

```reason
[@component]
let make = (~name: string, ~age: int) => {
  <Text> {name ++ " is " ++ string_of_int(age) ++ " years old"} </Text>
};
```

### Elements

Built-in JSX elements:

| Element       | Description                       |
| ------------- | --------------------------------- |
| `<Text>`      | Render text                       |
| `<Bold>`      | Bold text                         |
| `<Dim>`       | Dimmed text                       |
| `<Italic>`    | Italic text                       |
| `<Underline>` | Underlined text                   |
| `<Inverted>`  | Inverted colors                   |
| `<Column>`    | Vertical layout                   |
| `<Row>`       | Horizontal layout                 |
| `<Box>`       | Constrained box with width/height |

### Hooks

#### useState

```reason
let (value, setValue) = Component.useState(initialValue);
```

#### useEffect

```reason
// Run when dependencies change
Hooks.useEffect(() => {
  // Effect code
  Some(() => {
    // Cleanup (optional)
  })
}, [|dependency1, dependency2|]);

// Run every render
Hooks.useEffectAlways(() => {
  // Effect code
  None
});
```

#### useMemo

```reason
// Memoize a computed value until dependencies change
let memoizedValue =
  Hooks.useMemo(() => {
    // Expensive calculation
  }, [|dependency1, dependency2|]);
```

#### useKeyDown

```reason
Event.useKeyDown((key, modifiers) => {
  switch (key, modifiers) {
  | (Key.Char('q'), _) => quit()
  | (Key.Char('c'), {Key.ctrl: true, _}) => quit()
  | (Key.Arrow_up, _) => moveUp()
  | _ => ()
  }
});
```

#### useQuit

```reason
let quit = Event.useQuit();
// Call quit() to exit the application
```

### Context

Create shared state accessible by any component:

```reason
// Define context
module ThemeContext = {
  include Context.Make({
    type t = string;
    let default = "dark";
  });
};

// Provide value
ThemeContext.provide("light", children);

// Consume value
let theme = ThemeContext.use();
```

### Key Types

```reason
type Key.t =
  | Arrow_up | Arrow_down | Arrow_left | Arrow_right
  | Char(char)
  | Enter | Escape | Backspace
  | Unknown;

type Key.modifiers = {
  ctrl: bool,
  alt: bool,
  shift: bool,
};
```

## Examples

### Hello World

```reason
open Matcha;

[@component]
let make = () => {
  let quit = Event.useQuit();

  Event.useKeyDown((key, _) => {
    switch (key) {
    | Key.Char('q') => quit()
    | _ => ()
    }
  });

  <Column>
    <Bold> <Text> "Hello, World!" </Text> </Bold>
    <Dim> <Text> "\nPress Q to quit" </Text> </Dim>
  </Column>;
};
```

### Counter

```reason
open Matcha;

[@component]
let make = () => {
  let quit = Event.useQuit();
  let (count, setCount) = Component.useState(0);

  Event.useKeyDown((key, _) => {
    switch (key) {
    | Key.Arrow_up => setCount(count + 1)
    | Key.Arrow_down => setCount(max(0, count - 1))
    | Key.Char('r') => setCount(0)
    | Key.Char('q') => quit()
    | _ => ()
    }
  });

  <Column>
    <Text> {"Count: " ++ string_of_int(count)} </Text>
    <Dim> <Text> "\n↑: Inc  ↓: Dec  r: Reset  q: Quit" </Text> </Dim>
  </Column>;
};
```

### Async Data Fetching with useEffect

```reason
[@component]
let make = (~userId: string) => {
  let (data, setData) = Component.useState(None);
  let (loading, setLoading) = Component.useState(false);

  Hooks.useEffect(() => {
    setLoading(true);
    let cancelled = ref(false);

    let _ = Thread.create(() => {
      Thread.delay(1.0); // Simulate network delay
      if (!cancelled^) {
        setData(Some("User data for " ++ userId));
        setLoading(false);
      }
    }, ());

    Some(() => { cancelled := true }); // Cleanup cancels fetch
  }, [|userId|]);

  if (loading) {
    <Dim> <Text> "Loading..." </Text> </Dim>;
  } else {
    switch (data) {
    | None => <Text> "No data" </Text>
    | Some(d) => <Text> d </Text>
    };
  };
};
```

## Running Examples

```bash
# Build all examples
dune build

# Run examples
dune exec matcha-example-hello-world
dune exec matcha-example-counter
dune exec matcha-example-people-list
```

## License

MIT
