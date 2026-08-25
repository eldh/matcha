/*
 * Tests for Context (lib/Context.re)
 *
 * Context.provide(ctx, value, children) returns
 * Element.WithContext(setup, teardown, children) - a plain data value holding
 * two closures. Runtime.renderElement handles it by calling `setup(); render
 * children; teardown()` (see lib/Runtime.re, the Element.WithContext case).
 * Since setup/teardown are ordinary functions and Element.t's WithContext
 * constructor is public (no .mli hides it), we can test the scoping logic
 * directly - by pattern-matching the returned element and invoking
 * setup/teardown ourselves - without needing a running app. We also render
 * through Runtime.startHeadless to prove the same scoping holds when driven
 * by the real renderer.
 */
open Matcha;

/* Count non-overlapping occurrences of `needle` in `haystack`. */
let countOccurrences = (haystack: string, needle: string): int => {
  let hlen = String.length(haystack);
  let nlen = String.length(needle);
  if (nlen == 0) {
    0;
  } else {
    let count = ref(0);
    let i = ref(0);
    while (i^ <= hlen - nlen) {
      if (String.sub(haystack, i^, nlen) == needle) {
        incr(count);
        i := i^ + nlen;
      } else {
        incr(i);
      };
    };
    count^;
  };
};

let run = () => {
  /* ==========================================================================
   * Context.create / Context.use defaults
   * ========================================================================== */
  Test.group("Context - create & default value", () => {
    Test.run("use() returns the default value with no provider", () => {
      let ctx = Context.create(42);
      Test.assertEqual(Context.use(ctx), 42, "default value");
    });

    Test.run("each create() call gets an independent context", () => {
      let ctxA = Context.create("a");
      let ctxB = Context.create("b");
      Test.assertEqualStr(Context.use(ctxA), "a", "ctxA default");
      Test.assertEqualStr(Context.use(ctxB), "b", "ctxB default");
    });
  });

  /* ==========================================================================
   * Context.provide / Context.use scoping (driven directly)
   * ========================================================================== */
  Test.group("Context - provide/use scoping", () => {
    Test.run("value is visible via use() inside the provide scope", () => {
      let ctx = Context.create(0);
      let elem = Context.provide(ctx, 42, Element.Empty);
      switch (elem) {
      | Element.WithContext(setup, teardown, _children) =>
        setup();
        Test.assertEqual(Context.use(ctx), 42, "value visible during scope");
        teardown();
      | _ => Test.assertTrue(false, "expected Element.WithContext")
      };
    });

    Test.run("value is restored after the provide scope exits", () => {
      let ctx = Context.create(0);
      let elem = Context.provide(ctx, 42, Element.Empty);
      switch (elem) {
      | Element.WithContext(setup, teardown, _children) =>
        setup();
        teardown();
        Test.assertEqual(Context.use(ctx), 0, "restored to default");
      | _ => Test.assertTrue(false, "expected Element.WithContext")
      };
    });

    Test.run("provide preserves the children element unchanged", () => {
      let ctx = Context.create(0);
      let child = Element.Text("hello");
      let elem = Context.provide(ctx, 42, child);
      switch (elem) {
      | Element.WithContext(_setup, _teardown, children) =>
        switch (children) {
        | Element.Text(s) => Test.assertEqualStr(s, "hello", "children passed through")
        | _ => Test.assertTrue(false, "expected Element.Text child")
        }
      | _ => Test.assertTrue(false, "expected Element.WithContext")
      };
    });
  });

  /* ==========================================================================
   * Nested providers: shadowing and restoration
   * ========================================================================== */
  Test.group("Context - nested providers", () => {
    Test.run("inner provider shadows outer, both restore correctly on exit", () => {
      let ctx = Context.create(0);
      let outer = Context.provide(ctx, 1, Element.Empty);
      let inner = Context.provide(ctx, 2, Element.Empty);
      switch (outer, inner) {
      | (
          Element.WithContext(setupOuter, teardownOuter, _),
          Element.WithContext(setupInner, teardownInner, _),
        ) =>
        setupOuter();
        Test.assertEqual(Context.use(ctx), 1, "outer value active");

        setupInner();
        Test.assertEqual(Context.use(ctx), 2, "inner value shadows outer");

        teardownInner();
        Test.assertEqual(Context.use(ctx), 1, "restored to outer after inner exits");

        teardownOuter();
        Test.assertEqual(Context.use(ctx), 0, "restored to default after outer exits");
      | _ => Test.assertTrue(false, "expected two Element.WithContext values")
      };
    });

    Test.run("three levels of nesting restore in LIFO order", () => {
      let ctx = Context.create("default");
      let l1 = Context.provide(ctx, "one", Element.Empty);
      let l2 = Context.provide(ctx, "two", Element.Empty);
      let l3 = Context.provide(ctx, "three", Element.Empty);
      switch (l1, l2, l3) {
      | (
          Element.WithContext(s1, t1, _),
          Element.WithContext(s2, t2, _),
          Element.WithContext(s3, t3, _),
        ) =>
        s1();
        s2();
        s3();
        Test.assertEqualStr(Context.use(ctx), "three", "innermost active");
        t3();
        Test.assertEqualStr(Context.use(ctx), "two", "back to level two");
        t2();
        Test.assertEqualStr(Context.use(ctx), "one", "back to level one");
        t1();
        Test.assertEqualStr(Context.use(ctx), "default", "back to default");
      | _ => Test.assertTrue(false, "expected three Element.WithContext values")
      };
    });
  });

  /* ==========================================================================
   * Exception safety: teardown must run even when rendering children raises
   *
   * FIXED: Runtime.renderElement's WithContext case now wraps the child
   * render in Fun.protect(~finally=teardown), so an exception during render
   * can no longer leave the context stuck at the provided value. This test
   * exercises the real renderElement path.
   * ========================================================================== */
  Test.group("Context - exception safety", () => {
    Test.run(
      "context value is restored when rendering the children raises",
      () => {
        let ctx = Context.create(0);
        let elem =
          Context.provide(
            ctx,
            99,
            Element.Lazy(() => raise(Failure("boom"))),
          );
        let rootCtx = Hooks.createContext(_behavior => ());
        let constraints: Runtime.constraints = {
          availWidth: 20,
          availHeight: 5,
        };
        switch (Runtime.renderElement(elem, rootCtx, constraints, ~path="")) {
        | _ => Test.assertTrue(false, "render should have raised")
        | exception (Failure(_)) => ()
        };
        Test.assertEqual(
          Context.use(ctx),
          0,
          "value restored to default after exception during render",
        );
      },
    );
  });

  /* ==========================================================================
   * Context.Make functor - direct API
   * ========================================================================== */
  Test.group("Context - Make functor (direct)", () => {
    module Theme =
      Context.Make({
        type t = string;
        let default = "light";
      });

    Test.run("use() returns the functor's default with no provider", () => {
      Test.assertEqualStr(Theme.use(), "light", "default")
    });

    Test.run("provide()/use() scoping works the same as the raw API", () => {
      let elem = Theme.provide("dark", Element.Empty);
      switch (elem) {
      | Element.WithContext(setup, teardown, _) =>
        setup();
        Test.assertEqualStr(Theme.use(), "dark", "overridden during scope");
        teardown();
        Test.assertEqualStr(Theme.use(), "light", "restored after scope");
      | _ => Test.assertTrue(false, "expected Element.WithContext")
      };
    });

    Test.run("nested Make-functor providers shadow and restore", () => {
      let outer = Theme.provide("dark", Element.Empty);
      let inner = Theme.provide("solarized", Element.Empty);
      switch (outer, inner) {
      | (
          Element.WithContext(setupOuter, teardownOuter, _),
          Element.WithContext(setupInner, teardownInner, _),
        ) =>
        setupOuter();
        setupInner();
        Test.assertEqualStr(Theme.use(), "solarized", "inner shadows outer");
        teardownInner();
        Test.assertEqualStr(Theme.use(), "dark", "restored to outer");
        teardownOuter();
        Test.assertEqualStr(Theme.use(), "light", "restored to default");
      | _ => Test.assertTrue(false, "expected two Element.WithContext values")
      };
    });
  });

  /* ==========================================================================
   * Headless integration: context threaded through a real component tree
   * ========================================================================== */
  Test.group("Context - headless integration", () => {
    module CountContext =
      Context.Make({
        type t = int;
        let default = 0;
      });

    module Inner = {
      [@component]
      let make = () => {
        let v = CountContext.use();
        <Text> {"inner:" ++ string_of_int(v)} </Text>;
      };
    };

    module DefaultApp = {
      [@component]
      let make = () => <Inner />;
    };

    module ProvidedApp = {
      [@component]
      let make = () => CountContext.provide(5, <Inner />);
    };

    module NestedApp = {
      [@component]
      let make = () =>
        CountContext.provide(
          1,
          Element.vstack([
            <Inner />, /* outer value: 1 */
            CountContext.provide(2, <Inner />), /* shadowed: 2 */
            <Inner />, /* restored: 1 */
          ]),
        );
    };

    Test.run("component reading context with no provider sees the default", () => {
      let handle = Runtime.startHeadless((module DefaultApp));
      let output = handle.getOutput(true);
      Test.assertContains(output, "inner:0", "default value used");
      handle.quit();
    });

    Test.run("component reading context under a provider sees the provided value", () => {
      let handle = Runtime.startHeadless((module ProvidedApp));
      let output = handle.getOutput(true);
      Test.assertContains(output, "inner:5", "provided value used");
      handle.quit();
    });

    Test.run("nested provider shadows for its subtree only, siblings after it restore", () => {
      let handle = Runtime.startHeadless((module NestedApp));
      let output = handle.getOutput(true);
      Test.assertContains(output, "inner:2", "inner override visible");
      Test.assertEqual(
        countOccurrences(output, "inner:1"),
        2,
        "outer value seen both before and after the nested override",
      );
      handle.quit();
    });
  });
};
