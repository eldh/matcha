/* Test to verify != is physical inequality */

let test_physical_inequality = () => {
  /* Test with functions - physical != should work */
  let f1 = (x) => x + 1;
  let f2 = (x) => x + 1;
  let f3 = f1;

  Printf.printf("f1 != f2: %b (should be true - different closures)\n", f1 != f2);
  Printf.printf("f1 != f3: %b (should be false - same reference)\n", f1 != f3);

  /* Test with Obj.t wrapped functions */
  let obj1 = Obj.repr(f1);
  let obj2 = Obj.repr(f2);
  let obj3 = Obj.repr(f3);

  Printf.printf("obj1 != obj2: %b (should be true)\n", obj1 != obj2);
  Printf.printf("obj1 != obj3: %b (should be false)\n", obj1 != obj3);

  /* This should NOT crash - != is physical comparison */
  Printf.printf("Test passed - no crash!\n");
};

let () = test_physical_inequality();
