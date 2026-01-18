/* Test different comparison methods */

let test_comparisons = () => {
  let f1 = (x) => x + 1;
  let f2 = (x) => x + 1;
 let obj1 = Obj.repr(f1);
  let obj2 = Obj.repr(f2);
  let obj3 = Obj.repr(f1);

  Printf.printf("Testing comparison methods on Obj.t with functions...\n\n");

  /* Method 1: != operator - THIS CRASHES */
  try({
    let result = obj1 != obj2;
    Printf.printf("!= operator: %b (SUCCESS - no crash)\n", result);
  }) {
  | Invalid_argument(msg) => Printf.printf("!= operator: FAILED with: %s\n", msg)
  };

  /* Method 2: == operator */
  try({
    let result = obj1 == obj2;
    Printf.printf("== operator: %b\n", result);
    let result2 = obj1 == obj3;
    Printf.printf("== operator (same ref): %b (SUCCESS)\n", result2);
  }) {
  | Invalid_argument(msg) => Printf.printf("== operator: FAILED with: %s\n", msg)
  };

  /* Method 3: phys_equal */
  try({
    let result = obj1 === obj2;
    Printf.printf("=== operator (phys_equal): %b\n", result);
    let result2 = obj1 === obj3;
    Printf.printf("=== operator (same ref): %b (SUCCESS)\n", result2);
  }) {
  | Invalid_argument(msg) => Printf.printf("=== operator: FAILED with: %s\n", msg)
  };

  Printf.printf("\nConclusion: Use === (phys_equal) or == for Obj.t comparison\n");
};

let () = test_comparisons();
