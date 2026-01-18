/*
 * Test runner - runs all test suites
 *
 * Run with: dune exec test/run_tests.exe
 * Or use: dune runtest
 */

let () = {
  print_endline("");
  print_endline("Running Matcha Tests");
  print_endline("====================");
  print_endline("");

  /* Run all test suites */
  Element_tests.run();
  Headless_tests.run();

  /* Print summary and exit */
  Test.finish();
};
