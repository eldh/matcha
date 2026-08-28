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
  Key_tests.run();
  Textwidth_tests.run();
  Styledtext_tests.run();
  Layout_tests.run();
  Textarea_tests.run();
  Context_tests.run();
  Headless_tests.run();
  Mock_tests.run();
  Hooks_regression_tests.run();
  Perf_tests.run();
  Golden_tests.run();
  Vterm_tests.run();
  Pty_tests.run();
  Framediff_tests.run();
  Repro_tests.run();
  Timer_tests.run();
  Paste_tests.run();
  Mouse_parse_tests.run();
  Mouse_tests.run();
  Focus_tests.run();
  Liveregion_tests.run();
  Static_tests.run();
  Scrollview_tests.run();
  Chat_tests.run();
  Claudecode_tests.run();

  /* Print summary and exit */
  Test.finish();
};
