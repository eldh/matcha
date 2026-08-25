/*
 * The launcher for examples/claude-code. The app itself lives in
 * ClaudeCodeApp.re, as a library, so that test/claudecode_tests.re can drive
 * the very same component headlessly - see the header comment there.
 */
let () =
  Matcha.Runtime.start(~screen=Fullscreen, (module ClaudeCodeApp.App));
