/*
 * The launcher for examples/chat. The app itself lives in ChatApp.re, as a
 * library, so that test/chat_tests.re can drive the very same component
 * headlessly - see the header comment there.
 */
let () = Matcha.Runtime.start((module ChatApp.App));
