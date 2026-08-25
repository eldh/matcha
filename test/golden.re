/*
 * Golden frame testing helpers.
 *
 * A "golden" is a saved terminal frame (post stripAnsi, whitespace
 * normalized) that we compare future renders against. Goldens live in
 * test/goldens/<name>.txt.
 *
 * To (re)generate goldens after an intentional rendering change:
 *   UPDATE_GOLDENS=1 dune exec test/run_tests.exe
 */
open Matcha;

/* Resolve the directory that holds golden files.
 * Under `dune runtest` the test binary's cwd is _build/default/test, where
 * the globbed goldens txt-file deps land directly in ./goldens.
 * Under `dune exec test/run_tests.exe` from the repo root, cwd is the repo
 * root, so the goldens live under test/goldens.
 */
let goldenDir = (): string =>
  if (Sys.file_exists("goldens")) {
    "goldens";
  } else {
    "test/goldens";
  };

/* Strip trailing whitespace (spaces/tabs/CR) from every line. This is layout
 * padding noise that shouldn't cause golden mismatches. */
let normalize = (s: string): string => {
  let stripTrailingWs = (line: string): string => {
    let len = String.length(line);
    let rec findEnd = i =>
      if (i <= 0) {
        0;
      } else if (line.[i - 1] == ' ' || line.[i - 1] == '\t' || line.[i - 1] == '\r') {
        findEnd(i - 1);
      } else {
        i;
      };
    String.sub(line, 0, findEnd(len));
  };
  s
  |> String.split_on_char('\n')
  |> List.map(stripTrailingWs)
  |> String.concat("\n");
};

let isUpdating = (): bool =>
  switch (Sys.getenv_opt("UPDATE_GOLDENS")) {
  | Some("1") => true
  | _ => false
  };

let writeGolden = (path: string, contents: string): unit => {
  let oc = open_out(path);
  output_string(oc, contents);
  close_out(oc);
};

let readGolden = (path: string): string => {
  let ic = open_in_bin(path);
  let len = in_channel_length(ic);
  let contents = really_input_string(ic, len);
  close_in(ic);
  contents;
};

/* Truncate a string for display in a failure message so a single huge line
 * (e.g. from a pathologically wide render) can't blow up test output. */
let truncateForDisplay = (s: string, maxLen: int): string =>
  if (String.length(s) <= maxLen) {
    s;
  } else {
    String.sub(s, 0, maxLen) ++ "... (truncated, " ++ string_of_int(String.length(s)) ++ " chars total)";
  };

/* Find the first line where expected and actual differ, for a readable
 * failure message rather than dumping two huge blobs. */
let firstDiffLine =
    (expected: string, actual: string): option((int, string, string)) => {
  let expectedLines = Array.of_list(String.split_on_char('\n', expected));
  let actualLines = Array.of_list(String.split_on_char('\n', actual));
  let maxLen = max(Array.length(expectedLines), Array.length(actualLines));
  let rec go = i =>
    if (i >= maxLen) {
      None;
    } else {
      let e = i < Array.length(expectedLines) ? expectedLines[i] : "<no line - actual is shorter>";
      let a = i < Array.length(actualLines) ? actualLines[i] : "<no line - expected is shorter>";
      if (e != a) {
        Some((i, truncateForDisplay(e, 300), truncateForDisplay(a, 300)));
      } else {
        go(i + 1);
      };
    };
  go(0);
};

/* Compare `actual` against the golden file <goldenDir>/<name>.txt.
 * With UPDATE_GOLDENS=1, (re)writes the golden instead of comparing. */
let check = (name: string, actual: string): unit => {
  let actual = normalize(actual);
  let path = goldenDir() ++ "/" ++ name ++ ".txt";
  if (isUpdating()) {
    writeGolden(path, actual);
    print_endline("updated golden " ++ name);
  } else if (!Sys.file_exists(path)) {
    Test.assertTrue(
      false,
      "Missing golden file: "
      ++ path
      ++ ". Run `UPDATE_GOLDENS=1 dune exec test/run_tests.exe` from the repo root to generate it.",
    );
  } else {
    let expected = readGolden(path);
    if (expected != actual) {
      /* Deliberately do NOT dump the full expected/actual strings here:
       * a pathological render (e.g. huge dimensions) could make either
       * string enormous, and embedding both in an exception message would
       * blow up test output/memory. A truncated first-diff-line summary
       * plus overall lengths is enough to diagnose real mismatches; use
       * UPDATE_GOLDENS=1 and `git diff` to see the full picture. */
      let detail =
        switch (firstDiffLine(expected, actual)) {
        | Some((lineNo, e, a)) =>
          Printf.sprintf(
            "first difference at line %d:\n      expected: %S\n      actual:   %S",
            lineNo,
            e,
            a,
          )
        | None => "(strings differ but no differing line found - trailing content mismatch)"
        };
      Test.assertTrue(
        false,
        Printf.sprintf(
          "Golden mismatch for %s (%s)\n      expected length: %d, actual length: %d\n      %s\n      Run `UPDATE_GOLDENS=1 dune exec test/run_tests.exe` and inspect with `git diff` if this change is intentional.",
          name,
          path,
          String.length(expected),
          String.length(actual),
          detail,
        ),
      );
    };
  };
};

/* Render a stateless/root-level HooksComponent headlessly and check its
 * output against a golden. */
let checkComponent =
    (name: string, ~width=80, ~height=24, module C: Runtime.HooksComponent)
    : unit => {
  let config: Runtime.headlessConfig = {width, height};
  let handle = Runtime.startHeadless(~config, (module C));
  let output = handle.getOutput(true);
  handle.quit();
  check(name, output);
};

/* Hard cap on bytes captured from an example subprocess. This is a safety
 * net against a misbehaving example (e.g. one that renders based on a
 * garbage/uninitialized terminal size and produces a pathologically large
 * frame) - it keeps the test suite bounded in time and memory even if a
 * child never reaches EOF on its own within the timeout. */
let maxCaptureBytes = 4 * 1024 * 1024; /* 4 MB */

/* Read available bytes from `fd` until EOF, `timeoutSec` elapses, or
 * `maxCaptureBytes` have been captured (whichever comes first). */
let readAllWithTimeout = (fd: Unix.file_descr, timeoutSec: float): string => {
  let buf = Buffer.create(4096);
  let chunk = Bytes.create(4096);
  let deadline = Unix.gettimeofday() +. timeoutSec;
  let rec loop = () => {
    let remaining = deadline -. Unix.gettimeofday();
    if (remaining <= 0.0) {
      (); /* timed out - stop reading */
    } else if (Buffer.length(buf) >= maxCaptureBytes) {
      (); /* captured enough - stop reading (see maxCaptureBytes above) */
    } else {
      let (ready, _, _) =
        try(Unix.select([fd], [], [], remaining)) {
        | Unix.Unix_error(Unix.EINTR, _, _) => ([], [], [])
        };
      switch (ready) {
      | [] => loop() /* EINTR retry / spurious wakeup - deadline check above bounds this */
      | _ =>
        let n = Unix.read(fd, chunk, 0, 4096);
        if (n == 0) {
          (); /* EOF */
        } else {
          Buffer.add_subbytes(buf, chunk, 0, n);
          loop();
        };
      };
    };
  };
  loop();
  Buffer.contents(buf);
};

/* Run a built example binary headlessly (as a subprocess) and return its
 * stdout output (ANSI stripped). Does not compare against any golden -
 * use `checkExample` for that. Exposed separately for examples whose
 * output isn't fully deterministic (e.g. async-fetch), where callers may
 * want to assert on a stable substring instead of an exact golden match. */
let runExample = (name: string): string => {
  let relPath = "../examples/" ++ name ++ "/main.exe";
  let path =
    if (Sys.file_exists(relPath)) {
      relPath;
    } else {
      "_build/default/examples/" ++ name ++ "/main.exe";
    };

  let devnull = Unix.openfile("/dev/null", [Unix.O_RDONLY], 0);
  let (readEnd, writeEnd) = Unix.pipe();
  Unix.set_close_on_exec(readEnd);

  let pathEnv =
    switch (Sys.getenv_opt("PATH")) {
    | Some(p) => p
    | None => "/usr/bin:/bin"
    };
  let env = [|
    "PATH=" ++ pathEnv,
    "MATCHA_HEADLESS=1",
    "MATCHA_WIDTH=80",
    "MATCHA_HEIGHT=24",
  |];

  let pid = Unix.create_process_env(path, [|path|], env, devnull, writeEnd, Unix.stderr);
  Unix.close(devnull);
  Unix.close(writeEnd);

  let output = readAllWithTimeout(readEnd, 10.0);
  Unix.close(readEnd);

  /* If the child is still running (e.g. read timed out), kill it so we
   * don't hang the test suite; otherwise this is a harmless no-op on an
   * already-exited (zombie) pid. */
  (try(Unix.kill(pid, Sys.sigkill)) {
   | _ => ()
   });
  (try(ignore(Unix.waitpid([], pid))) {
   | _ => ()
   });

  Element.stripAnsi(output);
};

/* Like runExample, but feed the given bytes to the child's STDIN (then
 * close it, so the headless loop exits on EOF). The bytes are written
 * before the child gets a chance to read, so a multi-key string typically
 * arrives in ONE read() - which is exactly what makes this useful: it
 * exercises the byte-fed loop's event batching (deliverAll + flushDirty in
 * lib/Runtime.re), which the in-process handle path never touches. */
let runExampleWithInput = (name: string, input: string): string => {
  let relPath = "../examples/" ++ name ++ "/main.exe";
  let path =
    if (Sys.file_exists(relPath)) {
      relPath;
    } else {
      "_build/default/examples/" ++ name ++ "/main.exe";
    };

  let (stdinRead, stdinWrite) = Unix.pipe();
  Unix.set_close_on_exec(stdinWrite);
  let (readEnd, writeEnd) = Unix.pipe();
  Unix.set_close_on_exec(readEnd);

  let pathEnv =
    switch (Sys.getenv_opt("PATH")) {
    | Some(p) => p
    | None => "/usr/bin:/bin"
    };
  let env = [|
    "PATH=" ++ pathEnv,
    "MATCHA_HEADLESS=1",
    "MATCHA_WIDTH=80",
    "MATCHA_HEIGHT=24",
  |];

  let pid =
    Unix.create_process_env(path, [|path|], env, stdinRead, writeEnd, Unix.stderr);
  Unix.close(stdinRead);
  Unix.close(writeEnd);

  let bytes = Bytes.of_string(input);
  ignore(Unix.write(stdinWrite, bytes, 0, Bytes.length(bytes)));
  Unix.close(stdinWrite); /* EOF ends the child's loop */

  let output = readAllWithTimeout(readEnd, 10.0);
  Unix.close(readEnd);

  (try(Unix.kill(pid, Sys.sigkill)) {
   | _ => ()
   });
  (try(ignore(Unix.waitpid([], pid))) {
   | _ => ()
   });

  Element.stripAnsi(output);
};

/* Run a built example binary headlessly and check its first rendered frame
 * against a golden named "example-<name>". */
let checkExample = (name: string): unit => check("example-" ++ name, runExample(name));
