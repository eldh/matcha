/*
 * Guards for the opam package's install manifest (matcha.install)
 *
 * Matcha is a library. `opam install matcha` must not put anything into
 * the user's PATH - no example, no demo, no debug binary. Until this test
 * was added, every example under examples/ carried a
 * `(public_name matcha-example-<foo>)` in its dune file, so `dune build`
 * generated a `bin:` section listing all 15 example executables in
 * matcha.install; `opam install matcha` would have dropped 15 demo
 * binaries into the user's PATH. Those public_name lines were removed.
 *
 * This suite reads the dune-generated matcha.install directly and checks
 * that no bin: section, and no example binary, ever comes back - so the
 * regression cannot be reintroduced silently by a new example dune file.
 */

/* Read a whole file into a string. */
let readFile = (path: string): string => {
  let ic = open_in_bin(path);
  let n = in_channel_length(ic);
  let s = really_input_string(ic, n);
  close_in(ic);
  s;
};

/* The manifest lives at _build/default/matcha.install. The test binary
 * runs with its cwd at _build/default/test/, so reach it via
 * "../matcha.install"; fall back to the repo-root-relative path so this
 * also works when run from the repo root. Mirrors the idiom in
 * test/golden.re's runExample.
 *
 * Called from inside each Test.run body (rather than once, shared, in the
 * group) so a missing file fails that one test cleanly through Test.run's
 * own try/with, instead of raising past it and crashing the whole runner
 * before later suites (and the summary) ever print. */
let readManifest = (): string => {
  let relPath = "../matcha.install";
  let path =
    if (Sys.file_exists(relPath)) {
      relPath;
    } else {
      "_build/default/matcha.install";
    };
  if (!Sys.file_exists(path)) {
    Test.assertTrue(
      false,
      "matcha.install not found at \""
      ++ relPath
      ++ "\" or \"_build/default/matcha.install\" - run `dune build matcha.install` first",
    );
    "";
  } else {
    readFile(path);
  };
};

let run = () =>
  Test.group("Packaging (matcha.install)", () => {
    Test.run("the install manifest has no bin: section", () => {
      let manifest = readManifest();
      let lines = String.split_on_char('\n', manifest);
      let offending =
        List.find_opt(
          line => {
            let trimmed = String.trim(line);
            String.length(trimmed) >= 4
            && String.sub(trimmed, 0, 4) == "bin:";
          },
          lines,
        );
      switch (offending) {
      | None => ()
      | Some(line) =>
        Test.assertTrue(
          false,
          "matcha.install has a bin: section - matcha is a library and must "
          ++ "install no executables. Offending line: \""
          ++ line
          ++ "\"",
        )
      };
    });

    Test.run("no example binary is installed", () => {
      let manifest = readManifest();
      Test.assertFalse(
        Test.contains(manifest, "matcha-example"),
        "matcha.install mentions \"matcha-example\" - an examples/*/dune file "
        ++ "has probably regained a (public_name ...) stanza",
      );
      Test.assertFalse(
        Test.contains(manifest, "examples/"),
        "matcha.install mentions \"examples/\" - an examples/*/dune file has "
        ++ "probably regained a (public_name ...) stanza",
      );
    });

    Test.run("the library itself is still installed", () => {
      /* Positive control: cases above must not be passing vacuously
       * because the manifest is empty or truncated. */
      let manifest = readManifest();
      Test.assertContains(
        manifest,
        "lib:",
        "matcha.install has no lib: section at all",
      );
      Test.assertContains(
        manifest,
        "matcha/matcha.cmi",
        "matcha.install no longer installs the library's compiled interface",
      );
      Test.assertContains(
        manifest,
        "matcha/ppx/",
        "matcha.install no longer installs the matcha.ppx sublibrary - "
        ++ "consumers would lose JSX support",
      );
    });
  });
