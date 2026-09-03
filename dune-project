(lang dune 3.0)

(name matcha)

(generate_opam_files true)

(source
 (github eldh/matcha))

(license MIT)

(authors "Andreas Eldh")

(maintainers "Andreas Eldh")

; The test suite lives in its own package, and that package is NEVER
; published. `dune build -p matcha` - which is exactly what opam runs -
; ignores stanzas belonging to any other package, so the suite is excluded
; from an install.
;
; It has to be excluded. The suite opens pseudo-terminals, spawns the example
; binaries as subprocesses and drains them against wall-clock deadlines. That
; is the right way to test a terminal UI and the wrong thing to do inside
; somebody's package build: matcha 0.2.0 and 0.3.0 both failed opam CI on an
; emulated riscv64 runner and two experimental macOS ones for precisely this
; reason, while sixty-seven other jobs passed.
(package
 (name matcha-tests)
 (allow_empty)
 (synopsis "Test suite for matcha (not published)")
 (description
  "The matcha test suite, kept in its own package so that it never runs as part of installing matcha. Not published to the opam repository.")
 (depends
  (ocaml
   (>= 5.3.0))
  (reason
   (>= 3.12.0))
  matcha))

(package
 (name matcha)
 (synopsis "A React-like terminal UI library for ReasonML")
 (description
  "Build interactive command-line applications with a familiar component-based architecture, hooks, and JSX syntax. Components compose with hooks (useState, useEffect, useMemo, Context), lay out with a flexbox-like engine, and read keyboard, mouse and paste input through one normalized model. Overlays, modals, scroll views and a static transcript region are built in. A headless runtime renders frames to strings, so an application can be tested - or driven by an agent - without a terminal.")
 (tags
  ("terminal" "tui" "cli" "reason" "react" "ui" "hooks" "jsx"))
 (depends
  (ocaml
   (>= 5.3.0))
  (reason
   (>= 3.12.0))
  (ppxlib
   (and
    (>= 0.36.0)
    (< 0.39.0)))))
