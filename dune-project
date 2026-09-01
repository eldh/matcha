(lang dune 3.0)

(name matcha)

(generate_opam_files true)

(source
 (github eldh/matcha))

(license MIT)

(authors "Andreas Eldh")

(maintainers "Andreas Eldh")

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
