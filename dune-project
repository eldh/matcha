(lang dune 3.0)

(name rere)

(generate_opam_files true)

(source
 (github eldh/rere))

(license MIT)

(authors "Andreas Eldh")

(maintainers "Andreas Eldh")

(package
 (name rere)
 (synopsis "A React-like terminal UI library for ReasonML")
 (description
  "Build interactive command-line applications with a familiar component-based architecture, hooks, and JSX syntax. Features include useState, useEffect, Context API, and JSX support via PPX.")
 (depends
  (ocaml
   (>= 4.14.0))
  (dune
   (>= 3.0))
  (reason
   (>= 3.8.0))
  (ppxlib
   (>= 0.28.0))))
