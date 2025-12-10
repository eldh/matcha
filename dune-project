(lang dune 3.0)

(name rere)

(generate_opam_files true)

(package
 (name rere)
 (synopsis "A ReasonML terminal UI component framework")
 (depends
  (ocaml
   (>= 4.14.0))
  (dune
   (>= 3.0))
  (reason
   (>= 3.8.0))
  (ppxlib
   (>= 0.28.0))))
