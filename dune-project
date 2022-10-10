(lang dune 3.4)

(name stlc)

(generate_opam_files true)

(authors "Cody Rivera")

(using menhir 2.1)

(package
 (name stlc)
 (synopsis "Simple linear session-typed language")
 (depends ocaml dune))
