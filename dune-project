(lang dune 2.0)

(name iostream)
(generate_opam_files true)

(source
 (github c-cube/ocaml-iostream))

(authors "Simon Cruanes")
(maintainers "Simon Cruanes")
(license MIT)
(documentation https://c-cube.github.io/ocaml-iostream)

(package
 (name iostream)
 (synopsis "Generic, composable IO input and output streams")
 (depends ocaml dune)
 (tags
  (topics io channels streams)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
