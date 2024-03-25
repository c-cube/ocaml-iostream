(lang dune 2.0)

(name iostream)
(generate_opam_files true)
(version 0.2.1)

(source
 (github c-cube/ocaml-iostream))

(authors "Simon Cruanes")
(maintainers "Simon Cruanes")
(license MIT)
(documentation https://c-cube.github.io/ocaml-iostream)

(package
 (name iostream)
 (synopsis "Generic, composable IO input and output streams")
 (depends
   (ocaml (>= 4.08))
   (dune (>= 2.0))
   (ounit2 :with-test))
 (depopts
   base-unix)
 (tags
  (topics io channels streams)))

(package
 (name iostream-camlzip)
 (synopsis "Stream (de)compression using deflate")
 (depends
   (ocaml (>= 4.08))
   (dune (>= 2.0))
   (iostream (= :version))
   camlzip
   (ounit2 :with-test))
 (tags
  (topics io channels streams zip deflate)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
