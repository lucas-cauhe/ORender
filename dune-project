(lang dune 3.14)
(using ctypes 0.3)

(name computer_gfx)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name computer_gfx)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune (alcotest :with-test)  (iter (= 1.8)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
