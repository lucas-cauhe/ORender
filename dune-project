(lang dune 3.14)
(using ctypes 0.3)

(name computer_gfx)

(generate_opam_files true)

(source
 (github lucas-cauhe/ORender))

(authors "Lucas Cauhé Viñao" "Andrei Dumbrava")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name computer_gfx)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends (ocaml (= 5.2.0)) dune (alcotest :with-test) )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
