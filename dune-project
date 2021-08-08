(lang dune 2.7)
(generate_opam_files true)

(source (github rbardou/protype))
(license MIT)
(authors "Romain Bardou")
(maintainers "Romain Bardou")

(package
 (name protype)
 (synopsis "Type descriptions for introspection")
 (description "This library defines a type to describe types, from which one can define serializers and deserializers, generate random or dummy values, etc.")
 (depends (ocaml (>= 4.07))))

(package
 (name protype-tests)
 (synopsis "Test cases for Protype")
 (description "This library defines a set of test cases that can be used to test encoding libraries based on Protype.")
 (depends (ocaml (>= 4.07))))
