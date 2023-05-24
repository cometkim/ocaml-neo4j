(lang dune 3.6)

(name neo4j)

(generate_opam_files true)

(source
 (github cometkim/ocaml-neo4j))

(authors "Hyeseong Kim <hey@hyeseong.kim>")

(maintainers "Hyeseong Kim <hey@hyeseong.kim>")

(license MIT)

(package
 (name neo4j)
 (synopsis "Unified interface to the Neoj4 database")
 (depends ocaml dune))

(package
 (name neo4j-eio)
 (synopsis "Eio support for Neo4j")
 (depends ocaml dune))

(package
 (name neo4j-lwt-unix)
 (synopsis "Lwt support for Neoj4")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
