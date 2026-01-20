(lang dune 3.20)

(name swara_scribe)

(generate_opam_files true)

(source
 (github TastyVeggy/swara-scribe))

(package
 (name swara_scribe)
 (synopsis "Sargam notation engine")
 (description
  "Lightweight, CLI-based notation engine designed to render Indian Classical (Sargam) music scores from plain-text files")
 (depends ocaml dune cairo2 ppx_deriving))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
