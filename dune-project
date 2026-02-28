(lang dune 3.19)

(name cows)
(documentation "https://syaiful6.github.io/cows/")

(source
  (github syaiful6/cows))

(license MIT)

(authors
  "Syaiful Bahri")

(maintainers
  "Syaiful Bahri")

(generate_opam_files true)

(package
  (name cows)
  (synopsis "An OCaml library for creating WebSocket-capable servers for CoHttp EIO")
  (description "\
  An OCaml library for creating WebSocket-capable servers for CoHttp EIO
  ")
  (depends
    (ocaml (>= 5.2))
    base64
    dune
    (digestif (>= 1.3.0))
    (cohttp-eio (>= 6.2.1))
    (eio_main :with-test)
    (alcotest :with-test)
    (odoc :with-doc)))
