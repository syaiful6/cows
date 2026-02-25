{
  mkShell,
  treefmt,
  ocamlPackages,
  systemfd,
  watchexec,
}:
mkShell {
  inputsFrom = with ocamlPackages; [
    cows
  ];
  buildInputs =
    (with ocamlPackages; [
      eio_main
      ocaml-lsp
      ocamlformat
      utop
    ])
    ++ [
      treefmt
      systemfd
      watchexec
    ];
}
