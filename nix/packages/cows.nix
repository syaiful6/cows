{
  buildDunePackage,
  lib,
  digestif,
  eio,
  cohttp,
  cohttp-eio,
  logs,
  base64,
  alcotest,
  doCheck ? true,
}:

buildDunePackage {
  pname = "cows";
  version = "0.1.0";

  src =
    let
      fs = lib.fileset;
    in
    fs.toSource {
      root = ../..;
      fileset = fs.unions [
        ../../src
        ../../dune-project
        ../../cows.opam
      ];
    };

  # add your dependencies here
  propagatedBuildInputs = [
    digestif
    logs
    eio
    cohttp
    cohttp-eio
    base64
  ];

  inherit doCheck;

  checkInputs = [ alcotest ];
}
