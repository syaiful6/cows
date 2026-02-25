final: prev:
let
  ocamlOverlay = final': prev': {
    http = final'.callPackage ../packages/http.nix { };
    cohttp = final'.callPackage ../packages/cohttp.nix {
      inherit (final.cows) doCheck;
    };
    cohttp-eio = final'.callPackage ../packages/cohttp-eio.nix {
      inherit (final.cows) doCheck;
    };
    cows = final'.callPackage ../packages/cows.nix {
      inherit (final.cows) doCheck;
    };
  };
in
{
  ocaml-ng.ocamlPackages_4_14 = prev.ocaml-ng.ocamlPackages_4_14.overrideScope ocamlOverlay;
  ocaml-ng.ocamlPackages_5_2 = prev.ocaml-ng.ocamlPackages_5_2.overrideScope ocamlOverlay;
  ocaml-ng.ocamlPackages_5_3 = prev.ocaml-ng.ocamlPackages_5_3.overrideScope ocamlOverlay;
  ocaml-ng.ocamlPackages_5_4 = prev.ocaml-ng.ocamlPackages_5_4.overrideScope ocamlOverlay;
  ocaml-ng.ocamlPackages = prev.ocaml-ng.ocamlPackages.overrideScope ocamlOverlay;

  cows = final.lib.makeScope final.newScope (self: {
    doCheck = true;
  });
}
