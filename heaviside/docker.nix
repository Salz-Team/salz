{ pkgs ? import <nixpkgs> {} }:

let
  heaviside = pkgs.haskellPackages.callPackage ./project.nix {};
in
pkgs.dockerTools.buildImage {
  name = "heaviside";
  contents = [ heaviside ];

  config = {
    Cmd = [ "${heaviside}/bin/heaviside" ];
    ExposedPorts = {
      "8000/tcp" = {};
    };
  };

}
