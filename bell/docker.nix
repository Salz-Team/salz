{ pkgs ? import <nixpkgs> {} }:

let
  bell = pkgs.haskellPackages.callPackage ./project.nix {};
in
pkgs.dockerTools.buildImage {
  name = "bell";
  contents = [ bell ];

  config = {
    Cmd = [ "${bell}/bin/bell" ];
    ExposedPorts = {
      "8000/tcp" = {};
    };
  };

}
