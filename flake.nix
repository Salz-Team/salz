{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in {
        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.graphviz

            # API stuff
            pkgs.go
            pkgs.age

          ];
          shellHook = ''
            export GOPATH="$(pwd)/.go"
            export GOCACHE=""
            export GO111MODULE='on'
         '';
        };
      }
    );
}
