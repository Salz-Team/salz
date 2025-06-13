{ description = "Salz";
    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
        flake-utils.url = "github:numtide/flake-utils";
    };

    outputs = { self, nixpkgs, flake-utils }:
        flake-utils.lib.eachDefaultSystem (system:
            let
                pkgs = import nixpkgs {
                    inherit system;
                    config = { allowUnfree = true; };
                };
            in {
                apps = {
                    api = flake-utils.lib.mkApp {
                        drv = pkgs.writeShellApplication {
                            name = "api";
                            runtimeInputs = [ pkgs.go (pkgs.callPackage ./nix/hcp.nix{}) ];
                            text = ''
                                pushd ./api > /dev/null && ./run.sh
                            '';
                        };
                    };
                };

                devShell = pkgs.mkShell {
                    buildInputs = [
                        pkgs.git
                        pkgs.graphviz
                        pkgs.sqlfluff
                        pkgs.jsonschema
                        pkgs.postgresql_16
                        (pkgs.callPackage ./nix/hcp.nix {})
                        pkgs.process-compose
                        pkgs.minio
                        pkgs.minio-client

                        pkgs.go

                        pkgs.nodePackages.npm
                        pkgs.nodePackages.typescript-language-server
                        pkgs.nodePackages.svelte-language-server
                    ];
                    shellHook = ''
                        export GOPATH="$(pwd)/.go"
                        export GOCACHE=""
                        export GO111MODULE='on'
                    '';
                };
            });
}
