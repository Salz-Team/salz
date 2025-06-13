{ description = "Salz";
    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    };

    outputs = { self, nixpkgs }:
            let
                forAllSystems = function:
                    nixpkgs.lib.genAttrs [
                        "x86_64-linux"
                        "aarch64-linux"
                    ] (
                        system:
                        function system (import nixpkgs {
                            inherit system;
                            config.allowUnfree = true;
                            overlays = [];
                        }));
                forAllSystemsPkgsOnly = function: (forAllSystems (_: pkgs: function pkgs));
            in {
                packages = forAllSystemsPkgsOnly (pkgs: {
                    api = pkgs.buildGoModule (finalAttrs: {
                        pname = "api";
                        version = "0.0.1";
                        src = ./api;
                        vendorHash = "sha256-tgWC6WTyP+juEF1t8yv1ZT4j2O5+YZzwI8JQ9CUHads=";
                    });
                });

                devShells = forAllSystems (system: pkgs: {
                    default = pkgs.mkShell {
                        inputsFrom = [
                            self.packages.${system}.api
                        ];

                        packages = [
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
            };
}
