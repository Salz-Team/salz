{
    description = "Salz";
    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    };

    outputs = { self, nixpkgs, flake-utils }:
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
                web = pkgs.buildNpmPackage (finalAttrs: {
                    pname = "web";
                    version = "0.0.1";
                    src = ./web;
                    npmDepsHash = "sha256-+uK8EW08Cq2hfaQdT/e8cg4SXD7Qixlq9bIs/C5+rWI=";
                });
            });
            devShells = forAllSystems (system: pkgs: {
                default = pkgs.mkShell {
                    # Pull in package dependencies
                    inputsFrom = [
                        self.packages.${system}.api
                        self.packages.${system}.web
                    ];

                    # Development-only packages
                    packages = [
                        # Infrastructure
                        pkgs.git
                        (pkgs.callPackage ./nix/hcp.nix {})
                        pkgs.process-compose

                        # Minio
                        pkgs.minio
                        pkgs.minio-client

                        # DB
                        pkgs.postgresql_16
                        pkgs.sqlfluff
                        pkgs.jsonschema

                        # Node
                        pkgs.nodePackages.typescript-language-server
                        pkgs.nodePackages.svelte-language-server
                    ];
                    shellHook = ''
                        export STATEDIR="$PWD/.state"
                        mkdir -p "$STATEDIR"
                        export ENV="local"

                        # Go
                        export GOPATH="$STATEDIR/.go"
                        export GOCACHE=""
                        export GO111MODULE='on'

                        # DB
                        export PGDATABASE=salz
                        export PGDATA="$STATEDIR/db"
                        export PGHOST=$PGDATA
                        export PG_URI="postgres://salz:superdupersecret@localhost:5432/salz?sslmode=disable"

                        # Web
                        export WEB_BASEURL="http://localhost:5173"

                        # Minio
                        export MINIO_ENDPOINT="localhost:9000"
                        export MINIO_DATA_DIR="$STATEDIR/minio/data"
                        export MINIO_CONFIG_DIR="$STATEDIR/minio/config"
                        export MC_CONFIG_DIR="$STATEDIR/minio/mc"
                        export MINIO_ROOT_USER="minioadmin"
                        export MINIO_ROOT_PASSWORD="minioadmin"
                        export MINIO_USE_SSL="false"

                        trap \
                          "
                          cd $PWD
                          rm -rf $NIX_SHELL_DIR
                          process-compose down
                          " \
                          EXIT

                        process-compose -D -f $PWD/process-compose.yaml
                    '';
                };
            });
        };
}
