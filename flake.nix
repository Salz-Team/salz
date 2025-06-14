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
                        export PGUSER=salz
                        export PGPASSWORD=superdupersecret

                        # Web
                        export WEB_BASEURL="http://localhost:5173"

                        # Minio
                        export MINIO_ENDPOINT="localhost:9000"
                        export MINIO_DATA_DIR="$STATEDIR/minio/data"
                        export MINIO_CONFIG_DIR="$STATEDIR/minio/config"
                        export MINIO_ROOT_USER="minioadmin"
                        export MINIO_ROOT_PASSWORD="minioadmin"
                        export MINIO_USE_SSL="false"
                        export MC_CONFIG_DIR="$STATEDIR/minio/mc"

                        mkdir -p "$MC_CONFIG_DIR"
                        cat <<EOF > "$MC_CONFIG_DIR/config.json"
                        {
                            "version": "10",
                            "aliases": {
                                "local": {
                                    "url": "http://$MINIO_ENDPOINT",
                                    "accessKey": "$MINIO_ROOT_USER",
                                    "secretKey": "$MINIO_ROOT_PASSWORD",
                                    "api": "S3v4",
                                    "path": "auto"
                                }
                            }
                        }
                        EOF

                        #API
                        export VAULT_ORG_ID="25ee0d1b-4e14-42f8-bf34-d3191ff704b7"
                        export VAULT_PROJECT_ID="3196ec06-6d59-4fc4-affa-fa3fdd12cd1c"
                        export VAULT_APP_NAME="salz"
                        hcp profile set organization_id "$VAULT_ORG_ID" --quiet
                        hcp profile set project_id "$VAULT_PROJECT_ID" --quiet
                        hcp profile set vault-secrets/app "$VAULT_APP_NAME" --quiet
                        hcp auth print-access-token >/dev/null 2>&1 || { echo >&2 "Not logged into HCP -- login via hcp auth login"; exit 1; }
                        export OAUTH_CLIENT_KEY=$(hcp vault-secrets secrets open github_oauth_client_id --format json | jq -r '.static_version.value')
                        export OAUTH_CLIENT_SECRET=$(hcp vault-secrets secrets open github_oauth_client_secret --format json | jq -r '.static_version.value')
                        export PG_URI="postgres://$PGUSER:$PGPASSWORD@localhost:5432/$PGDATABASE?sslmode=disable"
                        export MINIO_ACCESS_KEY="$MINIO_ROOT_USER"
                        export MINIO_SECRET_KEY="$MINIO_ROOT_PASSWORD"


                        # process Compose
                        export PC_PORT_NUM=8081

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
