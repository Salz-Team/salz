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
                    minio = flake-utils.lib.mkApp {
                        drv = pkgs.writeShellApplication {
                            name = "minio";
                            runtimeInputs = [ pkgs.minio ];
                            text = ''
                                minio server --json .state/minio
                            '';
                        };
                    };
                    db = flake-utils.lib.mkApp {
                        drv = pkgs.writeShellApplication {
                            name = "db";
                            runtimeInputs = [ pkgs.postgresql_16 ];
                            text = ''
                                export PGDATABASE=salz

                                export PGDATA=$PWD/.state/postgres/db
                                export PGHOST=$PGDATA

                                trap 'pg_ctl -D $PGDATA stop' EXIT

                                if ! test -d "$PGDATA"
                                then
                                    echo "PGDATA directory $PGDATA does not exist -- running setup"
                                    initdb -U salz -A md5 --pwfile=<(echo superdupersecret) "$PGDATA"
                                    OPT="unix_socket_directories"
                                    sed -i "s|^#$OPT.*$|$OPT = '$PGDATA'|" "$PGDATA/postgresql.conf"
                                    pg_ctl -D "$PGDATA" -l "$PGDATA/postgres.log"  start
                                    PGPASSWORD=superdupersecret createdb -U salz
                                    PGPASSWORD=superdupersecret psql -U salz -f "$PWD/db/init.sql"
                                    pg_ctl -D "$PGDATA" stop
                                fi

                                pg_ctl -D "$PGDATA" -l "$PGDATA/postgres.log"  start
                                tail -f "$PGDATA/postgres.log"
                            '';
                        };
                    };
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
