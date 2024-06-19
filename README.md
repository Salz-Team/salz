# Salz

See documentation on the architecture of the game
[here](documentation/architecture.md).

## Getting Started

We use [devenv.sh](https://devenv.sh/) to manage our dependencies and
development environment.

You will need to [install nix and devenv](https://devenv.sh/getting-started/).

You can run `make run` to start the services locally for development
(postgresql, minio, caddy). `make clean` will bring the services down and
clear the state of postgresql and minio.

`.devenv/processes.log` contains the logs of the services managed by devenv.

devenv will already have `psql` and `mc` configured so you can view the
database with `psql salz` and object storage with `mc ls local`.  The minio
web-console is at http://localhost:9111.
