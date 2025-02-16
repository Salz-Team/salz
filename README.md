# Salz

See documentation on the architecture of the game
[here](documentation/architecture.md).

## Getting Started

We use [devenv.sh](https://devenv.sh/) to manage our dependencies and
development environment.

You will need to [install nix and devenv](https://devenv.sh/getting-started/).

You can run `make run` to start the services locally for development
(postgresql, minio, caddy). You can the run `make pipeline-seed` to seed the
database with some players and `make pipepline-run` to run the game pipeline.
`make clean` will bring the services down and
clear the state of postgresql and minio.

`.devenv/processes.log` contains the logs of the services managed by devenv.

devenv will already have `psql` and `mc` configured so you can view the
database with `psql salz` and object storage with `mc ls local`.  The minio
web-console is at http://localhost:9111.

## Secrets management

We're using Hashicorp Vault for secrets management. Unfortunately,
the `hcp` command line utility is not available on the
nix package repository. Installation instructions here:
https://developer.hashicorp.com/hcp/docs/cli/install

Once you have the `hcp` cli installed, authenticate with Hashicorp Cloud:

```
hcp auth login
```

Initialize a profile with `hcp profile init --vault-secrets`. It should
set things up for the organization an dproject. I don't think we need to
configure service related configs.

Now you can use secrets via `hcp vault-secrets secrets`.

## Testing

So far, the only testing we do is for our
[json schema examples](documentation/contracts.md) which can be run via
`make test`.
