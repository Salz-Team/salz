# Developing this Project Locally

To load the development environment run `nix-shell --attr env release.nix`.

Now you can build the project:
```
cabal configure
cabal run heaviside
```
## Adding Dependencies

Add the dependencies to the `cabal` file and then run `cabal2nix . > project.nix`.
Then reload the development environment.

## Connecting your box to the dev server

This will all be done in [this](loadDevConnections.sh) script you can simply run `source loadDevConnections.sh`
But for documentation here are all of the connections from this docker container
to the outside world.

Envoronment Vars:

- RMQUSER :: RabbitMQ Username
- RMQPWD :: RabbitMQ Password
- RMQPORT :: RabbitMQ Port 
- PSQLUSER :: Postgres Username
- PSQLPWD :: Postgres Password
- PSQLPORT :: Postgres Port

Ports:

- 5672 :: rabbitmq
- 15672 :: RabbitMQ Management interface (for development)
- 5432 :: psql


# Building this Project

To build the binary run `nix-build release.nix` or to build the docker image run `nix-build docker.nix`

# How this Project was Setup

First run `cabal init` in the empty directory to initialize the haskell project.

Write a basic `release.nix` derivation that describes how to build our project.

`release.nix` referes to `project.nix` which is built by `cabal2nix . > project.nix`.
