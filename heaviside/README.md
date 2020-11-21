# Developing this Project

To load the development environment run `nix-shell --attr env release.nix`.

Now you can build the project:
```
cabal configure
cabal run heaviside
```

## Adding Dependencies

Add the dependencies to the `cabal` file and then run `cabal2nix . > project.nix`.
Then reload the development environment.

# Building this Project

To build the binary run `nix-build release.nix` or to build the docker image run `nix-build docker.nix`

# How this Project was Setup

First run `cabal init` in the empty directory to initialize the haskell project.

Write a basic `release.nix` derivation that describes how to build our project.

`release.nix` referes to `project.nix` which is built by `cabal2nix . > project.nix`.
