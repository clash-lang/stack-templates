#!/bin/bash
set -euo pipefail
set -x
IFS=$'\n\t'

cd "$(git rev-parse --show-toplevel)"


for project in projects-nix/*; do
    # Copy the directory and resolve symlinks
    # The reason we copy the directory rather than invoking `nix init ...` is because nix init does
    # not resolve symlinks. However these symlinks *are* resolved as soon as it gets copied over to
    # `clash-starters`. So to 'mimick' the behaviour of `nix init` we just copy over the directory.
    cp -rL "$project" ci-project
    # If files aren't referenced by git, Nix gets very confused
    # This is not needed outside of a git repository, but since the files get copied over in a
    # git repository, we need to do this
    git add -A
    cd ci-project

    # Test
    nix build . --accept-flake-config
    nix develop . --accept-flake-config -c cabal update
    # The `nix run` is supposed to run inside of `nix develop`
    nix develop . --accept-flake-config -c nix run . --accept-flake-config
    nix develop . --accept-flake-config -c cabal run doctests
    nix develop . --accept-flake-config -c cabal run test-library

    # Clean up
    rm -rf ./ci-project
done
