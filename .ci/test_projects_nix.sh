#!/bin/bash
set -euo pipefail
set -x
IFS=$'\n\t'

cd "$(git rev-parse --show-toplevel)"

for project in *.hsfiles; do
    nix-shell --pure --packages stack --run "stack new inst \"${project}\""

    cd inst

    if [[ -f "shell.nix" ]]; then
        # Build and test with nix. Note that nix-build already runs the tests,
        # but we'd also like to be able to run them using just cabal in a shell.
        nix-build
        if [[ ${project} != "deca.hsfiles" ]]; then
            nix-shell --pure --run "cabal run clash -- Example.Project --vhdl"
        else
            nix-shell --pure --run "cabal run clash -- DECA --vhdl"
        fi
        nix-shell --pure --run "cabal run doctests"
        nix-shell --pure --run "cabal run test-library"
    fi

    # Clean up
    cd ..
    rm -rf inst
done
