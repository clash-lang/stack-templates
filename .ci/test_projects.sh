#!/bin/bash
set -euo pipefail
set -x
IFS=$'\n\t'

cd "$(git rev-parse --show-toplevel)"

for project in *.hsfiles; do
    stack new inst "${project}" -p "author-name:My Name" -p "license:BSD-2-Clause"

    cd inst

    # Build and test with Stack
    stack build
    stack test
    #stack run clash -- Example.Project --vhdl

    # Build and test with Cabal
    cabal update
    cabal build
    cabal test
    #cabal run clash --write-ghc-environment-files=always -- Example.Project --vhdl

    # Clean up
    cd ..
    rm -rf inst
done
