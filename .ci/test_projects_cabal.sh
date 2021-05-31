#!/bin/bash
set -euo pipefail
set -x
IFS=$'\n\t'

cd "$(git rev-parse --show-toplevel)"

for project in *.hsfiles; do
    stack new inst "${project}"

    cd inst

    # Build and test with Cabal
    cabal update
    cabal build
    cabal run test-library --enable-tests
    cabal run doctests --enable-tests
    if [[ ${project} != "deca.hsfiles" ]]; then
        cabal run clash -- Example.Project --vhdl
    else
        cabal run clash -- DECA --vhdl
    fi

    # Clean up
    cd ..
    rm -rf inst
done
