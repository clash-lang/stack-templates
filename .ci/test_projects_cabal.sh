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
    cabal run test-library
    cabal run doctests
    case "${project}" in
        deca.hsfiles)
            cabal run clash -- DECA --vhdl
            ;;
        orangecrab.hsfile)
            cabal run clash -- Blink --vhdl
            ;;
        *)
            cabal run clash -- Example.Project --vhdl
            ;;
    esac

    # Clean up
    cd ..
    rm -rf inst
done
