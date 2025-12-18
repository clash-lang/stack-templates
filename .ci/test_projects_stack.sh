#!/bin/bash
set -euo pipefail
set -x
IFS=$'\n\t'

cd "$(git rev-parse --show-toplevel)"

for project in *.hsfiles; do
    stack new inst "${project}"

    cd inst

    # Build and test with Stack
    stack build
    stack test
    case "${project}" in
        deca.hsfiles)
            stack run clash -- DECA --vhdl
            ;;
        orangecrab.hsfile)
            stack run clash -- Blink --vhdl
            ;;
        *)
            stack run clash -- Example.Project --vhdl
            ;;
    esac

    # Clean up
    cd ..
    rm -rf inst
done
