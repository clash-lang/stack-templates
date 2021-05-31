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
    if [[ ${project} != "deca.hsfiles" ]]; then
        stack run clash -- Example.Project --vhdl
    else
        stack run clash -- DECA --vhdl
    fi

    # Clean up
    cd ..
    rm -rf inst
done
