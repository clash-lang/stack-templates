#!/bin/bash
set -euo pipefail
set -x
IFS=$'\n\t'

TOP="$(git rev-parse --show-toplevel)"
cd "${TOP}"

for project in *.hsfiles; do
    stack new inst "${project}"

    cd inst
    if [[ -f hie.yaml ]]; then
      cabal update
      cabal v2-build all --enable-tests --only-dependencies

      # If you ever run into:
      # 
      #   https://github.com/haskell/haskell-language-server/issues/2692
      #
      # Consider using:
      #
      #   https://github.com/martijnbastiaan/doctest-parallel/blob/6e553d79bb07b74a21e43512c9e62285ee9b72a3/.github/scripts/parse_hls_log.py
      #
      haskell-language-server-wrapper
    fi
    cd ..
    rm -rf inst
done
