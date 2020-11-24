#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

cd "$(git rev-parse --show-toplevel)"
./render.hs

if [[ $(git diff) != "" ]]; then
  git diff > /dev/stderr
  echo "Did you forget to call ./render.hs?" > /dev/stderr
  exit 1
fi
