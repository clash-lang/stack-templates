#!/bin/bash
set -euo pipefail
set -x
IFS=$'\n\t'

cd "$(git rev-parse --show-toplevel)"

# Clone clash-starters and rollback to very first commit
git clone git@github.com:clash-lang/clash-starters.git
cd clash-starters
    first_commit=$(git rev-list --max-parents=0 --abbrev-commit HEAD)
    git reset "${first_commit}" --hard
cd ..

# Render all projects in rendered/
for hsfile in *.hsfiles; do
    project="${hsfile%.*}"

    stack new "${project}" "${hsfile}" -p "author-name:Example Name" -p "license:BSD-2-Clause"
    mv "${project}" clash-starters
done

# Commit and push files
cd clash-starters
git config --global user.name "Clash DevOps"
git config --global user.email "devopsXXX@qbaylogic.com"
git add -A
git commit -m "Automated push from clash-lang/stack-templates"

if [[ "$1" == "main" ]]; then
    git push -f
fi
