#!/bin/bash
set -euo pipefail
set -x
IFS=$'\n\t'

ROOT=$(git rev-parse --show-toplevel)

cd "${ROOT}"

# Clone clash-starters and rollback to very first commit
git clone https://github.com/clash-lang/clash-starters.git
cd clash-starters
    first_commit=$(git rev-list --max-parents=0 --abbrev-commit HEAD)
    git reset "${first_commit}" --hard
cd ..

# Render all projects in rendered/
for hsfile in *.hsfiles; do
    project="${hsfile%.*}"

    stack new "${project}" "${hsfile}"
    mv "${project}" clash-starters
    cd clash-starters
        zip -r "${project}.zip" "${project}"
    cd ..
done

# Commit and push files
cd clash-starters
git config --global user.name "Clash DevOps"
git config --global user.email "devopsXXX@qbaylogic.com"
git add -A
git commit -m "Automated push from clash-lang/stack-templates"

if [[ "$1" == "master" ]]; then
    "${ROOT}"/.ci/install_ssh_keys.sh
    git remote add origin-ssh git@github.com:clash-lang/clash-starters.git
    export GIT_SSH_COMMAND="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
    git push -f origin-ssh "$(git branch --show-current)"
fi
