#!/bin/bash
set -euo pipefail
set -x
IFS=$'\n\t'

ROOT=$(git rev-parse --show-toplevel)
"${ROOT}"/.ci/install_ssh_keys.sh

export GIT_SSH_COMMAND="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"

cd "${ROOT}"

# Set up repo for clash-starters
git init -b main clash-starters

# Everything in clash-starters-files/ is put into the clash-starters
# repository verbatim.
cp -a clash-starters-files/* clash-starters/

# Nix projects are copied with symlinks dereferenced
cp -R --dereference --preserve projects-nix/* clash-starters/

# We also offer archives of Nix projects
for project_dir in projects-nix/*; do
    project="${project_dir#*/}"

    cd clash-starters
        zip -r "${project}.zip" "${project}"
        tar -czf "${project}.tar.gz" "${project}"
    cd ..
done

# Render all Stack projects for clash-starters
for hsfile in *.hsfiles; do
    project="${hsfile%.*}"

    stack new "${project}" "${hsfile}"
    mv "${project}" clash-starters
    cd clash-starters
        zip -r "${project}.zip" "${project}"
        tar -czf "${project}.tar.gz" "${project}"
    cd ..
done

# Commit and push files
cd clash-starters
git remote add origin git@github.com:clash-lang/clash-starters.git
git config --global user.name "Clash DevOps"
git config --global user.email "devopsXXX@qbaylogic.com"
git add -A
git commit -m "Automated push from clash-lang/stack-templates"

# Push
git push --force origin main:test-deployment
