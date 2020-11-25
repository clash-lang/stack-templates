#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

mkdir -p ~/.ssh
echo "${CLASH_STARTERS_PRIVATE_KEY}" > ~/.ssh/id_rsa
echo "${CLASH_STARTERS_PUBLIC_KEY}" > ~/.ssh/id_rsa.pub
chmod 400 ~/.ssh/id_rsa
wc ~/.ssh/*
