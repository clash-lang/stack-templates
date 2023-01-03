#!/usr/bin/env bash
nix-shell --pure --packages stack --run "stack new inst simple-nix"
cd inst
nix-build | cachix push clash-lang

