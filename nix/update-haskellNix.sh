#!/usr/bin/env bash
cd "$(dirname "$0")/.."
nix-shell -I nixpkgs=./nix -p nixWrapped \
  --run "nix flake update --update-input haskellNix"
