#!/usr/bin/env bash
set -euo pipefail
cd $(git rev-parse --show-toplevel)

nix run .#cabalProjectRegenerate

# Regenerate the list of the project packages:
nix eval .#pkgs.genProjectPackages > nix/project-packages-exes.nix.new
mv nix/project-packages-exes.nix.new nix/project-packages-exes.nix
