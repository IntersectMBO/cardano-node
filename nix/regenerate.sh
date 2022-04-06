#!/usr/bin/env bash
set -euo pipefail
cd $(git rev-parse --show-toplevel)

# Regenerate sha256 comments in cabal.project:
#nix run .#cabalProjectRegenerate

# Regenerate the list of the project packages:
nix eval .#pkgs.genProjectComponents > nix/materialized/project-components.nix.new
mv nix/materialized/project-components.nix.new nix/materialized/project-components.nix

# Regenerate meterialization (system specific)
system=$(nix eval --raw .#stdenv.hostPlatform.config)
rm -rf nix/materialized/$system.new
nix run .#generateMaterializedIohkNixUtils nix/materialized/$system.new/iohk-nix-utils/
nix run .#generateMaterialized ./nix/materialized/$system.new/plan-nix/
rm -rf nix/materialized/$system
mv nix/materialized/$system.new nix/materialized/$system
