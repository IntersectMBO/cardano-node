#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../.." && pwd)"
cd "$ROOT_DIR"

echo "[trace-schemas] Generating namespace list and documentation..."
nix run .#cardano-node -- \
  trace-documentation \
  --config configuration/cardano/mainnet-config.yaml \
  --output-namespace-list bench/trace-schemas/newNamespaces.txt \
  --output-file bench/trace-schemas/trace-documentation.md

echo "[trace-schemas] Generating schemas..."
nix run .#schema-gen

echo "[trace-schemas] Applying human overrides..."
nix run .#apply-schema-overrides -- --verbose

echo "[trace-schemas] Validating generated schemas..."
nix run .#validate-trace-schemas

echo "[trace-schemas] Done."
