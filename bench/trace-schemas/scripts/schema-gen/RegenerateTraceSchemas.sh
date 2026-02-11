#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../.." && pwd)"
cd "$ROOT_DIR"

echo "[trace-schemas] Generating namespace list and documentation..."
GHC_ENVIRONMENT=- nix develop -c cabal run cardano-node -- \
  trace-documentation \
  --config configuration/cardano/mainnet-config.yaml \
  --output-namespace-list bench/trace-schemas/newNamespaces.txt \
  --output-file bench/trace-schemas/trace-documentation.md

echo "[trace-schemas] Generating schemas..."
nix develop -c bash -c \
  'runghc bench/trace-schemas/scripts/schema-gen/GhciSchemaGen.hs'

echo "[trace-schemas] Applying human overrides..."
nix develop -c bash -c \
  "runghc bench/trace-schemas/scripts/schema-gen/ApplySchemaOverrides.hs --verbose"

echo "[trace-schemas] Validating generated schemas..."
nix develop -c bash -c \
  "runghc bench/trace-schemas/scripts/schema-gen/ValidateTraceSchemas.hs"

echo "[trace-schemas] Done."
