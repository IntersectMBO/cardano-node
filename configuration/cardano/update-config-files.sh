#!/usr/bin/env bash
set -euo pipefail

OUT=$(dirname "$(realpath "$0")")
ROOT=$(realpath "${OUT}/../..")

# Provide access to iohkNix environment configs
IOHK_NIX_CFGS=$(nix build --print-out-paths --no-link "${ROOT}"#hydraJobs.cardano-deployment)

# Provide access to iohkNix testnet templates
IOHK_NIX_OUT=$(nix eval --raw --impure \
  --expr "let f = builtins.getFlake \"git+file://\${toString $ROOT}\"; in f.inputs.iohkNix.outPath")

copyCfg() {
  echo "$1"
  cp "${IOHK_NIX_CFGS}/$1" "${OUT}/$1"
}

copyTmplCfg() {
  echo "testnet-template-$1"
  cp "${IOHK_NIX_OUT}/cardano-lib/testnet-template/$1" "${OUT}/testnet-template-$1"
}

echo "################################"
echo "# Copying Network Config Files #"
echo "################################"

# Mainnet
copyCfg "mainnet-alonzo-genesis.json"
copyCfg "mainnet-byron-genesis.json"
copyCfg "mainnet-checkpoints.json"
copyCfg "mainnet-config.json"
copyCfg "mainnet-config-bp.json"
copyCfg "mainnet-config-legacy.json"
copyCfg "mainnet-conway-genesis.json"
copyCfg "mainnet-peer-snapshot.json"
copyCfg "mainnet-shelley-genesis.json"
copyCfg "mainnet-topology.json"
copyCfg "mainnet-topology.json"

# Testnet-template
copyTmplCfg "alonzo.json"
copyTmplCfg "byron.json"
copyTmplCfg "config.json"
copyTmplCfg "conway.json"
copyTmplCfg "shelley.json"
copyTmplCfg "topology-empty-p2p.json"

# Prevent write errors on script retries due to nix store no-write default perm
chmod -R +w "${OUT}"
