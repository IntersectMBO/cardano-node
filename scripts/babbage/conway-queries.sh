#!/usr/bin/env bash

exec 2> >(while IFS= read -r line; do echo -e "\e[34m${line}\e[0m" >&2; done)

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -x
set -euo pipefail

UNAME=$(uname -s) SED=
case $UNAME in
  Darwin )      SED="gsed";;
  Linux )       SED="sed";;
esac

sprocket() {
  if [ "$UNAME" == "Windows_NT" ]; then
    # Named pipes names on Windows must have the structure: "\\.\pipe\PipeName"
    # See https://docs.microsoft.com/en-us/windows/win32/ipc/pipe-names
    echo -n '\\.\pipe\'
    echo "$1" | sed 's|/|\\|g'
  else
    echo "$1"
  fi
}

CARDANO_CLI="${CARDANO_CLI:-cardano-cli}"
NETWORK_MAGIC=42
ROOT=example
DREP_DIR=example/dreps
UTXO_DIR=example/utxo-keys
POOL_DIR=example/pools
TRANSACTIONS_DIR=example/transactions

mkdir -p "$TRANSACTIONS_DIR"

### TEST OTHER QUERIES

$CARDANO_CLI conway query leadership-schedule \
  --testnet-magic $NETWORK_MAGIC \
  --genesis "${ROOT}/genesis/shelley/genesis.json" \
  --cold-verification-key-file ${POOL_DIR}/cold1.vkey \
  --vrf-signing-key-file "${ROOT}/node-spo1/vrf.skey" \
  --current

$CARDANO_CLI conway query protocol-parameters --testnet-magic ${NETWORK_MAGIC}

$CARDANO_CLI conway query stake-pools --testnet-magic ${NETWORK_MAGIC}

$CARDANO_CLI conway query stake-distribution --testnet-magic ${NETWORK_MAGIC}

$CARDANO_CLI cardano-cli conway governance query drep-stake-distribution --testnet-magic ${NETWORK_MAGIC} --drep-verification-key-file "${DREP_DIR}/drep1.vkey"

$CARDANO_CLI conway query stake-address-info \
  --testnet-magic $NETWORK_MAGIC \
  --address "$(cardano-cli conway stake-address build --stake-verification-key-file "${UTXO_DIR}/stake1.vkey" --testnet-magic $NETWORK_MAGIC)"

$CARDANO_CLI conway query stake-address-info \
  --testnet-magic $NETWORK_MAGIC \
  --address "$(cardano-cli conway stake-address build --stake-verification-key-file "${UTXO_DIR}/stake1.vkey" --testnet-magic $NETWORK_MAGIC)"

$CARDANO_CLI conway query kes-period-info --testnet-magic $NETWORK_MAGIC --op-cert-file "${ROOT}/node-spo1/opcert.cert"
