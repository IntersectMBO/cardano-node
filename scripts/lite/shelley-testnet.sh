#!/usr/bin/env bash

ROOT="$(realpath "$(dirname "$0")/../..")"

configuration="${ROOT}/scripts/lite/configuration"

data_dir="$(mktemp).d"
mkdir -p "${data_dir}"

# Generate shelley genesis
ARGS=(
  --genesis-dir           "${data_dir}/genesis"
  --gen-genesis-keys      3
  --gen-utxo-keys         3
  --supply                9000
  --mainnet
)

cabal run exe:cardano-cli -- shelley genesis create "${ARGS[@]}"

# Compute genesis hash
cabal run exe:cardano-cli -- shelley genesis hash --genesis "${data_dir}/genesis/genesis.json" | tail -1 > "${data_dir}"/genesis/GENHASH

# Ensure the node is built
cabal run --no-stats cardano-node cardano-node --help >/dev/null || true

for i in 1 2 3; do
  # Use copy default configuration and topolgy to configuration directory for a particular node instance
  cp -af "${configuration}/shelley-$i.yaml" "${data_dir}"
  cp -af "${configuration}/topology-node-$i.json" "${data_dir}"
  db_dir="${data_dir}/db/node-$i"
  socket_dir="${data_dir}/socket"

  mkdir -p "${db_dir}"
  mkdir -p "${socket_dir}"

  esc=$(printf '\033')

  # We need the following for a shelley node to be able to mint blocks:
  # - KES signing key
  # - VRF signing key
  # - Operational certificate

  # Generate VRF keys
  mkdir -p "${data_dir}/genesis/vrf-keys"
  cardano-cli shelley node key-gen-VRF \
      --verification-key-file "${data_dir}/genesis/vrf-keys/vrf-$i.vkey" \
      --signing-key-file      "${data_dir}/genesis/vrf-keys/vrf-$i.skey"

  # Generate a KES keys
  mkdir -p "${data_dir}/genesis/kes-keys"
  cardano-cli shelley node key-gen-KES \
    --verification-key-file "${data_dir}/genesis/kes-keys/kes-$i.vkey" \
    --signing-key-file      "${data_dir}/genesis/kes-keys/kes-$i.skey"

  # Issue an operational certificate:
  mkdir -p "${data_dir}/genesis/delegate-opcerts"
  cardano-cli shelley node issue-op-cert \
      --kes-period 0 \
      --kes-verification-key-file                  "${data_dir}/genesis/kes-keys/kes-$i.vkey"  \
      --cold-signing-key-file                      "${data_dir}/genesis/delegate-keys/delegate$i.skey" \
      --operational-certificate-issue-counter-file "${data_dir}/genesis/delegate-keys/delegate$i.counter" \
      --out-file                                   "${data_dir}/genesis/delegate-opcerts/delegate$i.opcert"

  # Launch a node
  cabal run exe:cardano-node -- run \
    --database-path "${db_dir}" \
    --socket-path "${socket_dir}/node-$i-socket" \
    --port "300$i" \
    --config "${data_dir}/shelley-$i.yaml" \
    --topology "${data_dir}/topology-node-$i.json" \
    --signing-key "${data_dir}/genesis/delegate-keys.00$i.key" \
    --delegation-certificate "${data_dir}/genesis/delegation-cert.00$i.json" \
    --shelley-vrf-key "${data_dir}/genesis/vrf-keys/vrf-$i.skey" \
    --shelley-kes-key "${data_dir}/genesis/kes-keys/kes-$i.skey" \
    --shelley-operational-certificate "${data_dir}/genesis/delegate-opcerts/delegate$i.opcert" \
    | sed "s|^|${esc}[$((31+$i))m[node-$i]${esc}[0m |g" &
done

function cleanup()
{
  for child in $(jobs -p); do
    echo kill "$child" && kill "$child"
  done
}

cat

trap cleanup EXIT
