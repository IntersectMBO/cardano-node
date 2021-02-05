#!/usr/bin/env bash
## Script to attest that the node run the Pivo protocol version

set -Eeuo pipefail

ROOT="$(realpath "$(dirname "$0")/../..")"
configuration="${ROOT}/scripts/lite/pivo/data"

data_dir="$(mktemp).d"
echo "Node data directory: ${data_dir}"

db_dir="${data_dir}/db/node"
socket_dir="${data_dir}/socket"

mkdir -p "${db_dir}"
mkdir -p "${socket_dir}"
mkdir -p "${data_dir}/node"
mkdir -p "${data_dir}/genesis"

cp -af "${configuration}/node-config.yaml" "${data_dir}"
cp -af "${configuration}/topology.json" "${data_dir}"
cp -af "${configuration}/vrf.skey" "${data_dir}/node"
cp -af "${configuration}/kes.skey" "${data_dir}/node"
cp -af "${configuration}/opcert" "${data_dir}/node"
cp -af "${configuration}/genesis.json" "${data_dir}/genesis/genesis.json"

# Compute genesis hash
cabal run exe:cardano-cli -- shelley genesis hash --genesis "${data_dir}/genesis/genesis.json" | tail -1 > "${data_dir}"/genesis/GENHASH

# # Launch a node
cabal run exe:cardano-node -- run \
  --database-path "${db_dir}" \
  --socket-path "${socket_dir}/node-socket" \
  --port "3000" \
  --config "${data_dir}/node-config.yaml" \
  --topology "${data_dir}/topology.json" \
  --shelley-vrf-key "${data_dir}/node/vrf.skey" \
  --shelley-kes-key "${data_dir}/node/kes.skey" \
  --shelley-operational-certificate "${data_dir}/node/opcert"

function cleanup()
{
  for child in $(jobs -p); do
    echo kill "$child" && kill "$child"
  done
}

trap cleanup EXIT
