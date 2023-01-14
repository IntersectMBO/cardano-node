#!/usr/bin/env bash

# This script connects a node to mainnet

ROOT="$(cd "$(dirname "$0")/../.."; pwd -P)"
configuration="${ROOT}/configuration/cardano"

data_dir=tmp/mainnetsingle
mkdir -p "${data_dir}"
db_dir="${data_dir}/db/node"
mkdir -p "${db_dir}"
socket_dir="${data_dir}/socket"
mkdir -p "${socket_dir}"

rm -f nodefifo
mkfifo nodefifo
(
  exec 3<nodefifo  # open fifo for reading

# Launch a node
  exec cardano-node -- run \
    --config "${configuration}/mainnet-config.json" \
    --topology "${configuration}/mainnet-topology.json" \
    --database-path "${db_dir}" \
    --socket-path "${socket_dir}/node-1-socket" \
    --host-addr "127.0.0.1" \
    --port "3001" \
    --shutdown-ipc 3
) &
bpid=$!
echo "$bpid"
# > relay.log 2>&1
exec 4>nodefifo  # open fifo for writing
sleep 10  # let node start and run for a while

exec 4>&-  # close the pipe, node should shutdown

rm -f nodefifo


function cleanup()
{
  for child in $(jobs -p); do
    echo kill "$child" && kill "$child"
  done
}

trap cleanup EXIT
