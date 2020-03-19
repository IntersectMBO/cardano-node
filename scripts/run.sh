#!/usr/bin/env bash

# Unoffiical bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail
IFS=$'\n\t'

exec cardano-node run --config /config/config.yaml \
  --database-path /data/db \
  --genesis-file /config/genesis.json \
  --host-addr 127.0.0.1 \
  --port 3001 \
  --socket-path /data/ipc/node.socket \
  --topology /config/topology.json \
  $@