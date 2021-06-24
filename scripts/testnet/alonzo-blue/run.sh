#!/usr/bin/env bash

mkdir -p testnet/alonzo-blue

cp -r scripts/testnet/alonzo-blue/*.json testnet/alonzo-blue
cp -r scripts/testnet/alonzo-blue/*.yaml testnet/alonzo-blue

cardano-node run \
  --socket-path testnet/alonzo-blue/socket \
  --topology testnet/alonzo-blue/topology.yaml \
  --database-path testnet/alonzo-blue/db \
  --port 50001 \
  --config testnet/alonzo-blue/configuration.yaml
