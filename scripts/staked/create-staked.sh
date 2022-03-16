#!/usr/bin/env bash

mkdir -p example/staked

cardano-cli genesis create-staked \
  --genesis-dir example/staked \
  --testnet-magic 42 \
  --gen-genesis-keys 1 \
  --gen-utxo-keys 1 \
  --gen-pools 1 \
  --gen-stake-delegs 1 \
  --start-time $(gdate +%Y-%m-%dT%H:%M:%SZ --date='+30 seconds') \
  --supply 10000000000 \
  --supply-delegated 5000000000

cp scripts/staked/configuration.yaml  example/staked/configuration.yaml
cp scripts/staked/topology.json       example/staked/topology.json
