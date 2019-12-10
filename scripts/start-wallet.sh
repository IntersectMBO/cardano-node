#!/usr/bin/env bash

set -x

genesis_hash=`cat configuration/GenesisFiles/GENHASH`
genesis_root="configuration/GenesisFiles"
genesis_file="${genesis_root}/genesis.json"

cabal v2-run wallet-client -- \
    --log-config configuration/log-config-0.yaml \
    --topology configuration/simple-topology.json \
    --database-path ./db/ \
    --genesis-file ${genesis_file} \
    --socket-dir ./socket/ \
    --genesis-hash ${genesis_hash} \
    --port 1234 \
    --config configuration/log-config-0.yaml \
    $@
