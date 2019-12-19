#!/usr/bin/env bash

set -x

genesis_root="configuration/genesis"
genesis_hash=`cat "${genesis_root}/GENHASH"`
genesis_file="${genesis_root}/genesis.json"

cabal v2-run wallet-client -- \
    --config configuration/log-config-0.yaml \
    --socket-dir ./socket/ \
    --genesis-json ${genesis_file} \
    --genesis-hash ${genesis_hash} \
    $@
