#!/usr/bin/env bash

set -x

. $(dirname $0)/lib.sh
WALLET="$(executable_runner wallet-client)"

genesis_root="${configuration}/genesis"
genesis_hash=`cat "${genesis_root}/GENHASH"`
genesis_file="${genesis_root}/genesis.json"

${WALLET} \
    --config ${configuration}/log-config-0.yaml \
    --socket-dir ${root}/socket/ \
    --genesis-json ${genesis_file} \
    --genesis-hash ${genesis_hash} \
    $@
