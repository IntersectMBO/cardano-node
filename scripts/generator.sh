#!/usr/bin/env bash

CMD="cabal new-run -v0 -- cardano-cli "

. $(dirname $0)/lib-node.sh

NOW=`date "+%Y-%m-%d 00:00:00"`
NETARGS=(
        --real-pbft
        --genesis-file  "${genesis_file}"
        --genesis-hash  "${genesis_hash}"
        generate-txs
        --topology      "configuration/simple-topology.json"
        --genesis-file  "${genesis_file}"
        --database-path "./db/"
        --socket-dir    "./socket/"

)
TX_GEN_ARGS=(
        --num-of-txs     1000
        --inputs-per-tx  2
        --outputs-per-tx 2
        --tx-fee         1000000
        --tps            10
        --add-tx-size    100
        --sig-key        "${genesis_root}/delegate-keys.000.key"
        --sig-key        "${genesis_root}/delegate-keys.001.key"
        --sig-key        "${genesis_root}/delegate-keys.002.key"
)

function mkdlgkey () {
  printf -- "--signing-key            ${genesis_root}/delegate-keys.%03d.key"    "$1"
}
function mkdlgcert () {
  printf -- "--delegation-certificate ${genesis_root}/delegation-cert.%03d.json" "$1"
}

set -x
${CMD} \
    --log-config configuration/log-configuration.yaml \
    $(mkdlgkey 0) \
    $(mkdlgcert 0) \
    ${NETARGS[*]} \
    ${TX_GEN_ARGS[*]} \
    $@
