#!/usr/bin/env bash

CMD="cabal new-run -v0 -- cardano-cli "

. $(dirname $0)/lib-node.sh

NOW=`date "+%Y-%m-%d 00:00:00"`
NETARGS=(
        --real-pbft
        --genesis-file  "${genesis_file}"
        --genesis-hash  "${genesis_hash}"
        generate-txs
)
TX_GEN_ARGS=(
        --num-of-txs     10000
        --inputs-per-tx  1
        --outputs-per-tx 1
        --tx-fee         1000000
        --tps            1000
        --sig-key        "${genesis_root}/delegate-keys.000.key"
        --sig-key        "${genesis_root}/delegate-keys.001.key"
        --sig-key        "${genesis_root}/delegate-keys.002.key"
)

# --add-tx-size    100

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
