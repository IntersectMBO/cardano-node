#!/usr/bin/env bash

CMD="cabal new-run -v0 -- cardano-cli "

genesis="b0109"
genesis_root="configuration/${genesis}"
genesis_file="${genesis_root}/genesis.json"
if test ! -f "${genesis_file}"
then echo "ERROR: genesis ${genesis_file} does not exist!">&1; exit 1; fi
genesis_hash="b010944818186e0f1e4094d41b0612240f61908a5a262080c0be5d63ebd4766b"

NOW=`date "+%Y-%m-%d 00:00:00"`
NETARGS=(
        --real-pbft
        --genesis-file  "${genesis_file}"
        --genesis-hash  "${genesis_hash}"
        generate-txs
        --topology      "configuration/simple-topology.json"
)
TX_GEN_ARGS=(
        --num-of-txs     1000
        --outputs-per-tx 1
        --tx-fee         1000000
        --tps            10
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
    $(mkdlgkey 0) \
    $(mkdlgcert 0) \
    ${NETARGS[*]} \
    ${TX_GEN_ARGS[*]} \
    $@
