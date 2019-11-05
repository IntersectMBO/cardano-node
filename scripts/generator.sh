#!/usr/bin/env bash

CMD="cabal new-run -v0 -- cardano-cli "

genesis="c0c75"
genesis_root="configuration/${genesis}"
genesis_file="${genesis_root}/genesis.json"
if test ! -f "${genesis_file}"
then echo "ERROR: genesis ${genesis_file} does not exist!">&1; exit 1; fi
genesis_hash="c0c757817d86660accdc45b9d18c1274d51d6427b92995944d014e0ff056cb3e"

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
