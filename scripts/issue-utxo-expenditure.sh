#!/bin/sh

RUNNER=${RUNNER:-cabal new-run -v0 --}

. $(dirname $0)/lib-node.sh

default_from_key="${genesis_root}/delegate-keys.001.key"
default_to_key="${genesis_root}/delegate-keys.002.key"

## rob the bank
default_lovelace="862000000000000"

case $# in
        2 ) tx="$1"
            txid="$2"
            outindex="0"
            from_key="${default_from_key}"
            to_key="${default_to_key}"
            lovelace=${default_lovelace};;
        6 ) tx="$1"
            txid="$2"
            outindex="$3"
            from_key="$4"
            to_key="$5"
            lovelace="$6";;
        * ) cat >&2 <<EOF
Usage:  $(basename $0) TX-FILE IN-TXID IN-INDEX FROM-KEY-FILE TO-KEY-FILE LOVELACE
EOF
            exit 1;; esac

addr=$(scripts/get-default-key-address.sh ${to_key})

args=" --real-pbft
       --genesis-file        ${genesis_file}
       --genesis-hash        ${genesis_hash}
       issue-utxo-expenditure
       --tx                  ${tx}
       --wallet-key          ${from_key}
       --txin             (\"${txid}\",${outindex})
       --txout            (\"${addr}\",${lovelace})
       --topology            configuration/simple-topology.json
       --genesis-file        \"${genesis_file}\"
       --database-path       ./db/
       --socket-dir          ./socket/
"
set -x
${RUNNER} cardano-cli ${args}
