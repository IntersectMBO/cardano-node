#!/bin/sh

RUNNER=${RUNNER:-cabal new-run -v0 --}

genesis="33873"
genesis_root="configuration/${genesis}"
genesis_file="${genesis_root}/genesis.json"
if test ! -f "${genesis_file}"
then echo "ERROR: genesis ${genesis_file} does not exist!">&1; exit 1; fi
genesis_hash="$(${RUNNER} cardano-cli --log-config configuration/log-configuration.yaml --real-pbft print-genesis-hash --genesis-json ${genesis_file})"
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

args=" --genesis-file        ${genesis_file}
       --genesis-hash        ${genesis_hash}
       --tx                  ${tx}
       --wallet-key          ${from_key}
       --txin             (\"${txid}\",${outindex})
       --txout            (\"${addr}\",${lovelace})
"
set -x
${RUNNER} cardano-cli --real-pbft --log-config configuration/log-configuration.yaml issue-utxo-expenditure ${args}
