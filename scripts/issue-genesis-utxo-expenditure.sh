#!/bin/sh

RUNNER=${RUNNER:-cabal new-run -v0 --}

genesis_hash="c0c757817d86660accdc45b9d18c1274d51d6427b92995944d014e0ff056cb3e"
genesis_file="/home/shevchenko/Code/cardano-node/configuration/c0c75/genesis.json"

from_addr="2cWKMJemoBain3UWCzSY5wZvcf8uQ2MAaSy8hedrwpqsbYCm4QnBgPn3cEH7KF3X7DKoZ"
from_key="/home/shevchenko/Code/cardano-node/configuration/c0c75/delegate-keys.000.key"
default_to_key="/home/shevchenko/Code/cardano-node/configuration/c0c75/delegate-keys.001.key"

## rob the bank
default_lovelace="863000000000000"

case $# in
        1 ) tx="$1"
            proto_magic="$(jq '.protocolConsts | .protocolMagic' "${genesis_file}")"
            addr="$(scripts/get-default-key-address.sh ${default_to_key})"
            lovelace=${default_lovelace};;
        3 ) tx="$1";
            addr="$2";
            lovelace="$3";;
        * ) cat >&2 <<EOF
Usage:  $(basename $0) TX-FILE TO-ADDR LOVELACE
EOF
            exit 1;; esac

args=" --real-pbft
       --genesis-file        ${genesis_file}
       --genesis-hash        ${genesis_hash}
       issue-genesis-utxo-expenditure
       --tx                  ${tx}
       --wallet-key          ${from_key}
       --rich-addr-from    \"${from_addr}\"
       --txout            (\"${addr}\",${lovelace})
       --topology            configuration/simple-topology.json
       --genesis-file       \"${genesis_file}\"
       --database-path       ./db/
       --socket-dir          ./socket/
"
set -x
${RUNNER} cardano-cli ${args}
