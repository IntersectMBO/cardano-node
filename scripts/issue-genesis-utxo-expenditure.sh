#!/bin/sh

RUNNER=${RUNNER:-cabal new-run -v0 --}

genesis_hash="1c1cf538d599170f4ed8a4cf8fcb3ef2e530e30a6de15e74b4088c002797972e"
genesis_file="${genesis_root}/genesis.json"

from_addr="2cWKMJemoBain3UWCzSY5wZvcf8uQ2MAaSy8hedrwpqsbYCm4QnBgPn3cEH7KF3X7DKoZ"
from_key="${genesis_root}/delegate-keys.000.key"
default_to_key="${genesis_root}/delegate-keys.001.key"

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
       --topology            configuration/simple-topology-0.json
       --genesis-file       \"${genesis_file}\"
       --database-path       ./db/
       --socket-dir          ./socket/
"
set -x
${RUNNER} cardano-cli ${args}
