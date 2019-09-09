#!/bin/sh

RUNNER=${RUNNER:-cabal new-run exe:cardano-cli --}

genesis="33873"
genesis_root="configuration/${genesis}"
genesis_file="${genesis_root}/genesis.json"
if test ! -f "${genesis_file}"
then echo "ERROR: genesis ${genesis_file} does not exist!">&1; exit 1; fi
genesis_hash="$(${RUNNER} --real-pbft print-genesis-hash --genesis-json ${genesis_file})"
from_addr="2cWKMJemoBahGYHvphuM3cmwhgWZmRzPSRX5xdx11A1aJ168wLgRpD7naamfWk4dfQ28c"
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

args=" --genesis-file        ${genesis_file}
       --genesis-hash        ${genesis_hash}
       --tx                  ${tx}
       --wallet-key          ${from_key}
       --rich-addr-from    \"${from_addr}\"
       --txout            (\"${addr}\",${lovelace})
"
set -x
${RUNNER} cardano-cli --real-pbft issue-genesis-utxo-expenditure ${args}
