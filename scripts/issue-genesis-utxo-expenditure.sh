#!/usr/bin/env bash

TX=${1?scripts.issue-genesis-utxo-expenditure.Error: no tx name specified}

. $(dirname $0)/lib-node.sh defaults/liveview
CLI="$(executable_quiet_runner cardano-cli)"
CONFIG="configuration/defaults/liveview/config-0.yaml"
from_addr="2cWKMJemoBain3UWCzSY5wZvcf8uQ2MAaSy8hedrwpqsbYCm4QnBgPn3cEH7KF3X7DKoZ"
from_key="${configuration_root}/genesis/delegate-keys.000.key"
default_to_key="${configuration_root}/genesis/delegate-keys.001.key"

## ADA to be spent
default_lovelace="863000000000000"
case $# in
        1 ) proto_magic="$(jq '.protocolConsts | .protocolMagic' "${genesis_file}")"
            addr="$(${scripts}/get-default-key-address.sh ${default_to_key})"
            lovelace=${default_lovelace};;
        3 ) addr="$2";
            lovelace="$3";;
        * ) cat >&2 <<EOF
Usage:  $(basename $0) TX-FILE TO-ADDR LOVELACE
EOF
            exit 1;; esac

args=" issue-genesis-utxo-expenditure
       --config              "$CONFIG"
       --tx                  ${TX}
       --wallet-key          ${from_key}
       --rich-addr-from    \"${from_addr}\"
       --txout            (\"${addr}\",${lovelace})
     "
set -x
${CLI} ${args}
