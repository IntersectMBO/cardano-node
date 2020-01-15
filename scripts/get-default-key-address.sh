#!/usr/bin/env bash

. $(dirname $0)/lib-node.sh
CLI="$(executable_quiet_runner cardano-cli)"

proto_magic="$(jq '.protocolConsts | .protocolMagic' "${genesis_file}")"

key="$1"
test -r "${key}" || {
        cat <<EOF
Usage:  $(basename $0) SIGNING-KEY-FILE"

Print the default, non-HD address of a signing key.
EOF
}
${CLI} signing-key-address \
          --real-pbft \
          --testnet-magic ${proto_magic} \
          --secret ${key} \
        | head -n1 | xargs echo -n
