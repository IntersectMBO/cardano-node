#!/usr/bin/env bash

RUNNER=${RUNNER:-cabal v2-run -v0 --}

. $(dirname $0)/lib-node.sh

proto_magic="$(jq '.protocolConsts | .protocolMagic' "${genesis_file}")"

key="$1"
test -r "${key}" || {
        cat <<EOF
Usage:  $(basename $0) SIGNING-KEY-FILE"

Print the default, non-HD address of a signing key.
EOF
}
${RUNNER} cardano-cli \
          --real-pbft \
          signing-key-address \
          --testnet-magic ${proto_magic} \
          --secret ${key} \
        | head -n1 | xargs echo -n
