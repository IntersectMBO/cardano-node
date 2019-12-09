#!/bin/sh

if [ -e configuration/GenesisFiles/GENHASH ]
then
    :
else
    echo "configuration/GenesisFiles/GENHASH does not exist"
	./scripts/genesis.sh
fi


RUNNER=${RUNNER:-cabal v2-run -v0 --}

# . $(dirname $0)/lib-node.sh

genesis_root="../cardano-node/configuration/GenesisFiles"
genesis_file="${genesis_root}/genesis.json"

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
