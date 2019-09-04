#!/usr/bin/env bash

set -e

genesis="33873"
genesis_root="configuration/${genesis}"
genesis_file="${genesis_root}/genesis.json"
if test ! -f "${genesis_file}"
then echo "ERROR: genesis ${genesis_file} does not exist!">&1; exit 1; fi

cabal new-build "exe:cardano-cli"
genesis_hash="$(cabal new-exec -- cardano-cli --real-pbft print-genesis-hash --genesis-json ${genesis_file})"

CMD=`find dist-newstyle/ -type f -name "cardano-node"`
test -n "${CMD}" || {
        cabal new-build exe:cardano-node || {
                echo "ERROR: couldn't find or cabal new-build exe:cardano-node" >&2
                exit 1
        }
        CMD=`find dist-newstyle/ -type f -name "cardano-node"`
}


exec cabal new-exec chairman -- --real-pbft \
                                -n 0 -n 1 -n 2 \
                                -k 10 -s 250 \
                                -t 1000 \
                                --genesis-file "${genesis_file}" \
                                --genesis-hash "${genesis_hash}" \
                                --pbft-signature-threshold 0.7 \
                                --require-network-magic \
                                --database-path "db"
