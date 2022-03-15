#!/bin/bash

set -e

OUT=$(dirname $(realpath $0))
ROOT=$(realpath ${OUT}/../..)
nix build "${ROOT}"#cardano-deployment
SRC="${ROOT}/result"

copyFile() {
  echo $1
  cp ${SRC}/$1 ${OUT}/$1
}

echo "#################"
echo "# Copying files #"
echo "#################"

copyFile "mainnet-alonzo-genesis.json"
copyFile "mainnet-byron-genesis.json"
copyFile "mainnet-config.json"
copyFile "mainnet-shelley-genesis.json"
copyFile "mainnet-topology.json"
