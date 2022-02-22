#!/usr/bin/env bash

cabal build -j3 cardano-node:exe:cardano-node
ln -s "$(pwd)"/dist-newstyle/build/x86_64-linux/ghc-8.10.5/cardano-node-1.33.0/x/cardano-node/build/cardano-node/cardano-node cardano-node.exe
./cardano-node.exe run --topology ./configuration/cardano/mainnet-topology.json --database-path ./state --port 3001 --config ./configuration/cardano/mainnet-config.yaml  --socket-path \\.\pipe\cardano-node
