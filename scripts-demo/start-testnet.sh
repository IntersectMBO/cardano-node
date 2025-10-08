#!/usr/bin/env bash
rm -fr ./testnet-data
rm -fr ../cardano-api/rpc.socket
cabal build cardano-node cardano-cli cardano-testnet
(
  tries=0
  while [ $tries -lt 60 ]; do
    if [ -S ./testnet-data/socket/node1/rpc.sock ]; then
      break
    fi
    sleep 1
    tries=$((tries + 1))
  done

  if [ $tries -eq 60 ]; then
    echo "Timeout: Socket not found in 60 seconds." >&2
    exit 1
  fi

  ln -sf `pwd`/testnet-data/socket/node1/rpc.sock ../cardano-api/rpc.socket
) & cabal run cardano-testnet -- cardano --testnet-magic 42 --enable-grpc --output-dir ./testnet-data
