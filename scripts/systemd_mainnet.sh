#!/usr/bin/env bash

exec /home/jordan/Repos/Work/cardano-haskell/cardano-node/cardano-node/cardano-node run --config /home/jordan/Repos/Work/cardano-haskell/cardano-node/configuration/defaults/mainnet/configuration.yaml --database-path db --socket-path socket/mainnet-socket --topology /home/jordan/Repos/Work/cardano-haskell/cardano-node/configuration/defaults/mainnet/topology.json --use-activated-sockets
