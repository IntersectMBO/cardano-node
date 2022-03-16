#!/usr/bin/env bash

pushd example/staked

cardano-node run \
  --config                          configuration.yaml \
  --topology                        topology.json \
  --database-path                   db \
  --socket-path                     node.sock \
  --shelley-kes-key                 delegate-keys/delegate1.kes.skey \
  --shelley-vrf-key                 delegate-keys/delegate1.vrf.skey \
  --shelley-operational-certificate pools/opcert1.cert \
  --port                            3003
