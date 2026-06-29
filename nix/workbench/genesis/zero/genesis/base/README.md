Files for mainnet

- https://book.world.dev.cardano.org/environments/mainnet/shelley-genesis.json
- https://book.world.dev.cardano.org/environments/mainnet/alonzo-genesis.json
- https://book.world.dev.cardano.org/environments/mainnet/conway-genesis.json

Future hard fork (not yet activated on mainnet)

- dijkstra-genesis.json: Dijkstra has not activated on mainnet yet, so the
  mainnet environment above does not (yet) serve a `dijkstra-genesis.json`. As
  an interim reference, this file is the dijkstra spec from IOG's `iohk-nix`
  testnet-template, which the cardano-node 10.7.0 release notes ("Known issues"
  section) point to as the expected pre-mainnet shape.

  - Release notes: https://github.com/IntersectMBO/cardano-node/releases/tag/10.7.0
  - Source file:   https://github.com/input-output-hk/iohk-nix/blob/master/cardano-lib/testnet-template/dijkstra.json

  When Dijkstra activates on mainnet and a `dijkstra-genesis.json` is published
  under
  `https://book.world.dev.cardano.org/environments/mainnet/`, replace this file
  with the official one.

