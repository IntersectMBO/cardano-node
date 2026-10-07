## Changelog

### Unreleased Dijkstra integration (node 11.1.1.1 proposal)

* Use complete API defaults for fallback Dijkstra genesis, including the V4 cost model.
* Render the Receiving script purpose in node traces.
* Align normal-project benchmark dependency bounds with API 11.9, CLI 11.3,
  network/diffusion 1.3 and Plutus 1.71 (locli 2.4.0.1, tx-generator 2.18.0.1
  and plutus-scripts-bench 1.0.5.1 proposals).
* Refresh the package-index dates and verified CHaP Nix input to include the
  published DMQ 0.7.2.0 consensus-5.1 metadata revision.
* Align Dijkstra testnet configuration with genesis protocol major 12, the local
  protected-address activation proposal. Enable `ExperimentalHardForksEnabled`
  only for requested Dijkstra testnets; earlier-era configurations remain unchanged.
  Upstream agreement remains separate.

Changelogs for components can be found as follows:

- [cardano-testnet](https://github.com/IntersectMBO/cardano-node/blob/master/cardano-testnet/CHANGELOG.md)
- [cardano-submit-api](https://github.com/IntersectMBO/cardano-node/blob/master/cardano-submit-api/CHANGELOG.md)
- [trace-forward](https://github.com/IntersectMBO/cardano-node/blob/master/trace-forward/CHANGELOG.md)
- [cardano-tracer](https://github.com/IntersectMBO/cardano-node/blob/master/cardano-tracer/CHANGELOG.md)
- [cardano-node-capi](https://github.com/IntersectMBO/cardano-node/blob/master/cardano-node-capi/CHANGELOG.md)
- [cardano-node-chairman](https://github.com/IntersectMBO/cardano-node/blob/master/cardano-node-chairman/CHANGELOG.md)
- [bench/tx-generator](https://github.com/IntersectMBO/cardano-node/blob/master/bench/tx-generator/CHANGELOG.md)
