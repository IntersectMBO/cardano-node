# Changelog for cardano-node

## Unreleased changes

## cardano-node 1.0.0

- Complete rewrite compared to previous cardano-sl series.

- New modular design. The cardano-node is the top level for the node and
  aggregates the other components from other packages: consensus, ledger and
  networking, with configuration, CLI, logging and monitoring.

- The node no longer incorporates wallet or explorer functionality. The wallet
  backend and explorer backend are separate components that run in separate
  external processes that communicate with the node via local IPC.
