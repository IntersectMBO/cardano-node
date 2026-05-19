### Maintenance

- Unified retry/wait functions in `Testnet.Components.Query` by factoring out a common `retryUntilRightM` core.
  `retryUntilJustM` and `retryUntilM` are now thin wrappers over this shared primitive, eliminating duplicated timeout/polling logic.
- Removed `watchEpochStateUpdate` and migrated all call sites to `retryUntilJustM`/`retryUntilM`.
- Simplified `waitForBlocks` (dropped `MonadCatch` constraint, eliminated `EpochInterval maxBound` hack).
  Now mirrors `waitForEpochs`: relies solely on the shared retry loop's timeout instead of an outer block-count predicate, avoiding the drift between two independent snapshots of the starting block number.
- Simplified `checkDRepState` by replacing direct `foldEpochState` usage with `EpochStateView` polling.
- Simplified `assertNewEpochState` by replacing `watchEpochStateUpdate` with `retryUntilRightM`.
- Removed unused `nodeConfigPath` and `socketPath` fields from `EpochStateView`.
- Added `maybeExtractGovernanceActionExpiry` in `Testnet.EpochStateProcessing`, which reads a proposal's `gasExpiresAfter` epoch from the gov state.
- Rewrote the `Gov Action Timeout` integration test to derive its wait target from the proposal's actual expiry epoch, removing the race window caused by not knowing which epoch the proposal was recorded in.
  The check now waits one full epoch past the removal boundary so the RATIFY-produced state is @k@-deep stable and cannot be invalidated by a chain rollback.
