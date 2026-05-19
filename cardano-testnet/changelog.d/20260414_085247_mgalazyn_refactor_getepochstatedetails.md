
### Changed

- Refactored `getEpochStateDetails` to fail immediately when the `foldEpochState` background thread encounters an error, instead of waiting for the full timeout.
  Previously, if the background thread failed, consumers would wait the entire 15-second timeout before reporting a generic failure.
  Now the error is captured into the shared `IORef` and surfaced with a meaningful message as soon as a consumer reads it.
- Increased the epoch state initialisation timeout from 15 to 25 seconds.
- Simplified `getEpochStateDetails` interface by removing the continuation parameter -- callers now use `fmap` to extract the fields they need.

