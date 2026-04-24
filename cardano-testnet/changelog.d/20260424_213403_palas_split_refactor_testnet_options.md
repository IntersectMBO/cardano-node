
### Removed

- Removed `CardanoTestnetOptions` type and `CreateEnvOptions` wrapper (replaced by purpose-specific types).
- Removed dead fields `cardanoNodeLoggingFormat` and `cardanoOutputDir`.

### Changed

- Split `CardanoTestnetOptions` into `TestnetCreationOptions` and `TestnetRuntimeOptions` so each function receives only the fields it uses.
- `CardanoTestnetCliOptions` is now a sum type (`StartFromScratch | StartFromEnv`), making `--node-env` and `--num-pool-nodes` structurally mutually exclusive in the CLI parser.
- Simplified `CardanoTestnetCreateEnvOptions` and `createTestnetEnv` signatures (fewer arguments, genesis options and on-chain params folded into `TestnetCreationOptions`).

### Added

- `readNodeOptionsFromEnv`: scans an existing testnet environment directory to classify nodes as SPO or relay.

