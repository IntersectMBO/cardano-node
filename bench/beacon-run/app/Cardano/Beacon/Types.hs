
module Cardano.Beacon.Types where



data BeaconCommand =
    BeaconRun

data BeaconOptions = BeaconOptions {
        -- | Baseline version.
        versionA           :: !Version
        -- | Other version to compare.
      , versionB           :: !Version
        -- | Path to the Cardano node's 'mainnet/db' folder. If you built the
        -- node from source and run it from the `cardano-node` directory this
        -- path would point to this directory.
      , nodeHome           :: !FilePath
        -- | path for the config.json file. This is relative to @nodeHome@. When
        -- not present this defaults to
        -- "/configuration/cardano/mainnet-config.json"
      , configPath         :: !FilePath
        -- | path for the db passed to db-analyzer. This is relative to
        -- @nodeHome@. When not present this defaults to "/mainnet/db"
      , dbPath             :: !FilePath
      , analyseFromSlot    :: !Int
      , numBlocksToProcess :: !Int
        -- | Whether to overwrite the CSV files that 'db-analyser' produces.
      , overwriteData      :: !OverwriteData
        -- | Whether to the plot the CSV files that 'db-analyser' produces.
      , doPlotting         :: !ShouldEmitPlots
    }