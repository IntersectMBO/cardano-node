{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.UpdateTimeStamps
  ( hprop_update_time_stamps
  ) where

import           Cardano.Testnet
import           Cardano.Testnet.Test.Utils (nodesProduceBlocks)

import           Prelude

import           Data.Default.Class (def)
import           GHC.Float (double2Int)

import           Testnet.Components.Configuration (startTimeOffsetSeconds)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types (UpdateTimestamps (..),
                   GenesisHashesPolicy (..), GenesisOptions (..),
                   UserProvidedEnv (..))

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Can have its start time modified/"'@
hprop_update_time_stamps :: H.Property
hprop_update_time_stamps = integrationRetryWorkspace 2 "update-time-stamps" $ \tmpDir -> H.runWithDefaultWatchdog_ $ do

  let testnetOptions = def { cardanoOutputDir = UserProvidedEnv tmpDir }
      genesisOptions = def { genesisEpochLength = 200 }

  -- Generate the sandbox
  conf <- mkConf tmpDir
  createTestnetEnv
    testnetOptions genesisOptions def
    -- Do not add hashes to the main config file, so that genesis files
    -- can be modified without having to recompute hashes every time.
    conf{genesisHashesPolicy = WithoutHashes}

  -- Wait for 20% more than `startTimeOffsetSeconds`, to ensure that
  -- the time bounds in the sandbox' config files are no longer valid
  H.threadDelay $ double2Int $ realToFrac startTimeOffsetSeconds * 1_000_000 * 1.2

  -- Call `createTestnetEnv` again to update the time stamps
  createTestnetEnv
    testnetOptions genesisOptions
    def
    conf{updateTimestamps = UpdateTimestamps}

  -- Run testnet with generated config
  runtime <- cardanoTestnet testnetOptions genesisOptions conf

  nodesProduceBlocks tmpDir runtime
