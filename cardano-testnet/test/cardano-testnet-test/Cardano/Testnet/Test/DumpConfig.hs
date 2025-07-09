{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.DumpConfig
  ( hprop_dump_config
  ) where

import           Cardano.Api (ShelleyGenesis, runExceptT, sgSystemStart)
import           Cardano.Api.Byron (GenesisData (..))
import qualified Cardano.Api.Byron as Byron

import           Cardano.Prelude (canonicalEncodePretty)
import           Cardano.Testnet hiding (shelleyGenesisFile)
import           Cardano.Testnet.Test.Utils (nodesProduceBlocks)

import           Prelude

import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Default.Class (def)
import qualified Data.Time.Clock as Time
import           GHC.Float (double2Int)
import           System.FilePath ((</>))

import           Testnet.Components.Configuration (startTimeOffsetSeconds)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types (GenesisHashesPolicy (..), GenesisOptions (..),
                   UserProvidedData (..), UserProvidedEnv (..))

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Supports dumping/loading config files/"'@
hprop_dump_config :: H.Property
hprop_dump_config = integrationRetryWorkspace 2 "dump-config-files" $ \tmpDir -> H.runWithDefaultWatchdog_ $ do

  let testnetOptions = def { cardanoOutputDir = UserProvidedEnv tmpDir }
      genesisOptions = def { genesisEpochLength = 200 }
      byronGenesisFile = tmpDir </> "byron-genesis.json"
      shelleyGenesisFile = tmpDir </> "shelley-genesis.json"

  -- Generate the sandbox
  conf <- mkConf tmpDir
  createTestnetEnv
    testnetOptions genesisOptions def
    NoUserProvidedData NoUserProvidedData NoUserProvidedData
    -- Do not add hashes to the main config file, so that genesis files
    -- can be modified without having to recompute hashes every time.
    conf{genesisHashesPolicy = WithoutHashes}

  -- Wait for 20% more than `startTimeOffsetSeconds`, to ensure that
  -- the time bounds in the sandbox' config files are no longer valid
  H.threadDelay $ double2Int $ realToFrac startTimeOffsetSeconds * 1_000_000 * 1.2

  currentTime <- H.noteShowIO Time.getCurrentTime
  startTime <- H.noteShow $ Time.addUTCTime startTimeOffsetSeconds currentTime

  -- Update start time in Byron genesis file
  eByron <- runExceptT $ Byron.readGenesisData byronGenesisFile
  (byronGenesis', _byronHash) <- H.leftFail eByron
  let byronGenesis = byronGenesis'{gdStartTime = startTime}
  H.lbsWriteFile byronGenesisFile $ canonicalEncodePretty byronGenesis

  -- Update start time in Shelley genesis file
  eShelley <- H.readJsonFile shelleyGenesisFile
  shelleyGenesis' :: ShelleyGenesis <- H.leftFail eShelley
  let shelleyGenesis = shelleyGenesis'{sgSystemStart = startTime}
  H.lbsWriteFile shelleyGenesisFile $ encodePretty shelleyGenesis

  -- Run testnet with generated config
  runtime <- cardanoTestnet testnetOptions genesisOptions conf

  nodesProduceBlocks tmpDir runtime
