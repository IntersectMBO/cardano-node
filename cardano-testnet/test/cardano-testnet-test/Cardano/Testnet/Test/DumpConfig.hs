{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.DumpConfig
  ( hprop_dump_config
  ) where

import           Prelude

import qualified Data.Aeson as A
import           Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe
import qualified Data.Aeson.KeyMap as A
import           Data.Default.Class (def)
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import           System.Exit (ExitCode (..))
import           System.FilePath ((</>))
import qualified System.Process as IO

import           Cardano.Api (writeFileJSON)
import qualified Cardano.Api.Byron as Byron
import           Cardano.Api.Byron (GenesisData (..))
import qualified Cardano.Api.Shelley as Shelley
import           Cardano.Api.Shelley (ShelleyGenesis (..))
import           Cardano.Prelude (canonicalEncodePretty)
import           Cardano.Testnet hiding (shelleyGenesisFile)
import           Testnet.Components.Configuration (startTimeOffsetSeconds)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Start.Types (ConfigFilesBehaviour (..), GenesisOptions (..))

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import           Testnet.Process.Run (execCli',mkExecConfig)
import           Hedgehog.Extras (defaultExecConfig)


-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Dumping config files/"'@
hprop_dump_config :: H.Property
hprop_dump_config = integrationWorkspace "dump-config-files" $ \tempAbsBasePath -> H.runWithDefaultWatchdog_ $ do

  H.workspace "config_files" $ \tmpDir -> do
    let shelleyOptions = def { genesisEpochLength = 200 }
        configFile = tmpDir </> "configuration.yaml"
        byronGenesisFile = tmpDir </> "byron-genesis.json"
        shelleyGenesisFile = tmpDir </> "shelley-genesis.json"

    -- Generate config files in a temporary directory
    let generateTestnetOptions = def
          { cardanoConfigFilesBehaviour = OnlyGenerate
          , cardanoOutputDir = Just tmpDir
          }
    confGenerate <- mkConf tmpDir
    _ <- cardanoTestnetDefault generateTestnetOptions shelleyOptions confGenerate

    currentTime <- H.noteShowIO Time.getCurrentTime
    startTime <- H.noteShow $ Time.addUTCTime startTimeOffsetSeconds currentTime

    -- Update start time in Byron genesis file
    eByron <- Byron.runExceptT $ Byron.readGenesisData byronGenesisFile
    (byronGenesis', _) <- H.leftFail eByron
    let byronGenesis = byronGenesis'{gdStartTime = startTime}
    H.lbsWriteFile byronGenesisFile $ canonicalEncodePretty byronGenesis

    -- Update start time in Shelley genesis file
    eShelley <- H.readJsonFile shelleyGenesisFile
    shelleyGenesis' :: Shelley.ShelleyGenesis <- H.leftFail eShelley
    let shelleyGenesis = shelleyGenesis'{sgSystemStart = startTime}
    H.lbsWriteFile shelleyGenesisFile $ encodePretty shelleyGenesis

    -- Update hashes in the main configuration file
    byronHash <- T.strip . T.pack <$> execCli' defaultExecConfig
      [ "byron"
      , "genesis"
      , "print-genesis-hash"
      , "--genesis-json", byronGenesisFile
      ]

    shelleyHash <- T.strip . T.pack <$> execCli' defaultExecConfig
      [ "hash"
      , "genesis-file"
      , "--genesis", shelleyGenesisFile
      ]

    eConfig <- H.readJsonFile configFile
    -- TODO: There should be a type for this config, with proper JSON instances
    config' :: A.Object <- H.leftFail eConfig
    let config = A.fromList
          [ ("ShelleyGenesisHash", A.String shelleyHash)
          , ("ByronGenesisHash", A.String byronHash)
          ] <> config' -- Monoid operation is left-biased, so old values are overwritten

    eWrite <- H.evalIO $ writeFileJSON configFile config
    H.leftFail eWrite

    -- Run testnet with generated config
    let runTestnetOptions = def
          { cardanoConfigFilesBehaviour = GenerateAndRun -- This *is* the default value, but better make it explicit
          , cardanoNodes = UserProvidedNodeOptions configFile
          }
    confRun <- mkConf tempAbsBasePath
    TestnetRuntime
      { testnetNodes = [singleNode]
      } <- cardanoTestnetDefault runTestnetOptions shelleyOptions confRun
    


    H.assert $ isJust $ poolKeys singleNode
    -- Let the node run for a minute, to let problems time to happen
    H.threadDelay 30_000 -- milliseconds
    -- If nothing happened, kill the node and exit with success

    poolSprocket1 <- H.noteShow $ nodeSprocket singleNode
    execConfig <- mkExecConfig tempAbsBasePath poolSprocket1 42
    s <- execCli' execConfig
       [ "query", "stake-distribution"
     
       ]
    H.note_ s


    exit <- H.evalIO $ do
      let handle = nodeProcessHandle singleNode
      IO.terminateProcess handle
      IO.getProcessExitCode handle
    H.diff exit (==) $ Just $ ExitFailure 143 -- gracefully exit when hit by SIGTERM
