{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.DumpConfig
  ( hprop_dump_config
  ) where

import           Prelude

import qualified Data.Aeson as A
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Default.Class (def)
import qualified Data.Time.Clock as Time
import           GHC.Float (double2Int)
import           System.Exit (ExitCode (..))
import           System.FilePath ((</>))
import qualified System.Process as IO

import           Cardano.Api (BlockNo (..), ChainTip (..))
import qualified Cardano.Api.Byron as Byron
import           Cardano.Api.Byron (GenesisData (..))
import qualified Cardano.Api.Shelley as Shelley
import           Cardano.Api.Shelley (ShelleyGenesis (..))
import           Cardano.CLI.Type.Output (QueryTipLocalStateOutput (..))
import           Cardano.Prelude (canonicalEncodePretty)
import           Cardano.Testnet hiding (shelleyGenesisFile)
import           Testnet.Components.Configuration (startTimeOffsetSeconds)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Start.Types (GenesisHashesPolicy (..), GenesisOptions (..), UserProvidedData (..), UserProvidedEnv(..))

import           Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import           Testnet.Process.Run (execCli',mkExecConfig)

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Dumping config files/"'@
hprop_dump_config :: H.Property
hprop_dump_config = integrationWorkspace "dump-config-files" $ \tmpDir -> H.runWithDefaultWatchdog_ $ do

  let testnetOptions = def { cardanoOutputDir = UserProvidedEnv tmpDir }
      genesisOptions = def { genesisEpochLength = 200 }
      byronGenesisFile = tmpDir </> "byron-genesis.json"
      shelleyGenesisFile = tmpDir </> "shelley-genesis.json"

  -- Generate the sandbox
  conf <- mkConf tmpDir
  createTestnetEnv
    testnetOptions genesisOptions
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
  eByron <- Byron.runExceptT $ Byron.readGenesisData byronGenesisFile
  (byronGenesis', _byronHash) <- H.leftFail eByron
  let byronGenesis = byronGenesis'{gdStartTime = startTime}
  H.lbsWriteFile byronGenesisFile $ canonicalEncodePretty byronGenesis

  -- Update start time in Shelley genesis file
  eShelley <- H.readJsonFile shelleyGenesisFile
  shelleyGenesis' :: Shelley.ShelleyGenesis <- H.leftFail eShelley
  let shelleyGenesis = shelleyGenesis'{sgSystemStart = startTime}
  H.lbsWriteFile shelleyGenesisFile $ encodePretty shelleyGenesis

  -- Run testnet with generated config
  TestnetRuntime
    { testnetNodes
    , testnetMagic
    } <- cardanoTestnet testnetOptions genesisOptions conf

  -- There should only be one SPO node among three
  TestnetNode
    { nodeProcessHandle
    , nodeSprocket
    } <- case testnetNodes of
      [spoNode, _relayNode1, _relayNode2] -> do
        (isTestnetNodeSpo <$> testnetNodes) === [True, False, False]
        pure spoNode
      _ -> H.failure

  -- Check that blocks have been produced on the chain after 2 minutes at most
  H.byDurationM 5 120 "Expected blocks to be minted" $ do
    execConfig <- mkExecConfig tmpDir nodeSprocket testnetMagic
    tipStr <- H.noteM $ execCli' execConfig
      [ "query", "tip"
      , "--output-json"
      ]
    QueryTipLocalStateOutput
      { localStateChainTip = tip
      } <- H.nothingFail $ A.decode $ B.pack tipStr

    case tip of
      ChainTipAtGenesis -> H.failure
      ChainTip _ _ (BlockNo blockNo) ->
        -- Blocks have been produced if the tip of the chain is > 0
        H.assertWith blockNo (> 0)

  -- If everything went fine, terminate the node and exit with success
  exit <- H.evalIO $ do
    IO.terminateProcess nodeProcessHandle
    IO.waitForProcess nodeProcessHandle
  -- Nodes are expected to exit successfully when terminated
  exit === ExitSuccess
