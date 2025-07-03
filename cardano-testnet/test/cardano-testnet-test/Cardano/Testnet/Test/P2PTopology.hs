{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.P2PTopology
  ( hprop_p2p_topology
  ) where

import           Cardano.Api (BlockNo (..), ChainTip (..))
import           Cardano.CLI.Type.Output (QueryTipLocalStateOutput (..))
import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import           Cardano.Testnet hiding (shelleyGenesisFile)

import           Prelude

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Default.Class (def)
import           System.Exit (ExitCode (..))
import           System.FilePath ((</>))
import qualified System.Process as IO

import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types (GenesisOptions (..), NodeId,
                   UserProvidedData (..), UserProvidedEnv (..), TopologyType (..))

import           Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H


-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Can be started with P2P topology file/"'@
hprop_p2p_topology :: H.Property
hprop_p2p_topology = integrationRetryWorkspace 2 "p2p-topology" $ \tmpDir -> H.runWithDefaultWatchdog_ $ do

  let testnetOptions = def { cardanoOutputDir = UserProvidedEnv tmpDir }
      genesisOptions = def { genesisEpochLength = 200 }
      someTopologyFile = tmpDir </> "node-data" </> "node1" </> "topology.json"

  -- Generate the sandbox
  conf <- mkConf tmpDir
  createTestnetEnv
    testnetOptions genesisOptions P2PTopology
    NoUserProvidedData NoUserProvidedData NoUserProvidedData
    conf

  -- Check that the topology is indeed P2P
  eTopology <- H.readJsonFile someTopologyFile
  (_topology :: P2P.NetworkTopology NodeId) <- H.leftFail eTopology

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
