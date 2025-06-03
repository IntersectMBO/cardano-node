{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.RunTestnet
  ( hprop_run_testnet
  ) where

import           Prelude

import           Cardano.Api (BlockNo (..), ChainTip (..))
import           Cardano.CLI.Type.Output (QueryTipLocalStateOutput (..))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Default.Class (def)
import           System.Exit (ExitCode (..))
import qualified System.Process as IO

import           Cardano.Testnet
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Start.Types (GenesisOptions (..))

import           Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import           Testnet.Process.Run (execCli',mkExecConfig)

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Testnet produces blocks/"'@
hprop_run_testnet :: H.Property
hprop_run_testnet = integrationWorkspace "run-testnet" $ \tmpDir -> H.runWithDefaultWatchdog_ $ do

  let shelleyOptions = def { genesisEpochLength = 200 }
      testnetOptions = def

  conf <- mkConf tmpDir
  TestnetRuntime
    { testnetNodes
    , testnetMagic
    } <- cardanoTestnetDefault testnetOptions shelleyOptions conf

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
