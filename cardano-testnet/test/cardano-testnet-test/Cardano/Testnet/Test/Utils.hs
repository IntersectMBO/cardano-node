{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Utils
  ( nodesProduceBlocks
  ) where

import           Cardano.Api (BlockNo (..), ChainTip (..), MonadIO)
import           Cardano.CLI.Type.Output (QueryTipLocalStateOutput (..))

import           Prelude

import           Control.Monad.Catch (MonadCatch)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import           GHC.Stack.Types (HasCallStack)
import           System.Exit (ExitCode (..))
import qualified System.Process as IO

import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Types (TestnetNode(..), TestnetRuntime (..), isTestnetNodeSpo)

import           Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Checks that nodes are running as expected and producing blocks
nodesProduceBlocks :: ()
  => HasCallStack
  => H.MonadAssertion m
  => H.MonadTest m
  => MonadCatch m
  => MonadIO m
  => FilePath
  -> TestnetRuntime
  -> m ()
nodesProduceBlocks envDir TestnetRuntime{testnetNodes, testnetMagic} = do

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
    execConfig <- mkExecConfig envDir nodeSprocket testnetMagic
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
