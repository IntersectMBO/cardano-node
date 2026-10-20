{-# LANGUAGE OverloadedStrings #-}

module Cardano.Testnet.Test.StatusRules
  ( unit_node_state
  , unit_classify_network
  , unit_exit_codes
  ) where

import           Cardano.Api (BlockNo (..), SlotNo (..), deserialiseFromRawBytesHex)

import           Prelude

import           Data.List.NonEmpty (NonEmpty (..))
import           System.Exit (ExitCode (..))

import           Testnet.Status.Run (calcNodeState, classifyNetwork)
import           Testnet.Status.Types

import           Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | A tip of the given age, in seconds.
tipOfAge :: Double -> TipInfo
tipOfAge age = TipInfo (SlotNo 100) (BlockNo 10) sampleHash (realToFrac age)
  where
    sampleHash =
      either (\e -> error $ "tipOfAge: bad fixture hash: " <> show e) id $
        deserialiseFromRawBytesHex "cc061706c16ad2b35450f1528f5652ff172f9b0acabcdc29284e2363d3bebb2a"

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Node state/"'@
unit_node_state :: H.Property
unit_node_state = H.propertyOnce $ do
  let limit = 60
  calcNodeState limit (Just True) (AnsweredTip (tipOfAge 1)) === NodeOk
  calcNodeState limit (Just True) (AnsweredTip (tipOfAge 60)) === NodeOk
  calcNodeState limit (Just True) (AnsweredTip (tipOfAge 61)) === NodeStalled
  -- A node that answers but has no blocks yet is stalled.
  calcNodeState limit (Just True) AnsweredNoBlocks === NodeStalled
  -- The answer decides: a node that answers is never down, whatever its pid says.
  calcNodeState limit (Just False) (AnsweredTip (tipOfAge 1)) === NodeOk
  -- No answer: the pid tells down from unreachable.
  calcNodeState limit (Just False) NoAnswer === NodeDown
  calcNodeState limit (Just True) NoAnswer === NodeUnreachable
  calcNodeState limit Nothing NoAnswer === NodeUnreachable

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Network status/"'@
unit_classify_network :: H.Property
unit_classify_network = H.propertyOnce $ do
  classifyNetwork (NodeOk :| [NodeOk, NodeOk]) === NetworkRunning
  classifyNetwork (NodeOk :| [NodeDown, NodeUnreachable]) === NetworkDegraded
  classifyNetwork (NodeOk :| [NodeStalled]) === NetworkDegraded
  classifyNetwork (NodeStalled :| [NodeDown, NodeUnreachable]) === NetworkStalled
  classifyNetwork (NodeDown :| [NodeUnreachable]) === NetworkStopped

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Status exit codes/"'@
unit_exit_codes :: H.Property
unit_exit_codes = H.propertyOnce $ do
  -- 0 means the chain is producing blocks, even when some nodes are not ok.
  exitCodeForStatus NetworkRunning === ExitSuccess
  exitCodeForStatus NetworkDegraded === ExitSuccess
  exitCodeForStatus NetworkStalled === ExitFailure 3
  exitCodeForStatus NetworkStopped === ExitFailure 4
  exitCodeForStatus NoTestnet === ExitFailure 5
