{-# LANGUAGE OverloadedStrings #-}

module Cardano.Testnet.Test.StatusRules
  ( unit_node_state
  , unit_classify_network
  , unit_exit_codes
  , unit_best_tip
  , unit_blocks_behind
  ) where

import           Cardano.Api (BlockNo (..), SlotNo (..), deserialiseFromRawBytesHex)

import           Prelude

import           Data.List.NonEmpty (NonEmpty (..))
import           System.Exit (ExitCode (..))

import           Testnet.Status.Run (blocksBehind, calcNodeState, classifyNetwork, newestTip)
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
        deserialiseFromRawBytesHex "3525888dc2c18eb5f5f34cb0edf911aa8b91ba485d195bb7d70dff7a7a32af91"

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
  calcNodeState limit (Just False) (NoAnswer "refused") === NodeDown
  calcNodeState limit (Just True) (NoAnswer "refused") === NodeUnreachable
  calcNodeState limit Nothing (NoAnswer "refused") === NodeUnreachable

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

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Best tip/"'@
unit_best_tip :: H.Property
unit_best_tip = H.propertyOnce $ do
  -- With 0.1 s slots, node A answers at 10.2 s with slot 100, so its tip is
  -- 0.2 s old. Node B answers later, at 12 s, with slot 110, so its tip is
  -- 1 s old. B's tip is the newest, even though its age is bigger.
  let tipA = (tipOfAge 0.2) { tipSlot = SlotNo 100 }
      tipB = (tipOfAge 1) { tipSlot = SlotNo 110 }
  newestTip [tipA, tipB] === Just tipB
  newestTip [tipB, tipA] === Just tipB
  newestTip [] === Nothing

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Blocks behind/"'@
unit_blocks_behind :: H.Property
unit_blocks_behind = H.propertyOnce $ do
  let tipAtBlock n = (tipOfAge 1) { tipBlockNo = BlockNo n }
  -- Counted from the tip with the most blocks, which is not always the newest
  -- one, so the number is never negative.
  blocksBehind [Just (tipAtBlock 8), Just (tipAtBlock 10), Nothing] === [Just 2, Just 0, Nothing]
  blocksBehind [Just ((tipAtBlock 10) { tipSlot = SlotNo 90 }), Just ((tipAtBlock 9) { tipSlot = SlotNo 95 })]
    === [Just 0, Just 1]
  blocksBehind [Nothing, Nothing] === [Nothing, Nothing]
  blocksBehind [] === []
