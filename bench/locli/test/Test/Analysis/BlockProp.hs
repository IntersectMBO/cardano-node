{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Chain reconstruction from the nodes' views of their forged blocks.
module Test.Analysis.BlockProp where

import           Cardano.Prelude

import qualified Data.Map.Strict as Map
import           Data.Text.Short (ShortText)

import           Cardano.Analysis.API
import           Cardano.Analysis.BlockProp
import           Cardano.Util

import           Hedgehog

import           Test.Analysis.MachPerf (run, t0)


-- | A block forged by a host for a slot (1 s slots from 't0'), on top of a parent.
forge :: ShortText -> ShortText -> ShortText -> Word64 -> Word64 -> Int -> ForgerEvents UTCTime
forge host blk prev height slot delta = ForgerEvents
  { bfeHost = Host host, bfeBlock = Hash blk, bfeBlockPrev = Hash prev
  , bfeBlockNo = BlockNo height, bfeSlotNo = SlotNo slot, bfeSlotStart = SlotStart start
  , bfeEpochNo = EpochNo 0, bfeBlockSize = SJust 1000
  , bfeStarted = SJust (at 0.01), bfeBlkCtx = SNothing, bfeLgrState = SNothing, bfeLgrView = SNothing
  , bfeLeading = SJust (at 0.02), bfeTicked = SNothing, bfeMemSnap = SNothing, bfeForged = SJust (at 0.03)
  , bfeAnnounced = SNothing, bfeAnnouncedCum = SNothing, bfeSending = SNothing, bfeAdopted = SJust (at 0.04)
  , bfeChainDelta = delta, bfeErrs = [] }
 where start = addUTCTime (fromIntegral slot) t0
       at    = (`addUTCTime` start)

-- | The chain is b0 .. b5 (b6 is the tip, which the rebuild leaves out as not yet settled).
--   Rivals: x1 for slot 1; x2, x2' for slot 2; y3 at height 3 for slot 3; z5 at height 3
--   for slot 5 (a node that missed b3 and y3): a height rival of b3 and a slot rival of b4.
--   b3 was adopted through a chain switch (chain delta 2).
forges :: [ForgerEvents UTCTime]
forges =
  [ forge "A" "b0" "g"  0 0 1
  , forge "A" "b1" "b0" 1 1 1, forge "B" "x1"  "b0" 1 1 1
  , forge "B" "b2" "b1" 2 2 1, forge "C" "x2"  "b1" 2 2 1, forge "D" "x2'" "b1" 2 2 1
  , forge "A" "b3" "b2" 3 4 2, forge "D" "y3"  "b2" 3 3 1, forge "C" "z5"  "b2" 3 5 1
  , forge "B" "b4" "b3" 4 5 1
  , forge "A" "b5" "b4" 5 6 1
  , forge "B" "b6" "b5" 6 7 1
  ]

machViews :: [(LogObjectSource, MachView)]
machViews =
  [ (LogObjectSourceOther (show host), MachView
      { mvHost = host
      , mvHashBlocks = Map.fromList [ (bfeBlock f, MFE f) | f <- forges, bfeHost f == host ]
      , mvStarted = SNothing, mvBlkCtx = SNothing, mvLgrState = SNothing, mvLgrView = SNothing
      , mvLeading = SNothing, mvTicked = SNothing, mvMemSnap = SNothing })
  | host <- ["A", "B", "C", "D"] ]

-- | Slot battles count the other blocks for a block's slot (also at other heights), height
--   battles those at its height for other slots. The CDFs cover the chain blocks in the
--   analysed slots (slot conditions apply), regardless of block conditions: b3, adopted through
--   a chain switch, fails the unitary chain delta, yet its height battles count.
prop_blockProp_slot_and_height_battles = withTests 1 $ property $ do
  let filters = [CSlot (SlotGEq 2), CBlock BUnitaryChainDelta]
      chain   = rebuildChain run filters [] machViews
  (cMainChain chain <&> \be -> (show (beBlock be) :: [Char], unCount (beSlotBattle be), unCount (beHeightBattle be)))
    === [("b0", 0, 0), ("b1", 1, 0), ("b2", 2, 0), ("b3", 0, 2), ("b4", 1, 0), ("b5", 0, 0)]
  bp <- evalEither . first renderBlockPropError $ blockProp run chain
  (cdfSize (cdfSlotBattle bp),   cdfAverageVal (cdfSlotBattle bp))   === (4, 0.75)
  (cdfSize (cdfHeightBattle bp), cdfAverageVal (cdfHeightBattle bp)) === (4, 0.5)
  -- propagation only covers blocks passing all conditions: b2, b4, b5
  cdfSize (cdfForgerForge bp) === 3

tests :: IO Bool
tests =
  checkSequential $$discover
