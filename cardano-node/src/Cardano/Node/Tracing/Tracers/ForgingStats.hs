{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Node.Tracing.Tracers.ForgingStats
    ( ForgingStats (..)
    , calcForgeStats
  ) where

import           Cardano.Logging
import           Cardano.Node.Tracing.Tracers.StartLeadershipCheck (ForgeTracerType)
import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Consensus.Node.Tracers
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.Shelley.Node ()

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson (Value (..), (.=))


--------------------------------------------------------------------------------
-- ForgingStats Tracer
--------------------------------------------------------------------------------

-- | This structure stores counters of blockchain-related events,
--   per individual thread in fsStats.
data ForgingStats
  = ForgingStats {
    fsNodeCannotForgeNum :: !Int
  , fsNodeIsLeaderNum    :: !Int
  , fsBlocksForgedNum    :: !Int
  , fsLastSlot           :: !Int
  , fsSlotsMissedNum     :: !Int
  }

instance LogFormatting ForgingStats where
  forHuman ForgingStats {..} =
    "Node cannot forge "  <> showT fsNodeCannotForgeNum
    <> " node is leader " <> showT fsNodeIsLeaderNum
    <> " blocks forged "  <> showT fsBlocksForgedNum
    <> " slots missed "   <> showT fsSlotsMissedNum
  forMachine _dtal ForgingStats {..} =
    mconcat [ "kind" .= String "ForgingStats"
             , "nodeCannotForge" .= String (showT fsNodeCannotForgeNum)
             , "nodeIsLeader"    .= String (showT fsNodeIsLeaderNum)
             , "blocksForged"    .= String (showT fsBlocksForgedNum)
             , "slotsMissed"     .= String (showT fsSlotsMissedNum)
             ]
  asMetrics ForgingStats {..} =
    [ IntM "nodeCannotForge" (fromIntegral fsNodeCannotForgeNum)
    , IntM "nodeIsLeader"    (fromIntegral fsNodeIsLeaderNum)
    , IntM "blocksForged"    (fromIntegral fsBlocksForgedNum)
    , IntM "slotsMissed"     (fromIntegral fsSlotsMissedNum)
    ]

instance MetaTrace ForgingStats where
    namespaceFor ForgingStats {} = Namespace [] ["ForgingStats"]

    severityFor _ _ = Just Info

    documentFor _ = Just
      "nodeCannotForgeNum shows how many times this node could not forge.\
      \\nnodeIsLeaderNum shows how many times this node was leader.\
      \\nblocksForgedNum shows how many blocks did forge in this node.\
      \\nslotsMissed shows how many slots were missed in this node."

    metricsDocFor _ =
      [("nodeCannotForge",
        "How many times was this node unable to forge [a block]?")
      ,("nodeIsLeader",
        "How many times was this node slot leader?")
      ,("blocksForged",
        "How many blocks did this node forge?")
      ,("slotsMissed",
        "How many slots did this node miss?")
      ,("lastSlot",
        "")
      ]

    allNamespaces = [Namespace [] ["ForgingStats"]]


emptyForgingStats :: ForgingStats
emptyForgingStats = ForgingStats 0 0 0 0 0

calcForgeStats :: Trace IO ForgingStats
  -> IO (Trace IO (ForgeTracerType blk))
calcForgeStats tr =
  let tr' = contramap unfold tr
  in foldCondTraceM calculateForgingStats emptyForgingStats
      (\case
          Left Consensus.TraceStartLeadershipCheck{} -> True
          Left _ -> False
          Right _  -> True
          )
      tr'

calculateForgingStats :: MonadIO m
  => ForgingStats
  -> LoggingContext
  -> ForgeTracerType blk
  -> m ForgingStats
calculateForgingStats stats _context
    (Left TraceNodeCannotForge {}) =
      pure $ stats  { fsNodeCannotForgeNum  = fsNodeCannotForgeNum stats + 1 }
calculateForgingStats stats _context
    (Left TraceNodeIsLeader {}) =
        pure $ stats  { fsNodeIsLeaderNum  = fsNodeIsLeaderNum stats + 1 }
calculateForgingStats stats _context
    (Left TraceForgedBlock {}) =
        pure $ stats  { fsBlocksForgedNum  = fsBlocksForgedNum stats + 1 }
calculateForgingStats stats _context
    (Left (TraceNodeNotLeader (SlotNo slot'))) =
      let slot = fromIntegral slot'
      in if fsLastSlot stats == 0 || succ (fsLastSlot stats) == slot
            then pure $ stats { fsLastSlot = slot }
            else
              let missed = (slot - fsLastSlot stats)
              in pure $ stats { fsLastSlot = slot
                              , fsSlotsMissedNum = fsSlotsMissedNum stats + missed}

calculateForgingStats stats _context _message = pure stats
