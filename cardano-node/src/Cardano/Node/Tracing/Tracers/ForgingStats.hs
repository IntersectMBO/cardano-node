{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Node.Tracing.Tracers.ForgingStats
    ( ForgingStats (..)
    , ForgingResumed
    , newForgingResumed
    , noteForgingResumed
    , calcForgeStats
  ) where

import           Cardano.Logging
import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Consensus.Node.Tracers
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.Shelley.Node ()

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson (Value (..), (.=))
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)


-- | Counts how often block forging has been (re-)enabled, so that the slots
--   which elapsed while forging was disabled are not booked as missed
--   leadership checks.
--
--   While forging is disabled there is no forging thread, hence no leadership
--   check events at all (see @forkBlockForging@ in consensus' @NodeKernel@), so
--   'fsLastSlot' stays at the last slot before standby and the gap would
--   otherwise be counted on the next completed check.
--
--   This is a counter rather than a flag that the fold clears, because
--   'mkCardanoTracer'' applies its hook twice -- once for the message path and
--   once for the metrics path -- so 'calcForgeStats' builds two independent
--   folds over the same events. A flag would be consumed by whichever fold saw
--   the first event after the resume, and the other would still book the gap.
--   Each fold instead remembers the generation it has already accounted for in
--   its own 'fsResumeGen', so every fold gets exactly one fresh start per
--   resume, no matter how many folds there are or in which order they run.
newtype ForgingResumed = ForgingResumed (IORef Int)

newForgingResumed :: IO ForgingResumed
newForgingResumed = ForgingResumed <$> newIORef 0

-- | Call this whenever block forging is enabled, including the initial
--   transition out of non-producing mode.
noteForgingResumed :: ForgingResumed -> IO ()
noteForgingResumed (ForgingResumed ref) = atomicModifyIORef' ref (\n -> (n + 1, ()))

-- | The current generation. Read only -- never cleared, so that every fold can
--   observe the same resume independently.
currentResumeGen :: ForgingResumed -> IO Int
currentResumeGen (ForgingResumed ref) = readIORef ref

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
  , fsLastSlot           :: !Int -- Internal value, to track last slot.
  , fsResumeGen          :: !Int -- Internal value: the resume generation this
                                 -- fold has already accounted for.
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
      ]

    allNamespaces = [Namespace [] ["ForgingStats"]]


emptyForgingStats :: ForgingStats
emptyForgingStats = ForgingStats 0 0 0 0 0 0

calcForgeStats :: ForgingResumed
  -> Trace IO ForgingStats
  -> IO (Trace IO (TraceForgeEvent blk))
calcForgeStats resumed tr =
  let tr' = contramap unfold tr
  in foldCondTraceM (calculateForgingStats resumed) emptyForgingStats
      (\case
          Consensus.TraceStartLeadershipCheck{} -> True
          _  -> False
          )
      tr'

calculateForgingStats :: MonadIO m
  => ForgingResumed
  -> ForgingStats
  -> LoggingContext
  -> TraceForgeEvent blk
  -> m ForgingStats
calculateForgingStats _resumed stats _context
    TraceNodeCannotForge {} =
      pure $ stats  { fsNodeCannotForgeNum  = fsNodeCannotForgeNum stats + 1 }
calculateForgingStats resumed stats _context
    (TraceNodeIsLeader (SlotNo slot)) = do
      -- A completed leadership check: record the generation, so a resume
      -- accounted for here cannot also suppress a later, genuine gap.
      gen <- liftIO (currentResumeGen resumed)
      pure $ stats  { fsNodeIsLeaderNum = fsNodeIsLeaderNum stats + 1
                    , fsLastSlot = fromIntegral slot
                    , fsResumeGen = gen }
calculateForgingStats _resumed stats _context
    TraceForgedBlock {} =
        pure $ stats  { fsBlocksForgedNum  = fsBlocksForgedNum stats + 1 }
calculateForgingStats resumed stats _context
    (TraceNodeNotLeader (SlotNo slot')) = do
      -- Node is not a leader again: The number of blocks forged by
      -- this node should now be equal to the number of slots when
      -- this node was a leader.
      --
      -- The first completed check after forging was (re-)enabled starts a new
      -- run of slots: while forging was disabled no leadership check ran, so
      -- the elapsed slots were not missed, they were not due.
      gen <- liftIO (currentResumeGen resumed)
      let justResumed = gen /= fsResumeGen stats
          slot = fromIntegral slot'
      if justResumed || fsLastSlot stats == 0 || succ (fsLastSlot stats) == slot
        then pure $ stats { fsLastSlot = slot, fsResumeGen = gen }
        else
          let missed = slot - fsLastSlot stats
          in pure $ stats { fsLastSlot = slot
                          , fsResumeGen = gen
                          , fsSlotsMissedNum = fsSlotsMissedNum stats + missed }
calculateForgingStats _resumed stats _context _message = pure stats
