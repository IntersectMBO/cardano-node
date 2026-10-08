{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Node.Tracing.Tracers.ForgingStats
    ( ForgingStats (..)
    , ForgingStateVar
    , newForgingStateVar
    , noteForgingState
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


-- | The node's own record of whether block forging is on, and of how often it
--   has gone from off to on, so that the slots which elapsed while it was off
--   are not booked as missed leadership checks. The node has to keep this
--   itself: consensus takes the forging credentials through 'setBlockForging'
--   and offers nothing to read the current state back.
--
--   While forging is disabled there is no forging thread, hence no leadership
--   check events at all (see @forkBlockForging@ in consensus' @NodeKernel@), so
--   'fsLastSlot' stays at the last slot before standby and the gap would
--   otherwise be counted on the next completed check.
--
--   Only the off-to-on transition counts. Every @SIGHUP@ re-reads the
--   credentials, including one sent for a topology or RPC change alone, and a
--   reload that leaves forging on must not reset the count -- a genuine gap
--   that had accumulated before the signal would be discarded with it.
--
--   This is a counter rather than a flag that the fold clears, because
--   'mkCardanoTracer'' applies its hook twice -- once for the message path and
--   once for the metrics path -- so 'calcForgeStats' builds two independent
--   folds over the same events. A flag would be consumed by whichever fold saw
--   the first event after the resume, and the other would still book the gap.
--   Each fold instead remembers the generation it has already accounted for in
--   its own 'fsResumeGen', so every fold gets exactly one fresh start per
--   resume, no matter how many folds there are or in which order they run.
newtype ForgingStateVar = ForgingStateVar (IORef ForgingState)

data ForgingState = ForgingState
  { fgGen     :: !Int   -- ^ how often forging has gone from off to on
  , fgEnabled :: !Bool  -- ^ whether it is on now
  }

-- | Takes the state the node starts in: a node started with
--   @--start-as-non-producing-node@, or without usable credentials, starts off.
newForgingStateVar :: Bool -> IO ForgingStateVar
newForgingStateVar enabled = ForgingStateVar <$> newIORef (ForgingState 0 enabled)

-- | Call this wherever block forging is switched on or off, with the state it
--   is in afterwards, and call it /before/ 'setBlockForging': consensus'
--   @blockForgingController@ re-forks the forging threads at once and
--   @knownSlotWatcher@ has @wInitial = Nothing@, so a freshly forked thread
--   runs the check for the current slot immediately. Noting the state
--   afterwards would let the fold see the first event of the new run while the
--   generation was still the old one -- and book the whole standby interval.
noteForgingState :: ForgingStateVar -> Bool -> IO ()
noteForgingState (ForgingStateVar ref) enabled =
  atomicModifyIORef' ref $ \st ->
    ( ForgingState { fgGen     = if enabled && not (fgEnabled st)
                                   then fgGen st + 1
                                   else fgGen st
                   , fgEnabled = enabled }
    , () )

-- | The current generation. Read only -- never cleared, so that every fold can
--   observe the same resume independently.
currentResumeGen :: ForgingStateVar -> IO Int
currentResumeGen (ForgingStateVar ref) = fgGen <$> readIORef ref

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

calcForgeStats :: ForgingStateVar
  -> Trace IO ForgingStats
  -> IO (Trace IO (TraceForgeEvent blk))
calcForgeStats stateVar tr =
  let tr' = contramap unfold tr
  in foldCondTraceM (calculateForgingStats stateVar) emptyForgingStats
      (\case
          Consensus.TraceStartLeadershipCheck{} -> True
          _  -> False
          )
      tr'

calculateForgingStats :: MonadIO m
  => ForgingStateVar
  -> ForgingStats
  -> LoggingContext
  -> TraceForgeEvent blk
  -> m ForgingStats
calculateForgingStats _stateVar stats _context
    TraceNodeCannotForge {} =
      pure $ stats  { fsNodeCannotForgeNum  = fsNodeCannotForgeNum stats + 1 }
calculateForgingStats stateVar stats _context
    (TraceNodeIsLeader (SlotNo slot)) = do
      -- A completed leadership check: record the generation, so a resume
      -- accounted for here cannot also suppress a later, genuine gap.
      gen <- liftIO (currentResumeGen stateVar)
      pure $ stats  { fsNodeIsLeaderNum = fsNodeIsLeaderNum stats + 1
                    , fsLastSlot = fromIntegral slot
                    , fsResumeGen = gen }
calculateForgingStats _stateVar stats _context
    TraceForgedBlock {} =
        pure $ stats  { fsBlocksForgedNum  = fsBlocksForgedNum stats + 1 }
calculateForgingStats stateVar stats _context
    (TraceNodeNotLeader (SlotNo slot')) = do
      -- Node is not a leader again: The number of blocks forged by
      -- this node should now be equal to the number of slots when
      -- this node was a leader.
      --
      -- The first completed check after forging was (re-)enabled starts a new
      -- run of slots: while forging was disabled no leadership check ran, so
      -- the elapsed slots were not missed, they were not due.
      gen <- liftIO (currentResumeGen stateVar)
      let justResumed = gen /= fsResumeGen stats
          slot = fromIntegral slot'
      if justResumed || fsLastSlot stats == 0 || succ (fsLastSlot stats) == slot
        then pure $ stats { fsLastSlot = slot, fsResumeGen = gen }
        else
          let missed = slot - fsLastSlot stats
          in pure $ stats { fsLastSlot = slot
                          , fsResumeGen = gen
                          , fsSlotsMissedNum = fsSlotsMissedNum stats + missed }
calculateForgingStats _stateVar stats _context _message = pure stats
