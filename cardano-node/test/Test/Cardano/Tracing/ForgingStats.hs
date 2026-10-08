{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Tracing.ForgingStats
  ( tests
  ) where

import           Cardano.Logging
import           Cardano.Node.Tracing.Tracers.ForgingStats
import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..))

import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import           Data.Maybe (listToMaybe)

import           Hedgehog (Property, discover, property, withTests, (===))
import qualified Hedgehog

-- | Collects whatever the Forge.Stats fold emits.
collect :: IORef [ForgingStats] -> Trace IO ForgingStats
collect ioRef = Trace $ arrow $ emit $ \case
  (LoggingContext{}, Right stats) -> liftIO $ modifyIORef' ioRef (stats :)
  (LoggingContext{}, _)           -> pure ()

-- | The most recent emission, or 'Nothing' if the fold never emitted.
--   Total: @head@ trips -Wx-partial, which this repository treats as an error
--   from GHC 9.8 on.
latest :: IORef [ForgingStats] -> IO (Maybe ForgingStats)
latest = fmap listToMaybe . readIORef

-- | Feed a run of leadership-check events and return the stats the fold
--   emitted last. The fold emits on 'TraceStartLeadershipCheck', so each run
--   ends with one.
runEvents :: ForgingResumed -> [TraceForgeEvent ()] -> IO (Maybe ForgingStats)
runEvents resumed events = do
  ioRef <- newIORef []
  tr <- calcForgeStats resumed (collect ioRef)
  mapM_ (traceWith tr) events
  latest ioRef

-- | Slots that elapse while forging is disabled are not leadership checks that
--   were missed -- they were never due. Regression test for issue #6698, where
--   @slotsMissed_int@ jumped by the length of the standby interval as soon as
--   forging was re-enabled.
prop_standbyIsNotMissedSlots :: Property
prop_standbyIsNotMissedSlots = withTests 1 . property $ do
  stats <- liftIO $ do
    resumed <- newForgingResumed True
    ioRef    <- newIORef []
    tr       <- calcForgeStats resumed (collect ioRef)
    -- producing: two consecutive slots
    traceWith tr (TraceNodeNotLeader (SlotNo 100))
    traceWith tr (TraceNodeNotLeader (SlotNo 101))
    -- the credentials go away and a SIGHUP disables forging: no leadership
    -- check events at all for ~240 slots, then a SIGHUP enables it again
    noteForgingState resumed False
    noteForgingState resumed True
    traceWith tr (TraceNodeNotLeader (SlotNo 341))
    traceWith tr (TraceNodeNotLeader (SlotNo 342))
    traceWith tr (TraceStartLeadershipCheck (SlotNo 343))
    latest ioRef
  (fsSlotsMissedNum <$> stats) === Just 0

-- | The fix must not mask genuine misses: a gap with no intervening
--   resume is still counted (cf. issue #2890).
prop_genuineGapIsStillCounted :: Property
prop_genuineGapIsStillCounted = withTests 1 . property $ do
  stats <- liftIO $ runEvents' =<< newForgingResumed True
  (fsSlotsMissedNum <$> stats) === Just 4
 where
  runEvents' resumed = runEvents resumed
    [ TraceNodeNotLeader (SlotNo 100)
    , TraceNodeNotLeader (SlotNo 101)
    , TraceNodeNotLeader (SlotNo 105)   -- 102..105 were due but not checked
    , TraceStartLeadershipCheck (SlotNo 106)
    ]

-- | 'mkCardanoTracer'' applies its hook twice, once for the message path and
--   once for the metrics path, so 'calcForgeStats' builds two independent folds
--   over the same events sharing one 'ForgingResumed'. @slotsMissed_int@ comes
--   from the metrics fold. Both folds must get a fresh start on resume; a flag
--   that the fold consumed was taken by whichever ran first, and the other
--   still booked the standby interval (seen in a workbench cluster as
--   slotsMissed jumping by the standby duration).
prop_everyFoldGetsTheResume :: Property
prop_everyFoldGetsTheResume = withTests 1 . property $ do
  (a, b) <- liftIO $ do
    resumed <- newForgingResumed True
    refA <- newIORef []; refB <- newIORef []
    trA <- calcForgeStats resumed (collect refA)   -- message path
    trB <- calcForgeStats resumed (collect refB)   -- metrics path
    let feed e = traceWith trA e >> traceWith trB e
    feed (TraceNodeNotLeader (SlotNo 100))
    feed (TraceNodeNotLeader (SlotNo 101))
    noteForgingState resumed False                  -- standby ...
    noteForgingState resumed True                   -- ... then resume
    feed (TraceNodeNotLeader (SlotNo 341))
    feed (TraceStartLeadershipCheck (SlotNo 342))
    (,) <$> latest refA <*> latest refB
  (fsSlotsMissedNum <$> a) === Just 0
  (fsSlotsMissedNum <$> b) === Just 0

-- | A @SIGHUP@ sent for a topology or RPC change also re-reads the credentials
--   and so reaches 'updateBlockForging', but forging never stopped: no slots
--   went unchecked because none were undue. Booking the reload as a resume
--   would discard whatever gap had accumulated before the signal -- the very
--   number this tracer exists to report.
prop_reloadWhileForgingDoesNotMaskGap :: Property
prop_reloadWhileForgingDoesNotMaskGap = withTests 1 . property $ do
  stats <- liftIO $ do
    resumed <- newForgingResumed True
    ioRef    <- newIORef []
    tr       <- calcForgeStats resumed (collect ioRef)
    traceWith tr (TraceNodeNotLeader (SlotNo 100))
    traceWith tr (TraceNodeNotLeader (SlotNo 101))
    -- 102..105 are due but go unchecked; a reload lands in the middle of the
    -- gap and leaves forging on
    noteForgingState resumed True
    traceWith tr (TraceNodeNotLeader (SlotNo 105))
    traceWith tr (TraceStartLeadershipCheck (SlotNo 106))
    latest ioRef
  (fsSlotsMissedNum <$> stats) === Just 4

-- | The resume may land on a leader slot, so 'TraceNodeIsLeader' records the
--   generation as well. Without that, the resume would still be unaccounted for
--   when the next 'TraceNodeNotLeader' arrives, and it would excuse a genuine
--   gap that opened after forging was already back on.
prop_resumeAbsorbedByLeaderSlotDoesNotExcuseALaterGap :: Property
prop_resumeAbsorbedByLeaderSlotDoesNotExcuseALaterGap = withTests 1 . property $ do
  stats <- liftIO $ do
    resumed <- newForgingResumed True
    noteForgingState resumed False                 -- standby ...
    noteForgingState resumed True                  -- ... then resume
    runEvents resumed
      [ TraceNodeIsLeader (SlotNo 341)              -- the resume is spent here
      , TraceNodeNotLeader (SlotNo 345)             -- 342..345 were due
      , TraceStartLeadershipCheck (SlotNo 346)
      ]
  (fsSlotsMissedNum <$> stats) === Just 4

-- | Several off/on cycles may pass before any fold sees an event -- a flapping
--   KES agent, or a SIGHUP storm. They must collapse into exactly one fresh
--   start, which is why 'justResumed' compares the generations for inequality
--   rather than expecting the successor.
prop_severalCyclesGrantOneFreshStart :: Property
prop_severalCyclesGrantOneFreshStart = withTests 1 . property $ do
  stats <- liftIO $ do
    resumed <- newForgingResumed True
    ioRef    <- newIORef []
    tr       <- calcForgeStats resumed (collect ioRef)
    traceWith tr (TraceNodeNotLeader (SlotNo 100))
    traceWith tr (TraceNodeNotLeader (SlotNo 101))
    mapM_ (noteForgingState resumed) [False, True, False, True]
    traceWith tr (TraceNodeNotLeader (SlotNo 341))
    traceWith tr (TraceStartLeadershipCheck (SlotNo 342))
    latest ioRef
  (fsSlotsMissedNum <$> stats) === Just 0

-- | The accepted boundary of the fix: a gap that is already open when forging
--   is switched off is forgiven along with the standby interval, because the
--   fold only keeps the last slot it saw and cannot tell the two apart. The
--   loss is bounded by the standby, and pricing it would need a per-slot
--   record, which this tracer deliberately does not keep.
prop_gapStraddlingTheDisableIsForgiven :: Property
prop_gapStraddlingTheDisableIsForgiven = withTests 1 . property $ do
  stats <- liftIO $ do
    resumed <- newForgingResumed True
    ioRef    <- newIORef []
    tr       <- calcForgeStats resumed (collect ioRef)
    traceWith tr (TraceNodeNotLeader (SlotNo 100))
    -- 101..104 are due and go unchecked, then forging is switched off
    noteForgingState resumed False
    noteForgingState resumed True
    traceWith tr (TraceNodeNotLeader (SlotNo 341))
    traceWith tr (TraceStartLeadershipCheck (SlotNo 342))
    latest ioRef
  (fsSlotsMissedNum <$> stats) === Just 0

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
