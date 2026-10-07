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
    resumed <- newForgingResumed
    ioRef   <- newIORef []
    tr      <- calcForgeStats resumed (collect ioRef)
    -- producing: two consecutive slots
    traceWith tr (TraceNodeNotLeader (SlotNo 100))
    traceWith tr (TraceNodeNotLeader (SlotNo 101))
    -- forging disabled: no leadership check events at all for ~240 slots,
    -- then forging is re-enabled
    noteForgingResumed resumed
    traceWith tr (TraceNodeNotLeader (SlotNo 341))
    traceWith tr (TraceNodeNotLeader (SlotNo 342))
    traceWith tr (TraceStartLeadershipCheck (SlotNo 343))
    latest ioRef
  (fsSlotsMissedNum <$> stats) === Just 0

-- | The fix must not mask genuine misses: a gap with no intervening
--   resume is still counted (cf. issue #2890).
prop_genuineGapIsStillCounted :: Property
prop_genuineGapIsStillCounted = withTests 1 . property $ do
  stats <- liftIO $ runEvents' =<< newForgingResumed
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
    resumed <- newForgingResumed
    refA <- newIORef []; refB <- newIORef []
    trA <- calcForgeStats resumed (collect refA)   -- message path
    trB <- calcForgeStats resumed (collect refB)   -- metrics path
    let feed e = traceWith trA e >> traceWith trB e
    feed (TraceNodeNotLeader (SlotNo 100))
    feed (TraceNodeNotLeader (SlotNo 101))
    noteForgingResumed resumed                      -- standby, then resume
    feed (TraceNodeNotLeader (SlotNo 341))
    feed (TraceStartLeadershipCheck (SlotNo 342))
    (,) <$> latest refA <*> latest refB
  (fsSlotsMissedNum <$> a) === Just 0
  (fsSlotsMissedNum <$> b) === Just 0

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
