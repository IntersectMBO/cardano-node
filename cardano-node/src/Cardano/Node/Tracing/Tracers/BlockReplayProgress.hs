{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Node.Tracing.Tracers.BlockReplayProgress
  (  withReplayedBlock
   , ReplayBlockStats(..)
  ) where

import           Cardano.Api (textShow)

import           Cardano.Logging
import           Ouroboros.Consensus.Block (Point, SlotNo, realPointSlot)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import           Ouroboros.Network.Block (pointSlot, unSlotNo)
import           Ouroboros.Network.Point (withOrigin)

import           Control.Concurrent.MVar
import           Data.Aeson (Value (String), (.=))
import           Data.Text (pack)

newtype ReplayBlockState = ReplayBlockState
  { -- | Last slot for which a `ReplayBlockStats` message has been issued.
    rpsLastSlot      :: Maybe SlotNo
  }

data ReplayBlockStats = ReplayBlockStats
  { rpsCurSlot      :: SlotNo
  , rpsGoalSlot     :: SlotNo
  }

initialReplayBlockState :: ReplayBlockState
initialReplayBlockState = ReplayBlockState {rpsLastSlot = Nothing}

progressForMachine ::  ReplayBlockStats -> Double
progressForMachine (ReplayBlockStats curSlot goalSlot) =
  (fromIntegral (unSlotNo curSlot) * 100.0) / fromIntegral (unSlotNo $ max curSlot goalSlot)

progressForHuman ::  ReplayBlockStats -> Double
progressForHuman = round2 . progressForMachine where
  round2 :: Double -> Double
  round2 num =
    let
      f :: Int
      f = round $ num * 100
    in fromIntegral f / 100

--------------------------------------------------------------------------------
-- ReplayBlockStats Tracer
--------------------------------------------------------------------------------

instance LogFormatting ReplayBlockStats where
  forMachine _ stats =
    mconcat
      [ "kind" .= String "ReplayBlockStats"
      , "progress" .= String (pack $ show $ progressForMachine stats)
      ]

  forHuman stats@ReplayBlockStats {..} =
         "Replayed block: slot "
      <> textShow (unSlotNo rpsCurSlot)
      <> " out of "
      <> textShow (unSlotNo rpsGoalSlot)
      <> ". Progress: "
      <> textShow (progressForHuman stats)
      <> "%"

  asMetrics stats =
     [DoubleM "blockReplayProgress" (progressForMachine stats)]

instance MetaTrace ReplayBlockStats where
  namespaceFor ReplayBlockStats {} = Namespace [] ["LedgerReplay"]

  severityFor (Namespace _ ["LedgerReplay"]) _ = Just Info
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["LedgerReplay"]) = Just
    "Counts block replays and calculates the percent."
  documentFor _ = Nothing

  metricsDocFor (Namespace _ ["LedgerReplay"]) =
     [("blockReplayProgress", "Progress in percent")]
  metricsDocFor _ = []

  allNamespaces =
    [ Namespace [] ["LedgerReplay"]
    ]

withReplayedBlock :: Trace IO ReplayBlockStats
    -> IO (Trace IO (ChainDB.TraceEvent blk))
withReplayedBlock tr = do
  var <- newMVar initialReplayBlockState
  contramapMCond tr (process var)
  where
    process :: MVar ReplayBlockState
            -> (LoggingContext, Either TraceControl (ChainDB.TraceEvent blk))
            -> IO (Maybe (LoggingContext, Either TraceControl ReplayBlockStats))
    process _ (ctx, Left control) = pure (Just (ctx, Left control))
    process var (ctx, Right msg) = modifyMVar var $ \st -> do
      let (st', mbStats) = mbProduceBlockStats st msg
      pure (st', fmap ((ctx,) . Right) mbStats)

    mbProduceBlockStats :: ReplayBlockState -> ChainDB.TraceEvent blk -> (ReplayBlockState, Maybe ReplayBlockStats)
    mbProduceBlockStats st@(ReplayBlockState (Just lastSlot))
      (ChainDB.TraceLedgerDBEvent
       (LedgerDB.LedgerReplayEvent
        (LedgerDB.TraceReplayProgressEvent
         (LedgerDB.ReplayedBlock curSlot [] _ (LedgerDB.ReplayGoal replayToSlot)))))
            | progressFor (realPointSlot curSlot) replayToSlot - progressFor lastSlot replayToSlot >= 0.1 =
                 (ReplayBlockState (Just (realPointSlot curSlot)), Just (ReplayBlockStats (realPointSlot curSlot) (withOrigin 0 id $ pointSlot replayToSlot)))
            | otherwise = (st, Nothing)
      where
        progressFor :: SlotNo -> Point blk -> Double
        progressFor soFar goal = progressForHuman (ReplayBlockStats soFar (withOrigin 0 id $ pointSlot goal))
    mbProduceBlockStats (ReplayBlockState Nothing)
      (ChainDB.TraceLedgerDBEvent
       (LedgerDB.LedgerReplayEvent
        (LedgerDB.TraceReplayProgressEvent
         (LedgerDB.ReplayedBlock curSlot [] _ (LedgerDB.ReplayGoal replayToSlot))))) =
           (ReplayBlockState (Just (realPointSlot curSlot)), Just (ReplayBlockStats (realPointSlot curSlot) (withOrigin 0 id $ pointSlot replayToSlot)))
    mbProduceBlockStats st _ = (st, Nothing)
