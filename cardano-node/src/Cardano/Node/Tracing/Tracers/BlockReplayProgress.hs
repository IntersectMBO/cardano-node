{-# LANGUAGE RecordWildCards #-}

module Cardano.Node.Tracing.Tracers.BlockReplayProgress
  (  withReplayedBlock
   , ReplayBlockStats(..)
  ) where

import           Cardano.Api (textShow)

import           Cardano.Logging
import           Ouroboros.Consensus.Block (realPointSlot)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import           Ouroboros.Network.Block (pointSlot, unSlotNo)
import           Ouroboros.Network.Point (withOrigin)

import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson (Value (String), (.=))
import           Data.Text (pack)

data ReplayBlockStats = ReplayBlockStats
  { rpsDisplay      :: Bool
  , rpsProgress     :: Double
  , rpsLastProgress :: Double
  }

emptyReplayBlockStats :: ReplayBlockStats
emptyReplayBlockStats = ReplayBlockStats False 0.0 0.0

--------------------------------------------------------------------------------
-- ReplayBlockStats Tracer
--------------------------------------------------------------------------------

instance LogFormatting ReplayBlockStats where
  forMachine _dtal ReplayBlockStats {..} =
    mconcat
      [ "kind" .= String "ReplayBlockStats"
      , "progress" .= String (pack $ show rpsProgress)
      ]
  forHuman ReplayBlockStats {..} = "Block replay progress " <> textShow rpsProgress <> "%"
  asMetrics ReplayBlockStats {..} =
     [DoubleM "ChainDB.BlockReplayProgress" rpsProgress]

instance MetaTrace ReplayBlockStats where
  namespaceFor ReplayBlockStats {} = Namespace [] ["LedgerReplay"]

  severityFor (Namespace _ ["LedgerReplay"]) _ = Just Info
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["LedgerReplay"]) = Just
    "Counts block replays and calculates the percent."
  documentFor _ = Nothing

  metricsDocFor (Namespace _ ["LedgerReplay"]) =
     [("ChainDB.BlockReplayProgress", "Progress in percent")]
  metricsDocFor _ = []

  allNamespaces =
    [ Namespace [] ["LedgerReplay"]
    ]

withReplayedBlock :: Trace IO ReplayBlockStats
    -> IO (Trace IO (ChainDB.TraceEvent blk))
withReplayedBlock tr =
    let tr' = filterTrace filterFunction tr
        tr'' = contramap unfold tr'
    in foldTraceM replayBlockStats emptyReplayBlockStats tr''
  where
    filterFunction(_, ReplayBlockStats {..}) = rpsDisplay

replayBlockStats :: MonadIO m
  => ReplayBlockStats
  -> LoggingContext
  -> ChainDB.TraceEvent blk
  -> m ReplayBlockStats
replayBlockStats ReplayBlockStats {..} _context
    (ChainDB.TraceLedgerReplayEvent (LedgerDB.ReplayedBlock pt []
                                      (LedgerDB.ReplayStart replayTo) _)) = do
      let slotno = toInteger $ unSlotNo (realPointSlot pt)
          endslot = toInteger $ withOrigin 0 unSlotNo (pointSlot replayTo)
          progress' = (fromInteger slotno * 100.0) / fromInteger (max slotno endslot)
      pure $ if (progress' == 0.0 && not rpsDisplay)
                || ((progress' - rpsLastProgress) > 1.0)
                then ReplayBlockStats True progress' progress'
                else ReplayBlockStats False progress' rpsLastProgress
replayBlockStats st@ReplayBlockStats {} _context _ = pure st
