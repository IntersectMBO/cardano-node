{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}

module Cardano.Tracing.MicroBenchmarking
    ( MeasureTxs (..)
    , measureTxsStart
    , measureTxsEnd
    ) where

import           Cardano.Prelude

import           Control.Monad.Class.MonadTime (Time, DiffTime, diffTime)

import           Data.Aeson (Value (..), (.=))

import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Tracer
import           Cardano.BM.Data.Severity (Severity (..))

import           Control.Tracer.Transformers.ObserveOutcome

import           Ouroboros.Network.Block (SlotNo)

import           Ouroboros.Consensus.Ledger.Abstract (ProtocolLedgerView)
import           Ouroboros.Consensus.Mempool.API (GenTx, TraceEventMempool(..))
import           Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..))

-- | Definition of the measurement datatype for the transactions.
data MeasureTxs blk
    = MeasureTxsTimeStart [GenTx blk] Word Time
    | MeasureTxsTimeStop SlotNo blk [GenTx blk] Time

deriving instance (ProtocolLedgerView blk, Eq blk, Eq (GenTx blk)) => Eq (MeasureTxs blk)
deriving instance (ProtocolLedgerView blk, Show blk, Show (GenTx blk)) => Show (MeasureTxs blk)

-- Any Monad m, could be Identity in this case where we have all the data beforehand.
-- The result of this operation is the list of transactions that _made it in the block_
-- and the time it took them to get into the block.
instance (Monad m, Eq (GenTx blk)) => Outcome m (MeasureTxs blk) where
    type IntermediateValue  (MeasureTxs blk)    = [(GenTx blk, Time)]
    type OutcomeMetric      (MeasureTxs blk)    = [(GenTx blk, DiffTime)]

    --classifyObservable     :: a -> m OutcomeProgressionStatus
    classifyObservable = pure . \case
      MeasureTxsTimeStart {}    -> OutcomeStarts
      MeasureTxsTimeStop  {}    -> OutcomeEnds

    --captureObservableValue :: a -> m (IntermediateValue a)
    captureObservableValue (MeasureTxsTimeStart txs _ time) =
        pure [(tx, time) | tx <- txs]

    captureObservableValue (MeasureTxsTimeStop _sloNo _blk txs time) =
        pure [(tx, time) | tx <- txs]

    --computeOutcomeMetric   :: a -> IntermediateValue a -> IntermediateValue a -> m (OutcomeMetric a)
    computeOutcomeMetric _ xs ys = pure . computeFinalValues $ computeIntermediateValues xs ys
      where
        -- | Here we filter and match all the transactions that made it into
        -- a block.
        computeIntermediateValues
            :: Eq (GenTx blk)
            => [(GenTx blk, Time)]
            -> [(GenTx blk, Time)]
            -> [((GenTx blk, Time), (GenTx blk, Time))]
        computeIntermediateValues [] _  = []
        computeIntermediateValues _ []  = []
        --[ (x, y) | x@(xTx, _) <- xs, y@(yTx, _) <- ys, xTx == yTx ]
        computeIntermediateValues xs' ys' = do
            x@(xTx, _) <- xs'
            y@(yTx, _) <- ys'
            guard (xTx == yTx)
            return (x, y)

        -- | From all the transactions that made it into a block we simply
        -- diff the time it took them and associate that time with the transaction
        -- that made it into a block.
        computeFinalValues
            :: [((GenTx blk, Time), (GenTx blk, Time))]
            -> [(GenTx blk, DiffTime)]
        computeFinalValues intermediateValues =
            map (\((blk, timeStart), (_, timeEnd)) -> (blk, diffTime timeEnd timeStart)) intermediateValues


instance Transformable Text IO (MeasureTxs blk) where
  trTransformer _ verb tr = trStructured verb tr

instance DefinePrivacyAnnotation (MeasureTxs blk)
instance DefineSeverity (MeasureTxs blk) where
  defineSeverity _ = Info

-- TODO(KS): Clarify the structure of the type.
instance ToObject (MeasureTxs blk) where
  toObject _verb _ =
    mkObject [ "kind"       .= String "MeasureTxsTimeStart"
             ]

-- | Transformer for the start of the transaction, when the transaction was added
-- to the mempool.
measureTxsStart :: Tracer IO (LogObject Text) -> Tracer IO (TraceEventMempool blk)
measureTxsStart tracer = measureTxsStartInter $ toLogObject tracer
  where
    measureTxsStartInter :: Tracer IO (MeasureTxs blk) -> Tracer IO (TraceEventMempool blk)
    measureTxsStartInter tracer' = Tracer $ \case
        TraceMempoolAddTxs txs totalNum time    -> traceWith tracer' (MeasureTxsTimeStart txs totalNum time)
        _                                       -> pure ()

-- | Transformer for the end of the transaction, when the transaction was added to the
-- block and the block was forged.
measureTxsEnd :: Tracer IO (LogObject Text) -> Tracer IO (TraceForgeEvent blk (GenTx blk))
measureTxsEnd tracer = measureTxsEndInter $ toLogObject tracer
  where
    measureTxsEndInter :: Tracer IO (MeasureTxs blk) -> Tracer IO (TraceForgeEvent blk (GenTx blk))
    measureTxsEndInter tracer' = Tracer $ \case
        TraceAdoptedBlock slotNo blk txs time   -> traceWith tracer' (MeasureTxsTimeStop slotNo blk txs time)
        _                                       -> pure ()

