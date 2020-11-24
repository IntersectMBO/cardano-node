{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Tracing.MicroBenchmarking
    ( MeasureTxs (..)
    , measureTxsStart
    , measureTxsEnd
    -- * Re-exports so we localize the changes
    , Outcome (..)
    , OutcomeEnhancedTracer
    , mkOutcomeExtractor
    ) where

import           Cardano.Prelude

import           Control.Monad.Class.MonadTime (DiffTime, MonadTime, Time (..), diffTime,
                     getMonotonicTime)

import           Data.Aeson (Value (..), toJSON, (.=))
import           Data.Time.Clock (diffTimeToPicoseconds)

import           Cardano.BM.Data.Tracer (emptyObject, mkObject, trStructured)
import           Cardano.BM.Tracing

import           Control.Tracer.Transformers.ObserveOutcome

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, GenTxId, HasTxId, txId)
import           Ouroboros.Consensus.Mempool.API (MempoolSize (..), TraceEventMempool (..))
import           Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..))

--------------------------------------------------------------------------------
-- Measure transaction forging time
--------------------------------------------------------------------------------

-- | Definition of the measurement datatype for the transactions.
data MeasureTxs blk
    = MeasureTxsTimeStart (GenTx blk) !Word !Word !Time  -- num txs, total size in bytes
    | MeasureTxsTimeStop !SlotNo blk [GenTx blk] !Time

deriving instance (Eq blk, Eq (GenTx blk)) => Eq (MeasureTxs blk)
deriving instance (Show blk, Show (GenTx blk)) => Show (MeasureTxs blk)

instance Transformable Text IO (MeasureTxs blk) where
  trTransformer = trStructured

instance HasPrivacyAnnotation (MeasureTxs blk)
instance HasSeverityAnnotation (MeasureTxs blk) where
  getSeverityAnnotation _ = Info

-- TODO(KS): Time will be removed.
instance ToObject (MeasureTxs blk) where
  toObject _verb (MeasureTxsTimeStart _txs mempoolNumTxs mempoolNumBytes (Time time)) =
    mkObject
      [ "kind"              .= String "MeasureTxsTimeStart"
      , "mempoolNumTxs"     .= toJSON mempoolNumTxs
      , "mempoolNumBytes"   .= toJSON mempoolNumBytes
      , "time(ps)"          .= toJSON (diffTimeToPicoseconds time)
      ]
  toObject _verb (MeasureTxsTimeStop slotNo _blk _txs (Time time)) =
    mkObject
      [ "kind"              .= String "MeasureTxsTimeStop"
      , "slot"              .= toJSON (unSlotNo slotNo)
      , "time(ps)"          .= toJSON (diffTimeToPicoseconds time)
      ]

-- | Transformer for the start of the transaction, when the transaction was added
-- to the mempool.
measureTxsStart :: forall blk. Trace IO Text -> Tracer IO (TraceEventMempool blk)
measureTxsStart tracer = measureTxsStartInter $ toLogObject tracer
  where
    measureTxsStartInter :: Tracer IO (MeasureTxs blk) -> Tracer IO (TraceEventMempool blk)
    measureTxsStartInter tracer' = Tracer $ \case
        TraceMempoolAddedTx tx _mpSizeBefore mpSizeAfter ->
            traceWith tracer' =<< measureTxsEvent mpSizeAfter
          where
            measureTxsEvent :: MempoolSize -> IO (MeasureTxs blk)
            measureTxsEvent MempoolSize{msNumTxs, msNumBytes} =
              MeasureTxsTimeStart
                    tx
                    (fromIntegral msNumTxs)
                    (fromIntegral msNumBytes)
                <$> getMonotonicTime

        -- The rest of the constructors.
        _ -> pure ()

-- | Transformer for the end of the transaction, when the transaction was added to the
-- block and the block was forged.
measureTxsEnd :: forall blk. Trace IO Text -> Tracer IO (TraceForgeEvent blk)
measureTxsEnd tracer = measureTxsEndInter $ toLogObject tracer
  where
    measureTxsEndInter :: Tracer IO (MeasureTxs blk) -> Tracer IO (TraceForgeEvent blk)
    measureTxsEndInter tracer' = Tracer $ \case
        TraceAdoptedBlock slotNo blk txs    ->
            traceWith tracer' =<< measureTxsEvent
          where
            measureTxsEvent :: IO (MeasureTxs blk)
            measureTxsEvent = MeasureTxsTimeStop slotNo blk txs <$> getMonotonicTime

        -- The rest of the constructors.
        _ -> pure ()

-- Any Monad m, could be Identity in this case where we have all the data beforehand.
-- The result of this operation is the list of transactions that _made it in the block_
-- and the time it took them to get into the block.
instance (Monad m, HasTxId (GenTx blk)) => Outcome m (MeasureTxs blk) where
    type IntermediateValue  (MeasureTxs blk)    = [(GenTx blk, Time)]
    type OutcomeMetric      (MeasureTxs blk)    = [(GenTxId blk, DiffTime)]

    --classifyObservable     :: a -> m OutcomeProgressionStatus
    classifyObservable = pure . \case
      MeasureTxsTimeStart {}    -> OutcomeStarts
      MeasureTxsTimeStop  {}    -> OutcomeEnds

    --captureObservableValue :: a -> m (IntermediateValue a)
    captureObservableValue (MeasureTxsTimeStart tx _ _ time) =
        pure [(tx, time)]

    captureObservableValue (MeasureTxsTimeStop _sloNo _blk txs time) =
        pure [(tx, time) | tx <- txs]

    --computeOutcomeMetric   :: a -> IntermediateValue a -> IntermediateValue a -> m (OutcomeMetric a)
    computeOutcomeMetric _ xs ys = pure . computeFinalValues $ computeIntermediateValues xsTxId ysTxId
      where
        --xsTxId :: [(GenTxId blk, Time)]
        xsTxId = map (first txId) xs

        --ysTxId :: [(GenTxId blk, Time)]
        ysTxId = map (first txId) ys

        -- | Here we filter and match all the transactions that made it into
        -- a block.
        computeIntermediateValues
            :: [(GenTxId blk, Time)]
            -> [(GenTxId blk, Time)]
            -> [((GenTxId blk, Time), (GenTxId blk, Time))]
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
            :: [((GenTxId blk, Time), (GenTxId blk, Time))]
            -> [(GenTxId blk, DiffTime)]
        computeFinalValues intermediateValues =
            map (\((blk, timeStart), (_, timeEnd)) -> (blk, diffTime timeEnd timeStart)) intermediateValues

--------------------------------------------------------------------------------
-- Measure block forging time
--------------------------------------------------------------------------------

instance (Monad m, MonadTime m) => Outcome m (TraceForgeEvent blk) where
    type IntermediateValue  (TraceForgeEvent blk)    = (SlotNo, Time, MempoolSize)
    type OutcomeMetric      (TraceForgeEvent blk)    = Maybe (SlotNo, DiffTime, MempoolSize)

    --classifyObservable     :: a -> m OutcomeProgressionStatus
    classifyObservable = pure . \case
      TraceNodeIsLeader {}    -> OutcomeStarts
      TraceForgedBlock  {}    -> OutcomeEnds
      _                       -> OutcomeOther

    --captureObservableValue :: a -> m (IntermediateValue a)
    captureObservableValue (TraceNodeIsLeader slotNo) = do
        time <- getMonotonicTime
        pure (slotNo, time, mempty)

    captureObservableValue (TraceForgedBlock slotNo _ _blk mempoolSize) = do
        time <- getMonotonicTime
        pure (slotNo, time, mempoolSize)

    -- will never be called, just to make the pattern match complete
    captureObservableValue _ = do
        time <- getMonotonicTime
        pure (0, time, mempty)


    --computeOutcomeMetric   :: a -> IntermediateValue a -> IntermediateValue a -> m (OutcomeMetric a)
    computeOutcomeMetric _ (startSlot, absTimeStart, _) (stopSlot, absTimeStop, mempoolSize)
        | startSlot == stopSlot = pure $ Just (startSlot, diffTime absTimeStop absTimeStart, mempoolSize)
        | otherwise             = pure Nothing

instance HasPrivacyAnnotation (Either
                             (TraceForgeEvent blk)
                             (OutcomeFidelity
                                (Maybe
                                   (SlotNo, DiffTime, MempoolSize))))
instance HasSeverityAnnotation (Either
                             (TraceForgeEvent blk)
                             (OutcomeFidelity
                                (Maybe
                                   (SlotNo, DiffTime, MempoolSize)))) where
  getSeverityAnnotation _ = Info

instance Transformable Text IO
                        (Either
                             (TraceForgeEvent blk)
                             (OutcomeFidelity
                                (Maybe
                                   (SlotNo, DiffTime, MempoolSize)))) where
  trTransformer = trStructured

instance ToObject
                        (Either
                             (TraceForgeEvent blk)
                             (OutcomeFidelity
                                (Maybe
                                   (SlotNo, DiffTime, MempoolSize)))) where
  toObject _verb (Left _ev) = emptyObject
  toObject _verb (Right EndsBeforeStarted) = emptyObject
  toObject _verb (Right (ProgressedNormally (Just (slotno, difftime, mpsize)))) =
      mkObject
        [ "kind"         .= String "OutcomeTraceForgeEvent"
        , "slot"         .= toJSON (unSlotNo slotno)
        , "difftime"     .= toJSON (diffTimeToPicoseconds difftime)
        , "mempoolnumtx" .= toJSON (msNumTxs mpsize)
        , "mempoolbytes" .= toJSON (msNumBytes mpsize)
        ]
  toObject _verb (Right _) = emptyObject
