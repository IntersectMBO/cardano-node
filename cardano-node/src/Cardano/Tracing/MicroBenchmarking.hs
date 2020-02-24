{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cardano.Tracing.MicroBenchmarking
    ( MeasureTxs (..)
    , measureTxsStart
    , measureTxsEnd
    , MeasureBlockForging (..)
    , measureBlockForgeStart
    , measureBlockForgeEnd
    -- * Re-exports so we localize the changes
    , Outcome (..)
    , OutcomeEnhancedTracer
    , mkOutcomeExtractor
    ) where

import           Cardano.Prelude

import           Control.Monad.Class.MonadTime (DiffTime, Time (..), diffTime,
                                                getMonotonicTime)

import           Data.Aeson (Value (..), toJSON, (.=))
import           Data.Time.Clock (diffTimeToPicoseconds)

import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Data.Trace
import           Cardano.BM.Data.Tracer

import           Control.Tracer.Transformers.ObserveOutcome

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.Ledger.Abstract (ProtocolLedgerView)
import           Ouroboros.Consensus.Mempool.API
                   (ApplyTx (..), GenTx, GenTxId, HasTxId (..),
                    MempoolSize (..), TraceEventMempool (..), txId)
import           Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..))

--------------------------------------------------------------------------------
-- Measure transaction forging time
--------------------------------------------------------------------------------

-- | Definition of the measurement datatype for the transactions.
data MeasureTxs blk
    = MeasureTxsTimeStart (GenTx blk) !Word !Word !Time  -- num txs, total size in bytes
    | MeasureTxsTimeStop !SlotNo blk [GenTx blk] !Time

deriving instance (ProtocolLedgerView blk, Eq blk, Eq (GenTx blk)) => Eq (MeasureTxs blk)
deriving instance (ProtocolLedgerView blk, Show blk, Show (GenTx blk)) => Show (MeasureTxs blk)

instance Transformable Text IO (MeasureTxs blk) where
  trTransformer _ verb tr = trStructured verb tr

instance DefinePrivacyAnnotation (MeasureTxs blk)
instance DefineSeverity (MeasureTxs blk) where
  defineSeverity _ = Info

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
measureTxsEnd :: forall blk. Trace IO Text -> Tracer IO (TraceForgeEvent blk (GenTx blk))
measureTxsEnd tracer = measureTxsEndInter $ toLogObject tracer
  where
    measureTxsEndInter :: Tracer IO (MeasureTxs blk) -> Tracer IO (TraceForgeEvent blk (GenTx blk))
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
instance (Monad m, ApplyTx blk, HasTxId (GenTx blk)) => Outcome m (MeasureTxs blk) where
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
        xsTxId = map (\(genTx, _time) -> (txId genTx, _time)) xs

        --ysTxId :: [(GenTxId blk, Time)]
        ysTxId = map (\(genTx, _time) -> (txId genTx, _time)) ys

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

-- | Definition of the measurement datatype for the block forge time.
data MeasureBlockForging blk
    = MeasureBlockTimeStart !SlotNo  !Time
    | MeasureBlockTimeStop !SlotNo blk !MempoolSize !Time

deriving instance (ProtocolLedgerView blk, Eq blk, Eq (GenTx blk)) => Eq (MeasureBlockForging blk)
deriving instance (ProtocolLedgerView blk, Show blk, Show (GenTx blk)) => Show (MeasureBlockForging blk)

instance Transformable Text IO (MeasureBlockForging blk) where
  trTransformer _ verb tr = trStructured verb tr

instance DefinePrivacyAnnotation (MeasureBlockForging blk)
instance DefineSeverity (MeasureBlockForging blk) where
  defineSeverity _ = Info

instance ToObject (MeasureBlockForging blk) where
  toObject _verb (MeasureBlockTimeStart slotNo (Time time)) =
    mkObject
      [ "kind"              .= String "MeasureBlockTimeStart"
      , "slot"              .= toJSON (unSlotNo slotNo)
      , "time(ps)"          .= toJSON (diffTimeToPicoseconds time)
      ]
  toObject _verb (MeasureBlockTimeStop slotNo _blk mempoolSize (Time time)) =
    mkObject
      [ "kind"              .= String "MeasureBlockTimeStop"
      , "slot"              .= toJSON (unSlotNo slotNo)
      , "mempoolNumTxs"     .= toJSON (msNumTxs mempoolSize)
      , "mempoolNumBytes"   .= toJSON (msNumBytes mempoolSize)
      , "time(ps)"          .= toJSON (diffTimeToPicoseconds time)
      ]

-- | Transformer for the start of the block forge, when the current slot is the slot of the
-- node and the protocol starts.
measureBlockForgeStart :: Trace IO Text -> Tracer IO (TraceForgeEvent blk (GenTx blk))
measureBlockForgeStart tracer = measureBlockForgeStartInter $ toLogObject tracer
  where
    measureBlockForgeStartInter :: Tracer IO (MeasureBlockForging blk) -> Tracer IO (TraceForgeEvent blk (GenTx blk))
    measureBlockForgeStartInter tracer' = Tracer $ \case
        TraceNodeIsLeader slotNo
            -> traceWith tracer' =<< (MeasureBlockTimeStart slotNo <$> getMonotonicTime)
        _ -> pure ()

-- | Transformer for the end of the block forge, when the block was created/forged.
measureBlockForgeEnd :: Trace IO Text -> Tracer IO (TraceForgeEvent blk (GenTx blk))
measureBlockForgeEnd tracer = measureTxsEndInter $ toLogObject tracer
  where
    measureTxsEndInter :: Tracer IO (MeasureBlockForging blk) -> Tracer IO (TraceForgeEvent blk (GenTx blk))
    measureTxsEndInter tracer' = Tracer $ \case
        TraceForgedBlock slotNo blk mempoolSize
            -> traceWith tracer' =<< (MeasureBlockTimeStop slotNo blk mempoolSize <$> getMonotonicTime)
        _   -> pure ()


-- | The outcome for the block forging time. It's a @Maybe@ since
-- the slot number might not be equal from when we start the measurement.
instance (Monad m) => Outcome m (MeasureBlockForging blk) where
    type IntermediateValue  (MeasureBlockForging blk)    = (SlotNo, Time, MempoolSize)
    type OutcomeMetric      (MeasureBlockForging blk)    = Maybe (SlotNo, DiffTime, MempoolSize)

    --classifyObservable     :: a -> m OutcomeProgressionStatus
    classifyObservable = pure . \case
      MeasureBlockTimeStart {}    -> OutcomeStarts
      MeasureBlockTimeStop  {}    -> OutcomeEnds

    --captureObservableValue :: a -> m (IntermediateValue a)
    captureObservableValue (MeasureBlockTimeStart slotNo time) =
        pure (slotNo, time, mempty)

    captureObservableValue (MeasureBlockTimeStop slotNo _blk mempoolSize time) =
        pure (slotNo, time, mempoolSize)

    --computeOutcomeMetric   :: a -> IntermediateValue a -> IntermediateValue a -> m (OutcomeMetric a)
    computeOutcomeMetric _ (startSlot, absTimeStart, _) (stopSlot, absTimeStop, mempoolSize)
        | startSlot == stopSlot = pure $ Just (startSlot, (diffTime absTimeStop absTimeStart), mempoolSize)
        | otherwise             = pure Nothing
