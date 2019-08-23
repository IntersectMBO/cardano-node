{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Node.Tracers
  ( ConsensusTraceOptions
  , ProtocolTraceOptions
  , Tracers (..)
  , TraceConstraints
  , TraceOptions(..)
  , mkTracers
  , withTip
  ) where

import           Cardano.Prelude hiding (atomically, show)
import           Prelude (String, show, id)

import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Monad.Class.MonadSTM
import           Control.Tracer
import           Data.Functor.Const (Const (..))
import           Data.Functor.Contravariant (contramap)
import           Data.Text (Text, pack)

import           Cardano.BM.Data.Aggregated (Measurable (PureI))
import           Cardano.BM.Data.LogItem (LOContent (..), LogObject (..),
                                          PrivacyAnnotation (Confidential),
                                          mkLOMeta)
import           Cardano.BM.Tracing
import           Cardano.BM.Trace (traceNamedObject)
import           Cardano.BM.Data.Tracer (trStructured)


import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr, GenTx, GenTxId, TraceEventMempool (..))
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers,
                                                  ProtocolTracers' (..))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Storage.ChainDB as ChainDB
import qualified Ouroboros.Storage.LedgerDB.OnDisk as LedgerDB

import           Cardano.Node.ToObjectOrphans


data Tracers peer blk = Tracers {
      -- | Trace the ChainDB (flag '--trace-chain-db' will turn on textual output)
      chainDBTracer         :: Tracer IO (WithTip blk (ChainDB.TraceEvent blk))

      -- | Consensus-specific tracers.
    , consensusTracers      :: Consensus.Tracers IO peer blk

      -- | Tracers for the protocol messages.
    , protocolTracers       :: ProtocolTracers IO peer blk DeserialiseFailure

      -- | Trace the IP subscription manager.
    , ipSubscriptionTracer  :: Tracer IO String

      -- | Trace the DNS subscription manager
    , dnsSubscriptionTracer :: Tracer IO String

      -- | Trace the DNS resolver.
    , dnsResolverTracer     :: Tracer IO String
    }

-- | Tracing-related constraints for monitoring purposes.
--
-- When you need a 'Show' or 'Condense' instance for more types, just add the
-- appropriate constraint here. There's no need to modify the consensus
-- code-base, unless the corresponding instance is missing.
type TraceConstraints blk =
  ( Condense blk
  , Condense [blk]
  , Condense (ChainHash blk)
  , Condense (Header blk)
  , Condense (HeaderHash blk)
  , Condense (GenTx blk)
  , Show (ApplyTxErr blk)
  , Show (GenTx blk)
  , Show (GenTxId blk)
  , Show blk
  , Show (Header blk)
  )

-- | Tracing options. Each option enables a tracer which adds verbosity to the
-- log output.
data TraceOptions = TraceOptions
  { traceVerbosity       :: !TracingVerbosity
  , traceChainDB         :: !Bool
    -- ^ By default we use 'readableChainDB' tracer, if on this it will use
    -- more verbose tracer
  , traceConsensus       :: ConsensusTraceOptions
  , traceProtocols       :: ProtocolTraceOptions
  , traceIpSubscription  :: !Bool
  , traceDnsSubscription :: !Bool
  , traceDnsResolver     :: !Bool
  }

type ConsensusTraceOptions = Consensus.Tracers' () ()    (Const Bool)
type ProtocolTraceOptions  = ProtocolTracers'   () () () (Const Bool)

-- | tracing to LogObject, either structural or textual
--

instance (Condense (HeaderHash blk), ProtocolLedgerView blk)
            => Transformable Text IO (WithTip blk (ChainDB.TraceEvent blk)) where
    -- structure required, will call 'toObject'
    trTransformer StructuredLogging verb tr = trStructured verb tr
    -- textual output based on the readable ChainDB tracer
    trTransformer TextualRepresentation _verb tr = readableChainDBTracer $ Tracer $ \s ->
        traceWith tr =<<
            LogObject <$> pure ""
                      <*> (mkLOMeta Debug Public)
                      <*> pure (LogMessage $ pack s)
    -- user defined formatting of log output
    trTransformer UserdefinedFormatting verb tr = trStructured verb tr

-- | tracer transformer to text messages for TraceEvents

-- Converts the trace events from the ChainDB that we're interested in into
-- human-readable trace messages.
readableChainDBTracer
    :: forall m blk.
       (Monad m, Condense (HeaderHash blk), ProtocolLedgerView blk)
    => Tracer m String
    -> Tracer m (WithTip blk (ChainDB.TraceEvent blk))
readableChainDBTracer tracer = Tracer $ \case
    WithTip tip (ChainDB.TraceAddBlockEvent ev) -> case ev of
        ChainDB.StoreButDontChange   pt -> tr $ WithTip tip $
          "Ignoring block: " <> condense pt
        ChainDB.TryAddToCurrentChain pt -> tr $ WithTip tip $
          "Block fits onto the current chain: " <> condense pt
        ChainDB.TrySwitchToAFork pt _   -> tr $ WithTip tip $
          "Block fits onto some fork: " <> condense pt
        ChainDB.SwitchedToChain _ c     -> tr $ WithTip tip $
          "Chain changed, new tip: " <> condense (AF.headPoint c)
        ChainDB.AddBlockValidation ev' -> case ev' of
            ChainDB.InvalidBlock err pt -> tr $ WithTip tip $
              "Invalid block " <> condense pt <> ": " <> show err
            ChainDB.InvalidCandidate c err -> tr $ WithTip tip $
              "Invalid candidate " <> condense (AF.headPoint c) <> ": " <> show err
            ChainDB.ValidCandidate c -> tr $ WithTip tip $
              "Valid candidate " <> condense (AF.headPoint c)
        ChainDB.AddedBlockToVolDB pt     -> tr $ WithTip tip $
          "Chain added block " <> condense pt
        ChainDB.ChainChangedInBg c1 c2     -> tr $ WithTip tip $
          "Chain changed in bg, from " <> condense (AF.headPoint c1) <> " to "  <> condense (AF.headPoint c2)
    WithTip tip (ChainDB.TraceLedgerReplayEvent ev) -> case ev of
        LedgerDB.ReplayFromGenesis _replayTo -> tr $ WithTip tip $
          "Replaying ledger from genesis"
        LedgerDB.ReplayFromSnapshot snap tip' _replayTo -> tr $ WithTip tip $
          "Replaying ledger from snapshot " <> show snap <> " at " <>
          condense tip'
        LedgerDB.ReplayedBlock {} -> pure ()
    WithTip tip (ChainDB.TraceLedgerEvent ev) -> case ev of
        LedgerDB.TookSnapshot snap pt -> tr $ WithTip tip $
          "Took ledger snapshot " <> show snap <> " at " <> condense pt
        LedgerDB.DeletedSnapshot snap -> tr $ WithTip tip $
          "Deleted old snapshot " <> show snap
        LedgerDB.InvalidSnapshot snap failure -> tr $ WithTip tip $
          "Invalid snapshot " <> show snap <> show failure
    WithTip tip (ChainDB.TraceCopyToImmDBEvent ev) -> case ev of
        ChainDB.CopiedBlockToImmDB pt -> tr $ WithTip tip $
          "Copied block " <> condense pt <> " to the ImmutableDB"
        ChainDB.NoBlocksToCopyToImmDB -> tr $ WithTip tip $
          "There are no blocks to copy to the ImmutableDB"
    WithTip tip (ChainDB.TraceGCEvent ev) -> case ev of
        ChainDB.PerformedGC slot        -> tr $ WithTip tip $
          "Performed a garbage collection for " <> condense slot
        ChainDB.ScheduledGC slot _difft -> tr $ WithTip tip $
          "Scheduled a garbage collection for " <> condense slot
    WithTip tip (ChainDB.TraceOpenEvent ev) -> case ev of
        ChainDB.OpenedDB immTip tip' -> tr $ WithTip tip $
          "Opened db with immutable tip at " <> condense immTip <>
          " and tip " <> condense tip'
        ChainDB.ClosedDB immTip tip' -> tr $ WithTip tip $
          "Closed db with immutable tip at " <> condense immTip <>
          " and tip " <> condense tip'
        ChainDB.ReopenedDB immTip tip' -> tr $ WithTip tip $
          "Reopened db with immutable tip at " <> condense immTip <>
          " and tip " <> condense tip'
        ChainDB.OpenedImmDB immTip epoch -> tr $ WithTip tip $
          "Opened imm db with immutable tip at " <> condense immTip <>
          " and epoch " <> show epoch
        ChainDB.OpenedVolDB -> tr $ WithTip tip $
          "Opened vol db"
        ChainDB.OpenedLgrDB -> tr $ WithTip tip $
          "Opened lgr db"
    WithTip tip (ChainDB.TraceReaderEvent ev) -> case ev of
        ChainDB.NewReader readerid -> tr $ WithTip tip $
          "New reader with id: " <> condense readerid
        ChainDB.ReaderNoLongerInMem _ -> tr $ WithTip tip $
          "ReaderNoLongerInMem"
        ChainDB.ReaderSwitchToMem _ _ -> tr $ WithTip tip $
          "ReaderSwitchToMem"
        ChainDB.ReaderNewImmIterator _ _ -> tr $ WithTip tip $
          "ReaderNewImmIterator"
    WithTip tip (ChainDB.TraceInitChainSelEvent ev) -> case ev of
        ChainDB.InitChainSelValidation _ -> tr $ WithTip tip $
          "InitChainSelValidation"
    WithTip tip (ChainDB.TraceIteratorEvent ev) -> case ev of
        ChainDB.StreamFromVolDB _ _ _ -> tr $ WithTip tip $
          "StreamFromVolDB"
        _ -> pure ()  -- TODO add more iterator events
    WithTip tip (ChainDB.TraceImmDBEvent _ev) -> tr $ WithTip tip $
        "TraceImmDBEvent"

  where
    tr :: WithTip blk String -> m ()
    tr = traceWith (contramap (showWithTip id) tracer)


-- | Smart constructor of 'NodeTraces'.
--
mkTracers :: forall peer blk.
              ( ProtocolLedgerView blk
              , TraceConstraints blk
              , Show peer
              )
           => TraceOptions
           -> Tracer IO (LogObject Text)
           -> Tracers peer blk
mkTracers traceOptions tracer = Tracers
    { chainDBTracer
        = if traceChainDB traceOptions
          then toLogObject' TextualRepresentation (traceVerbosity traceOptions) tracer
          else toLogObject' StructuredLogging (traceVerbosity traceOptions) tracer
    , consensusTracers
        = mkConsensusTracers
    , protocolTracers
        = mkProtocolsTracers
    , ipSubscriptionTracer  -- TODO
        = enableTracer (traceIpSubscription traceOptions)
        $ withName "IpSubscription" tracer
    , dnsSubscriptionTracer  -- TODO
        = enableTracer (traceDnsSubscription traceOptions)
        $ withName "DnsSubscription" tracer
    , dnsResolverTracer  -- TODO
        = enableTracer (traceDnsResolver traceOptions)
        $ withName "DnsResolver" tracer
    }
  where
    enableTracer
      :: Show a
      => Bool
      -> Tracer IO String
      -> Tracer IO a
    enableTracer False = const nullTracer
    enableTracer True  = showTracing

    mempoolTraceTransformer :: Tracer IO (LogObject a)
                            -> Tracer IO (TraceEventMempool blk)
    mempoolTraceTransformer tr = Tracer $ \mempoolEvent -> do
        let logValue :: LOContent a
            logValue = LogValue "txsInMempool" $ PureI $ fromIntegral $ _txsInMempool mempoolEvent
        meta <- mkLOMeta Info Confidential
        traceNamedObject tr (meta, logValue)
        let txs = case mempoolEvent of
                  TraceMempoolAddTxs      txs0 _ -> txs0
                  TraceMempoolRejectedTxs txs0 _ -> txs0
                  _                              -> []
        let logValue' :: LOContent a
            logValue' = LogValue "txsProcessed" $ PureI $ fromIntegral $ length txs
        traceNamedObject tr (meta, logValue')

    mempoolTracer = Tracer $ \ev -> do
      traceWith (mempoolTraceTransformer tracer) ev
      traceWith (enableConsensusTracer Consensus.mempoolTracer
                $ withName "Mempool" tracer) ev

    enableConsensusTracer
      :: Show a
      => (ConsensusTraceOptions -> Const Bool b)
      -> Tracer IO String -> Tracer IO a
    enableConsensusTracer f = if getConst $ f $ traceConsensus traceOptions
      then showTracing
      else const nullTracer

    mkConsensusTracers :: Consensus.Tracers' peer blk (Tracer IO)
    mkConsensusTracers = Consensus.Tracers
      { Consensus.chainSyncClientTracer
        = enableConsensusTracer Consensus.chainSyncClientTracer
        $ withName "ChainSyncClient" tracer
      , Consensus.chainSyncServerTracer
        = enableConsensusTracer Consensus.chainSyncServerTracer
        $ withName "ChainSyncServer" tracer
      , Consensus.blockFetchDecisionTracer
        = enableConsensusTracer Consensus.blockFetchDecisionTracer
        $ withName "BlockFetchDecision" tracer
      , Consensus.blockFetchClientTracer
        = enableConsensusTracer Consensus.blockFetchClientTracer
        $ withName "BlockFetchClient" tracer
      , Consensus.blockFetchServerTracer
        = enableConsensusTracer Consensus.blockFetchServerTracer
        $ withName "BlockFetchServer" tracer
      , Consensus.txInboundTracer
        = enableConsensusTracer Consensus.txInboundTracer
        $ withName "TxInbound" tracer
      , Consensus.txOutboundTracer
        = enableConsensusTracer Consensus.txOutboundTracer
        $ withName "TxOutbound" tracer
      , Consensus.localTxSubmissionServerTracer
        = enableConsensusTracer Consensus.localTxSubmissionServerTracer
        $ withName "LocalTxSubmissionServer" tracer
      , Consensus.mempoolTracer
        = mempoolTracer
      , Consensus.forgeTracer
        = enableConsensusTracer Consensus.forgeTracer
        $ withName "Forge" tracer
      }

    enableProtocolTracer
      :: Show a
      => (ProtocolTraceOptions -> Const Bool b)
      -> Tracer IO String -> Tracer IO a
    enableProtocolTracer f = if getConst $ f $ traceProtocols traceOptions
      then showTracing
      else const nullTracer

    mkProtocolsTracers :: ProtocolTracers' peer blk DeserialiseFailure (Tracer IO)
    mkProtocolsTracers = ProtocolTracers
      { ptChainSyncTracer
        = enableProtocolTracer ptChainSyncTracer
        $ withName "ChainSyncProtocol" tracer
      , ptBlockFetchTracer
        = enableProtocolTracer ptBlockFetchTracer
        $ withName "BlockFetchProtocol" tracer
      , ptTxSubmissionTracer
        = enableProtocolTracer ptTxSubmissionTracer
        $ withName "TxSubmissionProtocol" tracer
      , ptLocalChainSyncTracer
        = enableProtocolTracer ptLocalChainSyncTracer
        $ withName "LocalChainSyncProtocol" tracer
      , ptLocalTxSubmissionTracer
        = enableProtocolTracer ptLocalTxSubmissionTracer
        $ withName "LocalTxSubmissionProtocol" tracer
      }


--
-- Tracing utils
--

withName :: String
         -> Tracer IO (LogObject Text)
         -> Tracer IO String
withName name tr = contramap pack $ toLogObject $ appendName (pack name) tr


-- | A way to satisfy tracer which requires current tip.  The tip is read from
-- a mutable cell.
--
withTip :: TVar IO (Point blk)
        -> Tracer IO (WithTip blk a)
        -> Tracer IO a
withTip varTip tr = Tracer $ \msg -> do
    tip <- atomically $ readTVar varTip
    traceWith (contramap (WithTip tip) tr) msg
