{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE StandaloneDeriving    #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Tracing.Tracers
  ( ConsensusTraceOptions
  , ProtocolTraceOptions
  , Tracers (..)
  , TraceConstraints
  , TraceOptions(..)
  , mkTracers
  , nullTracers
  , withTip
  ) where

import           Cardano.Prelude hiding (atomically)
import           Prelude (String)

import           Control.Monad.Class.MonadSTM
import           Control.Tracer

import           Codec.CBOR.Read (DeserialiseFailure)
import           Data.Functor.Const (Const (..))
import           Data.Functor.Contravariant (contramap)
import           Data.Text (Text, pack)
import qualified Network.Socket as Socket (SockAddr)
import           Network.Mux.Types (WithMuxBearer, MuxTrace)

import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.LogItem (LOContent (..), LogObject (..),
                                          PrivacyAnnotation (Confidential),
                                          mkLOMeta)
import           Cardano.BM.Tracing
import           Cardano.BM.Trace (traceNamedObject)
import           Cardano.BM.Data.Tracer (WithSeverity (..), addName,
                     annotateSeverity, filterSeverity)

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API (TraceEventMempool (..))
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers,
                     ProtocolTracers' (..), nullProtocolTracers)
import           Ouroboros.Consensus.Mempool.API (GenTx, GenTxId)
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Point,
                     blockNo, unBlockNo, unSlotNo)
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision)
import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))
import           Ouroboros.Network.NodeToNode (NodeToNodeProtocols,
                     WithAddr, ErrorPolicyTrace)
import           Ouroboros.Network.Point (fromWithOrigin)
import           Ouroboros.Network.Subscription

import qualified Ouroboros.Storage.ChainDB as ChainDB

import           Cardano.Config.Orphanage
import           Cardano.Config.Protocol (TraceConstraints)
import           Cardano.Config.Types
import           Cardano.Tracing.ToObjectOrphans
import           Cardano.Tracing.MicroBenchmarking

data Tracers peer blk = Tracers {
      -- | Trace the ChainDB (flag '--trace-chain-db' will turn on textual output)
      chainDBTracer         :: Tracer IO (WithTip blk (ChainDB.TraceEvent blk))

      -- | Consensus-specific tracers.
    , consensusTracers      :: Consensus.Tracers IO peer blk

      -- | Tracers for the protocol messages.
    , protocolTracers       :: ProtocolTracers IO peer blk DeserialiseFailure

      -- | Trace the IP subscription manager (flag '--trace-ip-subscription' will turn on textual output)
    , ipSubscriptionTracer  :: Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))

      -- | Trace the DNS subscription manager (flag '--trace-dns-subscription' will turn on textual output)
    , dnsSubscriptionTracer :: Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))

      -- | Trace the DNS resolver (flag '--trace-dns-resolver' will turn on textual output)
    , dnsResolverTracer     :: Tracer IO (WithDomainName DnsTrace)

      -- | Trace error policy resolution (flag '--trace-error-policy' will turn on textual output)
    , errorPolicyTracer     :: Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)

      -- | Trace the Mux (flag --trace-mux' will turn on textual output)
    , muxTracer             :: Tracer IO (WithMuxBearer peer (MuxTrace NodeToNodeProtocols))
    }

nullTracers :: Tracers peer blk
nullTracers = Tracers {
      chainDBTracer = nullTracer,
      consensusTracers = Consensus.nullTracers,
      protocolTracers = nullProtocolTracers,
      ipSubscriptionTracer = nullTracer,
      dnsSubscriptionTracer = nullTracer,
      dnsResolverTracer = nullTracer,
      errorPolicyTracer = nullTracer,
      muxTracer = nullTracer
    }

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
        = annotateSeverity $ filterSeverity (pure . const (tracingSeverity $ traceChainDB traceOptions))
          $ teeTraceChainTip (tracingFormatting $ traceChainDB traceOptions) tracingVerbosity
          $ addName "ChainDB" tracer
    , consensusTracers
        = mkConsensusTracers
    , protocolTracers
        = mkProtocolsTracers
    , ipSubscriptionTracer
        = annotateSeverity $ filterSeverity (pure . const (tracingSeverity $ traceIpSubscription traceOptions))
          $ toLogObject' (tracingFormatting $ traceIpSubscription traceOptions) tracingVerbosity
          $ addName "IpSubscription" tracer
    , dnsSubscriptionTracer
        = annotateSeverity $ filterSeverity (pure . const (tracingSeverity $ traceDnsSubscription traceOptions))
          $ toLogObject' (tracingFormatting $ traceDnsSubscription traceOptions) tracingVerbosity
          $ addName "DnsSubscription" tracer
    , dnsResolverTracer
        = annotateSeverity $ filterSeverity (pure . const (tracingSeverity $ traceDnsResolver traceOptions))
          $ toLogObject' (tracingFormatting $ traceDnsResolver traceOptions) tracingVerbosity
          $ addName "DnsResolver" tracer
    , errorPolicyTracer
        = annotateSeverity $ filterSeverity (pure . const (tracingSeverity $ traceErrorPolicy traceOptions))
          $ toLogObject' (tracingFormatting $ traceErrorPolicy traceOptions) tracingVerbosity
          $ addName "ErrorPolicy" tracer
    , muxTracer
        = annotateSeverity $ filterSeverity (pure . const Info)  -- filter out everything below this level
          $ toLogObject' (tracingFormatting $ traceMux traceOptions) tracingVerbosity
          $ addName "Mux" tracer
    }
  where
    tracingFormatting :: Bool -> TracingFormatting
    tracingFormatting True  = TextualRepresentation
    tracingFormatting False = StructuredLogging
    tracingSeverity :: Bool -> Severity
    tracingSeverity True  = Debug    -- if tracer flag is set
    tracingSeverity False = Info     -- min. serverity (default)
    tracingVerbosity :: TracingVerbosity
    tracingVerbosity = traceVerbosity traceOptions

    teeTraceChainTip :: TracingFormatting
                     -> TracingVerbosity
                     -> Tracer IO (LogObject Text)
                     -> Tracer IO (WithSeverity (WithTip blk (ChainDB.TraceEvent blk)))
    teeTraceChainTip tform tverb tr = Tracer $ \ev -> do
        traceWith (teeTraceChainTip' tr) ev
        traceWith (toLogObject' tform tverb tr) ev
    teeTraceChainTip' :: Tracer IO (LogObject Text)
                      -> Tracer IO (WithSeverity (WithTip blk (ChainDB.TraceEvent blk)))
    teeTraceChainTip' tr =
        Tracer $ \(WithSeverity _ (WithTip _tip ev')) ->
          case ev' of
              (ChainDB.TraceAddBlockEvent ev) -> case ev of
                  ChainDB.SwitchedToChain _ c -> do
                      meta <- mkLOMeta Critical Confidential
                      let tr' = appendName "metrics" tr
                          ChainInformation { slots, blocks, density } = chainInformation c
                          epochSlots :: Word64 = 21600  -- TODO
                      traceNamedObject tr' (meta, LogValue "density" . PureD $ fromRational density)
                      traceNamedObject tr' (meta, LogValue "slotNum" . PureI $ fromIntegral slots)
                      traceNamedObject tr' (meta, LogValue "blockNum" . PureI $ fromIntegral blocks)
                      traceNamedObject tr' (meta, LogValue "slotInEpoch" . PureI $ fromIntegral (slots `rem` epochSlots))
                      traceNamedObject tr' (meta, LogValue "epoch" . PureI $ fromIntegral (slots `div` epochSlots))
                  _ -> pure ()
              _ -> pure ()
    teeTraceBlockFetchDecision :: TracingFormatting
        -> TracingVerbosity
        -> Tracer IO (LogObject Text)
        -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
    teeTraceBlockFetchDecision tform tverb tr = Tracer $ \ev -> do
      traceWith (teeTraceBlockFetchDecision' tr) ev
      traceWith (toLogObject' tform tverb tr) ev
    teeTraceBlockFetchDecision' :: Tracer IO (LogObject Text)
                -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
    teeTraceBlockFetchDecision' tr =
        Tracer $ \(WithSeverity _ peers) -> do
          meta <- mkLOMeta Notice Confidential
          let tr' = appendName "peers" tr
          traceNamedObject tr' (meta, LogValue "connectedPeers" . PureI $ fromIntegral $ length peers)

    mempoolTraceTransformer :: Tracer IO (LogObject a)
                            -> Tracer IO (TraceEventMempool blk)
    mempoolTraceTransformer tr = Tracer $ \mempoolEvent -> do
        let tr' = appendName "metrics" tr
            (n, tot) = case mempoolEvent of
                  TraceMempoolAddTxs      txs0 tot0 _ -> (length txs0, tot0)
                  TraceMempoolRejectedTxs txs0 tot0 _ -> (length txs0, tot0)
                  TraceMempoolRemoveTxs   txs0 tot0  _-> (length txs0, tot0)
                  TraceMempoolManuallyRemovedTxs txs0 txs1 tot0
                                                    -> ( length txs0 + length txs1
                                                       , tot0
                                                       )
            logValue1 :: LOContent a
            logValue1 = LogValue "txsInMempool" $ PureI $ fromIntegral tot

            logValue2 :: LOContent a
            logValue2 = LogValue "txsProcessed" $ PureI $ fromIntegral n

        meta <- mkLOMeta Critical Confidential

        traceNamedObject tr (meta, logValue1)
        traceNamedObject tr' (meta, logValue1)

        traceNamedObject tr (meta, logValue2)
        traceNamedObject tr' (meta, logValue2)


    mempoolTracer :: Tracer IO (TraceEventMempool blk)
    mempoolTracer = Tracer $ \ev -> do
      traceWith (mempoolTraceTransformer tracer) ev
      traceWith (measureTxsStart tracer) ev
      traceWith (enableConsensusTracer Consensus.mempoolTracer
                $ withName "Mempool" tracer) ev

    forgeTracer :: Tracer IO (Consensus.TraceForgeEvent blk (GenTx blk))
    forgeTracer = Tracer $ \ev -> do
        traceWith (measureTxsEnd tracer) ev
        traceWith consensusForgeTracer ev
      where
        -- The consensus tracer.
        consensusForgeTracer = annotateSeverity $ filterSeverity (pure . const (tracingSeverity $ hasConsensusTraceFlag Consensus.forgeTracer))
          $ toLogObject' (tracingFormatting $ hasConsensusTraceFlag Consensus.forgeTracer) tracingVerbosity
          $ addName "Forge" tracer


    enableConsensusTracer
      :: Show a
      => (ConsensusTraceOptions -> Const Bool b)
      -> Tracer IO String -> Tracer IO a
    enableConsensusTracer f = if getConst $ f $ traceConsensus traceOptions
      then showTracing
      else const nullTracer
    hasConsensusTraceFlag
      :: (ConsensusTraceOptions -> Const Bool b)
      -> Bool
    hasConsensusTraceFlag f = getConst $ f $ traceConsensus traceOptions

    mkConsensusTracers :: Consensus.Tracers' peer blk (Tracer IO)
    mkConsensusTracers = Consensus.Tracers
      { Consensus.chainSyncClientTracer
        = annotateSeverity $ filterSeverity (pure . const (tracingSeverity $ hasConsensusTraceFlag Consensus.chainSyncClientTracer))
          $ toLogObject' (tracingFormatting $ hasConsensusTraceFlag Consensus.chainSyncClientTracer) tracingVerbosity
          $ addName "ChainSyncClient" tracer
      , Consensus.chainSyncServerHeaderTracer
        = annotateSeverity $ filterSeverity (pure . const (tracingSeverity $ hasConsensusTraceFlag Consensus.chainSyncServerHeaderTracer))
          $ toLogObject' (tracingFormatting $ hasConsensusTraceFlag Consensus.chainSyncServerHeaderTracer) tracingVerbosity
          $ addName "ChainSyncHeaderServer" tracer
      , Consensus.chainSyncServerBlockTracer
        = annotateSeverity $ filterSeverity (pure . const (tracingSeverity $ hasConsensusTraceFlag Consensus.chainSyncServerBlockTracer))
          $ toLogObject' (tracingFormatting $ hasConsensusTraceFlag Consensus.chainSyncServerBlockTracer) tracingVerbosity
          $ addName "ChainSyncBlockServer" tracer
      , Consensus.blockFetchDecisionTracer
        = annotateSeverity $ filterSeverity (pure . const (tracingSeverity $ hasConsensusTraceFlag Consensus.blockFetchDecisionTracer))
          $ teeTraceBlockFetchDecision (tracingFormatting $ hasConsensusTraceFlag Consensus.blockFetchDecisionTracer) tracingVerbosity
          $ addName "BlockFetchDecision" tracer
      , Consensus.blockFetchClientTracer
        = annotateSeverity $ filterSeverity (pure . const (tracingSeverity $ hasConsensusTraceFlag Consensus.blockFetchClientTracer))
          $ toLogObject' (tracingFormatting $ hasConsensusTraceFlag Consensus.blockFetchClientTracer) tracingVerbosity
          $ addName "BlockFetchClient" tracer
      , Consensus.blockFetchServerTracer
        = annotateSeverity $ filterSeverity (pure . const (tracingSeverity $ hasConsensusTraceFlag Consensus.blockFetchServerTracer))
          $ toLogObject' (tracingFormatting $ hasConsensusTraceFlag Consensus.blockFetchServerTracer) tracingVerbosity
          $ addName "BlockFetchServer" tracer
      , Consensus.txInboundTracer
        = annotateSeverity $ filterSeverity (pure . const (tracingSeverity $ hasConsensusTraceFlag Consensus.txInboundTracer))
          $ toLogObject' (tracingFormatting $ hasConsensusTraceFlag Consensus.txInboundTracer) tracingVerbosity
          $ addName "TxInbound" tracer
      , Consensus.txOutboundTracer
        = annotateSeverity $ filterSeverity (pure . const (tracingSeverity $ hasConsensusTraceFlag Consensus.txOutboundTracer))
          $ toLogObject' (tracingFormatting $ hasConsensusTraceFlag Consensus.txOutboundTracer) tracingVerbosity
          $ addName "TxOutbound" tracer
      , Consensus.localTxSubmissionServerTracer
        = annotateSeverity $ filterSeverity (pure . const (tracingSeverity $ hasConsensusTraceFlag Consensus.localTxSubmissionServerTracer))
          $ toLogObject' (tracingFormatting $ hasConsensusTraceFlag Consensus.localTxSubmissionServerTracer) tracingVerbosity
          $ addName "LocalTxSubmissionServer" tracer
      , Consensus.mempoolTracer
        = mempoolTracer
      , Consensus.forgeTracer
        = forgeTracer
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
      , ptBlockFetchSerialisedTracer
        = enableProtocolTracer ptBlockFetchSerialisedTracer
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

--instance Transformable Transformable Text IO (MeasureTxs blk) where
--  trTransformer _ verb tr = trStructured verb tr

-- | get information about a chain fragment

data ChainInformation = ChainInformation {
    slots :: Word64,
    blocks :: Word64,
    density :: Rational
    -- ^ the actual number of blocks created over the maximum
    -- expected number of blocks that could be created
  }

chainInformation :: forall block . AF.HasHeader block
                 => AF.AnchoredFragment block -> ChainInformation
chainInformation frag = ChainInformation
    { slots     = slotN
    , blocks    = blockN
    , density   = calcDensity blockD slotD
    }
  where
    calcDensity :: Word64 -> Word64 -> Rational
    calcDensity bl sl
      | sl > 0 = toRational bl / toRational sl
      | otherwise = 0
    slotN  = unSlotNo $ fromWithOrigin 0 (AF.headSlot frag)
    -- Slot of the tip - slot @k@ blocks back. Use 0 as the slot for genesis
    -- includes EBBs
    slotD   = slotN
            - unSlotNo (fromWithOrigin 0 (AF.lastSlot frag))
    -- Block numbers start at 1. We ignore the genesis EBB, which has block number 0.
    blockD = blockN - firstBlock
    blockN = unBlockNo $ fromMaybe 1 (AF.headBlockNo frag)
    firstBlock = case unBlockNo . blockNo <$> AF.last frag of
      -- Empty fragment, no blocks. We have that @blocks = 1 - 1 = 0@
      Left _ -> 1
      -- The oldest block is the genesis EBB with block number 0,
      -- don't let it contribute to the number of blocks
      Right 0 -> 1
      Right b -> b


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
