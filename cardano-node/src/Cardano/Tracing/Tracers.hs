{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
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

import           Cardano.Prelude hiding (atomically, show)
import           Prelude (String)

import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Monad.Class.MonadSTM
import           Control.Tracer
import           Data.Functor.Const (Const (..))
import           Data.Functor.Contravariant (contramap)
import           Data.Text (Text, pack)
import qualified Network.Socket as Socket (SockAddr)

import           Cardano.BM.Data.Aggregated (Measurable (PureI))
import           Cardano.BM.Data.LogItem (LOContent (..), LogObject (..),
                                          PrivacyAnnotation (Confidential),
                                          mkLOMeta)
import           Cardano.BM.Tracing
import           Cardano.BM.Trace (traceNamedObject)
import           Cardano.BM.Data.Tracer (addName)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr, GenTx, GenTxId, TraceEventMempool (..))
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers,
                                                  ProtocolTracers' (..))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Ouroboros.Network.Subscription

import qualified Ouroboros.Storage.ChainDB as ChainDB

import           Cardano.Tracing.ToObjectOrphans


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
  { tracingGlobalOff     :: !Bool
  , traceVerbosity       :: !TracingVerbosity
  , traceChainDB         :: !Bool
    -- ^ By default we use 'readableChainDB' tracer, if on this it will use
    -- more verbose tracer
  , traceConsensus       :: ConsensusTraceOptions
  , traceProtocols       :: ProtocolTraceOptions
  , traceIpSubscription  :: !Bool
  , traceDnsSubscription :: !Bool
  , traceDnsResolver     :: !Bool
  }

type ConsensusTraceOptions = Consensus.Tracers' () () () (Const Bool)
type ProtocolTraceOptions  = ProtocolTracers'   () () () (Const Bool)

nullTracers :: Tracers peer blk
nullTracers = Tracers {
      chainDBTracer = nullTracer,
      consensusTracers = nullConsensusTracers,
      protocolTracers = nullProtocolsTracers,
      ipSubscriptionTracer = nullTracer,
      dnsSubscriptionTracer = nullTracer,
      dnsResolverTracer = nullTracer
    }
  where
    nullConsensusTracers :: Consensus.Tracers' peer blk tip (Tracer IO)
    nullConsensusTracers = Consensus.Tracers {
        Consensus.chainSyncClientTracer = nullTracer,
        Consensus.chainSyncServerTracer = nullTracer,
        Consensus.blockFetchDecisionTracer = nullTracer,
        Consensus.blockFetchClientTracer = nullTracer,
        Consensus.blockFetchServerTracer = nullTracer,
        Consensus.txInboundTracer = nullTracer,
        Consensus.txOutboundTracer = nullTracer,
        Consensus.localTxSubmissionServerTracer = nullTracer,
        Consensus.mempoolTracer = nullTracer,
        Consensus.forgeTracer = nullTracer
      }
    nullProtocolsTracers :: ProtocolTracers' peer blk DeserialiseFailure (Tracer IO)
    nullProtocolsTracers = ProtocolTracers {
        ptChainSyncTracer = nullTracer,
        ptBlockFetchTracer = nullTracer,
        ptTxSubmissionTracer = nullTracer,
        ptLocalChainSyncTracer = nullTracer,
        ptLocalTxSubmissionTracer = nullTracer
      }

-- | Smart constructor of 'NodeTraces'.
--
mkTracers :: forall peer blk tip.
              ( ProtocolLedgerView blk
              , TraceConstraints blk
              , Show peer
              , tip ~ Point (Header blk)
              )
           => TraceOptions
           -> Tracer IO (LogObject Text)
           -> Tracers peer blk
mkTracers traceOptions tracer = Tracers
    { chainDBTracer
        = toLogObject' (tracingFormatting $ traceChainDB traceOptions) tracingVerbosity
          $ addName "ChainDB" tracer
    , consensusTracers
        = mkConsensusTracers
    , protocolTracers
        = mkProtocolsTracers
    , ipSubscriptionTracer
        = toLogObject' (tracingFormatting $ traceIpSubscription traceOptions) tracingVerbosity
          $ addName "IpSubscription" tracer
    , dnsSubscriptionTracer
        = toLogObject' (tracingFormatting $ traceDnsSubscription traceOptions) tracingVerbosity
          $ addName "DnsSubscription" tracer
    , dnsResolverTracer
        = toLogObject' (tracingFormatting $ traceDnsResolver traceOptions) tracingVerbosity
          $ addName "DnsResolver" tracer
    }
  where
    tracingFormatting :: Bool -> TracingFormatting
    tracingFormatting True  = TextualRepresentation
    tracingFormatting False = StructuredLogging
    tracingVerbosity :: TracingVerbosity
    tracingVerbosity = traceVerbosity traceOptions

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

    mkConsensusTracers :: Consensus.Tracers' peer blk tip (Tracer IO)
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
withTip :: LazyTVar IO (Point blk)
        -> Tracer IO (WithTip blk a)
        -> Tracer IO a
withTip varTip tr = Tracer $ \msg -> do
    tip <- atomically $ readTVar varTip
    traceWith (contramap (WithTip tip) tr) msg
