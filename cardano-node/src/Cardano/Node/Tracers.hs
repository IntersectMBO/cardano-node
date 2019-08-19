-- required for 'Show' instance of 'WithTip'
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

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
import           Prelude (String, show)

import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Monad.Class.MonadSTM
import           Control.Tracer
import           Data.Functor.Const (Const (..))
import           Data.Functor.Contravariant (contramap)
import           Data.Text (Text, pack)

import           Cardano.BM.Data.Aggregated (Measurable (PureI))
import           Cardano.BM.Data.LogItem (LOContent (LogValue), LogObject (..),
                                          PrivacyAnnotation (Confidential),
                                          mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Data.Tracer (ToLogObject (..), TracingVerbosity(..))
import           Cardano.BM.Trace (appendName, traceNamedObject)


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

import           Cardano.Node.ToObjectOrphans


data Tracers peer blk = Tracers {
      -- | Trace the ChainDB. By default we use 'readableChainDB' tracer but a
      -- more verbose one can be enabled.
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
          then contramap show tracer'
          else toLogObject' (traceVerbosity traceOptions) tracer
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
    tracer' :: Tracer IO String
    tracer' = contramap pack $ toLogObject' (traceVerbosity traceOptions) tracer

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
                  _                             -> []
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
