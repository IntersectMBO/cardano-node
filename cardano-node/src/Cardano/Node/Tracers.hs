-- required for 'Show' instance of 'WithTip'
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Cardano.Node.Tracers
  ( Tracers (..)
  , mkTracers
  , withTip
  ) where
import           Cardano.Prelude hiding (atomically, show)
import           Prelude (String, id, show)

import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Tracer
import           Data.Aeson (ToJSON (..), Value (..), (.=))
import           Data.Functor.Const (Const (..))
import           Data.Functor.Contravariant (contramap)
import           Data.Semigroup ((<>))
import           Data.Text (Text, pack)

import           Control.Monad.Class.MonadSTM

import           Cardano.BM.Data.Aggregated (Measurable (PureI))
import           Cardano.BM.Data.LogItem (LOContent (LogValue), LogObject (..),
                                          PrivacyAnnotation (Confidential),
                                          mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Data.Tracer (ToLogObject (..), ToObject (..),
                                         TracingVerbosity (..),
                                         Transformable (..),
                                         emptyObject, mkObject, trStructured)
import           Cardano.BM.Trace (appendName, traceNamedObject)


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
import           Ouroboros.Storage.Common
import qualified Ouroboros.Storage.LedgerDB.OnDisk as LedgerDB


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
instance (Condense (HeaderHash blk), ProtocolLedgerView blk)
            => Transformable Text IO (WithTip blk (ChainDB.TraceEvent blk)) where
    trTransformer = trStructured -- structure required, will call 'toObject'
instance (Condense (HeaderHash blk), ProtocolLedgerView blk)
            => ToObject (WithTip blk (ChainDB.TraceEvent blk)) where
    toObject MinimalVerbosity _ = emptyObject -- no output
    toObject verb (WithTip tip ev) =
        mkObject [ "tip" .= showTip tip
                 , "TraceEvent" .= toObject verb ev
                 ]
instance (Condense (HeaderHash blk), ProtocolLedgerView blk)
            => ToObject (ChainDB.TraceEvent blk) where
    toObject MinimalVerbosity _ = emptyObject -- no output
    toObject _ (ChainDB.TraceAddBlockEvent ev) =
        mkObject [ "kind" .= String "TraceAddBlockEvent"
                 , "event" .= String (
                                pack (
                                  case ev of
                                    ChainDB.StoreButDontChange   pt ->
                                      "Ignoring block: " <> condense pt
                                    ChainDB.TryAddToCurrentChain pt ->
                                      "Block fits onto the current chain: " <> condense pt
                                    ChainDB.TrySwitchToAFork pt _   ->
                                      "Block fits onto some fork: " <> condense pt
                                    ChainDB.SwitchedToChain _ c     ->
                                      "Chain changed, new tip: " <> condense (AF.headPoint c)
                                    ChainDB.AddBlockValidation ev'  -> case ev' of
                                      ChainDB.InvalidBlock err pt
                                        -> "Invalid block " <> condense pt <> ": " <> show err
                                      _ -> "ignore"
                                    _ -> "ignore"
                                )
                              )
                 ]
    toObject _ (ChainDB.TraceLedgerReplayEvent ev) =
        mkObject [ "kind" .= String "TraceLedgerReplayEvent"
                 , "event" .= String (
                                pack (
                                  case ev of
                                    LedgerDB.ReplayFromGenesis _replayTo ->
                                      "Replaying ledger from genesis"
                                    LedgerDB.ReplayFromSnapshot snap tip' _replayTo ->
                                      "Replaying ledger from snapshot " <> show snap <> " at " <>
                                      condense tip'
                                    LedgerDB.ReplayedBlock {} -> "ignore"
                                )
                              )
                 ]
    toObject _ (ChainDB.TraceLedgerEvent ev) =
        mkObject [ "kind" .= String "TraceLedgerEvent"
                 , "event" .= String (
                                pack (
                                  case ev of
                                    LedgerDB.TookSnapshot snap pt ->
                                      "Took ledger snapshot " <> show snap <> " at " <> condense pt
                                    LedgerDB.DeletedSnapshot snap ->
                                      "Deleted old snapshot " <> show snap
                                    LedgerDB.InvalidSnapshot snap failure ->
                                      "Invalid snapshot " <> show snap <> show failure
                                )
                              )
                 ]
    toObject _ (ChainDB.TraceCopyToImmDBEvent ev) =
        mkObject [ "kind" .= String "TraceCopyToImmDBEvent"
                 , "event" .= String (
                                pack (
                                  case ev of
                                    ChainDB.CopiedBlockToImmDB pt ->
                                      "Copied block " <> condense pt <> " to the ImmutableDB"
                                    _ -> "ignored"
                                )
                              )
                 ]
    toObject _ (ChainDB.TraceGCEvent ev) =
        mkObject [ "kind" .= String "TraceGCEvent"
                 , "event" .= String (
                                pack (
                                  case ev of
                                    ChainDB.PerformedGC slot       ->
                                      "Performed a garbage collection for " <> condense slot
                                    _ -> "ignored"
                                )
                              )
                 ]
    toObject _ (ChainDB.TraceOpenEvent ev) =
        mkObject [ "kind" .= String "TraceOpenEvent"
                 , "event" .= String (
                                pack (
                                  case ev of
                                    ChainDB.OpenedDB immTip tip' ->
                                      "Opened with immutable tip at " <> condense immTip <>
                                      " and tip " <> condense tip'
                                    _ -> "ignored"
                                )
                              )
                 ]
    toObject _verb _ = emptyObject -- no output

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
          else toLogObject' (CLI.traceVerbosity traceOptions) tracer
    , consensusTracers
        = mkConsensusTracers
    , protocolTracers
        = mkProtocolsTracers
    , ipSubscriptionTracer  -- TODO
        = enableTracer (CLI.traceIpSubscription traceOptions)
        $ withName "IpSubscription" tracer
    , dnsSubscriptionTracer  -- TODO
        = enableTracer (CLI.traceDnsSubscription traceOptions)
        $ withName "DnsSubscription" tracer
    , dnsResolverTracer  -- TODO
        = enableTracer (CLI.traceDnsResolver traceOptions)
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
                  TraceMempoolAddTxs      txs _ -> txs
                  TraceMempoolRejectedTxs txs _ -> txs
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

-- | Tracing wrapper which includes current tip in the logs (thus it requires
-- it from the context).
--
-- TODO: this should be moved to `ouroboros-consensus`.  Running in a seprate
-- STM transaction we risk reporting  wrong tip.
--
data WithTip blk a =
    WithTip
      (Point blk)
      -- ^ current tip point
      a
      -- ^ data

showWithTip :: Condense (HeaderHash blk)
            => (a -> String)
            -> WithTip blk a
            -> String
showWithTip customShow (WithTip tip a) = "[" ++ (showTip tip) ++ "] " ++ customShow a

showTip :: Condense (HeaderHash blk)
        => Point blk
        -> String
showTip tip = case pointHash tip of
    GenesisHash -> "genesis"
    BlockHash h -> take 7 (condense h)

instance ( Show a
         , Condense (HeaderHash blk)
         ) => Show (WithTip blk a) where

    show = showWithTip show

-- | A way to satisfy tracer which requires current tip.  The tip is read from
-- a mutable cell.
--
withTip :: TVar IO (Point blk)
        -> Tracer IO (WithTip blk a)
        -> Tracer IO a
withTip varTip tr = Tracer $ \msg -> do
    tip <- atomically $ readTVar varTip
    traceWith (contramap (WithTip tip) tr) msg
