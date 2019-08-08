-- required for 'Show' instance of 'WithTip'
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Tracers
  ( readableChainDBTracer
  , Tracers (..)
  , mkTracers
  , withTip
  ) where

import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Tracer
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
import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (appendName, traceNamedObject)


import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API (TraceEventMempool (..))
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers,
                                                  ProtocolTracers' (..))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.Common
import qualified Ouroboros.Storage.LedgerDB.OnDisk as LedgerDB

import           Cardano.Node.CLI (TraceConstraints)

import qualified CLI


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
        _ -> ignore
      _  -> ignore
    WithTip tip (ChainDB.TraceLedgerReplayEvent ev) -> case ev of
      LedgerDB.ReplayFromGenesis _replayTo -> tr $ WithTip tip $
        "Replaying ledger from genesis"
      LedgerDB.ReplayFromSnapshot snap tip' _replayTo -> tr $ WithTip tip $
        "Replaying ledger from snapshot " <> show snap <> " at " <>
        condense tip'
      LedgerDB.ReplayedBlock {} -> ignore
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
      _ -> ignore
    WithTip tip (ChainDB.TraceGCEvent ev) -> case ev of
      ChainDB.PerformedGC slot       -> tr $ WithTip tip $
        "Performed a garbage collection for " <> condense slot
      _ -> ignore
    WithTip tip (ChainDB.TraceOpenEvent ev) -> case ev of
      ChainDB.OpenedDB immTip tip' -> tr $ WithTip tip $
        "Opened with immutable tip at " <> condense immTip <>
        " and tip " <> condense tip'
      _ -> ignore
    _ -> ignore
  where
    tr :: WithTip blk String -> m ()
    tr = traceWith (contramap (showWithTip id) tracer)

    ignore :: m ()
    ignore = return ()

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

-- | Smart constructor of 'NodeTraces'.
--
mkTracers :: forall peer blk.
              ( ProtocolLedgerView blk
              , TraceConstraints blk
              , Show peer
              )
           => CLI.TraceOptions
           -> Tracer IO (LogObject Text)
           -> Tracers peer blk
mkTracers traceOptions tracer = Tracers
    { chainDBTracer
        = if CLI.traceChainDB traceOptions
          then contramap show tracer'
          else readableChainDBTracer tracer'
    , consensusTracers
        = mkConsensusTracers
    , protocolTracers
        = mkProtocolsTracers
    , ipSubscriptionTracer
        = enableTracer (CLI.traceIpSubscription traceOptions)
        $ withName "IpSubscription" tracer
    , dnsSubscriptionTracer
        = enableTracer (CLI.traceDnsSubscription traceOptions)
        $ withName "DnsSubscription" tracer
    , dnsResolverTracer
        = enableTracer (CLI.traceDnsResolver traceOptions)
        $ withName "DnsResolver" tracer
    }
  where
    tracer' :: Tracer IO String
    tracer' = contramap pack $ toLogObject tracer

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
        case mempoolEvent of
          TraceMempoolAddTxs      txs _ ->
              let logValue' :: LOContent a
                  logValue' = LogValue "txsProcessed" $ PureI $ fromIntegral $ length txs in
              traceNamedObject tr (meta, logValue')
          TraceMempoolRejectedTxs txs _ ->
              let logValue' :: LOContent a
                  logValue' = LogValue "txsProcessed" $ PureI $ fromIntegral $ length txs in
              traceNamedObject tr (meta, logValue')
          _                             -> return ()

    mempoolTracer = Tracer $ \ev -> do
      traceWith (mempoolTraceTransformer tracer) ev
      traceWith (enableConsensusTracer Consensus.mempoolTracer
                $ withName "Mempool" tracer) ev

    enableConsensusTracer
      :: Show a
      => (CLI.ConsensusTraceOptions -> Const Bool b)
      -> Tracer IO String -> Tracer IO a
    enableConsensusTracer f = if getConst $ f $ CLI.traceConsensus traceOptions
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
      => (CLI.ProtocolTraceOptions -> Const Bool b)
      -> Tracer IO String -> Tracer IO a
    enableProtocolTracer f = if getConst $ f $ CLI.traceProtocols traceOptions
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
showWithTip customShow (WithTip tip a) = case pointHash tip of
    GenesisHash -> "[genesis] " ++ customShow a
    BlockHash h -> mconcat
      [ "["
      , take 7 (condense h)
      , "] "
      , customShow a
      ]

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
