-- required for 'Show' instance of 'WithTip'
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Tracers
  ( readableChainDBTracer
  , Traces (..)
  , getNodeTraces
  , toConsensusTracers
  , withTip
  ) where

import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Tracer
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

import           Network.TypedProtocol.Driver (TraceSendRecv)

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch.ClientState (TraceFetchClientState,
                                                           TraceLabelPeer)
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision)
import           Ouroboros.Network.TxSubmission.Inbound (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound)

import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API (GenTx, GenTxId,
                                                  TraceEventMempool (..))
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.Common
import qualified Ouroboros.Storage.LedgerDB.OnDisk as LedgerDB

import           CLI


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
    WithTip tip (ChainDB.TraceLedgerEvent ev) -> case ev of
      ChainDB.InitLog ev' -> traceInitLog tip ev'
      ChainDB.TookSnapshot snap pt -> tr $ WithTip tip $
        "Took ledger snapshot " <> show snap <> " at " <> condense pt
      ChainDB.DeletedSnapshot snap -> tr $ WithTip tip $
        "Deleted old snapshot " <> show snap
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
    tr :: Show a => WithTip blk a -> m ()
    tr = traceWith (contramap show tracer)

    ignore :: m ()
    ignore = return ()

    traceInitLog :: Point blk -> LedgerDB.InitLog (Point blk) -> m ()
    traceInitLog tip = \case
      LedgerDB.InitFromGenesis -> tr $ WithTip tip "Initialised the ledger from genesis"
      LedgerDB.InitFromSnapshot snap tip' -> tr $ WithTip tip $
        "Initialised the ledger from snapshot " <> show snap <> " at " <>
        condense (tipToPoint tip')
      LedgerDB.InitFailure snap _failure initLog -> do
          tr $ WithTip tip $ "Snapshot " <> show snap <> " invalid"
          traceInitLog tip initLog

data Traces peer blk = Traces {
    -- | by default we use 'readableChainDB' tracer, if on this it will use more
    -- verbose tracer
    --
      tracerChainDB
      :: (Tracer IO(WithTip blk (ChainDB.TraceEvent blk)))

    -- | consensus tracer
    --
    -- TODO: it should be fixed with #839 ('ouroboros-network')
    --
    , tracerConsensus
      :: (Tracer IO String)

    -- | mempool tracer
    --
    , tracerMempool
      :: (Tracer IO (TraceEventMempool blk))

    -- | trace fetch decisions; it links to 'decisionTracer' in 'NodeParams'
    --

    , tracerFetchDecisions
      :: (Tracer IO [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])

    -- | trace fetch client; it links to 'fetchClientTracer' in 'NodeParams'
    --
    , tracerFetchClient
      :: (Tracer IO (TraceLabelPeer peer (TraceFetchClientState (Header blk))))

    -- | trace tx-submission server; it link to 'txInboundTracer' in 'NodeParams'
    --
    , tracerTxInbound
      :: (Tracer IO (TraceTxSubmissionInbound  (GenTxId blk) (GenTx blk)))

    -- | trace tx-submission client; it link to 'txOutboundTracer' in 'NodeParams'
    --
    , tracerTxOutbound
      :: (Tracer IO (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)))

    -- | trace chain syn messages
    --
    , tracerChainSync
      :: (Tracer IO (TraceSendRecv (ChainSync (Header blk) (Point blk)) peer DeserialiseFailure))

    -- | trace tx submission messages
    --
    , tracerTxSubmission
      :: (Tracer IO (TraceSendRecv (BlockFetch blk) peer DeserialiseFailure))

    -- | trace ip subscription manager
    --
    -- TODO: export types in `ouroboros-network`
    , traceIpSubscription
      :: (Tracer IO String)

    -- | trace dns subscription manager
    --
    -- TODO: export types in `ouroboros-network`
    , traceDnsSubscription
      :: (Tracer IO String)

    -- | trace dns resolution
    --
    -- TODO: export types in `ouroboros-network`
    , traceDnsResolver
      :: (Tracer IO String)
    }

-- | Smart constructor of 'NodeTraces'.
--
getNodeTraces :: forall peer blk.
              ( ProtocolLedgerView blk
              , Show blk
              , Show (Header blk)
              , Condense (HeaderHash blk)
              , Show peer
              )
           => TraceOptions
           -> Tracer IO (LogObject Text)
           -> Traces peer blk
getNodeTraces traceOptions tracer = Traces
    { tracerChainDB
        = if traceChainDB traceOptions
            then contramap show tracer'
            else readableChainDBTracer tracer'
    , tracerConsensus
        = tracer'
    , tracerMempool
        = mempoolTraceTransformer tracer
    , tracerChainSync
        = enableTracer (traceChainSync traceOptions)
        $ withName "ChainSyncProtocol" tracer
    , tracerTxSubmission
        = enableTracer (traceTxSubmission traceOptions)
        $ withName "TxSubmissionProtocol" tracer
    , tracerFetchDecisions
        = enableTracer (traceFetchDecisions traceOptions)
        $ withName "FetchDecision" tracer
    , tracerFetchClient
        = enableTracer (traceFetchClient traceOptions)
        $ withName "FetchClient" tracer
    , tracerTxInbound
        = enableTracer (traceTxInbound traceOptions)
        $ withName "TxInbound" tracer
    , tracerTxOutbound
        = enableTracer (traceTxOutbound traceOptions)
        $ withName "TxOutbound" tracer
    , traceIpSubscription
        = withName "IPSubscription" tracer
    , traceDnsSubscription
        = withName "DNSSubscription" tracer
    , traceDnsResolver
        = withName "DNSResolver" tracer
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
        let logValue = LogValue "txsInMempool" $ PureI $ fromIntegral $ _txsInMempool mempoolEvent
        meta <- mkLOMeta Info Confidential
        traceNamedObject tr (meta, logValue)
        case mempoolEvent of
          TraceMempoolAddTxs      txs _ ->
              let logValue' = LogValue "txsProcessed" $ PureI $ fromIntegral $ length txs in
              traceNamedObject tr (meta, logValue')
          TraceMempoolRejectedTxs txs _ ->
              let logValue' = LogValue "txsProcessed" $ PureI $ fromIntegral $ length txs in
              traceNamedObject tr (meta, logValue')
          _                             -> return ()


--TODO: there is still a significant mismatch in the sets of tracers here
toConsensusTracers :: Traces peer blk -> Tracers IO peer blk
toConsensusTracers Traces {
                     tracerChainDB        = _ --TODO
                   , tracerConsensus      = _ --TODO
                   , tracerMempool
                   , tracerFetchDecisions
                   , tracerFetchClient
                   , tracerTxInbound
                   , tracerTxOutbound
                   , tracerChainSync      = _ --TODO
                   , tracerTxSubmission   = _ --TODO
                   , traceIpSubscription  = _ --TODO
                   , traceDnsSubscription = _ --TODO
                   , traceDnsResolver     = _ --TODO
                   } =
    Tracers
      { chainSyncClientTracer         = nullTracer   --TODO
      , chainSyncServerTracer         = nullTracer   --TODO
      , blockFetchDecisionTracer      = tracerFetchDecisions
      , blockFetchClientTracer        = tracerFetchClient
      , blockFetchServerTracer        = nullTracer   --TODO
      , txInboundTracer               = tracerTxInbound
      , txOutboundTracer              = tracerTxOutbound
      , localTxSubmissionServerTracer = nullTracer   --TODO
      , mempoolTracer                 = tracerMempool
      , forgeTracer                   = nullTracer   --TODO
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

instance ( Show a
         , Condense (HeaderHash blk)
         ) => Show (WithTip blk a) where

    show (WithTip tip a) = case pointHash tip of
      GenesisHash -> "[genesis] " ++ show a
      BlockHash h -> mconcat [ "["
                             , take 7 (condense h)
                             , "] "
                             , show a
                             ]


-- | A way to satisfy tracer which requires current tip.  The tip is read from
-- a mutable cell.
--
withTip :: TVar IO (Point blk)
        -> Tracer IO (WithTip blk a)
        -> Tracer IO a
withTip varTip tr = Tracer $ \msg -> do
    tip <- atomically $ readTVar varTip
    traceWith (contramap (WithTip tip) tr) msg
