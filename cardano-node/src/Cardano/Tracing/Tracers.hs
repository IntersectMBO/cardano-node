{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Tracing.Tracers
  ( Tracers (..)
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
import           Data.Functor.Contravariant (contramap)
import           Data.Text (Text, pack)
import           Network.Mux (MuxTrace, WithMuxBearer)
import qualified Network.Socket as Socket (SockAddr)

import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.LogItem (LOContent (..), LoggerName,
                                          PrivacyAnnotation (Confidential),
                                          mkLOMeta)
import           Cardano.BM.ElidingTracer
import           Cardano.BM.Tracing
import           Cardano.BM.Trace (traceNamedObject, appendName)
import           Cardano.BM.Data.Tracer (WithSeverity (..), annotateSeverity)
import           Cardano.BM.Data.Transformers

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..), TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Mempool.API (GenTx, MempoolSize (..),
                                                  TraceEventMempool (..))
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers,
                                                  ProtocolTracers' (..),
                                                  nullProtocolTracers)
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Point, BlockNo(..),
                                          blockNo, unBlockNo, unSlotNo)
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision)
import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))
import           Ouroboros.Network.NodeToNode (WithAddr, ErrorPolicyTrace)
import           Ouroboros.Network.Point (fromWithOrigin)
import           Ouroboros.Network.Subscription

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB

import           Cardano.Config.Protocol (TraceConstraints)
import           Cardano.Config.Types
import           Cardano.Tracing.MicroBenchmarking
import           Cardano.Tracing.ToObjectOrphans

import           Control.Tracer.Transformers

data Tracers peer localPeer blk = Tracers
  { -- | Trace the ChainDB
    chainDBTracer :: Tracer IO (WithTip blk (ChainDB.TraceEvent blk))
    -- | Consensus-specific tracers.
  , consensusTracers :: Consensus.Tracers IO peer blk
    -- | Tracers for the protocol messages.
  , protocolTracers :: ProtocolTracers IO peer localPeer blk DeserialiseFailure
    -- | Trace the IP subscription manager
  , ipSubscriptionTracer :: Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -- | Trace the DNS subscription manager
  , dnsSubscriptionTracer :: Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))
    -- | Trace the DNS resolver
  , dnsResolverTracer :: Tracer IO (WithDomainName DnsTrace)
    -- | Trace error policy resolution
  , errorPolicyTracer :: Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -- | Trace local error policy resolution
  , localErrorPolicyTracer :: Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -- | Trace the Mux
  , muxTracer :: Tracer IO (WithMuxBearer peer MuxTrace)
  }

data ForgeTracers = ForgeTracers
  { ftForged :: Trace IO Text
  , ftForgeAboutToLead :: Trace IO Text
  , ftCouldNotForge :: Trace IO Text
  , ftAdopted :: Trace IO Text
  , ftDidntAdoptBlock :: Trace IO Text
  , ftForgedInvalid   :: Trace IO Text
  , ftTraceNodeNotLeader  :: Trace IO Text
  , ftTraceBlockFromFuture :: Trace IO Text
  , ftTraceSlotIsImmutable :: Trace IO Text
  , ftTraceNodeIsLeader :: Trace IO Text
  }

nullTracers :: Tracers peer localPeer blk
nullTracers = Tracers
  { chainDBTracer = nullTracer
  , consensusTracers = Consensus.nullTracers
  , protocolTracers = nullProtocolTracers
  , ipSubscriptionTracer = nullTracer
  , dnsSubscriptionTracer = nullTracer
  , dnsResolverTracer = nullTracer
  , errorPolicyTracer = nullTracer
  , localErrorPolicyTracer = nullTracer
  , muxTracer = nullTracer
  }


indexGCType :: ChainDB.TraceGCEvent a -> Int
indexGCType ChainDB.ScheduledGC{} = 1
indexGCType ChainDB.PerformedGC{} = 2

indexReplType :: ChainDB.TraceLedgerReplayEvent a -> Int
indexReplType LedgerDB.ReplayFromGenesis{} = 1
indexReplType LedgerDB.ReplayFromSnapshot{} = 2
indexReplType LedgerDB.ReplayedBlock{} = 3

instance ElidingTracer
  (WithSeverity (WithTip blk (ChainDB.TraceEvent blk))) where
  -- equivalent by type and severity
  isEquivalent (WithSeverity s1 (WithTip _tip1 (ChainDB.TraceLedgerReplayEvent ev1)))
                (WithSeverity s2 (WithTip _tip2 (ChainDB.TraceLedgerReplayEvent ev2))) = s1 == s2 &&
                  indexReplType ev1 == indexReplType ev2
  isEquivalent (WithSeverity s1 (WithTip _tip1 (ChainDB.TraceGCEvent ev1)))
                (WithSeverity s2 (WithTip _tip2 (ChainDB.TraceGCEvent ev2))) = s1 == s2 &&
                  indexGCType ev1 == indexGCType ev2
  isEquivalent _ _ = False
  -- the types to be elided
  doelide (WithSeverity _ (WithTip _ (ChainDB.TraceLedgerReplayEvent _))) = True
  doelide (WithSeverity _ (WithTip _ (ChainDB.TraceGCEvent _))) = True
  doelide _ = False
  conteliding _tform _tverb _tr _ (Nothing, _count) = return (Nothing, 0)
  conteliding _tform _tverb tr ev (_old, count) = do
      when (count > 0 && count `mod` 100 == 0) $ do  -- report every 100th elided message
          meta <- mkLOMeta (defineSeverity ev) (definePrivacyAnnotation ev)
          traceNamedObject tr (meta, LogValue "messages elided so far" (PureI $ toInteger count))
      return (Just ev, count + 1)

-- | Smart constructor of 'NodeTraces'.
--
mkTracers
  :: forall peer localPeer blk.
     ( LedgerSupportsProtocol blk
     , TraceConstraints blk
     , ShowQuery (Query blk)
     , Show peer
     , Show localPeer
     )
  => TraceOptions
  -> Trace IO Text
  -> IO (Tracers peer localPeer blk)
mkTracers traceOptions tracer = do
  -- We probably don't want to pay the extra IO cost per-counter-increment. -- sk
  staticMetaCC <- mkLOMeta Critical Confidential
  let name :: LoggerName = "metrics.Forge"
  forgeTracers <-
    ForgeTracers
      <$> (counting $ liftCounting staticMetaCC name "forged" tracer)
      <*> (counting $ liftCounting staticMetaCC name "forge-about-to-lead" tracer)
      <*> (counting $ liftCounting staticMetaCC name "could-not-forge" tracer)
      <*> (counting $ liftCounting staticMetaCC name "adopted" tracer)
      <*> (counting $ liftCounting staticMetaCC name "didnt-adopt" tracer)
      <*> (counting $ liftCounting staticMetaCC name "forged-invalid" tracer)
      <*> (counting $ liftCounting staticMetaCC name "node-not-leader" tracer)
      <*> (counting $ liftCounting staticMetaCC name "block-from-future" tracer)
      <*> (counting $ liftCounting staticMetaCC name "slot-is-immutable" tracer)
      <*> (counting $ liftCounting staticMetaCC name "node-is-leader" tracer)

  -- The outcomes we want to measure, the outcome extractor
  -- for measuring the time it takes a transaction to get into
  -- a block.
  --txsOutcomeExtractor <- mkOutcomeExtractor @_ @(MeasureTxs blk)
  blockForgeOutcomeExtractor <- mkOutcomeExtractor -- @_ @(MeasureBlockForging blk)

  elided <- newstate  -- for eliding messages in ChainDB tracer

  pure Tracers
    { chainDBTracer
        = tracerOnOff (traceChainDB traceOptions)
          $ annotateSeverity
          $ teeTraceChainTip tracingVerbosity elided
          $ appendName "ChainDB"
          $ tracer
    , consensusTracers
        = mkConsensusTracers blockForgeOutcomeExtractor forgeTracers traceOptions
    , protocolTracers
        = mkProtocolTracers traceOptions
    , ipSubscriptionTracer
        = tracerOnOff (traceIpSubscription traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ appendName "IpSubscription" tracer
    , dnsSubscriptionTracer
        = tracerOnOff (traceDnsSubscription traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ appendName "DnsSubscription" tracer
    , dnsResolverTracer
        = tracerOnOff (traceDnsResolver traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ appendName "DnsResolver" tracer
    , errorPolicyTracer
        = tracerOnOff (traceErrorPolicy traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ appendName "ErrorPolicy" tracer
    , localErrorPolicyTracer
        = tracerOnOff (traceLocalErrorPolicy traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ appendName "LocalErrorPolicy" tracer
    , muxTracer
        = tracerOnOff (traceMux traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ appendName "Mux" tracer
    }
  where
    -- Turn on/off a tracer depending on what was parsed from the command line.
    tracerOnOff :: Bool -> Tracer IO a -> Tracer IO a
    tracerOnOff False _      = nullTracer
    tracerOnOff True tracer' = tracer'
    tracingVerbosity :: TracingVerbosity
    tracingVerbosity = traceVerbosity traceOptions

    teeTraceChainTip :: TracingVerbosity
                     -> MVar (Maybe (WithSeverity (WithTip blk (ChainDB.TraceEvent blk))), Int)
                     -> Trace IO Text
                     -> Tracer IO (WithSeverity (WithTip blk (ChainDB.TraceEvent blk)))
    teeTraceChainTip tverb elided tr = Tracer $ \ev -> do
        traceWith (teeTraceChainTip' tr) ev
        traceWith (teeTraceChainTipElide StructuredLogging tverb elided tr) ev
        traceWith (teeTraceChainTipElide TextualRepresentation tverb elided (appendName "text" tr)) ev
    teeTraceChainTipElide :: TracingFormatting
                          -> TracingVerbosity
                          -> MVar (Maybe (WithSeverity (WithTip blk (ChainDB.TraceEvent blk))), Int)
                          -> Trace IO Text
                          -> Tracer IO (WithSeverity (WithTip blk (ChainDB.TraceEvent blk)))
    teeTraceChainTipElide = elideToLogObject

    traceChainInformation :: Trace IO Text
                          -> ChainInformation
                          -> IO ()
    traceChainInformation tr ChainInformation { slots, blocks, density } = do
        -- TODO this is executed each time the chain changes. How cheap is it?
        meta <- mkLOMeta Critical Confidential
        let tr' = appendName "metrics" tr
            traceD :: Text -> Double -> IO ()
            traceD msg d = traceNamedObject tr' (meta, LogValue msg (PureD d))
            traceI :: Integral a => Text -> a -> IO ()
            traceI msg i = traceNamedObject tr' (meta, LogValue msg (PureI (fromIntegral i)))

        traceD "density"     (fromRational density)
        traceI "slotNum"     slots
        traceI "blockNum"    blocks
        traceI "slotInEpoch" (slots `rem` epochSlots)
        traceI "epoch"       (slots `div` epochSlots)
      where
        epochSlots :: Word64 = 21600  -- TODO

    teeTraceChainTip' :: Trace IO Text
                      -> Tracer IO (WithSeverity (WithTip blk (ChainDB.TraceEvent blk)))
    teeTraceChainTip' tr =
        Tracer $ \(WithSeverity _ (WithTip _tip ev')) ->
          case ev' of
              (ChainDB.TraceAddBlockEvent ev) -> case ev of
                  ChainDB.SwitchedToAFork     _ _ c -> traceChainInformation tr (chainInformation c)
                  ChainDB.AddedToCurrentChain _ _ c -> traceChainInformation tr (chainInformation c)
                  _ -> pure ()
              _ -> pure ()
    teeTraceBlockFetchDecision :: TracingFormatting
        -> TracingVerbosity
        -> Trace IO Text
        -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
    teeTraceBlockFetchDecision tform tverb tr = Tracer $ \ev -> do
      traceWith (teeTraceBlockFetchDecision' tr) ev
      traceWith (toLogObject' tform tverb tr) ev
    teeTraceBlockFetchDecision' :: Trace IO Text
                -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
    teeTraceBlockFetchDecision' tr =
        Tracer $ \(WithSeverity _ peers) -> do
          meta <- mkLOMeta Notice Confidential
          let tr' = appendName "peers" tr
          traceNamedObject tr' (meta, LogValue "connectedPeers" . PureI $ fromIntegral $ length peers)

    mempoolMetricsTraceTransformer :: Trace IO a
                                   -> Tracer IO (TraceEventMempool blk)
    mempoolMetricsTraceTransformer tr = Tracer $ \mempoolEvent -> do
        let tr' = appendName "metrics" tr
            (n, tot) = case mempoolEvent of
                  TraceMempoolAddedTx     _tx0 _ tot0 -> (1, tot0)
                  TraceMempoolRejectedTx  _tx0 _ tot0 -> (1, tot0)
                  TraceMempoolRemoveTxs   txs0   tot0 -> (length txs0, tot0)
                  TraceMempoolManuallyRemovedTxs txs0 txs1 tot0
                                                    -> ( length txs0 + length txs1
                                                       , tot0
                                                       )
            logValue1 :: LOContent a
            logValue1 = LogValue "txsInMempool" $ PureI $ fromIntegral (msNumTxs tot)

            logValue2 :: LOContent a
            logValue2 = LogValue "txsProcessed" $ PureI $ fromIntegral n

            logValue3 :: LOContent a
            logValue3 = LogValue "mempoolBytes" $ PureI $ fromIntegral (msNumBytes tot)

        meta <- mkLOMeta Critical Confidential

        traceNamedObject tr (meta, logValue1)
        traceNamedObject tr' (meta, logValue1)

        traceNamedObject tr (meta, logValue2)
        traceNamedObject tr' (meta, logValue2)

        traceNamedObject tr (meta, logValue3)
        traceNamedObject tr' (meta, logValue3)

    mempoolTracer :: Tracer IO (TraceEventMempool blk)
    mempoolTracer = Tracer $ \ev -> do
        traceWith (mempoolMetricsTraceTransformer tracer) ev
        traceWith (measureTxsStart tracer) ev
        let tr = appendName "Mempool" tracer
        traceWith (mpTracer StructuredLogging tr) ev
        traceWith (mpTracer TextualRepresentation (appendName "text" tr)) ev
      where
        mpTracer :: TracingFormatting -> Trace IO Text -> Tracer IO (TraceEventMempool blk)
        mpTracer fmt tr = annotateSeverity $ toLogObject' fmt tracingVerbosity tr

    forgeTracer
        :: ForgeTracers
        -> TraceOptions
        -> Tracer IO (Consensus.TraceForgeEvent blk (GenTx blk))
    forgeTracer forgeTracers traceOpts = Tracer $ \ev -> do
        traceWith (measureTxsEnd tracer) ev
        -- traceWith (measureBlockForgeStart tracer) ev
        -- traceWith (measureBlockForgeEnd tracer) ev
        traceWith (consensusForgeTracer) ev
      where
        -- The consensus tracer.
        consensusForgeTracer = tracerOnOff (traceForge traceOpts)
          $ annotateSeverity
          $ teeForge forgeTracers tracingVerbosity
          $ appendName "Forge" tracer

    teeForge
      :: ForgeTracers
      -> TracingVerbosity
      -> Trace IO Text
      -> Tracer IO (WithSeverity (Consensus.TraceForgeEvent blk (GenTx blk)))
    teeForge ft tverb tr = Tracer $ \ev -> do
      traceWith (teeForge' tr) ev
      flip traceWith ev $ fanning $ \(WithSeverity _ e) ->
        case e of
          Consensus.TraceForgedBlock{} -> teeForge' (ftForged ft)
          Consensus.TraceStartLeadershipCheck{} -> teeForge' (ftForgeAboutToLead ft)
          Consensus.TraceNoLedgerState{} -> teeForge' (ftCouldNotForge ft)
          Consensus.TraceNoLedgerView{} -> teeForge' (ftCouldNotForge ft)
          Consensus.TraceAdoptedBlock{} -> teeForge' (ftAdopted ft)
          Consensus.TraceDidntAdoptBlock{} -> teeForge' (ftDidntAdoptBlock ft)
          Consensus.TraceForgedInvalidBlock{} -> teeForge' (ftForgedInvalid ft)
          Consensus.TraceNodeNotLeader{} -> teeForge' (ftTraceNodeNotLeader ft)
          Consensus.TraceBlockFromFuture{} -> teeForge' (ftTraceBlockFromFuture ft)
          Consensus.TraceSlotIsImmutable{} -> teeForge' (ftTraceSlotIsImmutable ft)
          Consensus.TraceNodeIsLeader{} -> teeForge' (ftTraceNodeIsLeader ft)

      traceWith (toLogObject' StructuredLogging tverb tr) ev
      traceWith (toLogObject' TextualRepresentation tverb (appendName "text" tr)) ev

    teeForge'
      :: Trace IO Text
      -> Tracer IO (WithSeverity (Consensus.TraceForgeEvent blk (GenTx blk)))
    teeForge' tr =
      Tracer $ \(WithSeverity _ ev) -> do
        meta <- mkLOMeta Critical Confidential
        traceNamedObject (appendName "metrics" tr) . (meta,) $
          case ev of
            Consensus.TraceForgedBlock slot _ _ _ ->
              LogValue "forgedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceStartLeadershipCheck slot ->
              LogValue "aboutToLeadSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceNoLedgerState slot _ ->
              LogValue "couldNotForgeSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceNoLedgerView slot _ ->
              LogValue "couldNotForgeSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceAdoptedBlock slot _ _ ->
              LogValue "adoptedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceDidntAdoptBlock slot _ ->
              LogValue "notAdoptedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceForgedInvalidBlock slot _ _ ->
              LogValue "forgedInvalidSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceNodeNotLeader slot ->
              LogValue "nodeNotLeader" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceBlockFromFuture slot _slotNo ->
              LogValue "blockFromFuture" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceSlotIsImmutable slot _tipPoint _tipBlkNo ->
              LogValue "slotIsImmutable" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceNodeIsLeader slot ->
              LogValue "nodeIsLeader" $ PureI $ fromIntegral $ unSlotNo slot

    mkConsensusTracers
        :: (OutcomeEnhancedTracer IO (Consensus.TraceForgeEvent blk (GenTx blk)) -> Tracer IO (Consensus.TraceForgeEvent blk (GenTx blk)))
        -> ForgeTracers -> TraceOptions -> Consensus.Tracers' peer blk (Tracer IO)
    mkConsensusTracers measureBlockForging forgeTracers traceOpts = Consensus.Tracers
      { Consensus.chainSyncClientTracer
        = tracerOnOff (traceChainSyncClient traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ appendName "ChainSyncClient" tracer
      , Consensus.chainSyncServerHeaderTracer
        =  tracerOnOff (traceChainSyncHeaderServer traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ appendName "ChainSyncHeaderServer" tracer
      , Consensus.chainSyncServerBlockTracer
        = tracerOnOff (traceChainSyncBlockServer traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ appendName "ChainSyncBlockServer" tracer
      , Consensus.blockFetchDecisionTracer
        = tracerOnOff (traceBlockFetchDecisions traceOpts)
          $ annotateSeverity
          $ teeTraceBlockFetchDecision StructuredLogging tracingVerbosity
          $ appendName "BlockFetchDecision" tracer
      , Consensus.blockFetchClientTracer
        = tracerOnOff (traceBlockFetchClient traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ appendName "BlockFetchClient" tracer
      , Consensus.blockFetchServerTracer
        = tracerOnOff (traceBlockFetchServer traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ appendName "BlockFetchServer" tracer
      , Consensus.txInboundTracer
        = tracerOnOff (traceTxInbound traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ appendName "TxInbound" tracer
      , Consensus.txOutboundTracer
        = tracerOnOff (traceTxOutbound traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ appendName "TxOutbound" tracer
      , Consensus.localTxSubmissionServerTracer
        = tracerOnOff (traceLocalTxSubmissionServer traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ appendName "LocalTxSubmissionServer" tracer
      , Consensus.mempoolTracer
        = tracerOnOff (traceMempool traceOpts) $ mempoolTracer
      , Consensus.forgeTracer
        = Tracer $ \ev -> do
            traceWith (forgeTracer forgeTracers traceOpts) ev
            traceWith ( measureBlockForging
                      $ toLogObject' StructuredLogging tracingVerbosity
                      $ appendName "ForgeTime" tracer) ev
      , Consensus.blockchainTimeTracer
        = Tracer $ \ev ->
            traceWith (toLogObject tracer) (readableTraceBlockchainTimeEvent ev)
      }

    readableTraceBlockchainTimeEvent :: TraceBlockchainTimeEvent -> Text
    readableTraceBlockchainTimeEvent ev = case ev of
        TraceStartTimeInTheFuture (SystemStart start) toWait ->
          "Waiting " <> show toWait <> " until genesis start time at " <> show start

    mkProtocolTracers :: TraceOptions -> ProtocolTracers' peer localPeer blk DeserialiseFailure (Tracer IO)
    mkProtocolTracers traceOpts = ProtocolTracers
      { ptChainSyncTracer
        = tracerOnOff (traceChainSyncProtocol traceOpts)
        $ showTracing $ withName "ChainSyncProtocol" tracer
      , ptChainSyncSerialisedTracer
        = tracerOnOff (traceChainSyncProtocol traceOpts)
        $ showTracing $ withName "ChainSyncProtocol" tracer
      , ptBlockFetchTracer
        = tracerOnOff (traceBlockFetchProtocol traceOpts)
        $ showTracing $ withName "BlockFetchProtocol" tracer
      , ptBlockFetchSerialisedTracer
        = tracerOnOff (traceBlockFetchProtocolSerialised traceOpts)
        $ showTracing $ withName "BlockFetchProtocol" tracer
      , ptTxSubmissionTracer
        = tracerOnOff (traceTxSubmissionProtocol traceOpts)
        $ toLogObject' StructuredLogging tracingVerbosity
        $ appendName "TxSubmissionProtocol" tracer
      , ptLocalChainSyncTracer
        = tracerOnOff (traceLocalChainSyncProtocol traceOpts)
        $ showTracing $ withName "LocalChainSyncProtocol" tracer
      , ptLocalTxSubmissionTracer
        = tracerOnOff (traceLocalTxSubmissionProtocol traceOpts)
        $ showTracing $ withName "LocalTxSubmissionProtocol" tracer
      , ptLocalStateQueryTracer
        = tracerOnOff (traceLocalStateQueryProtocol traceOpts)
        $ showTracing $ withName "LocalStateQueryProtocol" tracer
      }

-- | get information about a chain fragment

data ChainInformation = ChainInformation
  { slots :: Word64
  , blocks :: Word64
  , density :: Rational
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
    blockN = unBlockNo $ fromWithOrigin (BlockNo 1) (AF.headBlockNo frag)
    firstBlock = case unBlockNo . blockNo <$> AF.last frag of
      -- Empty fragment, no blocks. We have that @blocks = 1 - 1 = 0@
      Left _  -> 1
      -- The oldest block is the genesis EBB with block number 0,
      -- don't let it contribute to the number of blocks
      Right 0 -> 1
      Right b -> b


--
-- Tracing utils
--

withName :: String -> Trace IO Text -> Tracer IO String
withName name tr = contramap pack $ toLogObject $ appendName (pack name) tr


-- | A way to satisfy tracer which requires current tip.  The tip is read from
-- a mutable cell.
--
withTip :: TVar IO (Point blk) -> Tracer IO (WithTip blk a) -> Tracer IO a
withTip varTip tr = Tracer $ \msg -> do
    tip <- atomically $ readTVar varTip
    traceWith (contramap (WithTip tip) tr) msg
