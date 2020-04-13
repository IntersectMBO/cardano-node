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
  ) where

import           Cardano.Prelude hiding (atomically)
import           Prelude (String)

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

import           Ouroboros.Consensus.Block (Header, realPointSlot)
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..), TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Mempool.API
                   (GenTx, MempoolSize (..), TraceEventMempool (..))
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers,
                                                  ProtocolTracers' (..),
                                                  nullProtocolTracers)
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Point, BlockNo(..), HasHeader(..),
                                          StandardHash, blockNo, pointSlot,
                                          unBlockNo, unSlotNo)
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision, FetchDecline (..))
import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Point (fromWithOrigin, withOrigin)
import           Ouroboros.Network.Subscription

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB

import           Cardano.Config.Protocol (TraceConstraints)
import           Cardano.Config.Types
import           Cardano.Tracing.MicroBenchmarking
import           Cardano.Tracing.ToObjectOrphans ()

import           Control.Tracer.Transformers

data Tracers peer localPeer blk = Tracers
  { -- | Trace the ChainDB
    chainDBTracer :: Tracer IO (ChainDB.TraceEvent blk)
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
  , errorPolicyTracer :: Tracer IO (NtN.WithAddr Socket.SockAddr NtN.ErrorPolicyTrace)
    -- | Trace local error policy resolution
  , localErrorPolicyTracer :: Tracer IO (NtN.WithAddr NtC.LocalAddress NtN.ErrorPolicyTrace)
  , acceptPolicyTracer :: Tracer IO NtN.AcceptConnectionsPolicyTrace
    -- | Trace the Mux
  , muxTracer :: Tracer IO (WithMuxBearer peer MuxTrace)
  , handshakeTracer :: Tracer IO NtN.HandshakeTr
  , localHandshakeTracer :: Tracer IO NtC.HandshakeTr
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
  , acceptPolicyTracer = nullTracer
  , muxTracer = nullTracer
  , handshakeTracer = nullTracer
  , localHandshakeTracer = nullTracer
  }


indexGCType :: ChainDB.TraceGCEvent a -> Int
indexGCType ChainDB.ScheduledGC{} = 1
indexGCType ChainDB.PerformedGC{} = 2

indexReplType :: ChainDB.TraceLedgerReplayEvent a -> Int
indexReplType LedgerDB.ReplayFromGenesis{} = 1
indexReplType LedgerDB.ReplayFromSnapshot{} = 2
indexReplType LedgerDB.ReplayedBlock{} = 3

instance ElidingTracer (WithSeverity (ChainDB.TraceEvent blk)) where
  -- equivalent by type and severity
  isEquivalent (WithSeverity s1 (ChainDB.TraceLedgerReplayEvent ev1))
               (WithSeverity s2 (ChainDB.TraceLedgerReplayEvent ev2)) =
                  s1 == s2 && indexReplType ev1 == indexReplType ev2
  isEquivalent (WithSeverity s1 (ChainDB.TraceGCEvent ev1))
               (WithSeverity s2 (ChainDB.TraceGCEvent ev2)) =
                  s1 == s2 && indexGCType ev1 == indexGCType ev2
   
  isEquivalent _ _ = False
  -- the types to be elided
  doelide (WithSeverity _ (ChainDB.TraceLedgerReplayEvent _)) = True
  doelide (WithSeverity _ (ChainDB.TraceGCEvent _)) = True
  doelide _ = False
  conteliding _tverb _tr _ (Nothing, _count) = return (Nothing, 0)
  conteliding _tverb tr ev@(WithSeverity _ (ChainDB.TraceGCEvent _)) (_old, count) = do
      when (count > 0 && count `mod` 100 == 0) $ do  -- report every 100th message
          meta <- mkLOMeta (getSeverityAnnotation ev) (getPrivacyAnnotation ev)
          traceNamedObject tr (meta, LogValue "messages elided so far" (PureI $ toInteger count))
      return (Just ev, count + 1)
  conteliding _tverb tr ev@(WithSeverity _ (ChainDB.TraceLedgerReplayEvent (LedgerDB.ReplayedBlock pt replayTo))) (_old, count) = do
      let slotno = toInteger $ unSlotNo (realPointSlot pt)
          endslot = toInteger $ withOrigin 0 unSlotNo (pointSlot replayTo)
          startslot = if count == 0 then slotno else toInteger count
          progress :: Double = (fromInteger (slotno - startslot) * 100.0) / fromInteger ((max slotno endslot) - startslot)
      when (count > 0 && (slotno - startslot) `mod` 1000 == 0) $ do  -- report every 1000th slot
          meta <- mkLOMeta (getSeverityAnnotation ev) (getPrivacyAnnotation ev)
          traceNamedObject tr (meta, LogValue "block replay progress (%)" (PureD $ (fromInteger $ round (progress * 10.0)) / 10.0))
      return (Just ev, fromInteger startslot)
  conteliding _ _ _ _ = return (Nothing, 0)

instance (StandardHash header, Eq peer) => ElidingTracer
  (WithSeverity [TraceLabelPeer peer (FetchDecision [Point header])]) where
  -- equivalent by type and severity
  isEquivalent (WithSeverity s1 peers1)
               (WithSeverity s2 peers2) =
                  s1 == s2 && peers1 == peers2
  -- the types to be elided
  doelide (WithSeverity _ peers) =
    let checkDecision :: TraceLabelPeer peer (Either FetchDecline result) -> Bool
        checkDecision (TraceLabelPeer _peer (Left FetchDeclineChainNotPlausible)) = True
        checkDecision _ = False
    in any checkDecision peers
  conteliding _tverb _tr _ (Nothing, _count) = return (Nothing, 0)
  conteliding _tverb tr ev (_old, count) = do
      when (count > 0 && count `mod` 100 == 0) $ do  -- report every 100th elided message
          meta <- mkLOMeta (getSeverityAnnotation ev) (getPrivacyAnnotation ev)
          traceNamedObject tr (meta, LogValue "messages elided so far" (PureI $ toInteger count))
      return (Just ev, count + 1)

-- | Smart constructor of 'NodeTraces'.
--
mkTracers
  :: forall peer localPeer blk.
     ( LedgerSupportsProtocol blk
     , TraceConstraints blk
     , ShowQuery (Query blk)
     , Show peer, Eq peer
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

  -- prepare |Outcome| 
  blockForgeOutcomeExtractor <- mkOutcomeExtractor

  elidedChainDB <- newstate  -- for eliding messages in ChainDB tracer
  elidedFetchDecision <- newstate  -- for eliding messages in FetchDecision tracer

  pure Tracers
    { chainDBTracer
        = tracerOnOff (traceChainDB traceOptions)
          $ annotateSeverity
          $ teeTraceChainTip tracingVerbosity elidedChainDB
          $ appendName "ChainDB"
          $ tracer
    , consensusTracers
        = mkConsensusTracers elidedFetchDecision blockForgeOutcomeExtractor forgeTracers traceOptions
    , protocolTracers
        = mkProtocolTracers traceOptions
    , ipSubscriptionTracer
        = tracerOnOff (traceIpSubscription traceOptions)
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "IpSubscription" tracer
    , dnsSubscriptionTracer
        = tracerOnOff (traceDnsSubscription traceOptions)
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "DnsSubscription" tracer
    , dnsResolverTracer
        = tracerOnOff (traceDnsResolver traceOptions)
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "DnsResolver" tracer
    , errorPolicyTracer
        = tracerOnOff (traceErrorPolicy traceOptions)
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "ErrorPolicy" tracer
    , localErrorPolicyTracer
        = tracerOnOff (traceLocalErrorPolicy traceOptions)
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "LocalErrorPolicy" tracer
    , acceptPolicyTracer
        = tracerOnOff (traceAcceptPolicy traceOptions)
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "AcceptPolicy" tracer
    , muxTracer
        = tracerOnOff (traceMux traceOptions)
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "Mux" tracer
    , handshakeTracer
        = tracerOnOff (traceHandshake traceOptions)
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "Handshake" tracer
    , localHandshakeTracer
        = tracerOnOff (traceLocalHandshake traceOptions)
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "LocalHandshake" tracer
    }
  where
    -- Turn on/off a tracer depending on what was parsed from the command line.
    tracerOnOff :: Bool -> Tracer IO a -> Tracer IO a
    tracerOnOff False _      = nullTracer
    tracerOnOff True tracer' = tracer'
    tracingVerbosity :: TracingVerbosity
    tracingVerbosity = traceVerbosity traceOptions

    teeTraceChainTip :: TracingVerbosity
                     -> MVar (Maybe (WithSeverity (ChainDB.TraceEvent blk)), Int)
                     -> Trace IO Text
                     -> Tracer IO (WithSeverity (ChainDB.TraceEvent blk))
    teeTraceChainTip tverb elided tr = Tracer $ \ev -> do
        traceWith (teeTraceChainTip' tr) ev
        traceWith (teeTraceChainTipElide tverb elided tr) ev
    teeTraceChainTipElide :: TracingVerbosity
                          -> MVar (Maybe (WithSeverity (ChainDB.TraceEvent blk)), Int)
                          -> Trace IO Text
                          -> Tracer IO (WithSeverity (ChainDB.TraceEvent blk))
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
                      -> Tracer IO (WithSeverity (ChainDB.TraceEvent blk))
    teeTraceChainTip' tr =
        Tracer $ \(WithSeverity _ ev') ->
          case ev' of
              (ChainDB.TraceAddBlockEvent ev) -> case ev of
                  ChainDB.SwitchedToAFork     _ _ c -> traceChainInformation tr (chainInformation c)
                  ChainDB.AddedToCurrentChain _ _ c -> traceChainInformation tr (chainInformation c)
                  _ -> pure ()
              _ -> pure ()

    teeTraceBlockFetchDecision
        :: TracingVerbosity
        -> MVar (Maybe (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])]),Int)
        -> Trace IO Text
        -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
    teeTraceBlockFetchDecision tverb eliding tr = Tracer $ \ev -> do
      traceWith (teeTraceBlockFetchDecision' tr) ev
      traceWith (teeTraceBlockFetchDecisionElide tverb eliding tr) ev
    teeTraceBlockFetchDecision'
        :: Trace IO Text
        -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
    teeTraceBlockFetchDecision' tr =
        Tracer $ \(WithSeverity _ peers) -> do
          meta <- mkLOMeta Notice Confidential
          let tr' = appendName "peers" tr
          traceNamedObject tr' (meta, LogValue "connectedPeers" . PureI $ fromIntegral $ length peers)
    teeTraceBlockFetchDecisionElide 
        :: TracingVerbosity
        -> MVar (Maybe (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])]),Int)
        -> Trace IO Text
        -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
    teeTraceBlockFetchDecisionElide = elideToLogObject

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
        traceNamedObject tr' (meta, logValue1)
        traceNamedObject tr' (meta, logValue2)
        traceNamedObject tr' (meta, logValue3)

    mempoolTracer :: Tracer IO (TraceEventMempool blk)
    mempoolTracer = Tracer $ \ev -> do
        traceWith (mempoolMetricsTraceTransformer tracer) ev
        traceWith (measureTxsStart tracer) ev
        let tr = appendName "Mempool" tracer
        traceWith (mpTracer tr) ev
      where
        mpTracer :: Trace IO Text -> Tracer IO (TraceEventMempool blk)
        mpTracer tr = annotateSeverity $ toLogObject' tracingVerbosity tr

    forgeTracer
        :: ForgeTracers
        -> TraceOptions
        -> Tracer IO (Consensus.TraceForgeEvent blk (GenTx blk))
    forgeTracer forgeTracers traceOpts = Tracer $ \ev -> do
        traceWith (measureTxsEnd tracer) ev
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

      traceWith (toLogObject' tverb tr) ev

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
        :: MVar (Maybe (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])]),Int)
        -> (OutcomeEnhancedTracer IO (Consensus.TraceForgeEvent blk (GenTx blk)) -> Tracer IO (Consensus.TraceForgeEvent blk (GenTx blk)))
        -> ForgeTracers -> TraceOptions -> Consensus.Tracers' peer blk (Tracer IO)
    mkConsensusTracers elidingFetchDecision measureBlockForging forgeTracers traceOpts = Consensus.Tracers
      { Consensus.chainSyncClientTracer
        = tracerOnOff (traceChainSyncClient traceOpts)
          $ toLogObject' tracingVerbosity
          $ appendName "ChainSyncClient" tracer
      , Consensus.chainSyncServerHeaderTracer
        =  tracerOnOff (traceChainSyncHeaderServer traceOpts)
          $ toLogObject' tracingVerbosity
          $ appendName "ChainSyncHeaderServer" tracer
      , Consensus.chainSyncServerBlockTracer
        = tracerOnOff (traceChainSyncBlockServer traceOpts)
          $ toLogObject' tracingVerbosity
          $ appendName "ChainSyncBlockServer" tracer
      , Consensus.blockFetchDecisionTracer
        = tracerOnOff (traceBlockFetchDecisions traceOpts)
          $ annotateSeverity
          $ teeTraceBlockFetchDecision tracingVerbosity elidingFetchDecision
          $ appendName "BlockFetchDecision" tracer
      , Consensus.blockFetchClientTracer
        = tracerOnOff (traceBlockFetchClient traceOpts)
          $ toLogObject' tracingVerbosity
          $ appendName "BlockFetchClient" tracer
      , Consensus.blockFetchServerTracer
        = tracerOnOff (traceBlockFetchServer traceOpts)
          $ toLogObject' tracingVerbosity
          $ appendName "BlockFetchServer" tracer
      , Consensus.txInboundTracer
        = tracerOnOff (traceTxInbound traceOpts)
          $ toLogObject' tracingVerbosity
          $ appendName "TxInbound" tracer
      , Consensus.txOutboundTracer
        = tracerOnOff (traceTxOutbound traceOpts)
          $ toLogObject' tracingVerbosity
          $ appendName "TxOutbound" tracer
      , Consensus.localTxSubmissionServerTracer
        = tracerOnOff (traceLocalTxSubmissionServer traceOpts)
          $ toLogObject' tracingVerbosity
          $ appendName "LocalTxSubmissionServer" tracer
      , Consensus.mempoolTracer
        = tracerOnOff (traceMempool traceOpts) $ mempoolTracer
      , Consensus.forgeTracer
        = Tracer $ \ev -> do
            traceWith (forgeTracer forgeTracers traceOpts) ev
            traceWith ( measureBlockForging
                      $ toLogObject' tracingVerbosity
                      $ appendName "ForgeTime" tracer) ev
      , Consensus.blockchainTimeTracer
        = Tracer $ \ev ->
            traceWith (toLogObject tracer) (readableTraceBlockchainTimeEvent ev)
      }

    readableTraceBlockchainTimeEvent :: TraceBlockchainTimeEvent -> Text
    readableTraceBlockchainTimeEvent ev = case ev of
        TraceStartTimeInTheFuture (SystemStart start) toWait ->
          "Waiting " <> show toWait <> " until genesis start time at " <> show start

    mkProtocolTracers
      :: TraceOptions -> ProtocolTracers' peer localPeer blk DeserialiseFailure (Tracer IO)
    mkProtocolTracers traceOpts = ProtocolTracers
      { ptChainSyncTracer
        = tracerOnOff (traceChainSyncProtocol traceOpts)
        $ showTracing $ withName "ChainSyncProtocol" tracer
      , ptChainSyncSerialisedTracer
        = tracerOnOff (traceChainSyncProtocol traceOpts)
        $ showTracing $ withName "ChainSyncProtocol" tracer
      , ptBlockFetchTracer
        = tracerOnOff (traceBlockFetchProtocol traceOpts)
        $ toLogObject' tracingVerbosity
        $ appendName "BlockFetchProtocol" tracer
      , ptBlockFetchSerialisedTracer
        = tracerOnOff (traceBlockFetchProtocolSerialised traceOpts)
        $ showTracing $ withName "BlockFetchProtocolSerialised" tracer
      , ptTxSubmissionTracer
        = tracerOnOff (traceTxSubmissionProtocol traceOpts)
        $ toLogObject' tracingVerbosity
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

