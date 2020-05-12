{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Tracing.Tracers
  ( BlockchainCounters
  , Tracers (..)
  , TraceConstraints
  , TraceConfig
  , initialBlockchainCounters
  , mkTracers
  , nullTracers
  ) where

import           Cardano.Prelude hiding (atomically)
import           Prelude (String)

import           GHC.Clock (getMonotonicTimeNSec)
import           Control.Tracer

import           Codec.CBOR.Read (DeserialiseFailure)
import           Data.Functor.Contravariant (contramap)
import           Data.IORef (IORef, atomicModifyIORef')
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
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
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
import           Cardano.Config.TraceConfig
import           Cardano.Tracing.MicroBenchmarking

import           Control.Tracer.Transformers

data Tracers peer localPeer blk = Tracers
  { -- | Trace the ChainDB
    chainDBTracer :: Tracer IO (ChainDB.TraceEvent blk)
    -- | Consensus-specific tracers.
  , consensusTracers :: Consensus.Tracers IO peer localPeer blk
    -- | Tracers for the node-to-node protocols.
  , nodeToNodeTracers :: NodeToNode.Tracers IO peer blk DeserialiseFailure
    -- | Tracers for the node-to-client protocols
  , nodeToClientTracers :: NodeToClient.Tracers IO localPeer blk DeserialiseFailure
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
  , nodeToClientTracers = NodeToClient.nullTracers
  , nodeToNodeTracers = NodeToNode.nullTracers
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
  isEquivalent (WithSeverity _s1 (ChainDB.TraceAddBlockEvent _))
               (WithSeverity _s2 (ChainDB.TraceAddBlockEvent _)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceGCEvent _ev1))
               (WithSeverity _s2 (ChainDB.TraceAddBlockEvent _)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceAddBlockEvent _))
               (WithSeverity _s2 (ChainDB.TraceGCEvent _ev2)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceGCEvent _ev1))
               (WithSeverity _s2 (ChainDB.TraceCopyToImmDBEvent _)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceCopyToImmDBEvent _))
               (WithSeverity _s2 (ChainDB.TraceGCEvent _ev2)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceCopyToImmDBEvent _))
               (WithSeverity _s2 (ChainDB.TraceAddBlockEvent _)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceAddBlockEvent _))
               (WithSeverity _s2 (ChainDB.TraceCopyToImmDBEvent _)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceCopyToImmDBEvent _))
               (WithSeverity _s2 (ChainDB.TraceCopyToImmDBEvent _)) = True
  isEquivalent _ _ = False
  -- the types to be elided
  doelide (WithSeverity _ (ChainDB.TraceLedgerReplayEvent _)) = True
  doelide (WithSeverity _ (ChainDB.TraceGCEvent _)) = True
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.IgnoreBlockOlderThanK _))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.IgnoreInvalidBlock _ _))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.BlockInTheFuture _ _))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.StoreButDontChange _))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.TrySwitchToAFork _ _))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.SwitchedToAFork _ _ _))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.AddBlockValidation (ChainDB.InvalidBlock _ _)))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.AddBlockValidation (ChainDB.InvalidCandidate _)))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.AddBlockValidation ChainDB.CandidateContainsFutureBlocksExceedingClockSkew{}))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent _)) = True
  doelide (WithSeverity _ (ChainDB.TraceCopyToImmDBEvent _)) = True
  doelide _ = False
  conteliding _tverb _tr _ (Nothing, _count) = return (Nothing, 0)
  conteliding tverb tr ev@(WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.AddedToCurrentChain _pt _ _))) (_old, oldt) = do
      tnow <- fromIntegral <$> getMonotonicTimeNSec
      let deltat = tnow - oldt
      if (deltat > 1250000000)  -- report at most every 1250 ms
        then do
          traceWith (toLogObject' tverb tr) ev
          return (Just ev, tnow)
        else return (Just ev, oldt)
  conteliding _tverb _tr ev@(WithSeverity _ (ChainDB.TraceAddBlockEvent _)) (_old, count) =
      return (Just ev, count)
  conteliding _tverb _tr ev@(WithSeverity _ (ChainDB.TraceCopyToImmDBEvent _)) (_old, count) =
      return (Just ev, count)
  conteliding _tverb _tr ev@(WithSeverity _ (ChainDB.TraceGCEvent _)) (_old, count) =
      return (Just ev, count)
  conteliding _tverb tr ev@(WithSeverity _ (ChainDB.TraceLedgerReplayEvent (LedgerDB.ReplayedBlock pt replayTo))) (_old, count) = do
      let slotno = toInteger $ unSlotNo (realPointSlot pt)
          endslot = toInteger $ withOrigin 0 unSlotNo (pointSlot replayTo)
          startslot = if count == 0 then slotno else toInteger count
          progress :: Double = (fromInteger slotno * 100.0) / fromInteger (max slotno endslot)
      when (count > 0 && (slotno - startslot) `mod` 1000 == 0) $ do  -- report every 1000th slot
          meta <- mkLOMeta (getSeverityAnnotation ev) (getPrivacyAnnotation ev)
          traceNamedObject tr (meta, LogValue "block replay progress (%)" (PureD $ (fromInteger $ round (progress * 10.0)) / 10.0))
      return (Just ev, fromInteger startslot)
  conteliding _ _ _ _ = return (Nothing, 0)

instance (StandardHash header, Eq peer) => ElidingTracer
  (WithSeverity [TraceLabelPeer peer (FetchDecision [Point header])]) where
  -- equivalent by type and severity
  isEquivalent (WithSeverity s1 _peers1)
               (WithSeverity s2 _peers2) = s1 == s2
  -- the types to be elided
  doelide (WithSeverity _ peers) =
    let checkDecision :: TraceLabelPeer peer (Either FetchDecline result) -> Bool
        checkDecision (TraceLabelPeer _peer (Left FetchDeclineChainNotPlausible)) = True
        checkDecision (TraceLabelPeer _peer (Left (FetchDeclineConcurrencyLimit _ _))) = True
        checkDecision (TraceLabelPeer _peer (Left (FetchDeclinePeerBusy _ _ _))) = True
        checkDecision _ = False
    in any checkDecision peers
  conteliding _tverb _tr _ (Nothing, _count) = return (Nothing, 0)
  conteliding tverb tr ev (_old, count) = do
      when (count > 0 && count `mod` 1000 == 0) $  -- report every 1000th message
          traceWith (toLogObject' tverb tr) ev
      return (Just ev, count + 1)

-- | This structure stores counters of blockchain-related events.
--   These values will be traced periodically.
data BlockchainCounters = BlockchainCounters
  { bcTxsProcessedNum :: !Word64
  , bcBlocksForgedNum :: !Word64
  }

initialBlockchainCounters :: BlockchainCounters
initialBlockchainCounters = BlockchainCounters 0 0

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
  => TraceConfig
  -> Trace IO Text
  -> IORef BlockchainCounters
  -> IO (Tracers peer localPeer blk)
mkTracers traceConf tracer bcCounters = do
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
        = tracerOnOff traceConf traceChainDB
          $ annotateSeverity
          $ teeTraceChainTip tracingVerbosity elidedChainDB
          $ appendName "ChainDB"
          $ tracer
    , consensusTracers
        = mkConsensusTracers elidedFetchDecision blockForgeOutcomeExtractor forgeTracers
    , nodeToClientTracers
    , nodeToNodeTracers
    , ipSubscriptionTracer
        = tracerOnOff traceConf traceIpSubscription
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "IpSubscription" tracer
    , dnsSubscriptionTracer
        = tracerOnOff traceConf traceDnsSubscription
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "DnsSubscription" tracer
    , dnsResolverTracer
        = tracerOnOff traceConf traceDnsResolver
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "DnsResolver" tracer
    , errorPolicyTracer
        = tracerOnOff traceConf traceErrorPolicy
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "ErrorPolicy" tracer
    , localErrorPolicyTracer
        = tracerOnOff traceConf traceLocalErrorPolicy
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "LocalErrorPolicy" tracer
    , acceptPolicyTracer
        = tracerOnOff traceConf traceAcceptPolicy
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "AcceptPolicy" tracer
    , muxTracer
        = tracerOnOff traceConf traceMux
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "Mux" tracer
    , handshakeTracer
        = tracerOnOff traceConf traceHandshake
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "Handshake" tracer
    , localHandshakeTracer
        = tracerOnOff traceConf traceLocalHandshake
          $ annotateSeverity
          $ toLogObject' tracingVerbosity
          $ appendName "LocalHandshake" tracer
    }
  where
    -- Turn on/off a tracer depending on what was parsed from the command line.
    tracerOnOff :: TraceConfig -> TraceId -> Tracer IO a -> Tracer IO a
    tracerOnOff tc getter tracer' =
      if traceEnabled tc getter then tracer' else nullTracer

    teeTraceChainTip :: TracingVerbosity
                     -> MVar (Maybe (WithSeverity (ChainDB.TraceEvent blk)), Integer)
                     -> Trace IO Text
                     -> Tracer IO (WithSeverity (ChainDB.TraceEvent blk))
    teeTraceChainTip tverb elided tr = Tracer $ \ev -> do
        traceWith (teeTraceChainTip' tr) ev
        traceWith (teeTraceChainTipElide tverb elided tr) ev
    teeTraceChainTipElide :: TracingVerbosity
                          -> MVar (Maybe (WithSeverity (ChainDB.TraceEvent blk)), Integer)
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
        -> MVar (Maybe (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])]),Integer)
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
          meta <- mkLOMeta Info Confidential
          let tr' = appendName "peers" tr
          traceNamedObject tr' (meta, LogValue "connectedPeers" . PureI $ fromIntegral $ length peers)
          let tr2' = appendName "peersList" tr
          traceNamedObject tr2' (meta, LogStructured $ toObject MaximalVerbosity peers)
    teeTraceBlockFetchDecisionElide
        :: TracingVerbosity
        -> MVar (Maybe (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])]),Integer)
        -> Trace IO Text
        -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
    teeTraceBlockFetchDecisionElide = elideToLogObject

    mempoolMetricsTraceTransformer :: Trace IO a
                                   -> Tracer IO (TraceEventMempool blk)
    mempoolMetricsTraceTransformer tr = Tracer $ \mempoolEvent -> do
        let tr' = appendName "metrics" tr
            (_n, tot) = case mempoolEvent of
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
            logValue2 = LogValue "mempoolBytes" $ PureI $ fromIntegral (msNumBytes tot)

        meta <- mkLOMeta Critical Confidential
        traceNamedObject tr' (meta, logValue1)
        traceNamedObject tr' (meta, logValue2)

    notifyTxsProcessed :: Tracer IO (TraceEventMempool blk)
    notifyTxsProcessed = Tracer $ \case
      TraceMempoolRemoveTxs [] _ -> return ()
      TraceMempoolRemoveTxs txs _ -> do
        -- TraceMempoolRemoveTxs are previously valid transactions that are no longer valid because of
        -- changes in the ledger state. These transactions are already removed from the mempool,
        -- so we can treat them as completely processed.
        newCounter <- atomicModifyIORef' bcCounters $ \counters ->
          let nc = bcTxsProcessedNum counters + fromIntegral (length txs)
          in (counters { bcTxsProcessedNum = nc }, nc)
        meta <- mkLOMeta Notice Public
        traceNamedObject (appendName "metrics" tracer)
                         (meta, LogValue "txsProcessedNum" (PureI $ fromIntegral newCounter))
      -- The rest of the constructors.
      _ -> return ()

    mempoolTracer :: TraceConfig -> Tracer IO (TraceEventMempool blk)
    mempoolTracer tc = Tracer $ \ev -> do
        traceWith (mempoolMetricsTraceTransformer tracer) ev
        traceWith (notifyTxsProcessed) ev
        traceWith (measureTxsStart tracer) ev
        let tr = appendName "Mempool" tracer
        traceWith (mpTracer tr) ev
      where
        mpTracer :: Trace IO Text -> Tracer IO (TraceEventMempool blk)
        mpTracer tr = annotateSeverity $ toLogObject' (traceConfigVerbosity tc) tr

    forgeTracer
        :: ForgeTracers
        -> TraceConfig
        -> Tracer IO (Consensus.TraceForgeEvent blk (GenTx blk))
    forgeTracer forgeTracers tc = Tracer $ \ev -> do
        traceWith (measureTxsEnd tracer) ev
        traceWith (notifyBlockForging) ev
        traceWith (consensusForgeTracer) ev
      where
        -- The consensus tracer.
        consensusForgeTracer = tracerOnOff traceConf traceForge
          $ annotateSeverity
          $ teeForge forgeTracers (traceConfigVerbosity tc)
          $ appendName "Forge" tracer

        notifyBlockForging = Tracer $ \case
          Consensus.TraceForgedBlock _ _ _ _ -> do
            newCounter <- atomicModifyIORef' bcCounters $ \counters ->
              let nc = bcBlocksForgedNum counters + 1
              in (counters { bcBlocksForgedNum = nc }, nc)
            meta <- mkLOMeta Notice Public
            traceNamedObject (appendName "metrics" tracer)
                             (meta, LogValue "blocksForgedNum" (PureI $ fromIntegral newCounter))
          -- The rest of the constructors.
          _ -> pure ()

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
        :: MVar (Maybe (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])]),Integer)
        -> (OutcomeEnhancedTracer IO (Consensus.TraceForgeEvent blk (GenTx blk)) -> Tracer IO (Consensus.TraceForgeEvent blk (GenTx blk)))
        -> ForgeTracers
        -> Consensus.Tracers' peer localPeer blk (Tracer IO)
    mkConsensusTracers elidingFetchDecision measureBlockForging forgeTracers = Consensus.Tracers
      { Consensus.chainSyncClientTracer
        = tracerOnOff traceConf traceChainSyncClient
          $ toLogObject' tracingVerbosity
          $ appendName "ChainSyncClient" tracer
      , Consensus.chainSyncServerHeaderTracer
        =  tracerOnOff traceConf traceChainSyncHeaderServer
          $ toLogObject' tracingVerbosity
          $ appendName "ChainSyncHeaderServer" tracer
      , Consensus.chainSyncServerBlockTracer
        = tracerOnOff traceConf traceChainSyncBlockServer
          $ toLogObject' tracingVerbosity
          $ appendName "ChainSyncBlockServer" tracer
      , Consensus.blockFetchDecisionTracer
        = tracerOnOff traceConf traceBlockFetchDecisions
          $ annotateSeverity
          $ teeTraceBlockFetchDecision tracingVerbosity elidingFetchDecision
          $ appendName "BlockFetchDecision" tracer
      , Consensus.blockFetchClientTracer
        = tracerOnOff traceConf traceBlockFetchClient
          $ toLogObject' tracingVerbosity
          $ appendName "BlockFetchClient" tracer
      , Consensus.blockFetchServerTracer
        = tracerOnOff traceConf traceBlockFetchServer
          $ toLogObject' tracingVerbosity
          $ appendName "BlockFetchServer" tracer
      , Consensus.txInboundTracer
        = tracerOnOff traceConf traceTxInbound
          $ toLogObject' tracingVerbosity
          $ appendName "TxInbound" tracer
      , Consensus.txOutboundTracer
        = tracerOnOff traceConf traceTxOutbound
          $ toLogObject' tracingVerbosity
          $ appendName "TxOutbound" tracer
      , Consensus.localTxSubmissionServerTracer
        = tracerOnOff traceConf traceLocalTxSubmissionServer
          $ toLogObject' tracingVerbosity
          $ appendName "LocalTxSubmissionServer" tracer
      , Consensus.mempoolTracer
        = tracerOnOff traceConf traceMempool
          $ mempoolTracer traceConf
      , Consensus.forgeTracer
        = Tracer $ \ev -> do
            traceWith (forgeTracer forgeTracers traceConf) ev
            traceWith ( measureBlockForging
                      $ toLogObject' tracingVerbosity
                      $ appendName "ForgeTime" tracer) ev
      , Consensus.blockchainTimeTracer
        = Tracer $ \ev ->
            traceWith (toLogObject tracer) (readableTraceBlockchainTimeEvent ev)
      }

    tracingVerbosity :: TracingVerbosity
    tracingVerbosity = traceConfigVerbosity traceConf

    readableTraceBlockchainTimeEvent :: TraceBlockchainTimeEvent -> Text
    readableTraceBlockchainTimeEvent ev = case ev of
        TraceStartTimeInTheFuture (SystemStart start) toWait ->
          "Waiting " <> show toWait <> " until genesis start time at " <> show start
        TraceCurrentSlotUnknown time _ ->
          "Too far from the chain tip to determine the current slot number for the time "
           <> show time

    nodeToClientTracers
      :: NodeToClient.Tracers' localPeer blk DeserialiseFailure (Tracer IO)
    nodeToClientTracers = NodeToClient.Tracers
      { NodeToClient.tChainSyncTracer
        = tracerOnOff traceConf traceLocalChainSyncProtocol
        $ showTracing $ withName "LocalChainSyncProtocol" tracer
      , NodeToClient.tTxSubmissionTracer
        = tracerOnOff traceConf traceLocalTxSubmissionProtocol
        $ showTracing $ withName "LocalTxSubmissionProtocol" tracer
      , NodeToClient.tStateQueryTracer
        = tracerOnOff traceConf traceLocalStateQueryProtocol
        $ showTracing $ withName "LocalStateQueryProtocol" tracer
      }

    nodeToNodeTracers
      :: NodeToNode.Tracers' peer blk DeserialiseFailure (Tracer IO)
    nodeToNodeTracers = NodeToNode.Tracers
      { NodeToNode.tChainSyncTracer
        = tracerOnOff traceConf traceChainSyncProtocol
        $ showTracing $ withName "ChainSyncProtocol" tracer
      , NodeToNode.tChainSyncSerialisedTracer
        = tracerOnOff traceConf traceChainSyncProtocol
        $ showTracing $ withName "ChainSyncProtocol" tracer
      , NodeToNode.tBlockFetchTracer
        = tracerOnOff traceConf traceBlockFetchProtocol
        $ toLogObject' tracingVerbosity
        $ appendName "BlockFetchProtocol" tracer
      , NodeToNode.tBlockFetchSerialisedTracer
        = tracerOnOff traceConf traceBlockFetchProtocolSerialised
        $ showTracing $ withName "BlockFetchProtocolSerialised" tracer
      , NodeToNode.tTxSubmissionTracer
        = tracerOnOff traceConf traceTxSubmissionProtocol
        $ toLogObject' tracingVerbosity
        $ appendName "TxSubmissionProtocol" tracer
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
