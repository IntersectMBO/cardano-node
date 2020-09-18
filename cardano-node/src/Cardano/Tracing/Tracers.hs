{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Tracing.Tracers
  ( BlockchainCounters
  , Tracers (..)
  , TraceOptions
  , mkTracers
  , nullTracers
  ) where

import           Cardano.Prelude hiding (show)
import           Prelude (String, show)

import           GHC.Clock (getMonotonicTimeNSec)

import           Codec.CBOR.Read (DeserialiseFailure)
import           Data.Aeson (ToJSON (..), Value (..))
import qualified Data.HashMap.Strict as Map
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Text as Text
import           Network.Mux (MuxTrace, WithMuxBearer)
import qualified Network.Socket as Socket (SockAddr)

import           Control.Tracer
import           Control.Tracer.Transformers

import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.LogItem (LOContent (..), LoggerName)
import           Cardano.BM.Data.Tracer (WithSeverity (..), annotateSeverity)
import           Cardano.BM.Data.Transformers
import           Cardano.BM.Internal.ElidingTracer
import           Cardano.BM.Trace (traceNamedObject)
import           Cardano.BM.Tracing

import           Ouroboros.Consensus.Block (BlockProtocol, CannotForge, ConvertRawHash,
                     ForgeStateInfo, ForgeStateUpdateError, Header, realPointSlot)
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..),
                     TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerErr, LedgerState)
import           Ouroboros.Consensus.Ledger.Extended (ledgerState)
import           Ouroboros.Consensus.Ledger.Inspect (InspectLedger, LedgerEvent)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx, GenTxId, HasTxs)
import           Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Mempool.API (MempoolSize (..), TraceEventMempool (..))
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Consensus.Node.Run as Consensus (RunNode)
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.Protocol.Abstract (ValidationErr)

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (BlockNo (..), HasHeader (..), Point, StandardHash,
                     blockNo, pointSlot, unBlockNo, unSlotNo)
import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision, FetchDecline (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Point (fromWithOrigin, withOrigin)
import           Ouroboros.Network.Subscription

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB

import           Cardano.Tracing.Config
import           Cardano.Tracing.Constraints (TraceConstraints)
import           Cardano.Tracing.ConvertTxId (ConvertTxId)
import           Cardano.Tracing.Kernel
import           Cardano.Tracing.Metrics (HasKESMetricsData (..), KESMetricsData (..),
                     MaxKESEvolutions (..), OperationalCertStartKESPeriod (..))
import           Cardano.Tracing.MicroBenchmarking
import           Cardano.Tracing.Queries

-- For tracing instances
import           Cardano.Node.Protocol.Byron ()
import           Cardano.Node.Protocol.Shelley ()

{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use record patterns" -}

data Tracers peer localPeer blk = Tracers
  { -- | Trace the ChainDB
    chainDBTracer :: Tracer IO (ChainDB.TraceEvent blk)
    -- | Consensus-specific tracers.
  , consensusTracers :: Consensus.Tracers IO peer localPeer blk
    -- | Tracers for the node-to-node protocols.
  , nodeToNodeTracers :: NodeToNode.Tracers IO peer blk DeserialiseFailure
    --, serialisedBlockTracer :: NodeToNode.SerialisedTracer IO peer blk (SerialisedBlockTrace)
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
  , ftTraceNodeCannotForge :: Trace IO Text
  , ftTraceForgeStateUpdateError :: Trace IO Text
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
               (WithSeverity _s2 (ChainDB.TraceCopyToImmutableDBEvent _)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceCopyToImmutableDBEvent _))
               (WithSeverity _s2 (ChainDB.TraceGCEvent _ev2)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceCopyToImmutableDBEvent _))
               (WithSeverity _s2 (ChainDB.TraceAddBlockEvent _)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceAddBlockEvent _))
               (WithSeverity _s2 (ChainDB.TraceCopyToImmutableDBEvent _)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceCopyToImmutableDBEvent _))
               (WithSeverity _s2 (ChainDB.TraceCopyToImmutableDBEvent _)) = True
  isEquivalent _ _ = False
  -- the types to be elided
  doelide (WithSeverity _ (ChainDB.TraceLedgerReplayEvent _)) = True
  doelide (WithSeverity _ (ChainDB.TraceGCEvent _)) = True
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.IgnoreBlockOlderThanK _))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.IgnoreInvalidBlock _ _))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.BlockInTheFuture _ _))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.StoreButDontChange _))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.TrySwitchToAFork _ _))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.SwitchedToAFork{}))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.AddBlockValidation (ChainDB.InvalidBlock _ _)))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.AddBlockValidation (ChainDB.InvalidCandidate _)))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.AddBlockValidation ChainDB.CandidateContainsFutureBlocksExceedingClockSkew{}))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.AddedToCurrentChain events _ _  _))) = null events
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent _)) = True
  doelide (WithSeverity _ (ChainDB.TraceCopyToImmutableDBEvent _)) = True
  doelide _ = False
  conteliding _tverb _tr _ (Nothing, _count) = return (Nothing, 0)
  conteliding tverb tr ev@(WithSeverity _ (ChainDB.TraceAddBlockEvent ChainDB.AddedToCurrentChain{})) (_old, oldt) = do
      tnow <- fromIntegral <$> getMonotonicTimeNSec
      let deltat = tnow - oldt
      if deltat > 1250000000 -- report at most every 1250 ms
        then do
          traceWith (toLogObject' tverb tr) ev
          return (Just ev, tnow)
        else return (Just ev, oldt)
  conteliding _tverb _tr ev@(WithSeverity _ (ChainDB.TraceAddBlockEvent _)) (_old, count) =
      return (Just ev, count)
  conteliding _tverb _tr ev@(WithSeverity _ (ChainDB.TraceCopyToImmutableDBEvent _)) (_old, count) =
      return (Just ev, count)
  conteliding _tverb _tr ev@(WithSeverity _ (ChainDB.TraceGCEvent _)) (_old, count) =
      return (Just ev, count)
  conteliding _tverb tr ev@(WithSeverity _ (ChainDB.TraceLedgerReplayEvent (LedgerDB.ReplayedBlock pt [] replayTo))) (_old, count) = do
      let slotno = toInteger $ unSlotNo (realPointSlot pt)
          endslot = toInteger $ withOrigin 0 unSlotNo (pointSlot replayTo)
          startslot = if count == 0 then slotno else toInteger count
          progress :: Double = (fromInteger slotno * 100.0) / fromInteger (max slotno endslot)
      when (count > 0 && (slotno - startslot) `mod` 1000 == 0) $ do  -- report every 1000th slot
          meta <- mkLOMeta (getSeverityAnnotation ev) (getPrivacyAnnotation ev)
          traceNamedObject tr (meta, LogValue "block replay progress (%)" (PureD (fromInteger (round (progress * 10.0)) / 10.0)))
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
  { bcTxsProcessedNum        :: !Word64
  , bcBlocksForgedNum        :: !Word64
  , bcNodeCannotForgeNum     :: !Word64
  , bcNodeIsLeaderNum        :: !Word64
  , bcSlotsMissedNum         :: !Word64
  }

initialBlockchainCounters :: BlockchainCounters
initialBlockchainCounters = BlockchainCounters 0 0 0 0 0

-- | Tracers for all system components.
--
mkTracers
  :: forall peer localPeer blk.
     ( Consensus.RunNode blk
     , HasKESMetricsData blk
     , TraceConstraints blk
     , Show peer, Eq peer
     , Show localPeer
     )
  => TraceOptions
  -> Trace IO Text
  -> NodeKernelData blk
  -> IO (Tracers peer localPeer blk)
mkTracers tOpts@(TracingOn trSel) tr nodeKern = do
  bcCounters :: IORef BlockchainCounters <- newIORef initialBlockchainCounters
  consensusTracers <- mkConsensusTracers trSel verb tr nodeKern bcCounters
  elidedChainDB <- newstate  -- for eliding messages in ChainDB tracer

  pure Tracers
    { chainDBTracer = tracerOnOff' (traceChainDB trSel) $
        annotateSeverity . teeTraceChainTip tOpts elidedChainDB $ appendName "ChainDB" tr
    , consensusTracers = consensusTracers
    , nodeToClientTracers = nodeToClientTracers' trSel verb tr
    , nodeToNodeTracers = nodeToNodeTracers' trSel verb tr
    , ipSubscriptionTracer = tracerOnOff (traceIpSubscription trSel) verb "IpSubscription" tr
    , dnsSubscriptionTracer =  tracerOnOff (traceDnsSubscription trSel) verb "DnsSubscription" tr
    , dnsResolverTracer = tracerOnOff (traceDnsResolver trSel) verb "DnsResolver" tr
    , errorPolicyTracer = tracerOnOff (traceErrorPolicy trSel) verb "ErrorPolicy" tr
    , localErrorPolicyTracer = tracerOnOff (traceLocalErrorPolicy trSel) verb "LocalErrorPolicy" tr
    , acceptPolicyTracer = tracerOnOff (traceAcceptPolicy trSel) verb "AcceptPolicy" tr
    , muxTracer = tracerOnOff (traceMux trSel) verb "Mux" tr
    , handshakeTracer = tracerOnOff (traceHandshake trSel) verb "Handshake" tr
    , localHandshakeTracer = tracerOnOff (traceLocalHandshake trSel) verb "LocalHandshake" tr
    }
 where
   verb :: TracingVerbosity
   verb = traceVerbosity trSel

mkTracers TracingOff _ _ =
  pure Tracers
    { chainDBTracer = nullTracer
    , consensusTracers = Consensus.Tracers
      { Consensus.chainSyncClientTracer = nullTracer
      , Consensus.chainSyncServerHeaderTracer = nullTracer
      , Consensus.chainSyncServerBlockTracer = nullTracer
      , Consensus.blockFetchDecisionTracer = nullTracer
      , Consensus.blockFetchClientTracer = nullTracer
      , Consensus.blockFetchServerTracer = nullTracer
      , Consensus.forgeStateInfoTracer = nullTracer
      , Consensus.txInboundTracer = nullTracer
      , Consensus.txOutboundTracer = nullTracer
      , Consensus.localTxSubmissionServerTracer = nullTracer
      , Consensus.mempoolTracer = nullTracer
      , Consensus.forgeTracer = nullTracer
      , Consensus.blockchainTimeTracer = nullTracer
      , Consensus.keepAliveClientTracer = nullTracer
      }
    , nodeToClientTracers = NodeToClient.Tracers
      { NodeToClient.tChainSyncTracer = nullTracer
      , NodeToClient.tTxSubmissionTracer = nullTracer
      , NodeToClient.tStateQueryTracer = nullTracer
      }
    , nodeToNodeTracers = NodeToNode.Tracers
      { NodeToNode.tChainSyncTracer = nullTracer
      , NodeToNode.tChainSyncSerialisedTracer = nullTracer
      , NodeToNode.tBlockFetchTracer = nullTracer
      , NodeToNode.tBlockFetchSerialisedTracer = nullTracer
      , NodeToNode.tTxSubmissionTracer = nullTracer
      }
    , ipSubscriptionTracer = nullTracer
    , dnsSubscriptionTracer= nullTracer
    , dnsResolverTracer = nullTracer
    , errorPolicyTracer = nullTracer
    , localErrorPolicyTracer = nullTracer
    , acceptPolicyTracer = nullTracer
    , muxTracer = nullTracer
    , handshakeTracer = nullTracer
    , localHandshakeTracer = nullTracer
    }

--------------------------------------------------------------------------------
-- Chain DB Tracers
--------------------------------------------------------------------------------

teeTraceChainTip
  :: ( ConvertRawHash blk
     , LedgerSupportsProtocol blk
     , InspectLedger blk
     , ToObject (Header blk)
     , ToObject (LedgerEvent blk)
     )
  => TraceOptions
  -> MVar (Maybe (WithSeverity (ChainDB.TraceEvent blk)), Integer)
  -> Trace IO Text
  -> Tracer IO (WithSeverity (ChainDB.TraceEvent blk))
teeTraceChainTip TracingOff _ _ = nullTracer
teeTraceChainTip (TracingOn trSel) elided tr =
  Tracer $ \ev -> do
    traceWith (teeTraceChainTip' tr) ev
    traceWith (teeTraceChainTipElide (traceVerbosity trSel) elided tr) ev

teeTraceChainTipElide
  :: ( ConvertRawHash blk
     , LedgerSupportsProtocol blk
     , InspectLedger blk
     , ToObject (Header blk)
     , ToObject (LedgerEvent blk)
     )
  => TracingVerbosity
  -> MVar (Maybe (WithSeverity (ChainDB.TraceEvent blk)), Integer)
  -> Trace IO Text
  -> Tracer IO (WithSeverity (ChainDB.TraceEvent blk))
teeTraceChainTipElide = elideToLogObject

traceChainInformation :: Trace IO Text -> ChainInformation -> IO ()
traceChainInformation tr ChainInformation { slots, blocks, density, epoch, slotInEpoch } = do
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
  traceI "slotInEpoch" slotInEpoch
  traceI "epoch"       (unEpochNo epoch)

teeTraceChainTip'
  :: HasHeader (Header blk)
  => Trace IO Text -> Tracer IO (WithSeverity (ChainDB.TraceEvent blk))
teeTraceChainTip' tr =
    Tracer $ \(WithSeverity _ ev') ->
      case ev' of
          (ChainDB.TraceAddBlockEvent ev) -> case ev of
            ChainDB.SwitchedToAFork _warnings newTipInfo _ newChain ->
              traceChainInformation tr (chainInformation newTipInfo newChain)
            ChainDB.AddedToCurrentChain _warnings newTipInfo _ newChain ->
              traceChainInformation tr (chainInformation newTipInfo newChain)
            _ -> pure ()
          _ -> pure ()

--------------------------------------------------------------------------------
-- Consensus Tracers
--------------------------------------------------------------------------------

mkConsensusTracers
  :: forall blk peer localPeer.
     ( Show peer
     , Eq peer
     , LedgerQueries blk
     , ToJSON (GenTxId blk)
     , ToObject (ApplyTxErr blk)
     , ToObject (CannotForge blk)
     , ToObject (GenTx blk)
     , ToObject (LedgerErr (LedgerState blk))
     , ToObject (OtherHeaderEnvelopeError blk)
     , ToObject (ValidationErr (BlockProtocol blk))
     , ToObject (ForgeStateUpdateError blk)
     , Consensus.RunNode blk
     , HasKESMetricsData blk
     )
  => TraceSelection
  -> TracingVerbosity
  -> Trace IO Text
  -> NodeKernelData blk
  -> IORef BlockchainCounters
  -> IO (Consensus.Tracers' peer localPeer blk (Tracer IO))
mkConsensusTracers trSel verb tr nodeKern bcCounters = do
  blockForgeOutcomeExtractor <- mkOutcomeExtractor
  elidedFetchDecision <- newstate  -- for eliding messages in FetchDecision tr
  forgeTracers <- mkForgeTracers

  pure Consensus.Tracers
    { Consensus.chainSyncClientTracer = tracerOnOff (traceChainSyncClient trSel) verb "ChainSyncClient" tr
    , Consensus.chainSyncServerHeaderTracer = tracerOnOff (traceChainSyncHeaderServer trSel) verb "ChainSyncHeaderServer" tr
    , Consensus.chainSyncServerBlockTracer = tracerOnOff (traceChainSyncHeaderServer trSel) verb "ChainSyncBlockServer" tr
    , Consensus.blockFetchDecisionTracer = tracerOnOff' (traceBlockFetchDecisions trSel) $
        annotateSeverity $ teeTraceBlockFetchDecision verb elidedFetchDecision $ appendName "BlockFetchDecision" tr
    , Consensus.blockFetchClientTracer = tracerOnOff (traceBlockFetchClient trSel) verb "BlockFetchClient" tr
    , Consensus.blockFetchServerTracer = tracerOnOff (traceBlockFetchServer trSel) verb "BlockFetchServer" tr
    , Consensus.forgeStateInfoTracer = tracerOnOff' (traceForgeStateInfo trSel) $ forgeStateInfoTracer (Proxy @ blk) trSel tr
    , Consensus.txInboundTracer = tracerOnOff (traceTxInbound trSel) verb "TxInbound" tr
    , Consensus.txOutboundTracer = tracerOnOff (traceTxOutbound trSel) verb "TxOutbound" tr
    , Consensus.localTxSubmissionServerTracer = tracerOnOff (traceLocalTxSubmissionServer trSel) verb "LocalTxSubmissionServer" tr
    , Consensus.mempoolTracer = tracerOnOff' (traceMempool trSel) $ mempoolTracer trSel tr bcCounters
    , Consensus.forgeTracer = tracerOnOff' (traceForge trSel) $
        Tracer $ \ev -> do
          traceWith (forgeTracer verb tr forgeTracers nodeKern bcCounters) ev
          traceWith (blockForgeOutcomeExtractor
                    $ toLogObject' verb
                    $ appendName "ForgeTime" tr) ev

    , Consensus.blockchainTimeTracer = tracerOnOff' (traceBlockchainTime trSel) $
        Tracer $ \ev ->
          traceWith (toLogObject tr) (readableTraceBlockchainTimeEvent ev)
    , Consensus.keepAliveClientTracer = tracerOnOff (traceKeepAliveClient trSel) verb "KeepAliveClient" tr
    }
 where
   mkForgeTracers :: IO ForgeTracers
   mkForgeTracers = do
     -- We probably don't want to pay the extra IO cost per-counter-increment. -- sk
     staticMeta <- mkLOMeta Critical Confidential
     let name :: LoggerName = "metrics.Forge"
     ForgeTracers
       <$> counting (liftCounting staticMeta name "forged" tr)
       <*> counting (liftCounting staticMeta name "forge-about-to-lead" tr)
       <*> counting (liftCounting staticMeta name "could-not-forge" tr)
       <*> counting (liftCounting staticMeta name "adopted" tr)
       <*> counting (liftCounting staticMeta name "didnt-adopt" tr)
       <*> counting (liftCounting staticMeta name "forged-invalid" tr)
       <*> counting (liftCounting staticMeta name "node-not-leader" tr)
       <*> counting (liftCounting staticMeta name "cannot-forge" tr)
       <*> counting (liftCounting staticMeta name "forge-state-update-error" tr)
       <*> counting (liftCounting staticMeta name "block-from-future" tr)
       <*> counting (liftCounting staticMeta name "slot-is-immutable" tr)
       <*> counting (liftCounting staticMeta name "node-is-leader" tr)

teeForge ::
  forall blk
  . ( Consensus.RunNode blk
     , LedgerQueries blk
     , ToObject (CannotForge blk)
     , ToObject (LedgerErr (LedgerState blk))
     , ToObject (OtherHeaderEnvelopeError blk)
     , ToObject (ValidationErr (BlockProtocol blk))
     , ToObject (ForgeStateUpdateError blk)
     )
  => ForgeTracers
  -> NodeKernelData blk
  -> TracingVerbosity
  -> Trace IO Text
  -> Tracer IO (WithSeverity (Consensus.TraceForgeEvent blk))
teeForge ft nodeKern tverb tr = Tracer $ \ev@(WithSeverity sev event) -> do
  flip traceWith ev $ fanning $ \(WithSeverity _ e) ->
    case e of
      Consensus.TraceStartLeadershipCheck{} -> teeForge' (ftForgeAboutToLead ft)
      Consensus.TraceSlotIsImmutable{} -> teeForge' (ftTraceSlotIsImmutable ft)
      Consensus.TraceBlockFromFuture{} -> teeForge' (ftTraceBlockFromFuture ft)
      Consensus.TraceBlockContext{} -> nullTracer
      Consensus.TraceNoLedgerState{} -> teeForge' (ftCouldNotForge ft)
      Consensus.TraceLedgerState{} -> nullTracer
      Consensus.TraceNoLedgerView{} -> teeForge' (ftCouldNotForge ft)
      Consensus.TraceLedgerView{} -> nullTracer
      Consensus.TraceForgeStateUpdateError{} -> teeForge' (ftTraceForgeStateUpdateError ft)
      Consensus.TraceNodeCannotForge {} -> teeForge' (ftTraceNodeCannotForge ft)
      Consensus.TraceNodeNotLeader{} -> teeForge' (ftTraceNodeNotLeader ft)
      Consensus.TraceNodeIsLeader{} -> teeForge' (ftTraceNodeIsLeader ft)
      Consensus.TraceForgedBlock{} -> teeForge' (ftForged ft)
      Consensus.TraceDidntAdoptBlock{} -> teeForge' (ftDidntAdoptBlock ft)
      Consensus.TraceForgedInvalidBlock{} -> teeForge' (ftForgedInvalid ft)
      Consensus.TraceAdoptedBlock{} -> teeForge' (ftAdopted ft)
  case event of
    Consensus.TraceStartLeadershipCheck slot -> do
      !utxoSize <- mapNodeKernelDataIO nkUtxoSize nodeKern
      meta <- mkLOMeta sev Public
      traceNamedObject tr
        ( meta
        , LogStructured $ Map.fromList $
          [("kind", String "TraceStartLeadershipCheck")
          ,("slot", toJSON $ unSlotNo slot)]
          ++ fromSMaybe [] ((:[]) . ("utxoSize",) . toJSON <$> utxoSize))
    _ -> traceWith (toLogObject' tverb tr) ev
 where
   nkUtxoSize
     :: NodeKernel IO RemoteConnectionId LocalConnectionId blk -> IO Int
   nkUtxoSize NodeKernel{getChainDB} =
     atomically (ChainDB.getCurrentLedger getChainDB)
     <&> ledgerUtxoSize . ledgerState

teeForge'
  :: Trace IO Text
  -> Tracer IO (WithSeverity (Consensus.TraceForgeEvent blk))
teeForge' tr =
  Tracer $ \(WithSeverity _ ev) -> do
    meta <- mkLOMeta Critical Confidential
    traceNamedObject (appendName "metrics" tr) . (meta,) $
      case ev of
        Consensus.TraceStartLeadershipCheck slot ->
          LogValue "aboutToLeadSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceSlotIsImmutable slot _tipPoint _tipBlkNo ->
          LogValue "slotIsImmutable" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceBlockFromFuture slot _slotNo ->
          LogValue "blockFromFuture" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceBlockContext slot _tipBlkNo _tipPoint ->
          LogValue "blockContext" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceNoLedgerState slot _ ->
          LogValue "couldNotForgeSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceLedgerState slot _ ->
          LogValue "ledgerState" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceNoLedgerView slot _ ->
          LogValue "couldNotForgeSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceLedgerView slot ->
          LogValue "ledgerView" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceForgeStateUpdateError slot _reason ->
          LogValue "forgeStateUpdateError" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceNodeCannotForge slot _reason ->
          LogValue "nodeCannotForge" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceNodeNotLeader slot ->
          LogValue "nodeNotLeader" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceNodeIsLeader slot ->
          LogValue "nodeIsLeader" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceForgedBlock slot _ _ _ ->
          LogValue "forgedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceDidntAdoptBlock slot _ ->
          LogValue "notAdoptedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceForgedInvalidBlock slot _ _ ->
          LogValue "forgedInvalidSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceAdoptedBlock slot _ _ ->
          LogValue "adoptedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot

forgeTracer
  :: ( Consensus.RunNode blk
     , LedgerQueries blk
     , ToObject (CannotForge blk)
     , ToObject (LedgerErr (LedgerState blk))
     , ToObject (OtherHeaderEnvelopeError blk)
     , ToObject (ValidationErr (BlockProtocol blk))
     , ToObject (ForgeStateUpdateError blk)
     )
  => TracingVerbosity
  -> Trace IO Text
  -> ForgeTracers
  -> NodeKernelData blk
  -> IORef BlockchainCounters
  -> Tracer IO (Consensus.TraceForgeEvent blk)
forgeTracer verb tr forgeTracers nodeKern bcCounters =
  Tracer $ \ev -> do
    traceWith (measureTxsEnd tr) ev
    traceWith (notifyBlockForging bcCounters tr) ev
    traceWith (notifySlotsMissedIfNeeded bcCounters tr) ev
    -- Consensus tracer
    traceWith (annotateSeverity
                 $ teeForge forgeTracers nodeKern verb
                 $ appendName "Forge" tr) ev

notifyBlockForging
  :: IORef BlockchainCounters
  -> Trace IO Text
  -> Tracer IO (Consensus.TraceForgeEvent blk)
notifyBlockForging bcCounters tr = Tracer $ \case
  Consensus.TraceForgedBlock {} -> do
    updatedBlocksForged <- atomicModifyIORef' bcCounters (\cnts -> let nc = bcBlocksForgedNum cnts + 1
                                                                   in (cnts { bcBlocksForgedNum = nc }, nc)
                                                         )
    traceCounter "blocksForgedNum" updatedBlocksForged tr
  Consensus.TraceNodeCannotForge {} -> do
    -- It means that node have tried to forge new block, but because of misconfiguration
    -- (for example, invalid key) it's impossible.
    updatedNodeCannotForge <- atomicModifyIORef' bcCounters $ \cnts ->
      let nc = bcNodeCannotForgeNum cnts + 1
      in (cnts { bcNodeCannotForgeNum = nc }, nc)
    traceCounter "nodeCannotForge" updatedNodeCannotForge tr
  -- The rest of the constructors.
  _ -> pure ()

notifySlotsMissedIfNeeded
  :: IORef BlockchainCounters
  -> Trace IO Text
  -> Tracer IO (Consensus.TraceForgeEvent blk)
notifySlotsMissedIfNeeded bcCounters tr = Tracer $ \case
  Consensus.TraceNodeIsLeader {} -> do
    updatedNodeIsLeaderNum <- atomicModifyIORef' bcCounters (\cnts -> let nc = bcNodeIsLeaderNum cnts + 1
                                                                      in (cnts { bcNodeIsLeaderNum = nc }, nc)
                                                            )
    traceCounter "nodeIsLeaderNum" updatedNodeIsLeaderNum tr
  Consensus.TraceNodeNotLeader {} -> do
    -- Not is not a leader again, so now the number of blocks forged by this node
    -- should be equal to the number of slots when this node was a leader.
    counters <- readIORef bcCounters
    let howManyBlocksWereForged = bcBlocksForgedNum counters
        timesNodeWasALeader = bcNodeIsLeaderNum counters
        numberOfMissedSlots = timesNodeWasALeader - howManyBlocksWereForged
    if numberOfMissedSlots > 0
    then do
        -- Node was a leader more times than the number of forged blocks,
        -- it means that some slots were missed.
      updatesSlotsMissed <- atomicModifyIORef' bcCounters (\cnts -> let nc = bcSlotsMissedNum cnts + numberOfMissedSlots
                                                                    in (cnts { bcSlotsMissedNum = nc }, nc)
                                                          )
      traceCounter "slotsMissedNum" updatesSlotsMissed tr
    else return ()
  -- The rest of the constructors.
  _ -> pure ()


--------------------------------------------------------------------------------
-- Mempool Tracers
--------------------------------------------------------------------------------

notifyTxsProcessed :: IORef BlockchainCounters -> Trace IO Text -> Tracer IO (TraceEventMempool blk)
notifyTxsProcessed bcCounters tr = Tracer $ \case
  TraceMempoolRemoveTxs [] _ -> return ()
  TraceMempoolRemoveTxs txs _ -> do
    -- TraceMempoolRemoveTxs are previously valid transactions that are no longer valid because of
    -- changes in the ledger state. These transactions are already removed from the mempool,
    -- so we can treat them as completely processed.
    updatedTxProcessed <- atomicModifyIORef' bcCounters (\cnts -> let nc = bcTxsProcessedNum cnts + fromIntegral (length txs)
                                                                  in (cnts { bcTxsProcessedNum = nc }, nc)
                                                        )
    traceCounter "txsProcessedNum" updatedTxProcessed tr
  -- The rest of the constructors.
  _ -> return ()


mempoolMetricsTraceTransformer :: Trace IO a -> Tracer IO (TraceEventMempool blk)
mempoolMetricsTraceTransformer tr = Tracer $ \mempoolEvent -> do
  let tr' = appendName "metrics" tr
      (_n, tot) = case mempoolEvent of
                    TraceMempoolAddedTx     _tx0 _ tot0 -> (1, tot0)
                    TraceMempoolRejectedTx  _tx0 _ tot0 -> (1, tot0)
                    TraceMempoolRemoveTxs   txs0   tot0 -> (length txs0, tot0)
                    TraceMempoolManuallyRemovedTxs txs0 txs1 tot0 -> ( length txs0 + length txs1, tot0)
      logValue1 :: LOContent a
      logValue1 = LogValue "txsInMempool" $ PureI $ fromIntegral (msNumTxs tot)
      logValue2 :: LOContent a
      logValue2 = LogValue "mempoolBytes" $ PureI $ fromIntegral (msNumBytes tot)
  meta <- mkLOMeta Critical Confidential
  traceNamedObject tr' (meta, logValue1)
  traceNamedObject tr' (meta, logValue2)

mempoolTracer
  :: ( Show (ApplyTxErr blk)
     , ToJSON (GenTxId blk)
     , ToObject (ApplyTxErr blk)
     , ToObject (GenTx blk)
     )
  => TraceSelection
  -> Trace IO Text
  -> IORef BlockchainCounters
  -> Tracer IO (TraceEventMempool blk)
mempoolTracer tc tracer bChainCounters = Tracer $ \ev -> do
    traceWith (mempoolMetricsTraceTransformer tracer) ev
    traceWith (notifyTxsProcessed bChainCounters tracer) ev
    traceWith (measureTxsStart tracer) ev
    let tr = appendName "Mempool" tracer
    traceWith (mpTracer tc tr) ev

mpTracer :: ( Show (ApplyTxErr blk)
            , ToJSON (GenTxId blk)
            , ToObject (ApplyTxErr blk)
            , ToObject (GenTx blk)
            )
         => TraceSelection -> Trace IO Text -> Tracer IO (TraceEventMempool blk)
mpTracer tc tr = annotateSeverity $ toLogObject' (traceVerbosity tc) tr

--------------------------------------------------------------------------------
-- ForgeStateInfo Tracers
--------------------------------------------------------------------------------

forgeStateInfoMetricsTraceTransformer
  :: forall a blk. HasKESMetricsData blk
  => Proxy blk
  -> Trace IO a
  -> Tracer IO (ForgeStateInfo blk)
forgeStateInfoMetricsTraceTransformer p tr = Tracer $ \forgeStateInfo -> do
    case getKESMetricsData p forgeStateInfo of
      NoKESMetricsData -> pure ()
      TPraosKESMetricsData kesPeriodOfKey
                           (MaxKESEvolutions maxKesEvos)
                           (OperationalCertStartKESPeriod oCertStartKesPeriod) -> do
        let metricsTr = appendName "metrics" tr

            -- The KES period of the hot key is relative to the start KES
            -- period of the operational certificate.
            currentKesPeriod = oCertStartKesPeriod + kesPeriodOfKey

            oCertExpiryKesPeriod = oCertStartKesPeriod + fromIntegral maxKesEvos

            kesPeriodsUntilExpiry =
              max 0 (oCertExpiryKesPeriod - currentKesPeriod)

            logValues :: [LOContent a]
            logValues =
              [ LogValue "operationalCertificateStartKESPeriod"
                  $ PureI
                  $ fromIntegral oCertStartKesPeriod
              , LogValue "operationalCertificateExpiryKESPeriod"
                  $ PureI
                  $ fromIntegral oCertExpiryKesPeriod
              , LogValue "currentKESPeriod"
                  $ PureI
                  $ fromIntegral currentKesPeriod
              , LogValue "remainingKESPeriods"
                  $ PureI
                  $ fromIntegral kesPeriodsUntilExpiry
              ]

        meta <- mkLOMeta Critical Confidential
        mapM_ (traceNamedObject metricsTr . (meta,)) logValues

        -- Trace warning messages on the last 7 KES periods and, in the
        -- final and subsequent KES periods, trace alert messages.
        metaWarning <- mkLOMeta Warning Public
        metaAlert <- mkLOMeta Alert Public
        when (kesPeriodsUntilExpiry <= 7) $
          traceWith tr
            ( mempty
            , LogObject
                mempty
                (if kesPeriodsUntilExpiry <= 1 then metaAlert else metaWarning)
                (LogStructuredText mempty (expiryLogMessage kesPeriodsUntilExpiry))
            )
  where
    expiryLogMessage :: Word -> Text
    expiryLogMessage kesPeriodsUntilExpiry =
      "Operational key will expire in "
        <> (Text.pack . show) kesPeriodsUntilExpiry
        <> " KES periods."

forgeStateInfoTracer
  :: forall blk.
     ( HasKESMetricsData blk
     , Show (ForgeStateInfo blk)
     )
  => Proxy blk
  -> TraceSelection
  -> Trace IO Text
  -> Tracer IO (ForgeStateInfo blk)
forgeStateInfoTracer p _ts tracer = Tracer $ \ev -> do
    let tr = appendName "Forge" tracer
    traceWith (forgeStateInfoMetricsTraceTransformer p tr) ev
    traceWith (fsTracer tr) ev
  where
    fsTracer :: Trace IO Text -> Tracer IO (ForgeStateInfo blk)
    fsTracer tr = showTracing $ contramap Text.pack $ toLogObject tr

--------------------------------------------------------------------------------
-- NodeToClient Tracers
--------------------------------------------------------------------------------

nodeToClientTracers'
  :: Show localPeer
  => TraceSelection
  -> TracingVerbosity
  -> Trace IO Text
  -> NodeToClient.Tracers' localPeer blk DeserialiseFailure (Tracer IO)
nodeToClientTracers' trSel verb tr =
  NodeToClient.Tracers
  { NodeToClient.tChainSyncTracer =
    tracerOnOff (traceLocalChainSyncProtocol trSel) verb "LocalChainSyncProtocol" tr
  , NodeToClient.tTxSubmissionTracer =
    tracerOnOff (traceLocalTxSubmissionProtocol trSel) verb "LocalTxSubmissionProtocol" tr
  , NodeToClient.tStateQueryTracer =
    tracerOnOff (traceLocalStateQueryProtocol trSel) verb "LocalStateQueryProtocol" tr
  }

--------------------------------------------------------------------------------
-- NodeToNode Tracers
--------------------------------------------------------------------------------

nodeToNodeTracers'
  :: ( Consensus.RunNode blk
     , ConvertTxId blk
     , HasTxs blk
     , Show peer
     )
  => TraceSelection
  -> TracingVerbosity
  -> Trace IO Text
  -> NodeToNode.Tracers' peer blk DeserialiseFailure (Tracer IO)
nodeToNodeTracers' trSel verb tr =
  NodeToNode.Tracers
  { NodeToNode.tChainSyncTracer = tracerOnOff (traceChainSyncProtocol trSel) verb "ChainSyncProtocol" tr
  , NodeToNode.tChainSyncSerialisedTracer = showOnOff (traceChainSyncProtocol trSel) "ChainSyncProtocolSerialised" tr
  , NodeToNode.tBlockFetchTracer = tracerOnOff (traceBlockFetchProtocol trSel) verb "BlockFetchProtocol" tr
  , NodeToNode.tBlockFetchSerialisedTracer = showOnOff (traceBlockFetchProtocolSerialised trSel) "BlockFetchProtocolSerialised" tr
  , NodeToNode.tTxSubmissionTracer = tracerOnOff (traceTxSubmissionProtocol trSel) verb "TxSubmissionProtocol" tr
  }

teeTraceBlockFetchDecision
    :: ( Eq peer
       , HasHeader blk
       , Show peer
       )
    => TracingVerbosity
    -> MVar (Maybe (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])]),Integer)
    -> Trace IO Text
    -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
teeTraceBlockFetchDecision verb eliding tr =
  Tracer $ \ev -> do
    traceWith (teeTraceBlockFetchDecision' tr) ev
    traceWith (teeTraceBlockFetchDecisionElide verb eliding tr) ev


teeTraceBlockFetchDecision'
    :: Trace IO Text
    -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
teeTraceBlockFetchDecision' tr =
    Tracer $ \(WithSeverity _ peers) -> do
      meta <- mkLOMeta Info Confidential
      let tr' = appendName "peers" tr
      traceNamedObject tr' (meta, LogValue "connectedPeers" . PureI $ fromIntegral $ length peers)

teeTraceBlockFetchDecisionElide
    :: ( Eq peer
       , HasHeader blk
       , Show peer
       )
    => TracingVerbosity
    -> MVar (Maybe (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])]),Integer)
    -> Trace IO Text
    -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
teeTraceBlockFetchDecisionElide = elideToLogObject


-- | get information about a chain fragment

data ChainInformation = ChainInformation
  { slots :: Word64
  , blocks :: Word64
  , density :: Rational
    -- ^ the actual number of blocks created over the maximum expected number
    -- of blocks that could be created over the span of the last @k@ blocks.
  , epoch :: EpochNo
    -- ^ In which epoch is the tip of the current chain
  , slotInEpoch :: Word64
    -- ^ Relative slot number of the tip of the current chain within the
    -- epoch.
  }

chainInformation
  :: forall blk. HasHeader (Header blk)
  => ChainDB.NewTipInfo blk
  -> AF.AnchoredFragment (Header blk)
  -> ChainInformation
chainInformation newTipInfo frag = ChainInformation
    { slots       = slotN
    , blocks      = blockN
    , density     = calcDensity blockD slotD
    , epoch       = ChainDB.newTipEpoch newTipInfo
    , slotInEpoch = ChainDB.newTipSlotInEpoch newTipInfo
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


--------------------------------------------------------------------------------
-- Trace Helpers
--------------------------------------------------------------------------------

readableTraceBlockchainTimeEvent :: TraceBlockchainTimeEvent -> Text
readableTraceBlockchainTimeEvent ev = case ev of
    TraceStartTimeInTheFuture (SystemStart start) toWait ->
      "Waiting " <> (Text.pack . show) toWait <> " until genesis start time at " <> (Text.pack . show) start
    TraceCurrentSlotUnknown time _ ->
      "Too far from the chain tip to determine the current slot number for the time "
       <> (Text.pack . show) time

traceCounter
  :: Text
  -> Word64
  -> Trace IO Text
  -> IO ()
traceCounter logValueName aCounter tracer = do
  meta <- mkLOMeta Notice Public
  traceNamedObject (appendName "metrics" tracer)
                   (meta, LogValue logValueName (PureI $ fromIntegral aCounter))

tracerOnOff
  :: Transformable Text IO a
  => Bool -> TracingVerbosity -> LoggerName -> Trace IO Text -> Tracer IO a
tracerOnOff False _ _ _ = nullTracer
tracerOnOff True verb name trcer = annotateSeverity
                                $ toLogObject' verb
                                $ appendName name trcer

tracerOnOff'
  :: Bool -> Tracer IO a -> Tracer IO a
tracerOnOff' False _ = nullTracer
tracerOnOff' True tr = tr

instance Show a => Show (WithSeverity a) where
  show (WithSeverity _sev a) = show a

showOnOff
  :: (Show a, HasSeverityAnnotation a)
  => Bool -> LoggerName -> Trace IO Text -> Tracer IO a
showOnOff False _ _ = nullTracer
showOnOff True name trcer = annotateSeverity
                                $ showTracing
                                $ withName name trcer

withName :: Text -> Trace IO Text -> Tracer IO String
withName name tr = contramap Text.pack $ toLogObject $ appendName name tr
