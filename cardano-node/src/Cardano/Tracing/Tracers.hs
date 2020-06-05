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

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Tracing.Tracers
  ( BlockchainCounters
  , Tracers (..)
  , TraceConstraints
  , TraceOptions
  , initialBlockchainCounters
  , mkTracers
  , nullTracers
  ) where

import           Cardano.Prelude hiding (atomically, show)
import           Prelude (String, show)

import           GHC.Clock (getMonotonicTimeNSec)
import           Control.Tracer

import           Codec.CBOR.Read (DeserialiseFailure)
import           Data.Aeson (ToJSON)
import           Data.IORef (IORef, atomicModifyIORef', readIORef)
import qualified Data.Text as Text
import           Network.Mux (MuxTrace, WithMuxBearer)
import qualified Network.Socket as Socket (SockAddr)

import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.LogItem (LOContent (..), LoggerName,
                                          PrivacyAnnotation (Confidential),
                                          mkLOMeta)
import           Cardano.BM.ElidingTracer
import           Cardano.BM.Tracing
import           Cardano.BM.Trace (traceNamedObject, appendName)
import           Cardano.BM.Data.Tracer (WithSeverity (..), annotateSeverity)
import           Cardano.BM.Data.Transformers

import           Ouroboros.Consensus.Block (BlockProtocol, ForgeState, Header, realPointSlot)
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..), TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerErr, LedgerState)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx, GenTxId, HasTxs, TxId)
import           Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Mempool.API (MempoolSize (..), TraceEventMempool (..))
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import qualified Ouroboros.Consensus.Node.Run as Consensus (RunNode)
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import           Ouroboros.Consensus.Protocol.Abstract (CannotLead, ValidationErr)
import           Ouroboros.Consensus.Util.Condense (Condense) -- This should be removed
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Point, BlockNo(..), HasHeader(..),
                                          HeaderHash, StandardHash, blockNo,
                                          pointSlot, unBlockNo, unSlotNo)
import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision, FetchDecline (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Point (fromWithOrigin, withOrigin)
import           Ouroboros.Network.Subscription

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB

import           Cardano.Config.Protocol (TraceConstraints)
import           Cardano.Config.TraceConfig
import           Cardano.Config.Types (HasKESMetricsData (..), KESMetricsData (..),
                                       MaxKESEvolutions (..), OperationalCertStartKESPeriod (..))
import           Cardano.Tracing.MicroBenchmarking

import           Control.Tracer.Transformers

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
  , ftTraceNodeCannotLead :: Trace IO Text
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
  , bcNodeIsLeaderNum :: !Word64
  , bcSlotsMissedNum  :: !Word64
  , bcForksCreatedNum :: !Word64
  }

initialBlockchainCounters :: BlockchainCounters
initialBlockchainCounters = BlockchainCounters 0 0 0 0 0

-- | Smart constructor of 'NodeTraces'.
--
mkTracers
  :: forall peer localPeer blk.
     ( Consensus.RunNode blk
     , HasKESMetricsData blk
     , TraceConstraints blk
     , Show peer, Eq peer
     , Show localPeer
     )
  => ProtocolInfo IO blk
  -> TraceOptions
  -> Trace IO Text
  -> IORef BlockchainCounters
  -> IO (Tracers peer localPeer blk)
mkTracers protoInfo traceConf' tracer bcCounters = do
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
      <*> (counting $ liftCounting staticMetaCC name "not-cannot-lead" tracer)
      <*> (counting $ liftCounting staticMetaCC name "block-from-future" tracer)
      <*> (counting $ liftCounting staticMetaCC name "slot-is-immutable" tracer)
      <*> (counting $ liftCounting staticMetaCC name "node-is-leader" tracer)

  -- prepare |Outcome|
  blockForgeOutcomeExtractor <- mkOutcomeExtractor
  elidedChainDB <- newstate  -- for eliding messages in ChainDB tracer
  elidedFetchDecision <- newstate  -- for eliding messages in FetchDecision tracer

  case traceConf' of
    TracingOn traceConf ->
      let tVerb = traceVerbosity traceConf
      in pure Tracers
                { chainDBTracer = tracerOnOff' (traceChainDB traceConf) $
                    annotateSeverity . teeTraceChainTip traceConf' bcCounters elidedChainDB $ appendName "ChainDB" tracer
                , consensusTracers
                    = mkConsensusTracers' protoInfo elidedFetchDecision blockForgeOutcomeExtractor forgeTracers tracer (TracingOn traceConf) bcCounters
                , nodeToClientTracers = nodeToClientTracers' (TracingOn traceConf) tracer
                , nodeToNodeTracers = nodeToNodeTracers' (TracingOn traceConf) tracer
                , ipSubscriptionTracer = tracerOnOff (traceIpSubscription traceConf) tVerb "IpSubscription" tracer
                , dnsSubscriptionTracer =  tracerOnOff (traceDnsSubscription traceConf) tVerb "DnsSubscription" tracer
                , dnsResolverTracer = tracerOnOff (traceDnsResolver traceConf) tVerb "DnsResolver" tracer
                , errorPolicyTracer = tracerOnOff (traceErrorPolicy traceConf) tVerb "ErrorPolicy" tracer
                , localErrorPolicyTracer = tracerOnOff (traceLocalErrorPolicy traceConf) tVerb "LocalErrorPolicy" tracer
                , acceptPolicyTracer = tracerOnOff (traceAcceptPolicy traceConf) tVerb "AcceptPolicy" tracer
                , muxTracer = tracerOnOff (traceMux traceConf) tVerb "Mux" tracer
                , handshakeTracer = tracerOnOff (traceHandshake traceConf) tVerb "Handshake" tracer
                , localHandshakeTracer = tracerOnOff (traceLocalHandshake traceConf) tVerb "LocalHandshake" tracer
                }
    TracingOff ->
      pure Tracers
        { chainDBTracer = nullTracer
        , consensusTracers = mkConsensusTracers' protoInfo elidedFetchDecision blockForgeOutcomeExtractor forgeTracers nullTracer TracingOff  bcCounters
        , nodeToClientTracers = nodeToClientTracers' TracingOff nullTracer
        , nodeToNodeTracers = nodeToNodeTracers' TracingOff nullTracer
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
  :: ( Condense (HeaderHash blk)
     , LedgerSupportsProtocol blk
     , ToObject (Header blk)
     )
  => TraceOptions
  -> IORef BlockchainCounters
  -> MVar (Maybe (WithSeverity (ChainDB.TraceEvent blk)), Integer)
  -> Trace IO Text
  -> Tracer IO (WithSeverity (ChainDB.TraceEvent blk))
teeTraceChainTip tconf bChainCounters elided tr =
  case tconf of
    TracingOff -> nullTracer
    TracingOn tSelect ->
      Tracer $ \ev -> do
       traceWith (teeTraceChainTip' tr) ev
       traceWith (notifyForkIsCreated bChainCounters tr) ev
       traceWith (teeTraceChainTipElide (traceVerbosity tSelect) elided tr) ev

teeTraceChainTipElide
  :: ( Condense (HeaderHash blk)
     , LedgerSupportsProtocol blk
     , ToObject (Header blk)
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
            ChainDB.SwitchedToAFork     newTipInfo _ newChain ->
              traceChainInformation tr (chainInformation newTipInfo newChain)
            ChainDB.AddedToCurrentChain newTipInfo _ newChain ->
              traceChainInformation tr (chainInformation newTipInfo newChain)
            _ -> pure ()
          _ -> pure ()

notifyForkIsCreated
  :: IORef BlockchainCounters
  -> Trace IO Text
  -> Tracer IO (WithSeverity (ChainDB.TraceEvent blk))
notifyForkIsCreated bcCounters tr =
    Tracer $ \(WithSeverity _ ev') ->
      case ev' of
          ChainDB.TraceAddBlockEvent ev -> case ev of
              ChainDB.SwitchedToAFork {} -> do
                  updatesForksCreated <- atomicModifyIORef' bcCounters (\cnts -> let nc = bcForksCreatedNum cnts + 1
                                                                                 in (cnts { bcForksCreatedNum = nc }, nc)
                                                                       )
                  traceCounter "forksCreatedNum" updatesForksCreated tr
              _ -> pure ()
          _ -> pure ()

--------------------------------------------------------------------------------
-- Consensus Tracers
--------------------------------------------------------------------------------

mkConsensusTracers'
  :: ( Condense (HeaderHash blk) -- to remove
     , Show peer
     , Eq peer
     , ToJSON (GenTxId blk)
     , ToObject (ApplyTxErr blk)
     , ToObject (CannotLead (BlockProtocol blk))
     , ToObject (GenTx blk)
     , ToObject (LedgerErr (LedgerState blk))
     , ToObject (OtherHeaderEnvelopeError blk)
     , ToObject (ValidationErr (BlockProtocol blk))
     , Consensus.RunNode blk
     , HasKESMetricsData blk
     )
  => ProtocolInfo IO blk
  -> MVar (Maybe (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])]),Integer)
  -> (OutcomeEnhancedTracer IO (Consensus.TraceForgeEvent blk) -> Tracer IO (Consensus.TraceForgeEvent blk))
  -> ForgeTracers
  -> Trace IO Text
  -> TraceOptions
  -> IORef BlockchainCounters
  -> Consensus.Tracers' peer localPeer blk (Tracer IO)
mkConsensusTracers' protoInfo elidingFetchDecision measureBlockForging
                    forgeTracers tracer t@(TracingOn traceConf)
                    bChainCounters =
  let tVerb = traceVerbosity traceConf
  in Consensus.Tracers
      { Consensus.chainSyncClientTracer = tracerOnOff (traceChainSyncClient traceConf) tVerb "ChainSyncClient" tracer
      , Consensus.chainSyncServerHeaderTracer = tracerOnOff (traceChainSyncHeaderServer traceConf) tVerb "ChainSyncHeaderServer" tracer
      , Consensus.chainSyncServerBlockTracer = tracerOnOff (traceChainSyncHeaderServer traceConf) tVerb "ChainSyncBlockServer" tracer
      , Consensus.blockFetchDecisionTracer = tracerOnOff' (traceBlockFetchDecisions traceConf) $
          annotateSeverity $ teeTraceBlockFetchDecision t elidingFetchDecision $ appendName "BlockFetchDecision" tracer
      , Consensus.blockFetchClientTracer = tracerOnOff (traceBlockFetchClient traceConf) tVerb "BlockFetchClient" tracer
      , Consensus.blockFetchServerTracer = tracerOnOff (traceBlockFetchServer traceConf) tVerb "BlockFetchServer" tracer
      , Consensus.forgeStateTracer = tracerOnOff' (traceForgeState traceConf) $ forgeStateTracer protoInfo traceConf tracer
      , Consensus.txInboundTracer = tracerOnOff (traceTxInbound traceConf) tVerb "TxInbound" tracer
      , Consensus.txOutboundTracer = tracerOnOff (traceTxOutbound traceConf) tVerb "TxOutbound" tracer
      , Consensus.localTxSubmissionServerTracer = tracerOnOff (traceLocalTxSubmissionServer traceConf) tVerb "LocalTxSubmissionServer" tracer
      , Consensus.mempoolTracer = tracerOnOff' (traceMempool traceConf) $ mempoolTracer traceConf tracer bChainCounters
      , Consensus.forgeTracer = tracerOnOff' (traceForge traceConf) $
          Tracer $ \ev -> do
            traceWith (forgeTracer forgeTracers t bChainCounters tracer) ev
            traceWith ( measureBlockForging
                      $ toLogObject' tracingVerbosity
                      $ appendName "ForgeTime" tracer ) ev

      , Consensus.blockchainTimeTracer = tracerOnOff' (traceBlockchainTime traceConf) $
          Tracer $ \ev ->
            traceWith (toLogObject tracer) (readableTraceBlockchainTimeEvent ev)

      }
 where
  tracingVerbosity :: TracingVerbosity
  tracingVerbosity = traceVerbosity traceConf

mkConsensusTracers' _ _ _ _ _ TracingOff _ = Consensus.Tracers
  { Consensus.chainSyncClientTracer = nullTracer
  , Consensus.chainSyncServerHeaderTracer = nullTracer
  , Consensus.chainSyncServerBlockTracer = nullTracer
  , Consensus.blockFetchDecisionTracer = nullTracer
  , Consensus.blockFetchClientTracer = nullTracer
  , Consensus.blockFetchServerTracer = nullTracer
  , Consensus.forgeStateTracer = nullTracer
  , Consensus.txInboundTracer = nullTracer
  , Consensus.txOutboundTracer = nullTracer
  , Consensus.localTxSubmissionServerTracer = nullTracer
  , Consensus.mempoolTracer = nullTracer
  , Consensus.forgeTracer = nullTracer
  , Consensus.blockchainTimeTracer = nullTracer
  }

teeForge
  :: ( Condense (HeaderHash blk)
     , Consensus.RunNode blk
     , ToObject (CannotLead (BlockProtocol blk))
     , ToObject (LedgerErr (LedgerState blk))
     , ToObject (OtherHeaderEnvelopeError blk)
     , ToObject (ValidationErr (BlockProtocol blk))
     )
  => ForgeTracers
  -> TracingVerbosity
  -> Trace IO Text
  -> Tracer IO (WithSeverity (Consensus.TraceForgeEvent blk))
teeForge ft tverb tr = Tracer $ \ev -> do
  traceWith (teeForge' tr) ev
  flip traceWith ev $ fanning $ \(WithSeverity _ e) ->
    case e of
      Consensus.TraceNodeCannotLead _ _ -> teeForge' (ftTraceNodeCannotLead ft)
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
  -> Tracer IO (WithSeverity (Consensus.TraceForgeEvent blk))
teeForge' tr =
  Tracer $ \(WithSeverity _ ev) -> do
    meta <- mkLOMeta Critical Confidential
    traceNamedObject (appendName "metrics" tr) . (meta,) $
      case ev of
        Consensus.TraceNodeCannotLead slot _reason ->
          LogValue "nodeCannotLead" $ PureI $ fromIntegral $ unSlotNo slot
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


forgeTracer
  :: ( Condense (HeaderHash blk)
     , Consensus.RunNode blk
     , ToObject (CannotLead (BlockProtocol blk))
     , ToObject (LedgerErr (LedgerState blk))
     , ToObject (OtherHeaderEnvelopeError blk)
     , ToObject (ValidationErr (BlockProtocol blk))
     )
  => ForgeTracers
  -> TraceOptions
  -> IORef BlockchainCounters
  -> Trace IO Text
  -> Tracer IO (Consensus.TraceForgeEvent blk)
forgeTracer forgeTracers tOptions bcCounters tracer =
  case tOptions of
    TracingOff -> nullTracer
    TracingOn tselect ->
     Tracer $ \ev -> do
      traceWith (measureTxsEnd tracer) ev
      traceWith (notifyBlockForging bcCounters tracer) ev
      traceWith (notifySlotsMissedIfNeeded bcCounters tracer) ev
      -- Consensus tracer
      traceWith (annotateSeverity
                   $ teeForge forgeTracers (traceVerbosity tselect)
                   $ appendName "Forge" tracer) ev


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
-- ForgeState Tracers
--------------------------------------------------------------------------------

forgeStateMetricsTraceTransformer
  :: forall a blk. HasKESMetricsData blk
  => ProtocolInfo IO blk
  -> Trace IO a
  -> Tracer IO (ForgeState blk)
forgeStateMetricsTraceTransformer protoInfo tr = Tracer $ \forgeState -> do
    case getKESMetricsData protoInfo forgeState of
      NoKESMetricsData -> pure ()
      TPraosKESMetricsData _ _ NoOperationalCertConfigured -> pure ()
      TPraosKESMetricsData kesPeriodOfKey
                           (MaxKESEvolutions maxKesEvos)
                           (OperationalCertStartKESPeriod oCertStartKesPeriod) -> do
        let tr' = appendName "metrics" tr

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
        mapM_ (traceNamedObject tr' . (meta,)) logValues

forgeStateTracer
  :: forall blk.
     ( HasKESMetricsData blk
     , Show (ForgeState blk)
     )
  => ProtocolInfo IO blk
  -> TraceSelection
  -> Trace IO Text
  -> Tracer IO (ForgeState blk)
forgeStateTracer p _ts tracer = Tracer $ \ev -> do
    let tr = appendName "Forge" tracer
    traceWith (forgeStateMetricsTraceTransformer p tr) ev
    traceWith (fsTracer tr) ev
  where
    fsTracer :: Trace IO Text -> Tracer IO (ForgeState blk)
    fsTracer tr = showTracing $ contramap Text.pack $ toLogObject tr

--------------------------------------------------------------------------------
-- NodeToClient Tracers
--------------------------------------------------------------------------------

nodeToClientTracers'
  :: Show localPeer
  => TraceOptions
  -> Trace IO Text
  -> NodeToClient.Tracers' localPeer blk DeserialiseFailure (Tracer IO)
nodeToClientTracers' (TracingOn traceConf) tracer =
  let tVerb = traceVerbosity traceConf
  in NodeToClient.Tracers
       { NodeToClient.tChainSyncTracer
         = tracerOnOff (traceLocalChainSyncProtocol traceConf) tVerb "LocalChainSyncProtocol" tracer
       , NodeToClient.tTxSubmissionTracer
         = tracerOnOff (traceLocalTxSubmissionProtocol traceConf) tVerb "LocalTxSubmissionProtocol" tracer
       , NodeToClient.tStateQueryTracer
         = tracerOnOff (traceLocalStateQueryProtocol traceConf) tVerb "LocalStateQueryProtocol" tracer
       }


nodeToClientTracers' TracingOff _ = NodeToClient.Tracers
  { NodeToClient.tChainSyncTracer = nullTracer
  , NodeToClient.tTxSubmissionTracer = nullTracer
  , NodeToClient.tStateQueryTracer = nullTracer
  }

--------------------------------------------------------------------------------
-- NodeToNode Tracers
--------------------------------------------------------------------------------

nodeToNodeTracers'
  :: ( Consensus.RunNode blk
     , Condense (HeaderHash blk)
     , Condense (TxId (GenTx blk))
     , HasTxs blk
     , Show peer
     )
  => TraceOptions
  -> Trace IO Text
  -> NodeToNode.Tracers' peer blk DeserialiseFailure (Tracer IO)
nodeToNodeTracers' (TracingOn traceConf) tracer =
  let tVerb = traceVerbosity traceConf
  in NodeToNode.Tracers
       { NodeToNode.tChainSyncTracer = tracerOnOff (traceChainSyncProtocol traceConf) tVerb "ChainSyncProtocol" tracer
       , NodeToNode.tChainSyncSerialisedTracer = showOnOff (traceChainSyncProtocol traceConf) "ChainSyncProtocolSerialised" tracer
       , NodeToNode.tBlockFetchTracer = tracerOnOff (traceBlockFetchProtocol traceConf) tVerb "BlockFetchProtocol" tracer
       , NodeToNode.tBlockFetchSerialisedTracer = showOnOff (traceBlockFetchProtocolSerialised traceConf) "BlockFetchProtocolSerialised" tracer
       , NodeToNode.tTxSubmissionTracer = tracerOnOff (traceTxSubmissionProtocol traceConf) tVerb "TxSubmissionProtocol" tracer
       }

nodeToNodeTracers' TracingOff _ = NodeToNode.Tracers
  { NodeToNode.tChainSyncTracer = nullTracer
  , NodeToNode.tChainSyncSerialisedTracer = nullTracer
  , NodeToNode.tBlockFetchTracer = nullTracer
  , NodeToNode.tBlockFetchSerialisedTracer = nullTracer
  , NodeToNode.tTxSubmissionTracer = nullTracer
  }

teeTraceBlockFetchDecision
    :: ( Eq peer
       , HasHeader blk
       , Show peer
       )
    => TraceOptions
    -> MVar (Maybe (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])]),Integer)
    -> Trace IO Text
    -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
teeTraceBlockFetchDecision traceconf eliding tr =
  case traceconf of
    TracingOff -> nullTracer
    TracingOn traceSelection ->
      Tracer $ \ev -> do
        traceWith (teeTraceBlockFetchDecision' tr) ev
        traceWith (teeTraceBlockFetchDecisionElide (traceVerbosity traceSelection) eliding tr) ev


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
