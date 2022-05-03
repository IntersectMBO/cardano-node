{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Tracing.OrphanInstances.Network () where

import           Cardano.Prelude hiding (group, show)
import           Prelude (String, id, show)

import           Control.Monad.Class.MonadTime (DiffTime, Time (..))
import           Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (listValue)
import qualified Data.IP as IP
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text (pack)

import           Network.TypedProtocol.Codec (AnyMessageAndAgency (..))
import           Network.TypedProtocol.Core (PeerHasAgency (..))


import           Network.Mux (MiniProtocolNum (..), MuxTrace (..), WithMuxBearer (..))
import           Network.Socket (SockAddr (..))

import           Cardano.Node.Configuration.TopologyP2P (UseLedger (..))
import           Cardano.Node.Queries (ConvertTxId)
import           Cardano.Tracing.OrphanInstances.Common
import           Cardano.Tracing.Render

import           Ouroboros.Consensus.Block (ConvertRawHash (..), Header, getHeader)
import           Ouroboros.Consensus.Ledger.Query (BlockQuery, Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx, GenTxId,
                   HasTxs (..), TxId, txId)
import           Ouroboros.Consensus.Node.Run (RunNode, estimateBlockSize)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch.ClientState (TraceFetchClientState,
                   TraceLabelPeer (..))
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision, FetchDecline (..))
import           Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace (..))
import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.Types (AbstractState (..),
                   ConnectionManagerCounters (..), ConnectionManagerTrace (..),
                   OperationResult (..))
import qualified Ouroboros.Network.ConnectionManager.Types as ConnMgr
import           Ouroboros.Network.DeltaQ (GSV (..), PeerGSV (..))
import           Ouroboros.Network.Driver.Limits (ProtocolLimitFailure (..))
import           Ouroboros.Network.InboundGovernor (InboundGovernorTrace (..), RemoteSt (..))
import qualified Ouroboros.Network.InboundGovernor as InboundGovernor
import           Ouroboros.Network.InboundGovernor.State (InboundGovernorCounters (..))
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.NodeToClient (NodeToClientVersion, NodeToClientVersionData (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import           Ouroboros.Network.NodeToNode (ErrorPolicyTrace (..), NodeToNodeVersion,
                   NodeToNodeVersionData (..), RemoteAddress, TraceSendRecv (..), WithAddr (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import           Ouroboros.Network.PeerSelection.Governor (DebugPeerSelection (..),
                   PeerSelectionCounters (..), PeerSelectionState (..), PeerSelectionTargets (..),
                   TracePeerSelection (..))
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.LedgerPeers
import           Ouroboros.Network.PeerSelection.LocalRootPeers (LocalRootPeers, toGroupSets, toMap)
import           Ouroboros.Network.PeerSelection.PeerStateActions (PeerSelectionActionsTrace (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS (TraceLocalRootPeers (..),
                   TracePublicRootPeers (..))
import           Ouroboros.Network.PeerSelection.Types (PeerStatus (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch, Message (..))
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import           Ouroboros.Network.Protocol.Handshake (HandshakeException (..),
                   HandshakeProtocolError (..), RefuseReason (..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (LocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import           Ouroboros.Network.Protocol.LocalTxMonitor.Type (LocalTxMonitor)
import qualified Ouroboros.Network.Protocol.LocalTxMonitor.Type as LocalTxMonitor
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (LocalTxSubmission)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LocalTxSub
import           Ouroboros.Network.Protocol.TxSubmission2.Type as TxSubmission2
import           Ouroboros.Network.RethrowPolicy (ErrorCommand (..))
import           Ouroboros.Network.Server2 (ServerTrace (..))
import qualified Ouroboros.Network.Server2 as Server
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.Subscription (ConnectResult (..), DnsTrace (..),
                   SubscriberError (..), SubscriptionTrace (..), WithDomainName (..),
                   WithIPList (..))
import           Ouroboros.Network.TxSubmission.Inbound (ProcessedTxCount (..),
                   TraceTxSubmissionInbound (..))
import           Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound (..))

import qualified Ouroboros.Network.Diffusion as ND

{- HLINT ignore "Use record patterns" -}

--
-- * instances of @HasPrivacyAnnotation@ and @HasSeverityAnnotation@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance HasPrivacyAnnotation (ND.InitializationTracer ntnAddr ntcAddr)
instance HasSeverityAnnotation (ND.InitializationTracer ntnAddr ntcAddr) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (NtC.HandshakeTr LocalAddress NodeToClientVersion)
instance HasSeverityAnnotation (NtC.HandshakeTr LocalAddress NodeToClientVersion) where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation (NtN.HandshakeTr RemoteAddress NodeToNodeVersion)
instance HasSeverityAnnotation (NtN.HandshakeTr RemoteAddress NodeToNodeVersion) where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation NtN.AcceptConnectionsPolicyTrace
instance HasSeverityAnnotation NtN.AcceptConnectionsPolicyTrace where
  getSeverityAnnotation NtN.ServerTraceAcceptConnectionRateLimiting {} = Info
  getSeverityAnnotation NtN.ServerTraceAcceptConnectionHardLimit {} = Warning
  getSeverityAnnotation NtN.ServerTraceAcceptConnectionResume {} = Info


instance HasPrivacyAnnotation (TraceFetchClientState header)
instance HasSeverityAnnotation (TraceFetchClientState header) where
  getSeverityAnnotation BlockFetch.AddedFetchRequest {} = Info
  getSeverityAnnotation BlockFetch.SendFetchRequest {} = Info
  getSeverityAnnotation BlockFetch.AcknowledgedFetchRequest {} = Info
  getSeverityAnnotation BlockFetch.StartedFetchBatch {} = Info
  getSeverityAnnotation BlockFetch.CompletedBlockFetch {} = Info
  getSeverityAnnotation BlockFetch.CompletedFetchBatch {} = Info
  getSeverityAnnotation BlockFetch.RejectedFetchBatch {} = Info
  getSeverityAnnotation BlockFetch.ClientTerminating {} = Notice


instance HasPrivacyAnnotation (TraceSendRecv a)
instance HasSeverityAnnotation (TraceSendRecv a) where
  getSeverityAnnotation _ = Debug


instance HasPrivacyAnnotation a => HasPrivacyAnnotation (TraceLabelPeer peer a)
instance HasSeverityAnnotation a => HasSeverityAnnotation (TraceLabelPeer peer a) where
  getSeverityAnnotation (TraceLabelPeer _p a) = getSeverityAnnotation a


instance HasPrivacyAnnotation [TraceLabelPeer peer (FetchDecision [Point header])]
instance HasSeverityAnnotation [TraceLabelPeer peer (FetchDecision [Point header])] where
  getSeverityAnnotation [] = Debug
  getSeverityAnnotation xs =
      maximum $ map (\(TraceLabelPeer _ a) -> fetchDecisionSeverity a) xs
    where
      fetchDecisionSeverity :: FetchDecision a -> Severity
      fetchDecisionSeverity fd =
        case fd of
          Left FetchDeclineChainNotPlausible     -> Debug
          Left FetchDeclineChainNoIntersection   -> Notice
          Left FetchDeclineAlreadyFetched        -> Debug
          Left FetchDeclineInFlightThisPeer      -> Debug
          Left FetchDeclineInFlightOtherPeer     -> Debug
          Left FetchDeclinePeerShutdown          -> Info
          Left FetchDeclinePeerSlow              -> Info
          Left FetchDeclineReqsInFlightLimit {}  -> Info
          Left FetchDeclineBytesInFlightLimit {} -> Info
          Left FetchDeclinePeerBusy {}           -> Info
          Left FetchDeclineConcurrencyLimit {}   -> Info
          Right _                                -> Info


instance HasPrivacyAnnotation (TraceTxSubmissionInbound txid tx)
instance HasSeverityAnnotation (TraceTxSubmissionInbound txid tx) where
  getSeverityAnnotation TraceTxSubmissionCollected {} = Debug
  getSeverityAnnotation TraceTxSubmissionProcessed {} = Debug
  getSeverityAnnotation TraceTxInboundTerminated = Notice
  getSeverityAnnotation TraceTxInboundCannotRequestMoreTxs {} = Debug
  getSeverityAnnotation TraceTxInboundCanRequestMoreTxs {} = Debug


instance HasPrivacyAnnotation (TraceTxSubmissionOutbound txid tx)
instance HasSeverityAnnotation (TraceTxSubmissionOutbound txid tx) where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation (TraceKeepAliveClient remotePeer)
instance HasSeverityAnnotation (TraceKeepAliveClient remotePeer) where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation TraceLedgerPeers
instance HasSeverityAnnotation TraceLedgerPeers where
  getSeverityAnnotation ev =
    case ev of
      PickedPeer {}                  -> Debug
      PickedPeers {}                 -> Info
      FetchingNewLedgerState {}      -> Info
      DisabledLedgerPeers {}         -> Info
      TraceUseLedgerAfter {}         -> Info
      WaitingOnRequest {}            -> Debug
      RequestForPeers {}             -> Debug
      ReusingLedgerState {}          -> Debug
      FallingBackToBootstrapPeers {} -> Info


instance HasPrivacyAnnotation (WithAddr addr ErrorPolicyTrace)
instance HasSeverityAnnotation (WithAddr addr ErrorPolicyTrace) where
  getSeverityAnnotation (WithAddr _ ev) = case ev of
    ErrorPolicySuspendPeer {} -> Warning -- peer misbehaved
    ErrorPolicySuspendConsumer {} -> Notice -- peer temporarily not useful
    ErrorPolicyLocalNodeError {} -> Error
    ErrorPolicyResumePeer {} -> Debug
    ErrorPolicyKeepSuspended {} -> Debug
    ErrorPolicyResumeConsumer {} -> Debug
    ErrorPolicyResumeProducer {} -> Debug
    ErrorPolicyUnhandledApplicationException {} -> Error
    ErrorPolicyUnhandledConnectionException {} -> Error
    ErrorPolicyAcceptException {} -> Error


instance HasPrivacyAnnotation (WithDomainName DnsTrace)
instance HasSeverityAnnotation (WithDomainName DnsTrace) where
  getSeverityAnnotation (WithDomainName _ ev) = case ev of
    DnsTraceLookupException {} -> Error
    DnsTraceLookupAError {} -> Error
    DnsTraceLookupAAAAError {} -> Error
    DnsTraceLookupIPv6First -> Debug
    DnsTraceLookupIPv4First -> Debug
    DnsTraceLookupAResult {} -> Debug
    DnsTraceLookupAAAAResult {} -> Debug


instance HasPrivacyAnnotation (WithDomainName (SubscriptionTrace SockAddr))
instance HasSeverityAnnotation (WithDomainName (SubscriptionTrace SockAddr)) where
  getSeverityAnnotation (WithDomainName _ ev) = case ev of
    SubscriptionTraceConnectStart {} -> Notice
    SubscriptionTraceConnectEnd {} -> Notice
    SubscriptionTraceConnectException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Info
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Warning
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Debug
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Debug
    SubscriptionTraceConnectionExist {} -> Info
    SubscriptionTraceUnsupportedRemoteAddr {} -> Warning
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Debug


instance HasPrivacyAnnotation (WithIPList (SubscriptionTrace SockAddr))
instance HasSeverityAnnotation (WithIPList (SubscriptionTrace SockAddr)) where
  getSeverityAnnotation (WithIPList _ _ ev) = case ev of
    SubscriptionTraceConnectStart _ -> Info
    SubscriptionTraceConnectEnd _ connectResult -> case connectResult of
      ConnectSuccess -> Info
      ConnectSuccessLast -> Notice
      ConnectValencyExceeded -> Warning
    SubscriptionTraceConnectException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Info
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Error
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Notice
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Info
    SubscriptionTraceConnectionExist {} -> Notice
    SubscriptionTraceUnsupportedRemoteAddr {} -> Error
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Info


instance HasPrivacyAnnotation (Identity (SubscriptionTrace LocalAddress))
instance HasSeverityAnnotation (Identity (SubscriptionTrace LocalAddress)) where
  getSeverityAnnotation (Identity ev) = case ev of
    SubscriptionTraceConnectStart {} -> Notice
    SubscriptionTraceConnectEnd {} -> Notice
    SubscriptionTraceConnectException {} -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Notice
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Notice
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Warning
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Debug
    SubscriptionTraceStart {} -> Notice
    SubscriptionTraceRestart {} -> Notice
    SubscriptionTraceConnectionExist {} -> Debug
    SubscriptionTraceUnsupportedRemoteAddr {} -> Warning
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException {} -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Debug


instance Transformable Text IO (Identity (SubscriptionTrace LocalAddress)) where
  trTransformer = trStructuredText
instance HasTextFormatter (Identity (SubscriptionTrace LocalAddress)) where
  formatText a _ = pack (show a)


instance ToObject (Identity (SubscriptionTrace LocalAddress)) where
  toObject _verb (Identity ev) =
    mconcat [ "kind" .= ("SubscriptionTrace" :: String)
             , "event" .= show ev
             ]


instance HasPrivacyAnnotation (WithMuxBearer peer MuxTrace)
instance HasSeverityAnnotation (WithMuxBearer peer MuxTrace) where
  getSeverityAnnotation (WithMuxBearer _ ev) = case ev of
    MuxTraceRecvHeaderStart -> Debug
    MuxTraceRecvHeaderEnd {} -> Debug
    MuxTraceRecvStart {} -> Debug
    MuxTraceRecvEnd {} -> Debug
    MuxTraceSendStart {} -> Debug
    MuxTraceSendEnd -> Debug
    MuxTraceState {} -> Info
    MuxTraceCleanExit {} -> Notice
    MuxTraceExceptionExit {} -> Notice
    MuxTraceChannelRecvStart {} -> Debug
    MuxTraceChannelRecvEnd {} -> Debug
    MuxTraceChannelSendStart {} -> Debug
    MuxTraceChannelSendEnd {} -> Debug
    MuxTraceHandshakeStart -> Debug
    MuxTraceHandshakeClientEnd {} -> Info
    MuxTraceHandshakeServerEnd -> Debug
    MuxTraceHandshakeClientError {} -> Error
    MuxTraceHandshakeServerError {} -> Error
    MuxTraceRecvDeltaQObservation {} -> Debug
    MuxTraceRecvDeltaQSample {} -> Debug
    MuxTraceSDUReadTimeoutException -> Notice
    MuxTraceSDUWriteTimeoutException -> Notice
    MuxTraceStartEagerly _ _ -> Info
    MuxTraceStartOnDemand _ _ -> Info
    MuxTraceStartedOnDemand _ _ -> Info
    MuxTraceShutdown -> Debug
    MuxTraceTerminating {} -> Debug
    MuxTraceTCPInfo {} -> Debug

instance HasPrivacyAnnotation (TraceLocalRootPeers RemoteAddress exception)
instance HasSeverityAnnotation (TraceLocalRootPeers RemoteAddress exception) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation TracePublicRootPeers
instance HasSeverityAnnotation TracePublicRootPeers where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (TracePeerSelection addr)
instance HasSeverityAnnotation (TracePeerSelection addr) where
  getSeverityAnnotation ev =
    case ev of
      TraceLocalRootPeersChanged {} -> Notice
      TraceTargetsChanged        {} -> Notice
      TracePublicRootsRequest    {} -> Info
      TracePublicRootsResults    {} -> Info
      TracePublicRootsFailure    {} -> Error
      TraceGossipRequests        {} -> Debug
      TraceGossipResults         {} -> Debug
      TraceForgetColdPeers       {} -> Info
      TracePromoteColdPeers      {} -> Info
      TracePromoteColdLocalPeers {} -> Info
      TracePromoteColdFailed     {} -> Info
      TracePromoteColdDone       {} -> Info
      TracePromoteWarmPeers      {} -> Info
      TracePromoteWarmLocalPeers {} -> Info
      TracePromoteWarmFailed     {} -> Info
      TracePromoteWarmDone       {} -> Info
      TracePromoteWarmAborted    {} -> Info
      TraceDemoteWarmPeers       {} -> Info
      TraceDemoteWarmFailed      {} -> Info
      TraceDemoteWarmDone        {} -> Info
      TraceDemoteHotPeers        {} -> Info
      TraceDemoteLocalHotPeers   {} -> Info
      TraceDemoteHotFailed       {} -> Info
      TraceDemoteHotDone         {} -> Info
      TraceDemoteAsynchronous    {} -> Info
      TraceGovernorWakeup        {} -> Info
      TraceChurnWait             {} -> Info
      TraceChurnMode             {} -> Info

instance HasPrivacyAnnotation (DebugPeerSelection addr conn)
instance HasSeverityAnnotation (DebugPeerSelection addr conn) where
  getSeverityAnnotation _ = Debug

instance HasPrivacyAnnotation (PeerSelectionActionsTrace SockAddr)
instance HasSeverityAnnotation (PeerSelectionActionsTrace SockAddr) where
  getSeverityAnnotation ev =
   case ev of
     PeerStatusChanged {}       -> Info
     PeerStatusChangeFailure {} -> Error
     PeerMonitoringError {}     -> Error
     PeerMonitoringResult {}    -> Debug

instance HasPrivacyAnnotation PeerSelectionCounters
instance HasSeverityAnnotation PeerSelectionCounters where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (ConnectionManagerTrace addr connTrace)
instance HasSeverityAnnotation (ConnectionManagerTrace addr (ConnectionHandlerTrace versionNumber agreedOptions)) where
  getSeverityAnnotation ev =
    case ev of
      TrIncludeConnection {}                  -> Debug
      TrUnregisterConnection {}               -> Debug
      TrConnect {}                            -> Debug
      TrConnectError {}                       -> Info
      TrTerminatingConnection {}              -> Debug
      TrTerminatedConnection {}               -> Debug
      TrConnectionHandler _ ev'     ->
        case ev' of
          TrHandshakeSuccess {}               -> Info
          TrHandshakeClientError {}           -> Notice
          TrHandshakeServerError {}           -> Info
          TrError _ _ ShutdownNode            -> Critical
          TrError _ _ ShutdownPeer            -> Info

      TrShutdown                              -> Info
      TrConnectionExists {}                   -> Info
      TrForbiddenConnection {}                -> Info
      TrImpossibleConnection {}               -> Info
      TrConnectionFailure {}                  -> Info
      TrConnectionNotFound {}                 -> Debug
      TrForbiddenOperation {}                 -> Info

      TrPruneConnections {}                   -> Notice
      TrConnectionCleanup {}                  -> Debug
      TrConnectionTimeWait {}                 -> Debug
      TrConnectionTimeWaitDone {}             -> Debug
      TrConnectionManagerCounters {}          -> Info
      TrState {}                              -> Info
      ConnMgr.TrUnexpectedlyFalseAssertion {} -> Error
      TrUnknownConnection {}                  -> Debug

instance HasPrivacyAnnotation (ConnMgr.AbstractTransitionTrace addr)
instance HasSeverityAnnotation (ConnMgr.AbstractTransitionTrace addr) where
  getSeverityAnnotation _ = Debug

instance HasPrivacyAnnotation (ServerTrace addr)
instance HasSeverityAnnotation (ServerTrace addr) where
  getSeverityAnnotation ev =
    case ev of
      Server.TrAcceptConnection {}                      -> Debug
      Server.TrAcceptError {}                           -> Error
      Server.TrAcceptPolicyTrace {}                     -> Notice
      Server.TrServerStarted {}                         -> Notice
      Server.TrServerStopped {}                         -> Notice
      Server.TrServerError {}                           -> Critical

instance HasPrivacyAnnotation (InboundGovernorTrace addr)
instance HasSeverityAnnotation (InboundGovernorTrace addr) where
  getSeverityAnnotation ev =
    case ev of
      InboundGovernor.TrNewConnection {}           -> Debug
      InboundGovernor.TrResponderRestarted {}      -> Debug
      InboundGovernor.TrResponderStartFailure {}   -> Error
      InboundGovernor.TrResponderErrored {}        -> Info
      InboundGovernor.TrResponderStarted {}        -> Debug
      InboundGovernor.TrResponderTerminated {}     -> Debug
      InboundGovernor.TrPromotedToWarmRemote {}    -> Info
      InboundGovernor.TrPromotedToHotRemote {}     -> Info
      InboundGovernor.TrDemotedToColdRemote {}     -> Info
      InboundGovernor.TrDemotedToWarmRemote {}     -> Info
      InboundGovernor.TrWaitIdleRemote {}          -> Debug
      InboundGovernor.TrMuxCleanExit {}            -> Debug
      InboundGovernor.TrMuxErrored {}              -> Info
      InboundGovernor.TrInboundGovernorCounters {} -> Info
      InboundGovernor.TrRemoteState {}             -> Debug
      InboundGovernor.TrUnexpectedlyFalseAssertion {}
                                                   -> Error
      InboundGovernor.TrInboundGovernorError {}    -> Error

instance HasPrivacyAnnotation (Server.RemoteTransitionTrace addr)
instance HasSeverityAnnotation (Server.RemoteTransitionTrace addr) where
  getSeverityAnnotation _ = Debug

--
-- | instances of @Transformable@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance Transformable Text IO (ND.InitializationTracer RemoteAddress LocalAddress) where
  trTransformer = trStructuredText
instance HasTextFormatter (ND.InitializationTracer RemoteAddress LocalAddress) where
  formatText a _ = pack (show a)

instance Transformable Text IO (NtN.HandshakeTr RemoteAddress NodeToNodeVersion) where
  trTransformer = trStructuredText
instance HasTextFormatter (NtN.HandshakeTr RemoteAddress NodeToNodeVersion) where
  formatText a _ = pack (show a)


instance Transformable Text IO (NtC.HandshakeTr LocalAddress NodeToClientVersion) where
  trTransformer = trStructuredText
instance HasTextFormatter (NtC.HandshakeTr LocalAddress NodeToClientVersion) where
  formatText a _ = pack (show a)


instance Transformable Text IO NtN.AcceptConnectionsPolicyTrace where
  trTransformer = trStructuredText
instance HasTextFormatter NtN.AcceptConnectionsPolicyTrace where
  formatText a _ = pack (show a)


instance (StandardHash header, Show peer, ToObject peer)
      => Transformable Text IO [TraceLabelPeer peer (FetchDecision [Point header])] where
  trTransformer = trStructuredText
instance (StandardHash header, Show peer)
      => HasTextFormatter [TraceLabelPeer peer (FetchDecision [Point header])] where
  formatText a _ = pack (show a)

instance (HasHeader header, ConvertRawHash header, ToObject peer)
     => Transformable Text IO (TraceLabelPeer peer (TraceFetchClientState header)) where
  trTransformer = trStructured
instance (Show header, StandardHash header, Show peer)
     => HasTextFormatter (TraceLabelPeer peer (TraceFetchClientState header)) where
  formatText a _ = pack (show a)

instance ToObject peer
     => Transformable Text IO (TraceLabelPeer peer (NtN.TraceSendRecv (ChainSync (Header blk) (Point blk) (Tip blk)))) where
  trTransformer = trStructured
instance (Show peer, StandardHash blk, Show (Header blk))
     => HasTextFormatter (TraceLabelPeer peer (NtN.TraceSendRecv (ChainSync (Header blk) (Point blk) (Tip blk)))) where
  formatText a _ = pack (show a)

instance (ToObject peer, ToObject (AnyMessageAndAgency (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))))
     => Transformable Text IO (TraceLabelPeer peer (NtN.TraceSendRecv (TraceTxSubmissionInbound  (GenTxId blk) (GenTx blk)))) where
  trTransformer = trStructured

instance ToObject peer
     => Transformable Text IO (TraceLabelPeer peer (TraceTxSubmissionInbound  (GenTxId blk) (GenTx blk))) where
  trTransformer = trStructured

instance (ToObject peer, ConvertTxId blk, RunNode blk, HasTxs blk)
     => Transformable Text IO (TraceLabelPeer peer (NtN.TraceSendRecv (BlockFetch blk (Point blk)))) where
  trTransformer = trStructured

instance ToObject localPeer
     => Transformable Text IO (TraceLabelPeer localPeer (NtN.TraceSendRecv (ChainSync (Serialised blk) (Point blk) (Tip blk)))) where
  trTransformer = trStructured

instance (applyTxErr ~ ApplyTxErr blk, ToObject localPeer)
     => Transformable Text IO (TraceLabelPeer localPeer (NtN.TraceSendRecv (LocalTxMonitor (GenTxId blk) (GenTx blk) SlotNo))) where
  trTransformer = trStructured

instance (applyTxErr ~ ApplyTxErr blk, ToObject localPeer)
     => Transformable Text IO (TraceLabelPeer localPeer (NtN.TraceSendRecv (LocalTxSubmission (GenTx blk) applyTxErr))) where
  trTransformer = trStructured

instance (StandardHash blk, LocalStateQuery.ShowQuery (BlockQuery blk), ToObject localPeer)
     => Transformable Text IO (TraceLabelPeer localPeer (NtN.TraceSendRecv (LocalStateQuery blk (Point blk) (Query blk)))) where
  trTransformer = trStructured

instance (ToObject peer, Show (TxId (GenTx blk)), Show (GenTx blk))
     => Transformable Text IO (TraceLabelPeer peer (NtN.TraceSendRecv (TxSubmission2 (GenTxId blk) (GenTx blk)))) where
  trTransformer = trStructured

instance (ToObject peer, Show (TxId (GenTx blk)), Show (GenTx blk))
     => Transformable Text IO (TraceLabelPeer peer (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))) where
  trTransformer = trStructured

instance Transformable Text IO (TraceTxSubmissionInbound txid tx) where
  trTransformer = trStructuredText
instance HasTextFormatter (TraceTxSubmissionInbound txid tx) where
  formatText a _ = pack (show a)


instance (Show tx, Show txid)
      => Transformable Text IO (TraceTxSubmissionOutbound txid tx) where
  trTransformer = trStructuredText
instance (Show tx, Show txid)
      => HasTextFormatter (TraceTxSubmissionOutbound txid tx) where
  formatText a _ = pack (show a)


instance Show addr
      => Transformable Text IO (TraceKeepAliveClient addr) where
  trTransformer = trStructuredText
instance Show addr
      => HasTextFormatter (TraceKeepAliveClient addr) where
    formatText a _ = pack (show a)


instance Transformable Text IO TraceLedgerPeers where
  trTransformer = trStructuredText
instance HasTextFormatter TraceLedgerPeers where
  formatText _ = pack . show . toList


instance Show addr => Transformable Text IO (WithAddr addr ErrorPolicyTrace) where
  trTransformer = trStructuredText
instance Show addr => HasTextFormatter (WithAddr addr ErrorPolicyTrace) where
  formatText a _ = pack (show a)


instance Transformable Text IO (WithDomainName (SubscriptionTrace SockAddr)) where
  trTransformer = trStructuredText
instance HasTextFormatter (WithDomainName (SubscriptionTrace SockAddr)) where
  formatText a _ = pack (show a)


instance Transformable Text IO (WithDomainName DnsTrace) where
  trTransformer = trStructuredText
instance HasTextFormatter (WithDomainName DnsTrace) where
  formatText a _ = pack (show a)


instance Transformable Text IO (WithIPList (SubscriptionTrace SockAddr)) where
  trTransformer = trStructuredText
instance HasTextFormatter (WithIPList (SubscriptionTrace SockAddr)) where
  formatText a _ = pack (show a)


instance (Show peer, ToObject peer)
      => Transformable Text IO (WithMuxBearer peer MuxTrace) where
  trTransformer = trStructuredText
instance (Show peer)
      => HasTextFormatter (WithMuxBearer peer MuxTrace) where
  formatText (WithMuxBearer peer ev) = \_o ->
        "Bearer on " <> pack (show peer)
     <> " event: " <> pack (show ev)


instance Show exception => Transformable Text IO (TraceLocalRootPeers RemoteAddress exception) where
  trTransformer = trStructuredText
instance Show exception => HasTextFormatter (TraceLocalRootPeers RemoteAddress exception) where
    formatText a _ = pack (show a)

instance Transformable Text IO TracePublicRootPeers where
  trTransformer = trStructuredText
instance HasTextFormatter TracePublicRootPeers where
  formatText a _ = pack (show a)

instance Transformable Text IO (TracePeerSelection SockAddr) where
  trTransformer = trStructuredText
instance HasTextFormatter (TracePeerSelection SockAddr) where
  formatText a _ = pack (show a)

instance Show conn
      => Transformable Text IO (DebugPeerSelection SockAddr conn) where
  trTransformer = trStructuredText
instance HasTextFormatter (DebugPeerSelection SockAddr conn) where
  -- One can only change what is logged with respect to verbosity using json
  -- format.
  formatText _ obj = pack (show obj)

instance Transformable Text IO (PeerSelectionActionsTrace SockAddr) where
  trTransformer = trStructuredText
instance HasTextFormatter (PeerSelectionActionsTrace SockAddr) where
  formatText a _ = pack (show a)

instance Transformable Text IO PeerSelectionCounters where
  trTransformer = trStructuredText
instance HasTextFormatter PeerSelectionCounters where
  formatText a _ = pack (show a)

instance (Show addr, Show versionNumber, Show agreedOptions, ToObject addr,
          ToJSON addr, ToJSON versionNumber, ToJSON agreedOptions
         )
      => Transformable Text IO (ConnectionManagerTrace
                                 addr
                                 (ConnectionHandlerTrace versionNumber agreedOptions)) where
  trTransformer = trStructuredText
instance (Show addr, Show versionNumber, Show agreedOptions)
      => HasTextFormatter (ConnectionManagerTrace
                            addr
                            (ConnectionHandlerTrace versionNumber agreedOptions)) where
  formatText a _ = pack (show a)

instance (Show addr, ToJSON addr, ToObject addr)
      => Transformable Text IO (ConnMgr.AbstractTransitionTrace addr) where
  trTransformer = trStructuredText
instance Show addr
      => HasTextFormatter (ConnMgr.AbstractTransitionTrace addr) where
  formatText a _ = pack (show a)

instance (Show addr, ToObject addr, ToJSON addr)
      => Transformable Text IO (ServerTrace addr) where
  trTransformer = trStructuredText
instance Show addr
      => HasTextFormatter (ServerTrace addr) where
  formatText a _ = pack (show a)

instance (ToJSON addr, Show addr)
      => Transformable Text IO (InboundGovernorTrace addr) where
  trTransformer = trStructuredText
instance Show addr
      => HasTextFormatter (InboundGovernorTrace addr) where
  formatText a _ = pack (show a)

instance (Show addr, ToJSON addr)
      => Transformable Text IO (Server.RemoteTransitionTrace addr) where
  trTransformer = trStructuredText
instance Show addr
      => HasTextFormatter (Server.RemoteTransitionTrace addr) where
  formatText a _ = pack (show a)

--
-- | instances of @ToObject@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance ( ConvertTxId blk
         , RunNode blk
         , HasTxs blk
         )
      => ToObject (AnyMessageAndAgency (BlockFetch blk (Point blk))) where
  toObject MinimalVerbosity (AnyMessageAndAgency stok (MsgBlock blk)) =
    mconcat [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
             , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
             , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             ]

  toObject verb (AnyMessageAndAgency stok (MsgBlock blk)) =
    mconcat [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
             , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
             , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             , "txIds" .= toJSON (presentTx <$> extractTxs blk)
             ]
      where
        presentTx :: GenTx blk -> Value
        presentTx =  String . renderTxIdForVerbosity verb . txId

  toObject _v (AnyMessageAndAgency stok MsgRequestRange{}) =
    mconcat [ "kind" .= String "MsgRequestRange"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _v (AnyMessageAndAgency stok MsgStartBatch{}) =
    mconcat [ "kind" .= String "MsgStartBatch"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _v (AnyMessageAndAgency stok MsgNoBlocks{}) =
    mconcat [ "kind" .= String "MsgNoBlocks"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _v (AnyMessageAndAgency stok MsgBatchDone{}) =
    mconcat [ "kind" .= String "MsgBatchDone"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _v (AnyMessageAndAgency stok MsgClientDone{}) =
    mconcat [ "kind" .= String "MsgClientDone"
             , "agency" .= String (pack $ show stok)
             ]

instance LocalStateQuery.ShowQuery query
      => ToObject (AnyMessageAndAgency (LocalStateQuery blk pt query)) where
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgAcquire{}) =
    mconcat [ "kind" .= String "MsgAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgAcquired{}) =
    mconcat [ "kind" .= String "MsgAcquired"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgFailure{}) =
    mconcat [ "kind" .= String "MsgFailure"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgQuery{}) =
    mconcat [ "kind" .= String "MsgQuery"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgResult{}) =
    mconcat [ "kind" .= String "MsgResult"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgRelease{}) =
    mconcat [ "kind" .= String "MsgRelease"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgReAcquire{}) =
    mconcat [ "kind" .= String "MsgReAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgDone{}) =
    mconcat [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

instance ToObject (AnyMessageAndAgency (LocalTxMonitor txid tx slotno)) where
  toObject _verb (AnyMessageAndAgency stok LocalTxMonitor.MsgAcquire {}) =
    mconcat [ "kind" .= String "MsgAcuire"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxMonitor.MsgAcquired {}) =
    mconcat [ "kind" .= String "MsgAcuired"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxMonitor.MsgAwaitAcquire {}) =
    mconcat [ "kind" .= String "MsgAwaitAcuire"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxMonitor.MsgNextTx {}) =
    mconcat [ "kind" .= String "MsgNextTx"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxMonitor.MsgReplyNextTx {}) =
    mconcat [ "kind" .= String "MsgReplyNextTx"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxMonitor.MsgHasTx {}) =
    mconcat [ "kind" .= String "MsgHasTx"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxMonitor.MsgReplyHasTx {}) =
    mconcat [ "kind" .= String "MsgReplyHasTx"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxMonitor.MsgGetSizes {}) =
    mconcat [ "kind" .= String "MsgGetSizes"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxMonitor.MsgReplyGetSizes {}) =
    mconcat [ "kind" .= String "MsgReplyGetSizes"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxMonitor.MsgRelease {}) =
    mconcat [ "kind" .= String "MsgRelease"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxMonitor.MsgDone {}) =
    mconcat [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

instance ToObject (AnyMessageAndAgency (LocalTxSubmission tx err)) where
  toObject _verb (AnyMessageAndAgency stok LocalTxSub.MsgSubmitTx{}) =
    mconcat [ "kind" .= String "MsgSubmitTx"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxSub.MsgAcceptTx{}) =
    mconcat [ "kind" .= String "MsgAcceptTx"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxSub.MsgRejectTx{}) =
    mconcat [ "kind" .= String "MsgRejectTx"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxSub.MsgDone{}) =
    mconcat [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

instance ToObject (AnyMessageAndAgency (ChainSync blk pt tip)) where
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgRequestNext{}) =
     mconcat [ "kind" .= String "MsgRequestNext"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgAwaitReply{}) =
     mconcat [ "kind" .= String "MsgAwaitReply"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgRollForward{}) =
     mconcat [ "kind" .= String "MsgRollForward"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgRollBackward{}) =
     mconcat [ "kind" .= String "MsgRollBackward"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgFindIntersect{}) =
     mconcat [ "kind" .= String "MsgFindIntersect"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgIntersectFound{}) =
     mconcat [ "kind" .= String "MsgIntersectFound"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgIntersectNotFound{}) =
     mconcat [ "kind" .= String "MsgIntersectNotFound"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgDone{}) =
     mconcat [ "kind" .= String "MsgDone"
              , "agency" .= String (pack $ show stok)
              ]

instance (Show txid, Show tx)
      => ToObject (AnyMessageAndAgency (TxSubmission2 txid tx)) where
  toObject _verb (AnyMessageAndAgency stok MsgInit) =
    mconcat
      [ "kind" .= String "MsgInit"
      , "agency" .= String (pack $ show stok)
      ]
  toObject _verb (AnyMessageAndAgency stok (MsgRequestTxs txids)) =
    mconcat
      [ "kind" .= String "MsgRequestTxs"
      , "agency" .= String (pack $ show stok)
      , "txIds" .= String (pack $ show txids)
      ]
  toObject _verb (AnyMessageAndAgency stok (MsgReplyTxs txs)) =
    mconcat
      [ "kind" .= String "MsgReplyTxs"
      , "agency" .= String (pack $ show stok)
      , "txs" .= String (pack $ show txs)
      ]
  toObject _verb (AnyMessageAndAgency stok MsgRequestTxIds{}) =
    mconcat
      [ "kind" .= String "MsgRequestTxIds"
      , "agency" .= String (pack $ show stok)
      ]
  toObject _verb (AnyMessageAndAgency stok (MsgReplyTxIds _)) =
    mconcat
      [ "kind" .= String "MsgReplyTxIds"
      , "agency" .= String (pack $ show stok)
      ]
  toObject _verb (AnyMessageAndAgency stok MsgDone) =
    mconcat
      [ "kind" .= String "MsgDone"
      , "agency" .= String (pack $ show stok)
      ]

instance ToJSON peerAddr => ToJSON (ConnectionId peerAddr) where
  toJSON ConnectionId { localAddress, remoteAddress } =
    Aeson.object [ "localAddress"  .= toJSON localAddress
                 , "remoteAddress" .= toJSON remoteAddress
                 ]

instance Aeson.ToJSON ConnectionManagerCounters where
  toJSON ConnectionManagerCounters { fullDuplexConns
                                   , duplexConns
                                   , unidirectionalConns
                                   , inboundConns
                                   , outboundConns
                                   } =
    Aeson.object [ "fullDuplex"     .= toJSON fullDuplexConns
                 , "duplex"         .= toJSON duplexConns
                 , "unidirectional" .= toJSON unidirectionalConns
                 , "inbound"        .= inboundConns
                 , "outbound"       .= outboundConns
                 ]

instance ToObject (FetchDecision [Point header]) where
  toObject _verb (Left decline) =
    mconcat [ "kind" .= String "FetchDecision declined"
             , "declined" .= String (pack (show decline))
             ]
  toObject _verb (Right results) =
    mconcat [ "kind" .= String "FetchDecision results"
             , "length" .= String (pack $ show $ length results)
             ]

-- TODO: use 'ToJSON' constraints
instance (Show ntnAddr, Show ntcAddr) => ToObject (ND.InitializationTracer ntnAddr ntcAddr) where
  toObject _verb (ND.RunServer sockAddr) = mconcat
    [ "kind" .= String "RunServer"
    , "socketAddress" .= String (pack (show sockAddr))
    ]

  toObject _verb (ND.RunLocalServer localAddress) = mconcat
    [ "kind" .= String "RunLocalServer"
    , "localAddress" .= String (pack (show localAddress))
    ]
  toObject _verb (ND.UsingSystemdSocket localAddress) = mconcat
    [ "kind" .= String "UsingSystemdSocket"
    , "path" .= String (pack . show $ localAddress)
    ]

  toObject _verb (ND.CreateSystemdSocketForSnocketPath localAddress) = mconcat
    [ "kind" .= String "CreateSystemdSocketForSnocketPath"
    , "path" .= String (pack . show $ localAddress)
    ]
  toObject _verb (ND.CreatedLocalSocket localAddress) = mconcat
    [ "kind" .= String "CreatedLocalSocket"
    , "path" .= String (pack . show $ localAddress)
    ]
  toObject _verb (ND.ConfiguringLocalSocket localAddress socket) = mconcat
    [ "kind" .= String "ConfiguringLocalSocket"
    , "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.ListeningLocalSocket localAddress socket) = mconcat
    [ "kind" .= String "ListeningLocalSocket"
    , "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.LocalSocketUp localAddress fd) = mconcat
    [ "kind" .= String "LocalSocketUp"
    , "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show fd))
    ]
  toObject _verb (ND.CreatingServerSocket socket) = mconcat
    [ "kind" .= String "CreatingServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.ListeningServerSocket socket) = mconcat
    [ "kind" .= String "ListeningServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.ServerSocketUp socket) = mconcat
    [ "kind" .= String "ServerSocketUp"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.ConfiguringServerSocket socket) = mconcat
    [ "kind" .= String "ConfiguringServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.UnsupportedLocalSystemdSocket path) = mconcat
    [ "kind" .= String "UnsupportedLocalSystemdSocket"
    , "path" .= String (pack (show path))
    ]
  toObject _verb ND.UnsupportedReadySocketCase = mconcat
    [ "kind" .= String "UnsupportedReadySocketCase"
    ]
  toObject _verb (ND.DiffusionErrored exception) = mconcat
    [ "kind" .= String "DiffusionErrored"
    , "path" .= String (pack (show exception))
    ]

instance ToObject (NtC.HandshakeTr LocalAddress NodeToClientVersion) where
  toObject _verb (WithMuxBearer b ev) =
    mconcat [ "kind" .= String "LocalHandshakeTrace"
             , "bearer" .= show b
             , "event" .= show ev ]


instance ToObject (NtN.HandshakeTr RemoteAddress NodeToNodeVersion) where
  toObject _verb (WithMuxBearer b ev) =
    mconcat [ "kind" .= String "HandshakeTrace"
             , "bearer" .= show b
             , "event" .= show ev ]

instance ToJSON LocalAddress where
    toJSON (LocalAddress path) = String (pack path)

instance Aeson.ToJSONKey LocalAddress where

instance ToObject NtN.AcceptConnectionsPolicyTrace where
  toObject _verb (NtN.ServerTraceAcceptConnectionRateLimiting delay numOfConnections) =
    mconcat [ "kind" .= String "ServerTraceAcceptConnectionRateLimiting"
             , "delay" .= show delay
             , "numberOfConnection" .= show numOfConnections
             ]
  toObject _verb (NtN.ServerTraceAcceptConnectionHardLimit softLimit) =
    mconcat [ "kind" .= String "ServerTraceAcceptConnectionHardLimit"
             , "softLimit" .= show softLimit
             ]
  toObject _verb (NtN.ServerTraceAcceptConnectionResume numOfConnections) =
    mconcat [ "kind" .= String "ServerTraceAcceptConnectionResume"
             , "numberOfConnection" .= show numOfConnections
             ]


instance ConvertRawHash blk
      => ToObject (Point blk) where
  toObject _verb GenesisPoint =
    mconcat
      [ "kind" .= String "GenesisPoint" ]
  toObject verb (BlockPoint slot h) =
    mconcat
      [ "kind" .= String "BlockPoint"
      , "slot" .= toJSON (unSlotNo slot)
      , "headerHash" .= renderHeaderHashForVerbosity (Proxy @blk) verb h
      ]


instance ToObject SlotNo where
  toObject _verb slot =
    mconcat [ "kind" .= String "SlotNo"
             , "slot" .= toJSON (unSlotNo slot) ]

instance (HasHeader header, ConvertRawHash header)
  => ToObject (TraceFetchClientState header) where
  toObject _verb BlockFetch.AddedFetchRequest {} =
    mconcat [ "kind" .= String "AddedFetchRequest" ]
  toObject _verb BlockFetch.AcknowledgedFetchRequest {} =
    mconcat [ "kind" .= String "AcknowledgedFetchRequest" ]
  toObject _verb (BlockFetch.SendFetchRequest af) =
    mconcat [ "kind" .= String "SendFetchRequest"
             , "head" .= String (renderChainHash
                                  (renderHeaderHash (Proxy @header))
                                  (AF.headHash af))
             , "length" .= toJSON (fragmentLength af)]
   where
     -- NOTE: this ignores the Byron era with its EBB complication:
     -- the length would be underestimated by 1, if the AF is anchored
     -- at the epoch boundary.
     fragmentLength :: AF.AnchoredFragment header -> Int
     fragmentLength f = fromIntegral . unBlockNo $
        case (f, f) of
          (AS.Empty{}, AS.Empty{}) -> 0
          (firstHdr AS.:< _, _ AS.:> lastHdr) ->
            blockNo lastHdr - blockNo firstHdr + 1
  toObject _verb (BlockFetch.CompletedBlockFetch pt _ _ _ delay blockSize) =
    mconcat [ "kind"  .= String "CompletedBlockFetch"
             , "delay" .= (realToFrac delay :: Double)
             , "size"  .= blockSize
             , "block" .= String
               (case pt of
                  GenesisPoint -> "Genesis"
                  BlockPoint _ h -> renderHeaderHash (Proxy @header) h)
             ]
  toObject _verb BlockFetch.CompletedFetchBatch {} =
    mconcat [ "kind" .= String "CompletedFetchBatch" ]
  toObject _verb BlockFetch.StartedFetchBatch {} =
    mconcat [ "kind" .= String "StartedFetchBatch" ]
  toObject _verb BlockFetch.RejectedFetchBatch {} =
    mconcat [ "kind" .= String "RejectedFetchBatch" ]
  toObject _verb (BlockFetch.ClientTerminating outstanding) =
    mconcat [ "kind" .= String "ClientTerminating"
             , "outstanding" .= outstanding
             ]


instance (ToObject peer)
      => ToObject [TraceLabelPeer peer (FetchDecision [Point header])] where
  toObject MinimalVerbosity _ = mempty
  toObject _ [] = mempty
  toObject _ xs = mconcat
    [ "kind"  .= String "PeersFetch"
    , "peers" .= toJSON
      (foldl' (\acc x -> toObject MaximalVerbosity x : acc) [] xs) ]

instance (ToObject peer, ToObject a) => ToObject (TraceLabelPeer peer a) where
  toObject verb (TraceLabelPeer peerid a) =
    mconcat [ "peer" .= toObject verb peerid ] <> toObject verb a


instance ToObject (AnyMessageAndAgency ps)
      => ToObject (TraceSendRecv ps) where
  toObject verb (TraceSendMsg m) = mconcat
    [ "kind" .= String "Send" , "msg" .= toObject verb m ]
  toObject verb (TraceRecvMsg m) = mconcat
    [ "kind" .= String "Recv" , "msg" .= toObject verb m ]


instance ToObject (TraceTxSubmissionInbound txid tx) where
  toObject _verb (TraceTxSubmissionCollected count) =
    mconcat
      [ "kind" .= String "TxSubmissionCollected"
      , "count" .= toJSON count
      ]
  toObject _verb (TraceTxSubmissionProcessed processed) =
    mconcat
      [ "kind" .= String "TxSubmissionProcessed"
      , "accepted" .= toJSON (ptxcAccepted processed)
      , "rejected" .= toJSON (ptxcRejected processed)
      ]
  toObject _verb TraceTxInboundTerminated =
    mconcat
      [ "kind" .= String "TxInboundTerminated"
      ]
  toObject _verb (TraceTxInboundCanRequestMoreTxs count) =
    mconcat
      [ "kind" .= String "TxInboundCanRequestMoreTxs"
      , "count" .= toJSON count
      ]
  toObject _verb (TraceTxInboundCannotRequestMoreTxs count) =
    mconcat
      [ "kind" .= String "TxInboundCannotRequestMoreTxs"
      , "count" .= toJSON count
      ]


instance Aeson.ToJSONKey SockAddr where

instance Aeson.ToJSON SockAddr where
    toJSON (SockAddrInet port addr) =
        let ip = IP.fromHostAddress addr in
        Aeson.object [ "address" .= toJSON ip
                     , "port" .= show port
                     ]
    toJSON (SockAddrInet6 port _ addr _) =
        let ip = IP.fromHostAddress6 addr in
        Aeson.object [ "address" .= toJSON ip
                     , "port" .= show port
                     ]
    toJSON (SockAddrUnix path) =
        Aeson.object [ "socketPath" .= show path ]

-- TODO: use the json encoding of transactions
instance (Show txid, Show tx)
      => ToObject (TraceTxSubmissionOutbound txid tx) where
  toObject MaximalVerbosity (TraceTxSubmissionOutboundRecvMsgRequestTxs txids) =
    mconcat
      [ "kind" .= String "TxSubmissionOutboundRecvMsgRequestTxs"
      , "txIds" .= String (pack $ show txids)
      ]
  toObject _verb (TraceTxSubmissionOutboundRecvMsgRequestTxs _txids) =
    mconcat
      [ "kind" .= String "TxSubmissionOutboundRecvMsgRequestTxs"
      ]
  toObject MaximalVerbosity (TraceTxSubmissionOutboundSendMsgReplyTxs txs) =
    mconcat
      [ "kind" .= String "TxSubmissionOutboundSendMsgReplyTxs"
      , "txs" .= String (pack $ show txs)
      ]
  toObject _verb (TraceTxSubmissionOutboundSendMsgReplyTxs _txs) =
    mconcat
      [ "kind" .= String "TxSubmissionOutboundSendMsgReplyTxs"
      ]
  toObject _verb (TraceControlMessage controlMessage) =
    mconcat
      [ "kind" .= String "ControlMessage"
      , "controlMessage" .= String (pack $ show controlMessage)
      ]


instance Show remotePeer => ToObject (TraceKeepAliveClient remotePeer) where
  toObject _verb (AddSample peer rtt pgsv) =
    mconcat
      [ "kind" .= String "KeepAliveClient AddSample"
      , "address" .= show peer
      , "rtt" .= rtt
      , "sampleTime" .= show (dTime $ sampleTime pgsv)
      , "outboundG" .= (realToFrac $ gGSV (outboundGSV pgsv) :: Double)
      , "inboundG" .= (realToFrac $ gGSV (inboundGSV pgsv) :: Double)
      ]
    where
      gGSV :: GSV -> DiffTime
      gGSV (GSV g _ _) = g

      dTime :: Time -> Double
      dTime (Time d) = realToFrac d


instance ToObject TraceLedgerPeers where
  toObject _verb (PickedPeer addr _ackStake stake) =
    mconcat
      [ "kind" .= String "PickedPeer"
      , "address" .= show addr
      , "relativeStake" .= (realToFrac (unPoolStake stake) :: Double)
      ]
  toObject _verb (PickedPeers (NumberOfPeers n) addrs) =
    mconcat
      [ "kind" .= String "PickedPeers"
      , "desiredCount" .= n
      , "count" .= length addrs
      , "addresses" .= show addrs
      ]
  toObject _verb (FetchingNewLedgerState cnt) =
    mconcat
      [ "kind" .= String "FetchingNewLedgerState"
      , "numberOfPools" .= cnt
      ]
  toObject _verb DisabledLedgerPeers =
    mconcat
      [ "kind" .= String "DisabledLedgerPeers"
      ]
  toObject _verb (TraceUseLedgerAfter ula) =
    mconcat
      [ "kind" .= String "UseLedgerAfter"
      , "useLedgerAfter" .= UseLedger ula
      ]
  toObject _verb WaitingOnRequest =
    mconcat
      [ "kind" .= String "WaitingOnRequest"
      ]
  toObject _verb (RequestForPeers (NumberOfPeers np)) =
    mconcat
      [ "kind" .= String "RequestForPeers"
      , "numberOfPeers" .= np
      ]
  toObject _verb (ReusingLedgerState cnt age) =
    mconcat
      [ "kind" .= String "ReusingLedgerState"
      , "numberOfPools" .= cnt
      , "ledgerStateAge" .= age
      ]
  toObject _verb FallingBackToBootstrapPeers =
    mconcat
      [ "kind" .= String "FallingBackToBootstrapPeers"
      ]


instance Show addr => ToObject (WithAddr addr ErrorPolicyTrace) where
  toObject _verb (WithAddr addr ev) =
    mconcat [ "kind" .= String "ErrorPolicyTrace"
             , "address" .= show addr
             , "event" .= show ev ]


instance ToObject (WithIPList (SubscriptionTrace SockAddr)) where
  toObject _verb (WithIPList localAddresses dests ev) =
    mconcat [ "kind" .= String "WithIPList SubscriptionTrace"
             , "localAddresses" .= show localAddresses
             , "dests" .= show dests
             , "event" .= show ev ]


instance ToObject (WithDomainName DnsTrace) where
  toObject _verb (WithDomainName dom ev) =
    mconcat [ "kind" .= String "DnsTrace"
             , "domain" .= show dom
             , "event" .= show ev ]


instance ToObject (WithDomainName (SubscriptionTrace SockAddr)) where
  toObject _verb (WithDomainName dom ev) =
    mconcat [ "kind" .= String "SubscriptionTrace"
             , "domain" .= show dom
             , "event" .= show ev ]


instance ToObject peer => ToObject (WithMuxBearer peer MuxTrace) where
  toObject verb (WithMuxBearer b ev) =
    mconcat [ "kind" .= String "MuxTrace"
             , "bearer" .= toObject verb b
             , "event" .= show ev ]

instance Aeson.ToJSONKey RelayAccessPoint where

instance Show exception => ToObject (TraceLocalRootPeers RemoteAddress exception) where
  toObject _verb (TraceLocalRootDomains groups) =
    mconcat [ "kind" .= String "LocalRootDomains"
             , "localRootDomains" .= toJSON groups
             ]
  toObject _verb (TraceLocalRootWaiting d dt) =
    mconcat [ "kind" .= String "LocalRootWaiting"
             , "domainAddress" .= toJSON d
             , "diffTime" .= show dt
             ]
  toObject _verb (TraceLocalRootResult d res) =
    mconcat [ "kind" .= String "LocalRootResult"
             , "domainAddress" .= toJSON d
             , "result" .= Aeson.toJSONList res
             ]
  toObject _verb (TraceLocalRootGroups groups) =
    mconcat [ "kind" .= String "LocalRootGroups"
             , "localRootGroups" .= toJSON groups
             ]
  toObject _verb (TraceLocalRootFailure d dexception) =
    mconcat [ "kind" .= String "LocalRootFailure"
             , "domainAddress" .= toJSON d
             , "reason" .= show dexception
             ]
  toObject _verb (TraceLocalRootError d dexception) =
    mconcat [ "kind" .= String "LocalRootError"
             , "domainAddress" .= toJSON d
             , "reason" .= show dexception
             ]

instance ToJSON IP where
  toJSON ip = Aeson.object ["ip" .= String (pack . show $ ip)]

instance ToObject TracePublicRootPeers where
  toObject _verb (TracePublicRootRelayAccessPoint relays) =
    mconcat [ "kind" .= String "PublicRootRelayAddresses"
             , "relayAddresses" .= Aeson.toJSONList relays
             ]
  toObject _verb (TracePublicRootDomains domains) =
    mconcat [ "kind" .= String "PublicRootDomains"
             , "domainAddresses" .= Aeson.toJSONList domains
             ]
  toObject _verb (TracePublicRootResult b res) =
    mconcat [ "kind" .= String "PublicRootResult"
             , "domain" .= show b
             , "result" .= Aeson.toJSONList res
             ]
  toObject _verb (TracePublicRootFailure b d) =
    mconcat [ "kind" .= String "PublicRootFailure"
             , "domain" .= show b
             , "reason" .= show d
             ]

instance ToJSON PeerStatus where
  toJSON = String . pack . show

instance (Aeson.ToJSONKey peerAddr, ToJSON peerAddr, Show peerAddr)
  => ToJSON (LocalRootPeers peerAddr) where
  toJSON lrp =
    Aeson.object [ "kind" .= String "LocalRootPeers"
                 , "state" .= toJSON (toMap lrp)
                 , "groups" .= Aeson.toJSONList (toGroupSets lrp)
                 ]

instance ToJSON PeerSelectionTargets where
  toJSON (PeerSelectionTargets
            nRootPeers
            nKnownPeers
            nEstablishedPeers
            nActivePeers
         ) =
    Aeson.object [ "kind" .= String "PeerSelectionTargets"
                 , "targetRootPeers" .= nRootPeers
                 , "targetKnownPeers" .= nKnownPeers
                 , "targetEstablishedPeers" .= nEstablishedPeers
                 , "targetActivePeers" .= nActivePeers
                 ]

instance ToObject (TracePeerSelection SockAddr) where
  toObject _verb (TraceLocalRootPeersChanged lrp lrp') =
    mconcat [ "kind" .= String "LocalRootPeersChanged"
             , "previous" .= toJSON lrp
             , "current" .= toJSON lrp'
             ]
  toObject _verb (TraceTargetsChanged pst pst') =
    mconcat [ "kind" .= String "TargetsChanged"
             , "previous" .= toJSON pst
             , "current" .= toJSON pst'
             ]
  toObject _verb (TracePublicRootsRequest tRootPeers nRootPeers) =
    mconcat [ "kind" .= String "PublicRootsRequest"
             , "targetNumberOfRootPeers" .= tRootPeers
             , "numberOfRootPeers" .= nRootPeers
             ]
  toObject _verb (TracePublicRootsResults res group dt) =
    mconcat [ "kind" .= String "PublicRootsResults"
             , "result" .= Aeson.toJSONList (toList res)
             , "group" .= group
             , "diffTime" .= dt
             ]
  toObject _verb (TracePublicRootsFailure err group dt) =
    mconcat [ "kind" .= String "PublicRootsFailure"
             , "reason" .= show err
             , "group" .= group
             , "diffTime" .= dt
             ]
  toObject _verb (TraceGossipRequests targetKnown actualKnown aps sps) =
    mconcat [ "kind" .= String "GossipRequests"
             , "targetKnown" .= targetKnown
             , "actualKnown" .= actualKnown
             , "availablePeers" .= Aeson.toJSONList (toList aps)
             , "selectedPeers" .= Aeson.toJSONList (toList sps)
             ]
  toObject _verb (TraceGossipResults res) =
    mconcat [ "kind" .= String "GossipResults"
             , "result" .= Aeson.toJSONList (map ( bimap show id <$> ) res)
             ]
  toObject _verb (TraceForgetColdPeers targetKnown actualKnown sp) =
    mconcat [ "kind" .= String "ForgeColdPeers"
             , "targetKnown" .= targetKnown
             , "actualKnown" .= actualKnown
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TracePromoteColdPeers targetKnown actualKnown sp) =
    mconcat [ "kind" .= String "PromoteColdPeers"
             , "targetEstablished" .= targetKnown
             , "actualEstablished" .= actualKnown
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TracePromoteColdLocalPeers tLocalEst aLocalEst sp) =
    mconcat [ "kind" .= String "PromoteColdLocalPeers"
             , "targetLocalEstablished" .= tLocalEst
             , "actualLocalEstablished" .= aLocalEst
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TracePromoteColdFailed tEst aEst p d err) =
    mconcat [ "kind" .= String "PromoteColdFailed"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             , "delay" .= toJSON d
             , "reason" .= show err
             ]
  toObject _verb (TracePromoteColdDone tEst aEst p) =
    mconcat [ "kind" .= String "PromoteColdDone"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             ]
  toObject _verb (TracePromoteWarmPeers tActive aActive sp) =
    mconcat [ "kind" .= String "PromoteWarmPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TracePromoteWarmLocalPeers taa sp) =
    mconcat [ "kind" .= String "PromoteWarmLocalPeers"
             , "targetActualActive" .= Aeson.toJSONList taa
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TracePromoteWarmFailed tActive aActive p err) =
    mconcat [ "kind" .= String "PromoteWarmFailed"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  toObject _verb (TracePromoteWarmDone tActive aActive p) =
    mconcat [ "kind" .= String "PromoteWarmDone"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  toObject _verb (TracePromoteWarmAborted tActive aActive p) =
    mconcat [ "kind" .= String "PromoteWarmAborted"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  toObject _verb (TraceDemoteWarmPeers tEst aEst sp) =
    mconcat [ "kind" .= String "DemoteWarmPeers"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TraceDemoteWarmFailed tEst aEst p err) =
    mconcat [ "kind" .= String "DemoteWarmFailed"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  toObject _verb (TraceDemoteWarmDone tEst aEst p) =
    mconcat [ "kind" .= String "DemoteWarmDone"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             ]
  toObject _verb (TraceDemoteHotPeers tActive aActive sp) =
    mconcat [ "kind" .= String "DemoteHotPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TraceDemoteLocalHotPeers taa sp) =
    mconcat [ "kind" .= String "DemoteLocalHotPeers"
             , "targetActualActive" .= Aeson.toJSONList taa
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TraceDemoteHotFailed tActive aActive p err) =
    mconcat [ "kind" .= String "DemoteHotFailed"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  toObject _verb (TraceDemoteHotDone tActive aActive p) =
    mconcat [ "kind" .= String "DemoteHotDone"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  toObject _verb (TraceDemoteAsynchronous msp) =
    mconcat [ "kind" .= String "DemoteAsynchronous"
             , "state" .= toJSON msp
             ]
  toObject _verb TraceGovernorWakeup =
    mconcat [ "kind" .= String "GovernorWakeup"
             ]
  toObject _verb (TraceChurnWait dt) =
    mconcat [ "kind" .= String "ChurnWait"
             , "diffTime" .= toJSON dt
             ]
  toObject _verb (TraceChurnMode c) =
    mconcat [ "kind" .= String "ChurnMode"
             , "event" .= show c ]

-- Connection manager abstract state.  For explanation of each state see
-- <https://hydra.iohk.io/job/Cardano/ouroboros-network/native.network-docs.x86_64-linux/latest/download/2>
instance Aeson.ToJSON AbstractState where
    toJSON UnknownConnectionSt =
      Aeson.object [ "kind" .= String "UnknownConnectionSt" ]
    toJSON ReservedOutboundSt =
      Aeson.object [ "kind" .= String "ReservedOutboundSt" ]
    toJSON (UnnegotiatedSt provenance) =
      Aeson.object [ "kind" .= String "UnnegotiatedSt"
                   , "provenance" .= String (pack . show $ provenance)
                   ]
    toJSON (InboundIdleSt dataFlow) =
      Aeson.object [ "kind" .= String "InboundIdleSt"
                   , "dataFlow" .= String (pack . show $ dataFlow)
                   ]
    toJSON (InboundSt dataFlow) =
      Aeson.object [ "kind" .= String "InboundSt"
                   , "dataFlow" .= String (pack . show $ dataFlow)
                   ]
    toJSON OutboundUniSt =
      Aeson.object [ "kind" .= String "OutboundUniSt" ]
    toJSON (OutboundDupSt timeoutExpired) =
      Aeson.object [ "kind" .= String "OutboundDupSt"
                   , "timeoutState" .= String (pack . show $ timeoutExpired)
                   ]
    toJSON (OutboundIdleSt dataFlow) =
      Aeson.object [ "kind" .= String "OutboundIdleSt"
                   , "dataFlow" .= String (pack . show $ dataFlow)
                   ]
    toJSON DuplexSt =
      Aeson.object [ "kind" .= String "DuplexSt" ]
    toJSON WaitRemoteIdleSt =
      Aeson.object [ "kind" .= String "WaitRemoteIdleSt" ]
    toJSON TerminatingSt =
      Aeson.object [ "kind" .= String "TerminatingSt" ]
    toJSON TerminatedSt =
      Aeson.object [ "kind" .= String "TerminatedSt" ]


peerSelectionTargetsToObject :: PeerSelectionTargets -> Value
peerSelectionTargetsToObject
  PeerSelectionTargets { targetNumberOfRootPeers,
                         targetNumberOfKnownPeers,
                         targetNumberOfEstablishedPeers,
                         targetNumberOfActivePeers } =
    Object $
      mconcat [ "roots" .= targetNumberOfRootPeers
               , "knownPeers" .= targetNumberOfKnownPeers
               , "established" .= targetNumberOfEstablishedPeers
               , "active" .= targetNumberOfActivePeers
               ]

instance Show peerConn => ToObject (DebugPeerSelection SockAddr peerConn) where
  toObject verb (TraceGovernorState blockedAt wakeupAfter
                   PeerSelectionState { targets, knownPeers, establishedPeers, activePeers })
      | verb <= NormalVerbosity =
    mconcat [ "kind" .= String "DebugPeerSelection"
             , "blockedAt" .= String (pack $ show blockedAt)
             , "wakeupAfter" .= String (pack $ show wakeupAfter)
             , "targets" .= peerSelectionTargetsToObject targets
             , "numberOfPeers" .=
                 Object (mconcat [ "known" .= KnownPeers.size knownPeers
                                  , "established" .= EstablishedPeers.size establishedPeers
                                  , "active" .= Set.size activePeers
                                  ])
             ]
  toObject _ (TraceGovernorState blockedAt wakeupAfter ev) =
    mconcat [ "kind" .= String "DebugPeerSelection"
             , "blockedAt" .= String (pack $ show blockedAt)
             , "wakeupAfter" .= String (pack $ show wakeupAfter)
             , "peerSelectionState" .= String (pack $ show ev)
             ]

-- TODO: Write PeerStatusChangeType ToJSON at ouroboros-network
-- For that an export is needed at ouroboros-network
instance ToObject (PeerSelectionActionsTrace SockAddr) where
  toObject _verb (PeerStatusChanged ps) =
    mconcat [ "kind" .= String "PeerStatusChanged"
             , "peerStatusChangeType" .= show ps
             ]
  toObject _verb (PeerStatusChangeFailure ps f) =
    mconcat [ "kind" .= String "PeerStatusChangeFailure"
             , "peerStatusChangeType" .= show ps
             , "reason" .= show f
             ]
  toObject _verb (PeerMonitoringError connId s) =
    mconcat [ "kind" .= String "PeerMonitoridngError"
             , "connectionId" .= toJSON connId
             , "reason" .= show s
             ]
  toObject _verb (PeerMonitoringResult connId wf) =
    mconcat [ "kind" .= String "PeerMonitoringResult"
             , "connectionId" .= toJSON connId
             , "withProtocolTemp" .= show wf
             ]

instance ToObject PeerSelectionCounters where
  toObject _verb ev =
    mconcat [ "kind" .= String "PeerSelectionCounters"
             , "coldPeers" .= coldPeers ev
             , "warmPeers" .= warmPeers ev
             , "hotPeers" .= hotPeers ev
             ]

instance (Show (ClientHasAgency st), Show (ServerHasAgency st))
  => ToJSON (PeerHasAgency pr st) where
  toJSON (ClientAgency cha) =
    Aeson.object [ "kind" .= String "ClientAgency"
                 , "agency" .= show cha
                 ]
  toJSON (ServerAgency sha) =
    Aeson.object [ "kind" .= String "ServerAgency"
                 , "agency" .= show sha
                 ]

instance ToJSON ProtocolLimitFailure where
  toJSON (ExceededSizeLimit tok) =
    Aeson.object [ "kind" .= String "ProtocolLimitFailure"
                 , "agency" .= toJSON tok
                 ]
  toJSON (ExceededTimeLimit tok) =
    Aeson.object [ "kind" .= String "ProtocolLimitFailure"
                 , "agency" .= toJSON tok
                 ]

instance Show vNumber => ToJSON (RefuseReason vNumber) where
  toJSON (VersionMismatch vNumber tags) =
    Aeson.object [ "kind" .= String "VersionMismatch"
                 , "versionNumber" .= show vNumber
                 , "tags" .= Aeson.toJSONList tags
                 ]
  toJSON (HandshakeDecodeError vNumber t) =
    Aeson.object [ "kind" .= String "HandshakeDecodeError"
                 , "versionNumber" .= show vNumber
                 , "text" .= String (pack $ show t)
                 ]
  toJSON (Refused vNumber t) =
    Aeson.object [ "kind" .= String "Refused"
                 , "versionNumber" .= show vNumber
                 , "text" .= String (pack $ show t)
                 ]

instance Show vNumber => ToJSON (HandshakeProtocolError vNumber) where
  toJSON (HandshakeError rvNumber) =
    Aeson.object [ "kind" .= String "HandshakeError"
                 , "reason" .= toJSON rvNumber
                 ]
  toJSON (NotRecognisedVersion vNumber) =
    Aeson.object [ "kind" .= String "NotRecognisedVersion"
                 , "versionNumber" .= show vNumber
                 ]
  toJSON (InvalidServerSelection vNumber t) =
    Aeson.object [ "kind" .= String "InvalidServerSelection"
                 , "versionNumber" .= show vNumber
                 , "reason" .= String (pack $ show t)
                 ]

instance Show vNumber => ToJSON (HandshakeException vNumber) where
  toJSON (HandshakeProtocolLimit plf) =
    Aeson.object [ "kind" .= String "HandshakeProtocolLimit"
                 , "handshakeProtocolLimit" .= toJSON plf
                 ]
  toJSON (HandshakeProtocolError err) =
    Aeson.object [ "kind" .= String "HandshakeProtocolError"
                 , "reason" .= show err
                 ]

instance ToJSON NodeToNodeVersion where
  toJSON x = String (pack $ show x)

instance ToJSON NodeToClientVersion where
  toJSON x = String (pack $ show x)

instance ToJSON NodeToNodeVersionData where
  toJSON (NodeToNodeVersionData (NetworkMagic m) dm) =
    Aeson.object [ "kind" .= String "NodeToNodeVersionData"
                 , "networkMagic" .= toJSON m
                 , "diffusionMode" .= show dm
                 ]

instance ToJSON NodeToClientVersionData where
  toJSON (NodeToClientVersionData (NetworkMagic m)) =
    Aeson.object [ "kind" .= String "NodeToClientVersionData"
                 , "networkMagic" .= toJSON m
                 ]

instance (Show versionNumber, ToJSON versionNumber, ToJSON agreedOptions)
  => ToObject (ConnectionHandlerTrace versionNumber agreedOptions) where
  toObject _verb (TrHandshakeSuccess versionNumber agreedOptions) =
    mconcat
      [ "kind" .= String "HandshakeSuccess"
      , "versionNumber" .= toJSON versionNumber
      , "agreedOptions" .= toJSON agreedOptions
      ]
  toObject _verb (TrHandshakeClientError err) =
    mconcat
      [ "kind" .= String "HandshakeClientError"
      , "reason" .= toJSON err
      ]
  toObject _verb (TrHandshakeServerError err) =
    mconcat
      [ "kind" .= String "HandshakeServerError"
      , "reason" .= toJSON err
      ]
  toObject _verb (TrError e err cerr) =
    mconcat
      [ "kind" .= String "Error"
      , "context" .= show e
      , "reason" .= show err
      , "command" .= show cerr
      ]

instance (Show addr, Show versionNumber, Show agreedOptions, ToObject addr,
          ToJSON addr, ToJSON versionNumber, ToJSON agreedOptions)
      => ToObject (ConnectionManagerTrace addr (ConnectionHandlerTrace versionNumber agreedOptions)) where
  toObject verb ev =
    case ev of
      TrIncludeConnection prov peerAddr ->
        mconcat $ reverse
          [ "kind" .= String "IncludeConnection"
          , "remoteAddress" .= toObject verb peerAddr
          , "provenance" .= String (pack . show $ prov)
          ]
      TrUnregisterConnection prov peerAddr ->
        mconcat $ reverse
          [ "kind" .= String "UnregisterConnection"
          , "remoteAddress" .= toObject verb peerAddr
          , "provenance" .= String (pack . show $ prov)
          ]
      TrConnect (Just localAddress) remoteAddress ->
        mconcat
          [ "kind" .= String "ConnectTo"
          , "connectionId" .= toJSON ConnectionId { localAddress, remoteAddress }
          ]
      TrConnect Nothing remoteAddress ->
        mconcat
          [ "kind" .= String "ConnectTo"
          , "remoteAddress" .= toObject verb remoteAddress
          ]
      TrConnectError (Just localAddress) remoteAddress err ->
        mconcat
          [ "kind" .= String "ConnectError"
          , "connectionId" .= toJSON ConnectionId { localAddress, remoteAddress }
          , "reason" .= String (pack . show $ err)
          ]
      TrConnectError Nothing remoteAddress err ->
        mconcat
          [ "kind" .= String "ConnectError"
          , "remoteAddress" .= toObject verb remoteAddress
          , "reason" .= String (pack . show $ err)
          ]
      TrTerminatingConnection prov connId ->
        mconcat
          [ "kind" .= String "TerminatingConnection"
          , "provenance" .= String (pack . show $ prov)
          , "connectionId" .= toJSON connId
          ]
      TrTerminatedConnection prov remoteAddress ->
        mconcat
          [ "kind" .= String "TerminatedConnection"
          , "provenance" .= String (pack . show $ prov)
          , "remoteAddress" .= toObject verb remoteAddress
          ]
      TrConnectionHandler connId a ->
        mconcat
          [ "kind" .= String "ConnectionHandler"
          , "connectionId" .= toJSON connId
          , "connectionHandler" .= toObject verb a
          ]
      TrShutdown ->
        mconcat
          [ "kind" .= String "Shutdown"
          ]
      TrConnectionExists prov remoteAddress inState ->
        mconcat
          [ "kind" .= String "ConnectionExists"
          , "provenance" .= String (pack . show $ prov)
          , "remoteAddress" .= toObject verb remoteAddress
          , "state" .= toJSON inState
          ]
      TrForbiddenConnection connId ->
        mconcat
          [ "kind" .= String "ForbiddenConnection"
          , "connectionId" .= toJSON connId
          ]
      TrImpossibleConnection connId ->
        mconcat
          [ "kind" .= String "ImpossibleConnection"
          , "connectionId" .= toJSON connId
          ]
      TrConnectionFailure connId ->
        mconcat
          [ "kind" .= String "ConnectionFailure"
          , "connectionId" .= toJSON connId
          ]
      TrConnectionNotFound prov remoteAddress ->
        mconcat
          [ "kind" .= String "ConnectionNotFound"
          , "remoteAddress" .= toObject verb remoteAddress
          , "provenance" .= String (pack . show $ prov)
          ]
      TrForbiddenOperation remoteAddress connState ->
        mconcat
          [ "kind" .= String "ForbiddenOperation"
          , "remoteAddress" .= toObject verb remoteAddress
          , "connectionState" .= toJSON connState
          ]
      TrPruneConnections pruningSet numberPruned chosenPeers ->
        mconcat
          [ "kind" .= String "PruneConnections"
          , "prunedPeers" .= toJSON pruningSet
          , "numberPrunedPeers" .= toJSON numberPruned
          , "choiceSet" .= toJSON (toObject verb `Set.map` chosenPeers)
          ]
      TrConnectionCleanup connId ->
        mconcat
          [ "kind" .= String "ConnectionCleanup"
          , "connectionId" .= toJSON connId
          ]
      TrConnectionTimeWait connId ->
        mconcat
          [ "kind" .= String "ConnectionTimeWait"
          , "connectionId" .= toJSON connId
          ]
      TrConnectionTimeWaitDone connId ->
        mconcat
          [ "kind" .= String "ConnectionTimeWaitDone"
          , "connectionId" .= toJSON connId
          ]
      TrConnectionManagerCounters cmCounters ->
        mconcat
          [ "kind"  .= String "ConnectionManagerCounters"
          , "state" .= toJSON cmCounters
          ]
      TrState cmState ->
        mconcat
          [ "kind"  .= String "ConnectionManagerState"
          , "state" .= listValue (\(addr, connState) ->
                                         Aeson.object
                                           [ "remoteAddress"   .= toJSON addr
                                           , "connectionState" .= toJSON connState
                                           ])
                                       (Map.toList cmState)
          ]
      ConnMgr.TrUnexpectedlyFalseAssertion info ->
        mconcat
          [ "kind" .= String "UnexpectedlyFalseAssertion"
          , "info" .= String (pack . show $ info)
          ]
      TrUnknownConnection {} ->
        mconcat
          [ "kind" .= String "UnknownConnection"
          ]

instance ToJSON state => ToJSON (ConnMgr.MaybeUnknown state) where
    toJSON (ConnMgr.Known st) =
      Aeson.object
        [ "state" .= toJSON st
        , "type"  .= String "known"
        ]
    toJSON (ConnMgr.Race st) =
      Aeson.object
        [ "state" .= toJSON st
        , "type"  .= String "race"
        ]
    toJSON ConnMgr.Unknown =
      Aeson.object
        [ "type"  .= String "unknown" ]


instance (Show addr, ToObject addr, ToJSON addr)
      => ToObject (ConnMgr.AbstractTransitionTrace addr) where
    toObject _verb (ConnMgr.TransitionTrace addr tr) =
      mconcat [ "kind"    .= String "ConnectionManagerTransition"
               , "address" .= toJSON addr
               , "from"    .= toJSON (ConnMgr.fromState tr)
               , "to"      .= toJSON (ConnMgr.toState   tr)
               ]

instance (Show addr, ToObject addr, ToJSON addr)
      => ToObject (ServerTrace addr) where
  toObject verb (TrAcceptConnection peerAddr)     =
    mconcat [ "kind" .= String "AcceptConnection"
             , "address" .= toObject verb peerAddr
             ]
  toObject _verb (TrAcceptError exception)         =
    mconcat [ "kind" .= String "AcceptErroor"
             , "reason" .= show exception
             ]
  toObject verb (TrAcceptPolicyTrace policyTrace) =
    mconcat [ "kind" .= String "AcceptPolicyTrace"
             , "policy" .= toObject verb policyTrace
             ]
  toObject verb (TrServerStarted peerAddrs)       =
    mconcat [ "kind" .= String "AcceptPolicyTrace"
             , "addresses" .= toJSON (toObject verb `map` peerAddrs)
             ]
  toObject _verb TrServerStopped                   =
    mconcat [ "kind" .= String "ServerStopped"
             ]
  toObject _verb (TrServerError exception)         =
    mconcat [ "kind" .= String "ServerError"
             , "reason" .= show exception
             ]

instance ToJSON MiniProtocolNum where
  toJSON (MiniProtocolNum w) =
    Aeson.object [ "kind" .= String "MiniProtocolNum"
                 , "num" .= w
                 ]

instance ToJSON addr => ToJSON (OperationResult addr) where
  toJSON (UnsupportedState as) =
    Aeson.object [ "kind" .= String "UnsupportedState"
                 , "unsupportedState" .= toJSON as
                 ]
  toJSON (OperationSuccess addr) =
    Aeson.object [ "kind" .= String "OperationSuccess"
                 , "operationSuccess" .= toJSON addr
                 ]
  toJSON (TerminatedConnection as) =
    Aeson.object [ "kind" .= String "TerminatedConnection"
                 , "terminatedConnection" .= toJSON as
                 ]

instance ToJSON RemoteSt where
  toJSON = String . pack . show

instance ToJSON addr => Aeson.ToJSONKey (ConnectionId addr) where

instance ToObject NtN.RemoteAddress where
    toObject _verb (SockAddrInet port addr) =
        let ip = IP.fromHostAddress addr in
        mconcat [ "addr" .= show ip
                 , "port" .= show port
                 ]
    toObject _verb (SockAddrInet6 port _ addr _) =
        let ip = IP.fromHostAddress6 addr in
        mconcat [ "addr" .= show ip
                 , "port" .= show port
                 ]
    toObject _verb (SockAddrUnix path) =
        mconcat [ "path" .= show path ]


instance ToObject NtN.RemoteConnectionId where
    toObject verb (NtN.ConnectionId l r) =
        mconcat [ "local" .= toObject verb l
                 , "remote" .= toObject verb r
                 ]

instance ToObject LocalAddress where
    toObject _verb (LocalAddress path) =
        mconcat ["path" .= path]

instance ToObject NtC.LocalConnectionId where
    toObject verb (NtC.ConnectionId l r) =
        mconcat [ "local" .= toObject verb l
                 , "remote" .= toObject verb r
                 ]
instance (ToJSON addr, Show addr)
      => ToObject (InboundGovernorTrace addr) where
  toObject _verb (TrNewConnection p connId)            =
    mconcat [ "kind" .= String "NewConnection"
             , "provenance" .= show p
             , "connectionId" .= toJSON connId
             ]
  toObject _verb (TrResponderRestarted connId m)       =
    mconcat [ "kind" .= String "ResponderStarted"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             ]
  toObject _verb (TrResponderStartFailure connId m s)  =
    mconcat [ "kind" .= String "ResponderStartFailure"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             , "reason" .= show s
             ]
  toObject _verb (TrResponderErrored connId m s)       =
    mconcat [ "kind" .= String "ResponderErrored"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             , "reason" .= show s
             ]
  toObject _verb (TrResponderStarted connId m)         =
    mconcat [ "kind" .= String "ResponderStarted"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             ]
  toObject _verb (TrResponderTerminated connId m)      =
    mconcat [ "kind" .= String "ResponderTerminated"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             ]
  toObject _verb (TrPromotedToWarmRemote connId opRes) =
    mconcat [ "kind" .= String "PromotedToWarmRemote"
             , "connectionId" .= toJSON connId
             , "result" .= toJSON opRes
             ]
  toObject _verb (TrPromotedToHotRemote connId)        =
    mconcat [ "kind" .= String "PromotedToHotRemote"
             , "connectionId" .= toJSON connId
             ]
  toObject _verb (TrDemotedToColdRemote connId od)     =
    mconcat [ "kind" .= String "DemotedToColdRemote"
             , "connectionId" .= toJSON connId
             , "result" .= show od
             ]
  toObject _verb (TrDemotedToWarmRemote connId)     =
    mconcat [ "kind" .= String "DemotedToWarmRemote"
             , "connectionId" .= toJSON connId
             ]
  toObject _verb (TrWaitIdleRemote connId opRes) =
    mconcat [ "kind" .= String "WaitIdleRemote"
             , "connectionId" .= toJSON connId
             , "result" .= toJSON opRes
             ]
  toObject _verb (TrMuxCleanExit connId)               =
    mconcat [ "kind" .= String "MuxCleanExit"
             , "connectionId" .= toJSON connId
             ]
  toObject _verb (TrMuxErrored connId s)               =
    mconcat [ "kind" .= String "MuxErrored"
             , "connectionId" .= toJSON connId
             , "reason" .= show s
             ]
  toObject _verb (TrInboundGovernorCounters counters) =
    mconcat [ "kind" .= String "InboundGovernorCounters"
             , "idlePeers" .= idlePeersRemote counters
             , "coldPeers" .= coldPeersRemote counters
             , "warmPeers" .= warmPeersRemote counters
             , "hotPeers" .= hotPeersRemote counters
             ]
  toObject _verb (TrRemoteState st) =
    mconcat [ "kind" .= String "RemoteState"
             , "remoteSt" .= toJSON st
             ]
  toObject _verb (InboundGovernor.TrUnexpectedlyFalseAssertion info) =
    mconcat [ "kind" .= String "UnexpectedlyFalseAssertion"
             , "remoteSt" .= String (pack . show $ info)
             ]
  toObject _verb (InboundGovernor.TrInboundGovernorError err) =
    mconcat [ "kind" .= String "InboundGovernorError"
             , "remoteSt" .= String (pack . show $ err)
             ]

instance ToJSON addr
      => ToObject (Server.RemoteTransitionTrace addr) where
    toObject _verb (ConnMgr.TransitionTrace addr tr) =
      mconcat [ "kind"    .= String "InboundGovernorTransition"
               , "address" .= toJSON addr
               , "from"    .= toJSON (ConnMgr.fromState tr)
               , "to"      .= toJSON (ConnMgr.toState   tr)
               ]
