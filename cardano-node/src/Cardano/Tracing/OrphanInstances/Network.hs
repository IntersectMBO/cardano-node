{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Tracing.OrphanInstances.Network () where

import           Cardano.Prelude hiding (show)
import           Prelude (String, show)

import           Control.Monad.Class.MonadTime (DiffTime, Time (..))
import qualified Data.Set as Set
import           Data.Text (pack)

import           Network.TypedProtocol.Core (ClientHasAgency,
                     PeerHasAgency (..), ServerHasAgency)

import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket (SockAddr)

import           Cardano.Tracing.ConvertTxId (ConvertTxId)
import           Cardano.Tracing.OrphanInstances.Common
import           Cardano.Tracing.Render

import           Ouroboros.Consensus.Block (ConvertRawHash (..), getHeader)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, HasTxs (..), txId)
import           Ouroboros.Consensus.Node.Run (RunNode, estimateBlockSize)
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch.ClientState (TraceFetchClientState,
                     TraceLabelPeer (..))
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision, FetchDecline (..))
import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.Codec (AnyMessageAndAgency (..),)
import           Ouroboros.Network.DeltaQ (GSV (..), PeerGSV (..))
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import           Ouroboros.Network.NodeToNode (ErrorPolicyTrace (..), TraceSendRecv (..),
                     WithAddr (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.PeerSelection.Governor
                    (PeerSelectionState (..), PeerSelectionTargets (..),
                     DebugPeerSelection (..))
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch, Message (..))
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import           Ouroboros.Network.Protocol.Trans.Hello.Type (Hello)
import qualified Ouroboros.Network.Protocol.Trans.Hello.Type as Hello
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (LocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (LocalTxSubmission)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LocalTxSub
import           Ouroboros.Network.Protocol.TxSubmission.Type (Message (..), TxSubmission)
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.Subscription (ConnectResult (..), DnsTrace (..),
                     SubscriberError (..), SubscriptionTrace (..), WithDomainName (..),
                     WithIPList (..))
import           Ouroboros.Network.TxSubmission.Inbound (TraceTxSubmissionInbound (..))
import qualified Ouroboros.Network.TxSubmission.Inbound as TxSubmission.Inbound
import           Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound (..))
import           Ouroboros.Network.Diffusion (TraceLocalRootPeers, TracePublicRootPeers,
                   TracePeerSelection (..), PeerSelectionActionsTrace (..),
                   ConnectionManagerTrace (..), ConnectionHandlerTrace (..))
import           Ouroboros.Network.Server2 (ServerTrace)
import qualified Ouroboros.Network.Server2 as Server
import           Ouroboros.Network.RethrowPolicy (ErrorCommand (..))

import qualified Ouroboros.Network.Diffusion as ND

{- HLINT ignore "Use record patterns" -}

--
-- * instances of @HasPrivacyAnnotation@ and @HasSeverityAnnotation@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance HasPrivacyAnnotation ND.DiffusionInitializationTracer
instance HasSeverityAnnotation ND.DiffusionInitializationTracer where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation NtC.HandshakeTr
instance HasSeverityAnnotation NtC.HandshakeTr where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation NtN.HandshakeTr
instance HasSeverityAnnotation NtN.HandshakeTr where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation NtN.AcceptConnectionsPolicyTrace
instance HasSeverityAnnotation NtN.AcceptConnectionsPolicyTrace where
  getSeverityAnnotation NtN.ServerTraceAcceptConnectionRateLimiting {} = Info
  getSeverityAnnotation NtN.ServerTraceAcceptConnectionHardLimit {} = Warning


instance HasPrivacyAnnotation (TraceFetchClientState header)
instance HasSeverityAnnotation (TraceFetchClientState header) where
  getSeverityAnnotation BlockFetch.AddedFetchRequest {} = Info
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
  getSeverityAnnotation TxSubmission.Inbound.TxInboundTerminated = Notice
  getSeverityAnnotation TxSubmission.Inbound.TxInboundCannotRequestMoreTxs {} = Debug
  getSeverityAnnotation TxSubmission.Inbound.TxInboundCanRequestMoreTxs {} = Debug


instance HasPrivacyAnnotation (TraceTxSubmissionOutbound txid tx)
instance HasSeverityAnnotation (TraceTxSubmissionOutbound txid tx) where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation (TraceKeepAliveClient remotePeer)
instance HasSeverityAnnotation (TraceKeepAliveClient remotePeer) where
  getSeverityAnnotation _ = Info


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


instance HasPrivacyAnnotation (WithDomainName (SubscriptionTrace Socket.SockAddr))
instance HasSeverityAnnotation (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
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


instance HasPrivacyAnnotation (WithIPList (SubscriptionTrace Socket.SockAddr))
instance HasSeverityAnnotation (WithIPList (SubscriptionTrace Socket.SockAddr)) where
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
    mkObject [ "kind" .= ("SubscriptionTrace" :: String)
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

instance HasPrivacyAnnotation TraceLocalRootPeers
instance HasSeverityAnnotation TraceLocalRootPeers where
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
      TracePromoteColdFailed     {} -> Error
      TracePromoteColdDone       {} -> Info
      TracePromoteWarmPeers      {} -> Info
      TracePromoteWarmFailed     {} -> Error
      TracePromoteWarmDone       {} -> Info
      TraceDemoteWarmPeers       {} -> Info
      TraceDemoteWarmFailed      {} -> Error
      TraceDemoteWarmDone        {} -> Info
      TraceDemoteHotPeers        {} -> Info
      TraceDemoteHotFailed       {} -> Error
      TraceDemoteHotDone         {} -> Info
      TraceDemoteAsynchronous    {} -> Info
      TraceGovernorWakeup        {} -> Info

instance HasPrivacyAnnotation (DebugPeerSelection addr conn)
instance HasSeverityAnnotation (DebugPeerSelection addr conn) where
  getSeverityAnnotation _ = Debug

instance HasPrivacyAnnotation (PeerSelectionActionsTrace Socket.SockAddr)
instance HasSeverityAnnotation (PeerSelectionActionsTrace Socket.SockAddr) where
  getSeverityAnnotation ev =
   case ev of
     PeerStatusChanged {}       -> Info
     PeerStatusChangeFailure {} -> Error
     PeerMonitoringError {}     -> Error
     PeerMonitoringResult {}    -> Debug

instance HasPrivacyAnnotation (ConnectionManagerTrace addr connTrace)
instance HasSeverityAnnotation (ConnectionManagerTrace addr (ConnectionHandlerTrace versionNumber agreedOptions)) where
  getSeverityAnnotation ev =
    case ev of
      TrIncludeConnection {}     -> Debug
      TrUnregisterConnection {}  -> Debug
      TrIncludedConnection {}    -> Debug
      TrNegotiatedConnection {}  -> Info
      TrConnect {}               -> Debug
      TrConnectError {}          -> Warning
      TrReusedConnection {}      -> Info
      TrConnectionTerminating {} -> Debug
      TrConnectionTerminated {}  -> Debug
      TrConnectionHandler _ ev' ->
        case ev' of
          TrHandshakeSuccess {}     -> Info
          TrHandshakeClientError {} -> Error
          TrHandshakeServerError {} -> Info
          TrError ShutdownNode _ _  -> Critical
          TrError ShutdownPeer _ _  -> Info
      TrShutdown -> Info

      TrConnectionExists {}     -> Info
      TrForbiddenConnection {}  -> Info
      TrImpossibleConnection {} -> Info
      TrConnectionFailure {}    -> Info
      TrConnectionNotFound {}   -> Debug
      TrForbiddenOperation {}   -> Info

      TrDemotedToColdLocal {}   -> Debug
      TrWaitIdle {}             -> Debug
      TrPruneConnections {}     -> Notice
      TrCleanup {}              -> Debug
      TrConnectionExit {}       -> Debug

instance HasPrivacyAnnotation (ServerTrace addr)
instance HasSeverityAnnotation (ServerTrace addr) where
  getSeverityAnnotation ev =
    case ev of
      Server.TrAcceptConnection {}                      -> Debug
      Server.TrStartResponders {}                       -> Debug
      Server.TrAcceptError {}                           -> Error
      Server.TrAcceptPolicyTrace {}                     -> Notice
      Server.TrServerStarted {}                         -> Notice
      Server.TrServerStopped {}                         -> Notice
      Server.TrServerError {}                           -> Critical
      Server.TrResponderRestarted {}                    -> Debug
      Server.TrResponderTerminated {}                   -> Debug
      Server.TrResponderErrored {}                      -> Info
      Server.TrPromotedToWarmRemote {}                  -> Info
      Server.TrPromoteToWarmRemoteInUnsupportedState {} -> Error
      Server.TrDemotedToColdRemote {}                   -> Info
      Server.TrDemoteToColdRemoteInUnsupportedState {}  -> Error
      Server.TrDemotedToRemoteWaitIdle {}               -> Debug
      Server.TrPromotedToRemoteEstablished {}           -> Debug
      Server.TrEstablishedMiniProtocolTerminated {}     -> Info

--
-- | instances of @Transformable@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance Transformable Text IO ND.DiffusionInitializationTracer where
  trTransformer = trStructuredText
instance HasTextFormatter ND.DiffusionInitializationTracer where
  formatText a _ = pack (show a)

instance Transformable Text IO NtN.HandshakeTr where
  trTransformer = trStructuredText
instance HasTextFormatter NtN.HandshakeTr where
  formatText a _ = pack (show a)


instance Transformable Text IO NtC.HandshakeTr where
  trTransformer = trStructuredText
instance HasTextFormatter NtC.HandshakeTr where
  formatText a _ = pack (show a)


instance Transformable Text IO NtN.AcceptConnectionsPolicyTrace where
  trTransformer = trStructuredText
instance HasTextFormatter NtN.AcceptConnectionsPolicyTrace where
  formatText a _ = pack (show a)


instance (StandardHash header, Show peer)
      => Transformable Text IO [TraceLabelPeer peer (FetchDecision [Point header])] where
  trTransformer = trStructuredText
instance (StandardHash header, Show peer)
      => HasTextFormatter [TraceLabelPeer peer (FetchDecision [Point header])] where
  formatText a _ = pack (show a)


instance (Show peer, Show a, HasPrivacyAnnotation a, HasSeverityAnnotation a, ToObject a)
      => Transformable Text IO (TraceLabelPeer peer a) where
  trTransformer = trStructuredText
instance (Show peer, Show a)
      => HasTextFormatter (TraceLabelPeer peer a) where
  formatText a _ = pack (show a)


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


instance Show addr => Transformable Text IO (WithAddr addr ErrorPolicyTrace) where
  trTransformer = trStructuredText
instance Show addr => HasTextFormatter (WithAddr addr ErrorPolicyTrace) where
  formatText a _ = pack (show a)


instance Transformable Text IO (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  trTransformer = trStructuredText
instance HasTextFormatter (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  formatText a _ = pack (show a)


instance Transformable Text IO (WithDomainName DnsTrace) where
  trTransformer = trStructuredText
instance HasTextFormatter (WithDomainName DnsTrace) where
  formatText a _ = pack (show a)


instance Transformable Text IO (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  trTransformer = trStructuredText
instance HasTextFormatter (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  formatText a _ = pack (show a)


instance (Show peer, ToObject peer)
      => Transformable Text IO (WithMuxBearer peer MuxTrace) where
  trTransformer = trStructuredText
instance (Show peer)
      => HasTextFormatter (WithMuxBearer peer MuxTrace) where
  formatText (WithMuxBearer peer ev) = \_o ->
        "Bearer on " <> pack (show peer)
     <> " event: " <> pack (show ev)


instance Transformable Text IO TraceLocalRootPeers where
  trTransformer = trStructuredText
instance HasTextFormatter TraceLocalRootPeers where
    formatText a _ = pack (show a)

instance Transformable Text IO TracePublicRootPeers where
  trTransformer = trStructuredText
instance HasTextFormatter TracePublicRootPeers where
  formatText a _ = pack (show a)

instance Transformable Text IO (TracePeerSelection Socket.SockAddr) where
  trTransformer = trStructuredText
instance HasTextFormatter (TracePeerSelection Socket.SockAddr) where
  formatText a _ = pack (show a)

instance Show conn
      => Transformable Text IO (DebugPeerSelection Socket.SockAddr conn) where
  trTransformer = trStructuredText
instance HasTextFormatter (DebugPeerSelection Socket.SockAddr conn) where
  -- One can only change what is logged with respect to verbosity using json
  -- format.
  formatText _ obj = pack (show obj)

instance Transformable Text IO (PeerSelectionActionsTrace Socket.SockAddr) where
  trTransformer = trStructuredText
instance HasTextFormatter (PeerSelectionActionsTrace Socket.SockAddr) where
  formatText a _ = pack (show a)

instance (Show addr, Show versionNumber, Show agreedOptions, ToObject addr)
      => Transformable Text IO (ConnectionManagerTrace
                                 addr
                                 (ConnectionHandlerTrace versionNumber agreedOptions)) where
  trTransformer = trStructuredText
instance (Show addr, Show versionNumber, Show agreedOptions)
      => HasTextFormatter (ConnectionManagerTrace
                            addr
                            (ConnectionHandlerTrace versionNumber agreedOptions)) where
  formatText a _ = pack (show a)

instance Show addr
      => Transformable Text IO (ServerTrace addr) where
  trTransformer = trStructuredText
instance Show addr
      => HasTextFormatter (ServerTrace addr) where
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
    mkObject [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
             , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
             , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             ]

  toObject verb (AnyMessageAndAgency stok (MsgBlock blk)) =
    mkObject [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
             , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
             , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             , "txIds" .= toJSON (presentTx <$> extractTxs blk)
             ]
      where
        presentTx :: GenTx blk -> Value
        presentTx =  String . renderTxIdForVerbosity verb . txId
  toObject _v (AnyMessageAndAgency stok MsgRequestRange{}) =
    mkObject [ "kind" .= String "MsgRequestRange"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _v (AnyMessageAndAgency stok MsgStartBatch{}) =
    mkObject [ "kind" .= String "MsgStartBatch"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _v (AnyMessageAndAgency stok MsgNoBlocks{}) =
    mkObject [ "kind" .= String "MsgNoBlocks"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _v (AnyMessageAndAgency stok MsgBatchDone{}) =
    mkObject [ "kind" .= String "MsgBatchDone"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _v (AnyMessageAndAgency stok MsgClientDone{}) =
    mkObject [ "kind" .= String "MsgClientDone"
             , "agency" .= String (pack $ show stok)
             ]

instance ( ToObject (AnyMessageAndAgency ps)
         , forall (st :: ps). Show (ClientHasAgency st)
         , forall (st :: ps). Show (ServerHasAgency st)
         )
      => ToObject (AnyMessageAndAgency (Hello ps stIdle)) where
  toObject verb (AnyMessageAndAgency stok msg) =
    case (stok, msg) of
      (_, Hello.MsgHello) ->
        mkObject [ "kind" .= String "MsgHello"
                 , "agency" .= String (pack $ show stok)
                 ]
      ( ClientAgency (Hello.TokClientTalk tok)
        , Hello.MsgTalk msg' ) ->
        mkObject [ "kind" .= String "MsgTalk"
                 , "message" .=
                     toObject verb
                       (AnyMessageAndAgency (ClientAgency tok) msg')
                 ]
      ( ServerAgency (Hello.TokServerTalk tok)
        , Hello.MsgTalk msg' ) ->
        mkObject [ "kind" .= String "MsgTalk"
                 , "message" .=
                     toObject verb
                       (AnyMessageAndAgency (ServerAgency tok) msg')
                 ]

instance (forall result. Show (query result))
      => ToObject (AnyMessageAndAgency (LocalStateQuery blk pt query)) where
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgAcquire{}) =
    mkObject [ "kind" .= String "MsgAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgAcquired{}) =
    mkObject [ "kind" .= String "MsgAcquired"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgFailure{}) =
    mkObject [ "kind" .= String "MsgFailure"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgQuery{}) =
    mkObject [ "kind" .= String "MsgQuery"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgResult{}) =
    mkObject [ "kind" .= String "MsgResult"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgRelease{}) =
    mkObject [ "kind" .= String "MsgRelease"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgReAcquire{}) =
    mkObject [ "kind" .= String "MsgReAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgDone{}) =
    mkObject [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

instance ToObject (AnyMessageAndAgency (LocalTxSubmission tx err)) where
  toObject _verb (AnyMessageAndAgency stok LocalTxSub.MsgSubmitTx{}) =
    mkObject [ "kind" .= String "MsgSubmitTx"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxSub.MsgAcceptTx{}) =
    mkObject [ "kind" .= String "MsgAcceptTx"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxSub.MsgRejectTx{}) =
    mkObject [ "kind" .= String "MsgRejectTx"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxSub.MsgDone{}) =
    mkObject [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

instance ToObject (AnyMessageAndAgency (ChainSync blk pt tip)) where
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgRequestNext{}) =
     mkObject [ "kind" .= String "MsgRequestNext"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgAwaitReply{}) =
     mkObject [ "kind" .= String "MsgAwaitReply"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgRollForward{}) =
     mkObject [ "kind" .= String "MsgRollForward"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgRollBackward{}) =
     mkObject [ "kind" .= String "MsgRollBackward"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgFindIntersect{}) =
     mkObject [ "kind" .= String "MsgFindIntersect"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgIntersectFound{}) =
     mkObject [ "kind" .= String "MsgIntersectFound"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgIntersectNotFound{}) =
     mkObject [ "kind" .= String "MsgIntersectNotFound"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgDone{}) =
     mkObject [ "kind" .= String "MsgDone"
              , "agency" .= String (pack $ show stok)
              ]


instance Show peerAddr => ToObject (ConnectionId peerAddr) where
    toObject _verb ConnectionId { localAddress, remoteAddress } =
      mkObject [ "localAddress"  .= show localAddress
               , "remoteAddress" .= show remoteAddress
               ]


instance ToObject (FetchDecision [Point header]) where
  toObject _verb (Left decline) =
    mkObject [ "kind" .= String "FetchDecision declined"
             , "declined" .= String (pack (show decline))
             ]
  toObject _verb (Right results) =
    mkObject [ "kind" .= String "FetchDecision results"
             , "length" .= String (pack $ show $ length results)
             ]

instance ToObject ND.DiffusionInitializationTracer where
  toObject _verb (ND.RunServer sockAddr) = mkObject
    [ "kind" .= String "RunServer"
    , "socketAddress" .= String (pack (show sockAddr))
    ]

  toObject _verb (ND.RunLocalServer localAddress) = mkObject
    [ "kind" .= String "RunLocalServer"
    , "localAddress" .= String (pack (show localAddress))
    ]
  toObject _verb (ND.UsingSystemdSocket path) = mkObject
    [ "kind" .= String "UsingSystemdSocket"
    , "path" .= String (pack path)
    ]

  toObject _verb (ND.CreateSystemdSocketForSnocketPath path) = mkObject
    [ "kind" .= String "CreateSystemdSocketForSnocketPath"
    , "path" .= String (pack path)
    ]
  toObject _verb (ND.CreatedLocalSocket path) = mkObject
    [ "kind" .= String "CreatedLocalSocket"
    , "path" .= String (pack path)
    ]
  toObject _verb (ND.ConfiguringLocalSocket path socket) = mkObject
    [ "kind" .= String "ConfiguringLocalSocket"
    , "path" .= String (pack path)
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.ListeningLocalSocket path socket) = mkObject
    [ "kind" .= String "ListeningLocalSocket"
    , "path" .= String (pack path)
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.LocalSocketUp path fd) = mkObject
    [ "kind" .= String "LocalSocketUp"
    , "path" .= String (pack path)
    , "socket" .= String (pack (show fd))
    ]
  toObject _verb (ND.CreatingServerSocket socket) = mkObject
    [ "kind" .= String "CreatingServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.ListeningServerSocket socket) = mkObject
    [ "kind" .= String "ListeningServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.ServerSocketUp socket) = mkObject
    [ "kind" .= String "ServerSocketUp"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.ConfiguringServerSocket socket) = mkObject
    [ "kind" .= String "ConfiguringServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.UnsupportedLocalSystemdSocket path) = mkObject
    [ "kind" .= String "UnsupportedLocalSystemdSocket"
    , "path" .= String (pack (show path))
    ]
  toObject _verb ND.UnsupportedReadySocketCase = mkObject
    [ "kind" .= String "UnsupportedReadySocketCase"
    ]
  toObject _verb (ND.DiffusionErrored exception) = mkObject
    [ "kind" .= String "DiffusionErrored"
    , "path" .= String (pack (show exception))
    ]

instance ToObject NtC.HandshakeTr where
  toObject _verb (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "LocalHandshakeTrace"
             , "bearer" .= show b
             , "event" .= show ev ]


instance ToObject NtN.HandshakeTr where
  toObject _verb (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "HandshakeTrace"
             , "bearer" .= show b
             , "event" .= show ev ]


instance ToObject LocalAddress where
    toObject _verb (LocalAddress path) =
      mkObject [ "path" .= String (pack path) ]


instance ToObject NtN.AcceptConnectionsPolicyTrace where
  toObject _verb (NtN.ServerTraceAcceptConnectionRateLimiting delay numOfConnections) =
    mkObject [ "kind" .= String "ServerTraceAcceptConnectionRateLimiting"
             , "delay" .= show delay
             , "numberOfConnection" .= show numOfConnections
             ]
  toObject _verb (NtN.ServerTraceAcceptConnectionHardLimit softLimit) =
    mkObject [ "kind" .= String "ServerTraceAcceptConnectionHardLimit"
             , "softLimit" .= show softLimit
             ]


instance (Show txid, Show tx)
      => ToObject (AnyMessageAndAgency (TxSubmission txid tx)) where
  toObject _verb (AnyMessageAndAgency stok (MsgRequestTxs txids)) =
    mkObject
      [ "kind" .= String "MsgRequestTxs"
      , "agency" .= String (pack $ show stok)
      , "txIds" .= String (pack $ show txids)
      ]
  toObject _verb (AnyMessageAndAgency stok (MsgReplyTxs txs)) =
    mkObject
      [ "kind" .= String "MsgReplyTxs"
      , "agency" .= String (pack $ show stok)
      , "txs" .= String (pack $ show txs)
      ]
  toObject _verb (AnyMessageAndAgency stok (MsgRequestTxIds _ _ _)) =
    mkObject
      [ "kind" .= String "MsgRequestTxIds"
      , "agency" .= String (pack $ show stok)
      ]
  toObject _verb (AnyMessageAndAgency stok (MsgReplyTxIds _)) =
    mkObject
      [ "kind" .= String "MsgReplyTxIds"
      , "agency" .= String (pack $ show stok)
      ]
  toObject _verb (AnyMessageAndAgency stok MsgDone) =
    mkObject
      [ "kind" .= String "MsgDone"
      , "agency" .= String (pack $ show stok)
      ]
  --TODO: Can't use 'MsgKThxBye' because NodeToNodeV_2 is not introduced yet.
  toObject _verb (AnyMessageAndAgency stok _) =
    mkObject
      [ "kind" .= String "MsgKThxBye"
      , "agency" .= String (pack $ show stok)
      ]


instance ConvertRawHash blk
      => ToObject (Point blk) where
  toObject _verb GenesisPoint =
    mkObject
      [ "kind" .= String "GenesisPoint" ]
  toObject verb (BlockPoint slot h) =
    mkObject
      [ "kind" .= String "BlockPoint"
      , "slot" .= toJSON (unSlotNo slot)
      , "headerHash" .= renderHeaderHashForVerbosity (Proxy @blk) verb h
      ]


instance ToObject SlotNo where
  toObject _verb slot =
    mkObject [ "kind" .= String "SlotNo"
             , "slot" .= toJSON (unSlotNo slot) ]


instance ToObject (TraceFetchClientState header) where
  toObject _verb BlockFetch.AddedFetchRequest {} =
    mkObject [ "kind" .= String "AddedFetchRequest" ]
  toObject _verb BlockFetch.AcknowledgedFetchRequest {} =
    mkObject [ "kind" .= String "AcknowledgedFetchRequest" ]
  toObject _verb BlockFetch.CompletedBlockFetch {} =
    mkObject [ "kind" .= String "CompletedBlockFetch" ]
  toObject _verb BlockFetch.CompletedFetchBatch {} =
    mkObject [ "kind" .= String "CompletedFetchBatch" ]
  toObject _verb BlockFetch.StartedFetchBatch {} =
    mkObject [ "kind" .= String "StartedFetchBatch" ]
  toObject _verb BlockFetch.RejectedFetchBatch {} =
    mkObject [ "kind" .= String "RejectedFetchBatch" ]
  toObject _verb (BlockFetch.ClientTerminating outstanding) =
    mkObject [ "kind" .= String "ClientTerminating"
             , "outstanding" .= outstanding
             ]


instance Show peer
      => ToObject [TraceLabelPeer peer (FetchDecision [Point header])] where
  toObject MinimalVerbosity _ = emptyObject
  toObject _ [] = emptyObject
  toObject _ xs = mkObject
    [ "kind"  .= String "PeersFetch"
    , "peers" .= toJSON
      (foldl' (\acc x -> toObject MaximalVerbosity x : acc) [] xs) ]


instance (Show peer, ToObject a) => ToObject (TraceLabelPeer peer a) where
  toObject verb (TraceLabelPeer peerid a) =
    mkObject [ "peer" .= show peerid ] <> toObject verb a


instance ToObject (AnyMessageAndAgency ps)
      => ToObject (TraceSendRecv ps) where
  toObject verb (TraceSendMsg m) = mkObject
    [ "kind" .= String "Send" , "msg" .= toObject verb m ]
  toObject verb (TraceRecvMsg m) = mkObject
    [ "kind" .= String "Recv" , "msg" .= toObject verb m ]


instance ToObject (TraceTxSubmissionInbound txid tx) where
  toObject _verb TxSubmission.Inbound.TxInboundTerminated =
    mkObject [ "kind" .= String "TxInboundTerminated" ]
  toObject _verb (TxSubmission.Inbound.TxInboundCanRequestMoreTxs n) =
    mkObject [ "kind" .= String "TxInboundCanRequestMoreTxs"
             , "numberOfRequests" .= n
             ]
  toObject _verb (TxSubmission.Inbound.TxInboundCannotRequestMoreTxs n) =
    mkObject [ "kind" .= String "TxInboundCannotRequestMoreTxs"
             , "numberOfRequests" .= n
             ]


-- TODO: ouroboros-network should provide a newtype wrapper for 'SockAddr'.
instance ToObject Socket.SockAddr where
    toObject _verb sockAddr =
      mkObject [ "sockAddr" .= String (pack $ show sockAddr) ]


instance (Show txid, Show tx)
      => ToObject (TraceTxSubmissionOutbound txid tx) where
  toObject MaximalVerbosity (TraceTxSubmissionOutboundRecvMsgRequestTxs txids) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs"
      , "txIds" .= String (pack $ show txids)
      ]
  toObject _verb (TraceTxSubmissionOutboundRecvMsgRequestTxs _txids) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs"
      ]
  toObject MaximalVerbosity (TraceTxSubmissionOutboundSendMsgReplyTxs txs) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs"
      , "txs" .= String (pack $ show txs)
      ]
  toObject _verb (TraceTxSubmissionOutboundSendMsgReplyTxs _txs) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs"
      ]
  toObject _verb (TraceControlMessage controlMessage) =
    mkObject
      [ "kind" .= String "TraceControlMessage"
      , "controlMessage" .= String (pack $ show controlMessage)
      ]


instance Show remotePeer => ToObject (TraceKeepAliveClient remotePeer) where
  toObject _verb (AddSample peer rtt pgsv) =
    mkObject
      [ "kind" .= String "TraceKeepAliveClient AddSample"
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

instance Show addr => ToObject (WithAddr addr ErrorPolicyTrace) where
  toObject _verb (WithAddr addr ev) =
    mkObject [ "kind" .= String "ErrorPolicyTrace"
             , "address" .= show addr
             , "event" .= show ev ]


instance ToObject (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  toObject _verb (WithIPList localAddresses dests ev) =
    mkObject [ "kind" .= String "WithIPList SubscriptionTrace"
             , "localAddresses" .= show localAddresses
             , "dests" .= show dests
             , "event" .= show ev ]


instance ToObject (WithDomainName DnsTrace) where
  toObject _verb (WithDomainName dom ev) =
    mkObject [ "kind" .= String "DnsTrace"
             , "domain" .= show dom
             , "event" .= show ev ]


instance ToObject (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  toObject _verb (WithDomainName dom ev) =
    mkObject [ "kind" .= String "SubscriptionTrace"
             , "domain" .= show dom
             , "event" .= show ev ]


instance (ToObject peer) => ToObject (WithMuxBearer peer MuxTrace) where
  toObject verb (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "MuxTrace"
             , "bearer" .= toObject verb b
             , "event" .= show ev ]

instance ToObject TraceLocalRootPeers where
  toObject _verb ev =
    mkObject [ "kind" .= String "TraceLocalRootPeers"
             , "event" .= show ev ]

instance ToObject TracePublicRootPeers where
  toObject _verb ev =
    mkObject [ "kind" .= String "TracePublicRootPeers"
             , "event" .= show ev ]

instance ToObject (TracePeerSelection Socket.SockAddr) where
  toObject _verb ev =
    mkObject [ "kind" .= String "TracePeerSelection"
             , "event" .= show ev ]


peerSelectionTargetsToObject :: PeerSelectionTargets -> Value
peerSelectionTargetsToObject
  PeerSelectionTargets { targetNumberOfRootPeers,
                         targetNumberOfKnownPeers,
                         targetNumberOfEstablishedPeers,
                         targetNumberOfActivePeers } =
    Object $
      mkObject [ "roots" .= targetNumberOfRootPeers
               , "knownPeers" .= targetNumberOfKnownPeers
               , "established" .= targetNumberOfEstablishedPeers
               , "active" .= targetNumberOfActivePeers
               ]

instance Show peerConn => ToObject (DebugPeerSelection Socket.SockAddr peerConn) where
  toObject verb (TraceGovernorState blockedAt wakeupAfter
                   PeerSelectionState { targets, knownPeers, establishedPeers, activePeers })
      | verb <= NormalVerbosity =
    mkObject [ "kind" .= String "DebugPeerSelection"
             , "blockedAt" .= String (pack $ show blockedAt)
             , "wakeupAfter" .= String (pack $ show wakeupAfter)
             , "targets" .= peerSelectionTargetsToObject targets
             , "numberOfPeers" .=
                 Object (mkObject [ "known" .= KnownPeers.size knownPeers
                                  , "established" .= EstablishedPeers.size establishedPeers
                                  , "active" .= Set.size activePeers
                                  ])
             ]
  toObject _ (TraceGovernorState blockedAt wakeupAfter ev) =
    mkObject [ "kind" .= String "DebugPeerSelection"
             , "blockedAt" .= String (pack $ show blockedAt)
             , "wakeupAfter" .= String (pack $ show wakeupAfter)
             , "peerSelectionState" .= String (pack $ show ev)
             ]

instance ToObject (PeerSelectionActionsTrace Socket.SockAddr) where
  toObject _verb ev =
    mkObject [ "kind" .= String "PeerSelectionAction"
             , "event" .= show ev ]

instance (Show addr, Show versionNumber, Show agreedOptions, ToObject addr)
      => ToObject (ConnectionManagerTrace addr (ConnectionHandlerTrace versionNumber agreedOptions)) where
  toObject verb ev =
    case ev of
      TrIncludeConnection prov peerAddr ->
        mkObject $ reverse $
          [ "kind" .= String "IncludeConnection"
          , "remoteAddress" .= toObject verb peerAddr
          , "provenance" .= String (pack . show $ prov)
          ]
      TrUnregisterConnection prov peerAddr ->
        mkObject $ reverse $
          [ "kind" .= String "UnregisterConnection"
          , "remoteAddress" .= toObject verb peerAddr
          , "provenance" .= String (pack . show $ prov)
          ]
      TrIncludedConnection prov connId ->
        mkObject $ reverse $
          [ "kind" .= String "IncludedConnection"
          , "connectionId" .= toObject verb connId
          , "provenance" .= String (pack . show $ prov)
          ]
      TrNegotiatedConnection prov connId dataFlow ->
        mkObject
          [ "kind" .= String "NegotiatedConnection"
          , "connectionId" .= toObject verb connId
          , "provenance" .= String (pack . show $ prov)
          , "dataFlow" .= String (pack . show $ dataFlow)
          ]
      TrConnect (Just localAddress) remoteAddress ->
        mkObject
          [ "kind" .= String "ConnectTo"
          , "connectionId" .= toObject verb ConnectionId { localAddress, remoteAddress }
          ]
      TrConnect Nothing remoteAddress ->
        mkObject
          [ "kind" .= String "ConnectTo"
          , "remoteAddress" .= toObject verb remoteAddress
          ]
      TrConnectError (Just localAddress) remoteAddress err ->
        mkObject
          [ "king" .= String "ConnectError"
          , "connectionId" .= toObject verb ConnectionId { localAddress, remoteAddress }
          , "error" .= String (pack . show $ err)
          ]
      TrConnectError Nothing remoteAddress err ->
        mkObject
          [ "king" .= String "ConnectError"
          , "remoteAddress" .= toObject verb remoteAddress
          , "error" .= String (pack . show $ err)
          ]
      TrReusedConnection remoteAddress ->
        mkObject
          [ "kind" .= String "ReusedConnection"
          , "remoteAddress" .= toObject verb remoteAddress
          ]
      TrConnectionTerminating prov connId ->
        mkObject
          [ "kind" .= String "ConnectionFinished"
          , "connectionId" .= toObject verb connId
          , "provenance" .= String (pack . show $ prov)
          ]
      TrConnectionTerminated prov remoteAddress ->
        mkObject
          [ "kind" .= String "ConnectionFinished"
          , "remoteAddress" .= toObject verb remoteAddress
          , "provenance" .= String (pack . show $ prov)
          ]
      TrConnectionHandler connId a ->
        mkObject
          [ "kind" .= String "ConnectionHandler"
          , "connectionId" .= toObject verb connId
          -- TODO:  encode 'ConnectionHandlerTrace'
          , "connectionHandler" .= String (pack . show $ a)
          ]
      TrShutdown ->
        mkObject
          [ "kind" .= String "Shutdown"
          ]
      TrConnectionExists prov connId ->
        mkObject
          [ "kind" .= String "ConnectionExists"
          , "connectionId" .= toObject verb connId
          , "provenance" .= String (pack . show $ prov)
          ]
      TrForbiddenConnection connId ->
        mkObject
          [ "kind" .= String "ForbiddenConnection"
          , "connectionId" .= toObject verb connId
          ]
      TrImpossibleConnection connId ->
        mkObject
          [ "kind" .= String "ImpossibleConnection"
          , "connectionId" .= toObject verb connId
          ]
      TrConnectionFailure connId ->
        mkObject
          [ "kind" .= String "ConnectionFailure"
          , "connectionId" .= toObject verb connId
          ]
      TrConnectionNotFound prov remoteAddress ->
        mkObject
          [ "kind" .= String "ConnectionNotFound"
          , "remoteAddress" .= toObject verb remoteAddress
          , "provenance" .= String (pack . show $ prov)
          ]
      TrForbiddenOperation remoteAddress connState ->
        mkObject
          [ "kind" .= String "ForbiddenOperation"
          , "remoteAddress" .= toObject verb remoteAddress
          , "connectionState" .= String (pack . show $ connState)
          ]
      TrDemotedToColdLocal connId connState ->
        mkObject
          [ "kind" .= String "ConnectionDemoted"
          , "connectionId" .= toObject verb connId
          , "connectionState" .= String (pack . show $ connState)
          ]
      TrWaitIdle connId ->
        mkObject
          [ "kind" .= String "WaitIdle"
          , "connectionId" .= toObject verb connId
          ]
      TrPruneConnections peers ->
        mkObject
          [ "kind" .= String "PruneConnections"
          , "peers" .= toJSON (toObject verb `map` peers)
          ] 
      TrCleanup connId ->
        mkObject
          [ "kind" .= String "Cleanup"
          , "connectionId" .= toObject verb connId
          ]
      TrConnectionExit connId reason ->
        mkObject
          [ "kind" .= String "TrConnectionExit"
          , "connectionId" .= toObject verb connId
          , "reason" .= String (pack . show $ reason)
          ]


instance Show addr
      => ToObject (ServerTrace addr) where
  -- TODO: a better 'ToObject' instance
  toObject _verb ev =
    mkObject [ "kind" .= String "ServerTrace"
             , "event" .= show ev ]
