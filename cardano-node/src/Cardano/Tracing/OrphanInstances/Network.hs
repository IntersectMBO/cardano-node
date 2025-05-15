{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}
#if __GLASGOW_HASKELL__ < 904
-- Pattern synonym record fields with GHC-8.10 is issuing the `-Wname-shadowing`
-- warning.
{-# OPTIONS_GHC -Wno-name-shadowing #-}
#endif

module Cardano.Tracing.OrphanInstances.Network
  ( Verbose (..)
  , FetchDecisionToJSON (..)
  ) where

import           Cardano.Node.Queries (ConvertTxId)
import           Cardano.Tracing.OrphanInstances.Common
import           Cardano.Tracing.Render
import           Ouroboros.Consensus.Block (ConvertRawHash (..), Header, getHeader)
import           Ouroboros.Consensus.Ledger.Query (BlockQuery, Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx, GenTxId,
                   HasTxs (..), TxId, txId)
import           Ouroboros.Consensus.Node.Run (RunNode, estimateBlockSize)
import           Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers(..))
import           Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable(..))
import           Cardano.Network.Types (LedgerStateJudgement(..))
import qualified Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionState as Cardano
import qualified Ouroboros.Cardano.Network.PeerSelection.Governor.Types as Cardano
import qualified Ouroboros.Cardano.Network.PublicRootPeers as Cardano.PublicRootPeers
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch.ClientState (TraceFetchClientState,
                   TraceLabelPeer (..))
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision, FetchDecline (..))
import qualified Ouroboros.Network.BlockFetch.Decision.Trace as BlockFetch
import           Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace (..))
import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.Core as ConnMgr (Trace (..))
import           Ouroboros.Network.ConnectionManager.ConnMap (ConnMap (..), LocalAddr (..))
import           Ouroboros.Network.ConnectionManager.State (ConnStateId (..))
import           Ouroboros.Network.ConnectionManager.Types (AbstractState (..),
                   ConnectionManagerCounters (..),
                   OperationResult (..))
import qualified Ouroboros.Network.ConnectionManager.Types as ConnMgr
import qualified Ouroboros.Network.Diffusion.Common as Diffusion
import           Ouroboros.Network.DeltaQ (GSV (..), PeerGSV (..))
import           Ouroboros.Network.Driver.Limits (ProtocolLimitFailure (..))
import qualified Ouroboros.Network.Driver.Stateful as Stateful
import           Ouroboros.Network.ExitPolicy (RepromoteDelay (..))
import qualified Ouroboros.Network.InboundGovernor as InboundGovernor
import qualified Ouroboros.Network.InboundGovernor.State as InboundGovernor
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.NodeToClient (NodeToClientVersion (..),
                   NodeToClientVersionData (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import           Ouroboros.Network.NodeToNode (ErrorPolicyTrace (..), NodeToNodeVersion (..),
                   NodeToNodeVersionData (..), RemoteAddress, TraceSendRecv (..), WithAddr (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.PeerSelection.Governor (AssociationMode (..), DebugPeerSelection (..),
                   DebugPeerSelectionState (..), PeerSelectionCounters, PeerSelectionState (..),
                   PeerSelectionTargets (..), PeerSelectionView (..), TracePeerSelection (..),
                   peerSelectionStateToCounters)
import           Ouroboros.Network.PeerSelection.LedgerPeers
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import           Ouroboros.Network.PeerSelection.PeerStateActions (PeerSelectionActionsTrace (..))
import           Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import qualified Ouroboros.Network.PeerSelection.PublicRootPeers as PublicRootPeers
import           Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
                   (TraceLocalRootPeers (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers
                   (TracePublicRootPeers (..))
import           Ouroboros.Network.PeerSelection.State.KnownPeers (KnownPeerInfo (..))
import qualified Ouroboros.Network.PeerSelection.State.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
                   LocalRootPeers, WarmValency (..), LocalRootConfig (..))
import qualified Ouroboros.Network.PeerSelection.State.LocalRootPeers as LocalRootPeers
import           Ouroboros.Network.PeerSelection.Types (PeerStatus (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch, Message (..))
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import           Ouroboros.Network.Protocol.Handshake (HandshakeException (..),
                   HandshakeProtocolError (..), RefuseReason (..))
import qualified Ouroboros.Network.Protocol.KeepAlive.Type as KA
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (LocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import           Ouroboros.Network.Protocol.LocalTxMonitor.Type (LocalTxMonitor)
import qualified Ouroboros.Network.Protocol.LocalTxMonitor.Type as LocalTxMonitor
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (LocalTxSubmission)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LocalTxSub
import           Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..),
                   PeerSharingResult (..))
import qualified Ouroboros.Network.Protocol.PeerSharing.Type as PeerSharing
import           Ouroboros.Network.Protocol.TxSubmission2.Type as TxSubmission2
import           Ouroboros.Network.RethrowPolicy (ErrorCommand (..))
import           Ouroboros.Network.Server2 as Server
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.Subscription (ConnectResult (..), DnsTrace (..),
                   SubscriberError (..), SubscriptionTrace (..), WithDomainName (..),
                   WithIPList (..))
import           Ouroboros.Network.TxSubmission.Inbound (ProcessedTxCount (..),
                   TraceTxSubmissionInbound (..))
import           Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound (..))

import           Control.Exception (Exception (..), SomeException (..))
import           Control.Monad.Class.MonadTime.SI (DiffTime, Time (..))
import           Data.Aeson (FromJSON (..), Value (..))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (listValue)
import qualified Data.Aeson.Types as Aeson
import           Data.Bifunctor (Bifunctor (first))
import           Data.Data (Proxy (..))
import           Data.Foldable (Foldable (..))
import           Data.Functor.Identity (Identity (..))
import qualified Data.IP as IP
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.Mux (MiniProtocolNum (..))
import qualified Network.Mux as Mux
import           Network.Socket (SockAddr (..))
import           Network.TypedProtocol.Codec (AnyMessage (AnyMessageAndAgency))
import qualified Network.TypedProtocol.Stateful.Codec as Stateful

{- HLINT ignore "Use record patterns" -}

--
-- * instances of @HasPrivacyAnnotation@ and @HasSeverityAnnotation@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance HasPrivacyAnnotation (Diffusion.DiffusionTracer ntnAddr ntcAddr)
instance HasSeverityAnnotation (Diffusion.DiffusionTracer ntnAddr ntcAddr) where
  getSeverityAnnotation Diffusion.SystemdSocketConfiguration {} = Warning
  getSeverityAnnotation Diffusion.UnsupportedLocalSystemdSocket {} = Warning
  getSeverityAnnotation Diffusion.DiffusionErrored {} = Critical
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


instance HasPrivacyAnnotation (Stateful.TraceSendRecv (LocalStateQuery blk (Point blk) (Query blk)) f)
instance HasSeverityAnnotation (Stateful.TraceSendRecv (LocalStateQuery blk (Point blk) (Query blk)) f) where
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
          Left FetchDeclineChainIntersectionTooDeep -> Notice
          Left FetchDeclineAlreadyFetched        -> Debug
          Left FetchDeclineInFlightThisPeer      -> Debug
          Left FetchDeclineInFlightOtherPeer     -> Debug
          Left FetchDeclinePeerShutdown          -> Info
          Left FetchDeclinePeerStarting          -> Info
          Left FetchDeclinePeerSlow              -> Info
          Left FetchDeclineReqsInFlightLimit {}  -> Info
          Left FetchDeclineBytesInFlightLimit {} -> Info
          Left FetchDeclinePeerBusy {}           -> Info
          Left FetchDeclineConcurrencyLimit {}   -> Info
          Right _                                -> Info


instance HasPrivacyAnnotation (BlockFetch.TraceDecisionEvent peer header)
instance HasSeverityAnnotation (BlockFetch.TraceDecisionEvent peer header) where
  getSeverityAnnotation (BlockFetch.PeersFetch xs) = getSeverityAnnotation xs
  getSeverityAnnotation BlockFetch.PeerStarvedUs {} = Info


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
      PickedLedgerPeer {}            -> Debug
      PickedLedgerPeers {}           -> Info
      PickedBigLedgerPeer {}         -> Info
      PickedBigLedgerPeers {}        -> Info
      FetchingNewLedgerState {}      -> Info
      DisabledLedgerPeers {}         -> Info
      TraceUseLedgerPeers {}         -> Info
      WaitingOnRequest {}            -> Debug
      RequestForPeers {}             -> Debug
      ReusingLedgerState {}          -> Debug
      FallingBackToPublicRootPeers {} -> Info
      NotEnoughLedgerPeers {}        -> Warning
      NotEnoughBigLedgerPeers {}     -> Warning
      TraceLedgerPeersDomains {}     -> Debug
      TraceLedgerPeersResult {}      -> Debug
      TraceLedgerPeersFailure {}     -> Debug
      UsingBigLedgerPeerSnapshot {}  -> Debug


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


instance HasPrivacyAnnotation (Mux.WithBearer peer Mux.Trace)
instance HasSeverityAnnotation (Mux.WithBearer peer Mux.Trace) where
  getSeverityAnnotation (Mux.WithBearer _ ev) = case ev of
    Mux.TraceRecvHeaderStart -> Debug
    Mux.TraceRecvHeaderEnd {} -> Debug
    Mux.TraceRecvStart {} -> Debug
    Mux.TraceRecvEnd {} -> Debug
    Mux.TraceSendStart {} -> Debug
    Mux.TraceSendEnd -> Debug
    Mux.TraceState {} -> Info
    Mux.TraceCleanExit {} -> Notice
    Mux.TraceExceptionExit {} -> Notice
    Mux.TraceChannelRecvStart {} -> Debug
    Mux.TraceChannelRecvEnd {} -> Debug
    Mux.TraceChannelSendStart {} -> Debug
    Mux.TraceChannelSendEnd {} -> Debug
    Mux.TraceHandshakeStart -> Debug
    Mux.TraceHandshakeClientEnd {} -> Info
    Mux.TraceHandshakeServerEnd -> Debug
    Mux.TraceHandshakeClientError {} -> Error
    Mux.TraceHandshakeServerError {} -> Error
    Mux.TraceRecvDeltaQObservation {} -> Debug
    Mux.TraceRecvDeltaQSample {} -> Debug
    Mux.TraceSDUReadTimeoutException -> Notice
    Mux.TraceSDUWriteTimeoutException -> Notice
    Mux.TraceStartEagerly _ _ -> Info
    Mux.TraceStartOnDemand _ _ -> Info
    Mux.TraceStartedOnDemand _ _ -> Info
    Mux.TraceStartOnDemandAny {} -> Info
    Mux.TraceTerminating {} -> Debug
    Mux.TraceStopping -> Debug
    Mux.TraceStopped -> Debug
    Mux.TraceTCPInfo {} -> Debug

instance HasPrivacyAnnotation (TraceLocalRootPeers extraFlags RemoteAddress exception)
instance HasSeverityAnnotation (TraceLocalRootPeers extraFlags RemoteAddress exception) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation TracePublicRootPeers
instance HasSeverityAnnotation TracePublicRootPeers where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (TracePeerSelection extraDebugState extraFlags (Cardano.PublicRootPeers.ExtraPeers addr) addr)
instance HasSeverityAnnotation (TracePeerSelection extraDebugState extraFlags (Cardano.PublicRootPeers.ExtraPeers addr) addr) where
  getSeverityAnnotation ev =
    case ev of
      TraceLocalRootPeersChanged {} -> Notice
      TraceTargetsChanged        {} -> Notice
      TracePublicRootsRequest    {} -> Info
      TracePublicRootsResults    {} -> Info
      TracePublicRootsFailure    {} -> Error
      TracePeerShareRequests     {} -> Info
      TracePeerShareResults      {} -> Info
      TracePeerShareResultsFiltered {} -> Debug
      TracePickInboundPeers      {} -> Info
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
      TraceDemoteLocalAsynchronous {} -> Warning
      TraceGovernorWakeup        {} -> Info
      TraceChurnWait             {} -> Info
      TraceChurnMode             {} -> Info

      TraceForgetBigLedgerPeers  {} -> Info

      TraceBigLedgerPeersRequest {} -> Info
      TraceBigLedgerPeersResults {} -> Info
      TraceBigLedgerPeersFailure {} -> Error

      TracePromoteColdBigLedgerPeers {}      -> Info
      TracePromoteColdBigLedgerPeerFailed {} -> Info
      TracePromoteColdBigLedgerPeerDone {}   -> Info

      TracePromoteWarmBigLedgerPeers {}       -> Info
      TracePromoteWarmBigLedgerPeerFailed {}  -> Error
      TracePromoteWarmBigLedgerPeerDone {}    -> Info
      TracePromoteWarmBigLedgerPeerAborted {} -> Info

      TraceDemoteWarmBigLedgerPeers {}      -> Info
      TraceDemoteWarmBigLedgerPeerFailed {} -> Info
      TraceDemoteWarmBigLedgerPeerDone {}   -> Info

      TraceDemoteHotBigLedgerPeers {}      -> Info
      TraceDemoteHotBigLedgerPeerFailed {} -> Info
      TraceDemoteHotBigLedgerPeerDone {}   -> Info

      TraceDemoteBigLedgerPeersAsynchronous {} -> Warning

      TraceUseBootstrapPeersChanged {} -> Info
      TraceBootstrapPeersFlagChangedWhilstInSensitiveState -> Info

      TraceLedgerStateJudgementChanged {} -> Notice
      TraceOnlyBootstrapPeers {}          -> Notice

      TraceOutboundGovernorCriticalFailure {} -> Error

      TraceChurnAction {} -> Info
      TraceChurnTimeout {} -> Notice

      TraceDebugState {} -> Info

      TraceVerifyPeerSnapshot True  -> Info
      TraceVerifyPeerSnapshot False -> Error

instance HasPrivacyAnnotation (DebugPeerSelection extraState extraFlags (Cardano.PublicRootPeers.ExtraPeers addr) addr)
instance HasSeverityAnnotation (DebugPeerSelection extraState extraFlags (Cardano.PublicRootPeers.ExtraPeers addr) addr) where
  getSeverityAnnotation _ = Debug

instance HasPrivacyAnnotation (PeerSelectionActionsTrace SockAddr lAddr)
instance HasSeverityAnnotation (PeerSelectionActionsTrace SockAddr lAddr) where
  getSeverityAnnotation ev =
   case ev of
     PeerStatusChanged {}       -> Info
     PeerStatusChangeFailure {} -> Error
     PeerMonitoringError {}     -> Error
     PeerMonitoringResult {}    -> Debug
     AcquireConnectionError {}  -> Error

instance HasPrivacyAnnotation (PeerSelectionCounters extraCounters)
instance HasSeverityAnnotation (PeerSelectionCounters extraCounters) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (ConnMgr.Trace addr connTrace)
instance HasSeverityAnnotation (ConnMgr.Trace addr (ConnectionHandlerTrace versionNumber agreedOptions)) where
  getSeverityAnnotation ev =
    case ev of
      TrIncludeConnection {}                  -> Debug
      TrReleaseConnection {}                  -> Debug
      TrConnect {}                            -> Debug
      TrConnectError {}                       -> Info
      TrTerminatingConnection {}              -> Debug
      TrTerminatedConnection {}               -> Debug
      TrConnectionHandler _ ev'     ->
        case ev' of
          TrHandshakeSuccess {}               -> Info
          TrHandshakeQuery {}                 -> Info
          TrHandshakeClientError {}           -> Notice
          TrHandshakeServerError {}           -> Info
          TrConnectionHandlerError _ _ ShutdownNode            -> Critical
          TrConnectionHandlerError _ _ ShutdownPeer            -> Info

      TrShutdown                              -> Info
      TrConnectionExists {}                   -> Info
      TrForbiddenConnection {}                -> Info
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

instance HasPrivacyAnnotation (ConnMgr.AbstractTransitionTrace addr)
instance HasSeverityAnnotation (ConnMgr.AbstractTransitionTrace addr) where
  getSeverityAnnotation _ = Debug

instance HasPrivacyAnnotation (Server.Trace addr)
instance HasSeverityAnnotation (Server.Trace addr) where
  getSeverityAnnotation ev =
    case ev of
      Server.TrAcceptConnection {}                      -> Debug
      Server.TrAcceptError {}                           -> Error
      Server.TrAcceptPolicyTrace {}                     -> Notice
      Server.TrServerStarted {}                         -> Notice
      Server.TrServerStopped {}                         -> Notice
      Server.TrServerError {}                           -> Critical

instance HasPrivacyAnnotation (InboundGovernor.Trace addr)
instance HasSeverityAnnotation (InboundGovernor.Trace addr) where
  getSeverityAnnotation ev =
    case ev of
      InboundGovernor.TrNewConnection {}           -> Debug
      InboundGovernor.TrResponderRestarted {}      -> Debug
      InboundGovernor.TrResponderStartFailure {}   -> Info
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
      InboundGovernor.TrMaturedConnections {}      -> Info
      InboundGovernor.TrInactive {}                -> Debug

instance HasPrivacyAnnotation (Server.RemoteTransitionTrace addr)
instance HasSeverityAnnotation (Server.RemoteTransitionTrace addr) where
  getSeverityAnnotation _ = Debug

--
-- | instances of @Transformable@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance Transformable Text IO (Diffusion.DiffusionTracer RemoteAddress LocalAddress) where
  trTransformer = trStructuredText
instance HasTextFormatter (Diffusion.DiffusionTracer RemoteAddress LocalAddress) where
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


instance (StandardHash header, Show peer, ToJSON peer, ConvertRawHash header)
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

instance (StandardHash header, Show peer, ToJSON peer, ConvertRawHash header)
      => Transformable Text IO (BlockFetch.TraceDecisionEvent peer header) where
  trTransformer = trStructuredText
instance (StandardHash header, Show peer)
    => HasTextFormatter (BlockFetch.TraceDecisionEvent peer header) where
  formatText a _ = pack (show a)

instance ToObject peer
     => Transformable Text IO (TraceLabelPeer peer (NtN.TraceSendRecv (ChainSync (Header blk) (Point blk) (Tip blk)))) where
  trTransformer = trStructured
instance (Show peer, StandardHash blk, Show (Header blk))
     => HasTextFormatter (TraceLabelPeer peer (NtN.TraceSendRecv (ChainSync (Header blk) (Point blk) (Tip blk)))) where
  formatText a _ = pack (show a)

instance (ToObject peer, ToObject (AnyMessage (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))))
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

instance (forall fp. LocalStateQuery.ShowQuery (BlockQuery blk fp), ToObject localPeer)
     => Transformable Text IO (TraceLabelPeer localPeer (NtN.TraceSendRecv (LocalStateQuery blk (Point blk) (Query blk)))) where
  trTransformer = trStructured

instance (ToObject localPeer)
     => Transformable Text IO (TraceLabelPeer localPeer (NtN.TraceSendRecv KA.KeepAlive)) where
  trTransformer = trStructured

instance (ToObject localPeer, ToJSON addr)
     => Transformable Text IO (TraceLabelPeer localPeer (NtN.TraceSendRecv (PeerSharing.PeerSharing addr))) where
  trTransformer = trStructured

instance
  ( HasPrivacyAnnotation (Stateful.TraceSendRecv (LocalStateQuery blk (Point blk) (Query blk)) f)
  , HasSeverityAnnotation (Stateful.TraceSendRecv (LocalStateQuery blk (Point blk) (Query blk)) f)
  , forall fp. LocalStateQuery.ShowQuery (BlockQuery blk fp), ToObject localPeer)
     => Transformable Text IO (TraceLabelPeer localPeer (Stateful.TraceSendRecv (LocalStateQuery blk (Point blk) (Query blk)) f)) where
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
      => Transformable Text IO (Mux.WithBearer peer Mux.Trace) where
  trTransformer = trStructuredText
instance (Show peer)
      => HasTextFormatter (Mux.WithBearer peer Mux.Trace) where
  formatText (Mux.WithBearer peer ev) _o =
        "Bearer on " <> pack (show peer)
     <> " event: " <> pack (show ev)


instance Show exception => Transformable Text IO (TraceLocalRootPeers PeerTrustable RemoteAddress exception) where
  trTransformer = trStructuredText
instance Show exception => HasTextFormatter (TraceLocalRootPeers PeerTrustable RemoteAddress exception) where
    formatText a _ = pack (show a)

instance Transformable Text IO TracePublicRootPeers where
  trTransformer = trStructuredText
instance HasTextFormatter TracePublicRootPeers where
  formatText a _ = pack (show a)

instance Transformable Text IO (TracePeerSelection Cardano.DebugPeerSelectionState PeerTrustable (Cardano.PublicRootPeers.ExtraPeers SockAddr) SockAddr) where
  trTransformer = trStructuredText
instance (Show extraDebugState, Show extraFlags, Show (Cardano.PublicRootPeers.ExtraPeers addr)) => HasTextFormatter (TracePeerSelection extraDebugState extraFlags (Cardano.PublicRootPeers.ExtraPeers addr) SockAddr) where
  formatText a _ = pack (show a)

instance Transformable Text IO (DebugPeerSelection Cardano.ExtraState PeerTrustable (Cardano.PublicRootPeers.ExtraPeers SockAddr) SockAddr) where
  trTransformer = trStructuredText
instance HasTextFormatter (DebugPeerSelection extraDebugState extraFlags (Cardano.PublicRootPeers.ExtraPeers SockAddr) SockAddr) where
  -- One can only change what is logged with respect to verbosity using json
  -- format.
  formatText _ obj = pack (show obj)

instance Show lAddr => Transformable Text IO (PeerSelectionActionsTrace SockAddr lAddr) where
  trTransformer = trStructuredText
instance Show lAddr => HasTextFormatter (PeerSelectionActionsTrace SockAddr lAddr) where
  formatText a _ = pack (show a)

instance (ToJSON addr, Show addr) => Transformable Text IO (PeerSelectionCounters (Cardano.ExtraPeerSelectionSetsWithSizes addr)) where
  trTransformer = trStructuredText
instance Show extraCounters => HasTextFormatter (PeerSelectionCounters extraCounters) where
  formatText a _ = pack (show a)

instance (Show addr, Show versionNumber, Show agreedOptions, ToObject addr,
          ToJSON addr, ToJSON versionNumber, ToJSON agreedOptions
         )
      => Transformable Text IO (ConnMgr.Trace
                                 addr
                                 (ConnectionHandlerTrace versionNumber agreedOptions)) where
  trTransformer = trStructuredText
instance (Show addr, Show versionNumber, Show agreedOptions)
      => HasTextFormatter (ConnMgr.Trace
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
      => Transformable Text IO (Server.Trace addr) where
  trTransformer = trStructuredText
instance Show addr
      => HasTextFormatter (Server.Trace addr) where
  formatText a _ = pack (show a)

instance (ToJSON addr, Show addr)
      => Transformable Text IO (InboundGovernor.Trace addr) where
  trTransformer = trStructuredText
instance Show addr
      => HasTextFormatter (InboundGovernor.Trace addr) where
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
      => ToObject (AnyMessage (BlockFetch blk (Point blk))) where
  toObject MinimalVerbosity (AnyMessageAndAgency stok (MsgBlock blk)) =
    mconcat [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
             , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
             , "blockSize" .= toJSON (getSizeInBytes $ estimateBlockSize (getHeader blk))
             ]

  toObject verb (AnyMessageAndAgency stok (MsgBlock blk)) =
    mconcat [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
             , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
             , "blockSize" .= toJSON (getSizeInBytes $ estimateBlockSize (getHeader blk))
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

instance (forall result. Show (query result))
      => ToObject (AnyMessage (LocalStateQuery blk pt query)) where
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

instance (forall result. Show (query result))
      => ToObject (Stateful.AnyMessage (LocalStateQuery blk pt query) f) where
  toObject _verb (Stateful.AnyMessageAndAgency stok _ LocalStateQuery.MsgAcquire{}) =
    mconcat [ "kind" .= String "MsgAcquire"
            , "agency" .= String (pack $ show stok)
            ]
  toObject _verb (Stateful.AnyMessageAndAgency stok _ LocalStateQuery.MsgAcquired{}) =
    mconcat [ "kind" .= String "MsgAcquired"
            , "agency" .= String (pack $ show stok)
            ]
  toObject _verb (Stateful.AnyMessageAndAgency stok _ LocalStateQuery.MsgFailure{}) =
    mconcat [ "kind" .= String "MsgFailure"
            , "agency" .= String (pack $ show stok)
            ]
  toObject _verb (Stateful.AnyMessageAndAgency stok _ LocalStateQuery.MsgQuery{}) =
    mconcat [ "kind" .= String "MsgQuery"
            , "agency" .= String (pack $ show stok)
            ]
  toObject _verb (Stateful.AnyMessageAndAgency stok _ LocalStateQuery.MsgResult{}) =
    mconcat [ "kind" .= String "MsgResult"
            , "agency" .= String (pack $ show stok)
            ]
  toObject _verb (Stateful.AnyMessageAndAgency stok _ LocalStateQuery.MsgRelease{}) =
    mconcat [ "kind" .= String "MsgRelease"
            , "agency" .= String (pack $ show stok)
            ]
  toObject _verb (Stateful.AnyMessageAndAgency stok _ LocalStateQuery.MsgReAcquire{}) =
    mconcat [ "kind" .= String "MsgReAcquire"
            , "agency" .= String (pack $ show stok)
            ]
  toObject _verb (Stateful.AnyMessageAndAgency stok _ LocalStateQuery.MsgDone{}) =
    mconcat [ "kind" .= String "MsgDone"
            , "agency" .= String (pack $ show stok)
            ]

instance ToObject (AnyMessage (LocalTxMonitor txid tx slotno)) where
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
  toObject _verb (AnyMessageAndAgency stok LocalTxMonitor.MsgGetMeasures {}) =
    mconcat [ "kind" .= String "MsgGetMeasures"
            , "agency" .= String (pack $ show stok)
            ]
  toObject _verb (AnyMessageAndAgency stok LocalTxMonitor.MsgReplyGetMeasures {}) =
    mconcat [ "kind" .= String "MsgReplyMeasures"
            , "agency" .= String (pack $ show stok)
            ]

instance ToObject (AnyMessage (LocalTxSubmission tx err)) where
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

instance ToObject (AnyMessage (ChainSync blk pt tip)) where
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
      => ToObject (AnyMessage (TxSubmission2 txid tx)) where
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

instance ToObject (AnyMessage KA.KeepAlive) where
  toObject _verb (AnyMessageAndAgency stok KA.MsgKeepAlive {}) =
    mconcat
      [ "kind" .= String "MsgKeepAlive"
      , "agency" .= String (pack $ show stok)
      ]
  toObject _verb (AnyMessageAndAgency stok KA.MsgKeepAliveResponse {}) =
    mconcat
      [ "kind" .= String "MsgKeepAliveResponse"
      , "agency" .= String (pack $ show stok)
      ]
  toObject _verb (AnyMessageAndAgency stok KA.MsgDone) =
    mconcat
      [ "kind" .= String "MsgDone"
      , "agency" .= String (pack $ show stok)
      ]

instance ToJSON peerAddr => ToObject (AnyMessage (PeerSharing.PeerSharing peerAddr)) where
  toObject _verb (AnyMessageAndAgency stok (PeerSharing.MsgShareRequest num)) =
    mconcat
      [ "kind" .= String "MsgShareRequest"
      , "agency" .= String (pack $ show stok)
      , "ammount" .= PeerSharing.getAmount num
      ]
  toObject _verb (AnyMessageAndAgency stok (PeerSharing.MsgSharePeers peers)) =
    mconcat
      [ "kind" .= String "MsgSharePeers"
      , "agency" .= String (pack $ show stok)
      , "peers" .= peers
      ]
  toObject _verb (AnyMessageAndAgency stok PeerSharing.MsgDone) =
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

-- TODO: use 'ToJSON' constraints
instance (Show ntnAddr, Show ntcAddr) => ToObject (Diffusion.DiffusionTracer ntnAddr ntcAddr) where
  toObject _verb (Diffusion.RunServer sockAddr) = mconcat
    [ "kind" .= String "RunServer"
    , "socketAddress" .= String (pack (show sockAddr))
    ]

  toObject _verb (Diffusion.RunLocalServer localAddress) = mconcat
    [ "kind" .= String "RunLocalServer"
    , "localAddress" .= String (pack (show localAddress))
    ]
  toObject _verb (Diffusion.UsingSystemdSocket localAddress) = mconcat
    [ "kind" .= String "UsingSystemdSocket"
    , "path" .= String (pack . show $ localAddress)
    ]

  toObject _verb (Diffusion.CreateSystemdSocketForSnocketPath localAddress) = mconcat
    [ "kind" .= String "CreateSystemdSocketForSnocketPath"
    , "path" .= String (pack . show $ localAddress)
    ]
  toObject _verb (Diffusion.CreatedLocalSocket localAddress) = mconcat
    [ "kind" .= String "CreatedLocalSocket"
    , "path" .= String (pack . show $ localAddress)
    ]
  toObject _verb (Diffusion.ConfiguringLocalSocket localAddress socket) = mconcat
    [ "kind" .= String "ConfiguringLocalSocket"
    , "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (Diffusion.ListeningLocalSocket localAddress socket) = mconcat
    [ "kind" .= String "ListeningLocalSocket"
    , "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (Diffusion.LocalSocketUp localAddress fd) = mconcat
    [ "kind" .= String "LocalSocketUp"
    , "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show fd))
    ]
  toObject _verb (Diffusion.CreatingServerSocket socket) = mconcat
    [ "kind" .= String "CreatingServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (Diffusion.ListeningServerSocket socket) = mconcat
    [ "kind" .= String "ListeningServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (Diffusion.ServerSocketUp socket) = mconcat
    [ "kind" .= String "ServerSocketUp"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (Diffusion.ConfiguringServerSocket socket) = mconcat
    [ "kind" .= String "ConfiguringServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (Diffusion.UnsupportedLocalSystemdSocket path) = mconcat
    [ "kind" .= String "UnsupportedLocalSystemdSocket"
    , "path" .= String (pack (show path))
    ]
  toObject _verb Diffusion.UnsupportedReadySocketCase = mconcat
    [ "kind" .= String "UnsupportedReadySocketCase"
    ]
  toObject _verb (Diffusion.DiffusionErrored exception) = mconcat
    [ "kind" .= String "DiffusionErrored"
    , "path" .= String (pack (show exception))
    ]
  toObject _verb (Diffusion.SystemdSocketConfiguration config) = mconcat
    [ "kand" .= String "SystemdSocketConfiguration"
    , "message" .= String (pack (show config))
    ]

instance ToObject (NtC.HandshakeTr LocalAddress NodeToClientVersion) where
  toObject _verb (Mux.WithBearer b ev) =
    mconcat [ "kind" .= String "LocalHandshakeTrace"
             , "bearer" .= show b
             , "event" .= show ev ]


instance ToObject (NtN.HandshakeTr RemoteAddress NodeToNodeVersion) where
  toObject _verb (Mux.WithBearer b ev) =
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


instance ConvertRawHash header
      => ToJSON (Point header) where
  toJSON GenesisPoint = String "GenesisPoint"
  toJSON (BlockPoint (SlotNo slotNo) hash) =
    -- it is unlikely that there will be two short hashes in the same slot
    String $ renderHeaderHashForVerbosity
               (Proxy @header)
                MinimalVerbosity
                hash
          <> "@"
          <> pack (show slotNo)


newtype Verbose a = Verbose a

instance ConvertRawHash header
      => ToJSON (Verbose (Point header)) where
  toJSON (Verbose GenesisPoint) = String "GenesisPoint"
  toJSON (Verbose (BlockPoint (SlotNo slotNo) hash)) =
    -- it is unlikely that there will be two short hashes in the same slot
    String $ renderHeaderHashForVerbosity
               (Proxy @header)
                MaximalVerbosity
                hash
          <> "@"
          <> pack (show slotNo)


instance ConvertRawHash blk
      => ToObject (Point blk) where
  toObject _verb GenesisPoint =
    mconcat [ "point" .= String "GenesisPoint" ]
  toObject verb point@BlockPoint{} =
    mconcat [ "point" .=
                case verb of
                  MaximalVerbosity
                    -> toJSON (Verbose point)
                  _ -> toJSON point
            ]


instance ToObject SlotNo where
  toObject _verb slot =
    mconcat [ "kind" .= String "SlotNo"
             , "slot" .= toJSON (unSlotNo slot) ]

instance (ConvertRawHash blk) => ToObject (AF.Anchor blk) where
  toObject verb = \case
    AF.AnchorGenesis -> mconcat
      [ "kind" .= String "AnchorGenesis" ]
    AF.Anchor slot hash bno -> mconcat
      [ "kind" .= String "Anchor"
      , "slot" .= toJSON (unSlotNo slot)
      , "headerHash" .= renderHeaderHashForVerbosity (Proxy @blk) verb hash
      , "blockNo" .= toJSON (unBlockNo bno)
      ]

instance (ConvertRawHash blk, HasHeader blk) => ToObject (AF.AnchoredFragment blk) where
  toObject verb frag = mconcat
    [ "kind" .= String "AnchoredFragment"
    , "anchor" .= toObject verb (AF.anchor frag)
    , "headPoint" .= toObject verb (AF.headPoint frag)
    , "length" .= toJSON (AF.length frag)
    ]

instance ToJSON PeerGSV where
  toJSON PeerGSV { outboundGSV = GSV outboundG _ _
                 , inboundGSV = GSV inboundG _ _
                 } =
    Aeson.object ["G" .= (realToFrac (outboundG + inboundG) :: Double)]

instance (HasHeader header, ConvertRawHash header)
  => ToObject (TraceFetchClientState header) where
  toObject _verb BlockFetch.AddedFetchRequest {} =
    mconcat [ "kind" .= String "AddedFetchRequest" ]
  toObject _verb BlockFetch.AcknowledgedFetchRequest {} =
    mconcat [ "kind" .= String "AcknowledgedFetchRequest" ]
  toObject _verb (BlockFetch.SendFetchRequest af gsv) =
    mconcat [ "kind" .= String "SendFetchRequest"
             , "head" .= String (renderChainHash
                                  (renderHeaderHash (Proxy @header))
                                  (AF.headHash af))
             , "deltaq" .= toJSON gsv
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
             , "size"  .= getSizeInBytes blockSize
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

instance (ToJSON peer, ConvertRawHash header)
      => ToObject [TraceLabelPeer peer (FetchDecision [Point header])] where
  toObject MinimalVerbosity _ = mempty
  toObject _ [] = mempty
  toObject _ xs = mconcat
    [ "kind" .= String "FetchDecisions"
    , "decisions" .= toJSON xs
    ]

instance (ToObject peer, ToObject a) => ToObject (TraceLabelPeer peer a) where
  toObject verb (TraceLabelPeer peerid a) =
    mconcat [ "peer" .= toObject verb peerid ] <> toObject verb a

instance (ToJSON peer, ToJSON point)
      => ToJSON (TraceLabelPeer peer (FetchDecision [point])) where
  toJSON (TraceLabelPeer peer decision) =
    Aeson.object
      [ "peer" .= toJSON peer
      , "decision" .= toJSON (FetchDecisionToJSON decision)
      ]

instance (ToJSON peer, ToJSON (Verbose point))
    => ToJSON (Verbose (TraceLabelPeer peer (FetchDecision [point]))) where
              toJSON (Verbose (TraceLabelPeer peer decision)) =
                Aeson.object
                [ "peer" .= toJSON peer
                , "decision" .= toJSON (FetchDecisionToJSON $ map Verbose <$> decision)
                ]

newtype FetchDecisionToJSON point =
    FetchDecisionToJSON (FetchDecision [point])

instance ToJSON point
      => ToJSON (FetchDecisionToJSON point) where
  toJSON (FetchDecisionToJSON (Left decline)) =
    Aeson.object [ "declined" .= String (pack . show $ decline) ]
  toJSON (FetchDecisionToJSON (Right points)) =
    toJSON points

instance (ToJSON peer, ConvertRawHash header)
      => ToObject (BlockFetch.TraceDecisionEvent peer header) where
  toObject  verb (BlockFetch.PeersFetch as) = toObject verb as
  toObject _verb (BlockFetch.PeerStarvedUs peer) = mconcat
    [ "kind" .= String "PeerStarvedUs"
    , "peer" .= toJSON peer
    ]

instance ToObject (AnyMessage ps)
      => ToObject (TraceSendRecv ps) where
  toObject verb (TraceSendMsg m) = mconcat
    [ "kind" .= String "Send" , "msg" .= toObject verb m ]
  toObject verb (TraceRecvMsg m) = mconcat
    [ "kind" .= String "Recv" , "msg" .= toObject verb m ]


instance ToObject (Stateful.AnyMessage ps f)
      => ToObject (Stateful.TraceSendRecv ps f) where
  toObject verb (Stateful.TraceSendMsg m) = mconcat
    [ "kind" .= String "Send" , "msg" .= toObject verb m ]
  toObject verb (Stateful.TraceRecvMsg m) = mconcat
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

instance Aeson.ToJSONKey PeerTrustable where

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
      [ "kind" .= String "AddSample"
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
  toObject _verb (PickedBigLedgerPeer addr _ackStake stake) =
    mconcat
      [ "kind" .= String "PickedBigLedgerPeer"
      , "address" .= show addr
      , "relativeStake" .= (realToFrac (unPoolStake stake) :: Double)
      ]
  toObject _verb (PickedBigLedgerPeers (NumberOfPeers n) addrs) =
    mconcat
      [ "kind" .= String "PickedBigLedgerPeers"
      , "desiredCount" .= n
      , "count" .= length addrs
      , "addresses" .= show addrs
      ]
  toObject _verb (PickedLedgerPeer addr _ackStake stake) =
    mconcat
      [ "kind" .= String "PickedLedgerPeer"
      , "address" .= show addr
      , "relativeStake" .= (realToFrac (unPoolStake stake) :: Double)
      ]
  toObject _verb (PickedLedgerPeers (NumberOfPeers n) addrs) =
    mconcat
      [ "kind" .= String "PickedLedgerPeers"
      , "desiredCount" .= n
      , "count" .= length addrs
      , "addresses" .= show addrs
      ]
  toObject _verb (FetchingNewLedgerState cnt bigCnt) =
    mconcat
      [ "kind" .= String "FetchingNewLedgerState"
      , "numberOfLedgerPeers" .= cnt
      , "numberOfBigLedgerPeers" .= bigCnt
      ]
  toObject _verb DisabledLedgerPeers =
    mconcat
      [ "kind" .= String "DisabledLedgerPeers"
      ]
  toObject _verb (TraceUseLedgerPeers ulp) =
    mconcat
      [ "kind" .= String "UseLedgerPeers"
      , "useLedgerPeers" .= ulp
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
  toObject _verb FallingBackToPublicRootPeers =
    mconcat
      [ "kind" .= String "FallingBackToBootstrapPeers"
      ]
  toObject _verb (NotEnoughLedgerPeers (NumberOfPeers target) numOfLedgerPeers) =
    mconcat
      [ "kind" .= String "NotEnoughLedgerPeers"
      , "target" .= target
      , "numOfLedgerPeers" .= numOfLedgerPeers
      ]
  toObject _verb (NotEnoughBigLedgerPeers (NumberOfPeers target) numOfBigLedgerPeers) =
    mconcat
      [ "kind" .= String "NotEnoughBigLedgerPeers"
      , "target" .= target
      , "numOfBigLedgerPeers" .= numOfBigLedgerPeers
      ]
  toObject _verb (TraceLedgerPeersDomains daps) =
    mconcat
      [ "kind" .= String "TraceLedgerPeersDomains"
      , "domainAccessPoints" .= daps
      ]
  toObject _verb (TraceLedgerPeersResult dap ips) =
    mconcat
      [ "kind" .= String "TraceLedgerPeersResult"
      , "domainAccessPoint" .= show dap
      , "ips" .= map show ips
      ]
  toObject _verb (TraceLedgerPeersFailure dap reason) =
    mconcat
      [ "kind" .= String "TraceLedgerPeersFailure"
      , "domainAccessPoint" .= show dap
      , "error" .= show reason
      ]
  toObject _verb UsingBigLedgerPeerSnapshot =
    mconcat
      [ "kind" .= String "UsingBigLedgerPeerSnapshot"
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


instance ToObject peer => ToObject (Mux.WithBearer peer Mux.Trace) where
  toObject verb (Mux.WithBearer b ev) =
    mconcat [ "kind" .= String "Mux.Trace"
             , "bearer" .= toObject verb b
             , "event" .= show ev ]

instance Aeson.ToJSONKey RelayAccessPoint where

instance ToJSON HotValency where
  toJSON (HotValency v) = toJSON v
instance ToJSON WarmValency where
  toJSON (WarmValency v) = toJSON v

instance FromJSON HotValency where
  parseJSON v = HotValency <$> parseJSON v

instance FromJSON WarmValency where
  parseJSON v = WarmValency <$> parseJSON v

instance ToJSON (LocalRootConfig PeerTrustable) where
  toJSON LocalRootConfig { peerAdvertise,
                           extraFlags = peerTrustable,
                           diffusionMode } =
    Aeson.object
      [ "peerAdvertise" .= peerAdvertise
      , "diffusionMode" .= show diffusionMode
      , "extraFlags"    .= show peerTrustable
      ]

instance Show exception => ToObject (TraceLocalRootPeers PeerTrustable RemoteAddress exception) where
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
  toObject _verb (TraceLocalRootReconfigured _ _) =
    mconcat [ "kind" .= String "LocalRootReconfigured"
             ]
  toObject _verb (TraceLocalRootDNSMap dnsMap) =
    mconcat
      [ "kind" .= String "TraceLocalRootDNSMap"
      , "dnsMap" .= dnsMap
      ]

instance Aeson.ToJSONKey DomainAccessPoint where
  toJSONKey = Aeson.toJSONKeyText render
    where
      render da = mconcat
        [ Text.decodeUtf8 (dapDomain da)
        , ":"
        , Text.pack $ show @Int (fromIntegral (dapPortNumber da))
        ]

instance ToJSON IP where
  toJSON ip = String (pack . show $ ip)

instance ToObject TracePublicRootPeers where
  toObject _verb (TracePublicRootRelayAccessPoint relays) =
    mconcat [ "kind" .= String "PublicRootRelayAddresses"
             , "relayAddresses" .= toJSON relays
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

instance ToJSON KnownPeerInfo where
  toJSON (KnownPeerInfo
            nKnownPeerFailCount
            nKnownPeerTepid
            nKnownPeerSharing
            nKnownPeerAdvertise
            nKnownSuccessfulConnection
         ) =
    Aeson.object [ "kind" .= String "KnownPeerInfo"
                 , "failCount" .= nKnownPeerFailCount
                 , "tepid" .= nKnownPeerTepid
                 , "peerSharing" .= nKnownPeerSharing
                 , "peerAdvertise" .= nKnownPeerAdvertise
                 , "successfulConnection" .= nKnownSuccessfulConnection
                 ]

instance ToJSON PeerStatus where
  toJSON = String . pack . show

instance (Aeson.ToJSONKey peerAddr, ToJSON peerAddr, Ord peerAddr, Show peerAddr)
  => ToJSON (LocalRootPeers PeerTrustable peerAddr) where
  toJSON lrp =
    Aeson.object [ "kind" .= String "LocalRootPeers"
                 , "groups" .= Aeson.toJSONList (LocalRootPeers.toGroups lrp)
                 ]

instance ToJSON PeerSelectionTargets where
  toJSON (PeerSelectionTargets
            nRootPeers
            nKnownPeers
            nEstablishedPeers
            nActivePeers
            nKnownBigLedgerPeers
            nEstablishedBigLedgerPeers
            nActiveBigLedgerPeers
         ) =
    Aeson.object [ "kind" .= String "PeerSelectionTargets"
                 , "targetRootPeers" .= nRootPeers
                 , "targetKnownPeers" .= nKnownPeers
                 , "targetEstablishedPeers" .= nEstablishedPeers
                 , "targetActivePeers" .= nActivePeers

                 , "targetKnownBigLedgerPeers" .= nKnownBigLedgerPeers
                 , "targetEstablishedBigLedgerPeers" .= nEstablishedBigLedgerPeers
                 , "targetActiveBigLedgerPeers" .= nActiveBigLedgerPeers
                 ]

instance ToJSON peerAddr => ToJSON (PublicRootPeers (Cardano.PublicRootPeers.ExtraPeers peerAddr) peerAddr) where
  toJSON prp =
    Aeson.object [ "kind" .= String "PublicRootPeers"
                 , "bootstrapPeers" .= PublicRootPeers.getBootstrapPeers prp
                 , "ledgerPeers" .= PublicRootPeers.getLedgerPeers prp
                 , "bigLedgerPeers" .= PublicRootPeers.getBigLedgerPeers prp
                 , "publicConfigPeers" .= Map.keysSet (PublicRootPeers.getPublicConfigPeers prp)
                 ]

instance ToJSON RepromoteDelay where
  toJSON = toJSON . repromoteDelay

instance ToJSON addr => ToJSON (PeerSharingResult addr) where
  toJSON (PeerSharingResult addrs) = Aeson.toJSONList addrs
  toJSON PeerSharingNotRegisteredYet = String "PeerSharingNotRegisteredYet"

instance ToObject (TracePeerSelection Cardano.DebugPeerSelectionState PeerTrustable (Cardano.PublicRootPeers.ExtraPeers SockAddr) SockAddr) where
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
             , "result" .= toJSON res
             , "group" .= group
             , "diffTime" .= dt
             ]
  toObject _verb (TracePublicRootsFailure err group dt) =
    mconcat [ "kind" .= String "PublicRootsFailure"
             , "reason" .= show err
             , "group" .= group
             , "diffTime" .= dt
             ]
  toObject _verb (TraceBigLedgerPeersRequest tBigLedgerPeers nBigLedgerPeers) =
    mconcat [ "kind" .= String "BigLedgerPeersRequest"
             , "targetNumberOfBigLedgerPeers" .= tBigLedgerPeers
             , "numberOfBigLedgerPeers" .= nBigLedgerPeers
             ]
  toObject _verb (TraceBigLedgerPeersResults res group dt) =
    mconcat [ "kind" .= String "BigLedgerPeersResults"
             , "result" .= Aeson.toJSONList (toList res)
             , "group" .= group
             , "diffTime" .= dt
             ]
  toObject _verb (TraceBigLedgerPeersFailure err group dt) =
    mconcat [ "kind" .= String "BigLedgerPeersFailure"
             , "reason" .= show err
             , "group" .= group
             , "diffTime" .= dt
             ]
  toObject _verb (TraceForgetBigLedgerPeers targetKnown actualKnown sp) =
    mconcat [ "kind" .= String "ForgetBigLedgerPeers"
             , "targetKnown" .= targetKnown
             , "actualKnown" .= actualKnown
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TracePeerShareRequests targetKnown actualKnown (PeerSharingAmount numRequested) aps sps) =
    mconcat [ "kind" .= String "PeerShareRequests"
             , "targetKnown" .= targetKnown
             , "actualKnown" .= actualKnown
             , "numRequested" .= numRequested
             , "availablePeers" .= Aeson.toJSONList (toList aps)
             , "selectedPeers" .= Aeson.toJSONList (toList sps)
             ]
  toObject _verb (TracePeerShareResults res) =
    mconcat [ "kind" .= String "PeerShareResults"
             , "result" .= Aeson.toJSONList (map ( first show <$> ) res)
             ]
  toObject _verb (TracePeerShareResultsFiltered res) =
    mconcat [ "kind" .= String "PeerShareResultsFiltered"
             , "result" .= Aeson.toJSONList res
             ]
  toObject _verb (TraceForgetColdPeers targetKnown actualKnown sp) =
    mconcat [ "kind" .= String "ForgetColdPeers"
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
  toObject _verb (TracePromoteColdLocalPeers tLocalEst sp) =
    mconcat [ "kind" .= String "PromoteColdLocalPeers"
             , "targetLocalEstablished" .= tLocalEst
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
  toObject _verb (TracePromoteColdBigLedgerPeers targetKnown actualKnown sp) =
    mconcat [ "kind" .= String "PromoteColdBigLedgerPeers"
             , "targetEstablished" .= targetKnown
             , "actualEstablished" .= actualKnown
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TracePromoteColdBigLedgerPeerFailed tEst aEst p d err) =
    mconcat [ "kind" .= String "PromoteColdBigLedgerPeerFailed"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             , "delay" .= toJSON d
             , "reason" .= show err
             ]
  toObject _verb (TracePromoteColdBigLedgerPeerDone tEst aEst p) =
    mconcat [ "kind" .= String "PromoteColdBigLedgerPeerDone"
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
  toObject _verb (TracePromoteWarmBigLedgerPeers tActive aActive sp) =
    mconcat [ "kind" .= String "PromoteWarmBigLedgerPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TracePromoteWarmBigLedgerPeerFailed tActive aActive p err) =
    mconcat [ "kind" .= String "PromoteWarmBigLedgerPeerFailed"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  toObject _verb (TracePromoteWarmBigLedgerPeerDone tActive aActive p) =
    mconcat [ "kind" .= String "PromoteWarmBigLedgerPeerDone"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  toObject _verb (TracePromoteWarmBigLedgerPeerAborted tActive aActive p) =
    mconcat [ "kind" .= String "PromoteWarmBigLedgerPeerAborted"
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
  toObject _verb (TraceDemoteWarmBigLedgerPeers tEst aEst sp) =
    mconcat [ "kind" .= String "DemoteWarmBigLedgerPeers"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TraceDemoteWarmBigLedgerPeerFailed tEst aEst p err) =
    mconcat [ "kind" .= String "DemoteWarmBigLedgerPeerFailed"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  toObject _verb (TraceDemoteWarmBigLedgerPeerDone tEst aEst p) =
    mconcat [ "kind" .= String "DemoteWarmBigLedgerPeerDone"
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
  toObject _verb (TraceDemoteHotBigLedgerPeers tActive aActive sp) =
    mconcat [ "kind" .= String "DemoteHotBigLedgerPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TraceDemoteHotBigLedgerPeerFailed tActive aActive p err) =
    mconcat [ "kind" .= String "DemoteHotBigLedgerPeerFailed"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  toObject _verb (TraceDemoteHotBigLedgerPeerDone tActive aActive p) =
    mconcat [ "kind" .= String "DemoteHotBigLedgerPeerDone"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  toObject _verb (TraceDemoteAsynchronous msp) =
    mconcat [ "kind" .= String "DemoteAsynchronous"
             , "state" .= toJSON msp
             ]
  toObject _verb (TraceDemoteLocalAsynchronous msp) =
    mconcat [ "kind" .= String "DemoteLocalAsynchronous"
             , "state" .= toJSON msp
             ]
  toObject _verb (TraceDemoteBigLedgerPeersAsynchronous msp) =
    mconcat [ "kind" .= String "DemoteBigLedgerPeersAsynchronous"
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
  toObject _verb (TracePickInboundPeers targetNumberOfKnownPeers numberOfKnownPeers selected available) =
    mconcat [ "kind" .= String "PickInboundPeers"
            , "targetKnown" .= targetNumberOfKnownPeers
            , "actualKnown" .= numberOfKnownPeers
            , "selected" .= selected
            , "available" .= available
            ]
  toObject _verb (TraceLedgerStateJudgementChanged new) =
    mconcat [ "kind" .= String "LedgerStateJudgementChanged"
             , "new" .= show new ]
  toObject _verb TraceOnlyBootstrapPeers =
    mconcat [ "kind" .= String "OnlyBootstrapPeers" ]
  toObject _verb (TraceUseBootstrapPeersChanged ubp) =
    mconcat [ "kind" .= String "UseBootstrapPeersChanged"
             , "bootstrapPeers" .= show ubp ]
  toObject _verb TraceBootstrapPeersFlagChangedWhilstInSensitiveState =
    mconcat [ "kind" .= String "BootstrapPeersFlagChangedWhilstInSensitiveState"
            ]
  toObject _verb (TraceVerifyPeerSnapshot result) =
    mconcat [ "kind" .= String "VerifyPeerSnapshot"
            , "result" .= toJSON result ]
  toObject _verb (TraceOutboundGovernorCriticalFailure err) =
    mconcat [ "kind" .= String "OutboundGovernorCriticalFailure"
             , "reason" .= show err
             ]
  toObject _verb (TraceChurnAction duration action counter) =
    mconcat [ "kind" .= String "ChurnAction"
            , "action" .= show action
            , "counter" .= counter
            , "duration" .= duration
            ]
  toObject _verb (TraceChurnTimeout duration action counter) =
    mconcat [ "kind" .= String "ChurnTimeout"
            , "action" .= show action
            , "counter" .= counter
            , "duration" .= duration
            ]
  toObject _verb (TraceDebugState mtime ds) =
   mconcat [ "kind" .= String "DebugState"
            , "monotonicTime" .= mtime
            , "targets" .= peerSelectionTargetsToObject (dpssTargets ds)
            , "localRootPeers" .= dpssLocalRootPeers ds
            , "publicRootPeers" .= dpssPublicRootPeers ds
            , "knownPeers" .= KnownPeers.allPeers (dpssKnownPeers ds)
            , "establishedPeers" .= dpssEstablishedPeers ds
            , "activePeers" .= dpssActivePeers ds
            , "publicRootBackoffs" .= dpssPublicRootBackoffs ds
            , "publicRootRetryTime" .= dpssPublicRootRetryTime ds
            , "bigLedgerPeerBackoffs" .= dpssBigLedgerPeerBackoffs ds
            , "bigLedgerPeerRetryTime" .= dpssBigLedgerPeerRetryTime ds
            , "inProgressBigLedgerPeersReq" .= dpssInProgressBigLedgerPeersReq ds
            , "inProgressPeerShareReqs" .= dpssInProgressPeerShareReqs ds
            , "inProgressPromoteCold" .= dpssInProgressPromoteCold ds
            , "inProgressPromoteWarm" .= dpssInProgressPromoteWarm ds
            , "inProgressDemoteWarm" .= dpssInProgressDemoteWarm ds
            , "inProgressDemoteHot" .= dpssInProgressDemoteHot ds
            , "inProgressDemoteToCold" .= dpssInProgressDemoteToCold ds
            , "upstreamyness" .= dpssUpstreamyness ds
            , "fetchynessBlocks" .= dpssFetchynessBlocks ds
            , "ledgerStateJudgement" .= Cardano.debugLedgerStateJudgement (dpssExtraState ds)
            , "associationMode" .= dpssAssociationMode ds
            ]

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
                         targetNumberOfActivePeers,
                         targetNumberOfKnownBigLedgerPeers,
                         targetNumberOfEstablishedBigLedgerPeers,
                         targetNumberOfActiveBigLedgerPeers
                       } =
    Object $
      mconcat [ "roots" .= targetNumberOfRootPeers
               , "knownPeers" .= targetNumberOfKnownPeers
               , "established" .= targetNumberOfEstablishedPeers
               , "active" .= targetNumberOfActivePeers
               , "knownBigLedgerPeers" .= targetNumberOfKnownBigLedgerPeers
               , "establishedBigLedgerPeers" .= targetNumberOfEstablishedBigLedgerPeers
               , "activeBigLedgerPeers" .= targetNumberOfActiveBigLedgerPeers
               ]

instance ToObject (DebugPeerSelection Cardano.ExtraState PeerTrustable (Cardano.PublicRootPeers.ExtraPeers SockAddr) SockAddr) where
  toObject verb (TraceGovernorState blockedAt wakeupAfter
                   st@PeerSelectionState { targets })
      | verb <= NormalVerbosity =
    mconcat [ "kind" .= String "DebugPeerSelection"
             , "blockedAt" .= String (pack $ show blockedAt)
             , "wakeupAfter" .= String (pack $ show wakeupAfter)
             , "targets" .= peerSelectionTargetsToObject targets
             , "counters" .= toObject verb (peerSelectionStateToCounters
                                             Cardano.PublicRootPeers.toSet
                                             Cardano.cardanoPeerSelectionStatetoCounters
                                             st)

             ]
  toObject _ (TraceGovernorState blockedAt wakeupAfter ev) =
    mconcat [ "kind" .= String "DebugPeerSelection"
             , "blockedAt" .= String (pack $ show blockedAt)
             , "wakeupAfter" .= String (pack $ show wakeupAfter)
             , "peerSelectionState" .= String (pack $ show ev)
             ]

-- TODO: Write PeerStatusChangeType ToJSON at ouroboros-network
-- For that an export is needed at ouroboros-network
instance Show lAddr => ToObject (PeerSelectionActionsTrace SockAddr lAddr) where
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
    mconcat [ "kind" .= String "PeerMonitoringError"
             , "connectionId" .= toJSON connId
             , "reason" .= show s
             ]
  toObject _verb (PeerMonitoringResult connId wf) =
    mconcat [ "kind" .= String "PeerMonitoringResult"
             , "connectionId" .= toJSON connId
             , "withProtocolTemp" .= show wf
             ]
  toObject _verb (AcquireConnectionError exception) =
    mconcat [ "kind" .= String "AcquireConnectionError"
            , "error" .= displayException exception
            ]

instance ToJSON peeraddr => ToObject (PeerSelectionCounters (Cardano.ExtraPeerSelectionSetsWithSizes peeraddr)) where
  toObject _verb PeerSelectionCounters {..} =
    mconcat [ "kind" .= String "PeerSelectionCounters"

            , "knownPeers" .= numberOfKnownPeers
            , "rootPeers" .= numberOfRootPeers
            , "coldPeersPromotions" .= numberOfColdPeersPromotions
            , "establishedPeers" .= numberOfEstablishedPeers
            , "warmPeersDemotions" .= numberOfWarmPeersDemotions
            , "warmPeersPromotions" .= numberOfWarmPeersPromotions
            , "activePeers" .= numberOfActivePeers
            , "activePeersDemotions" .= numberOfActivePeersDemotions

            , "knownBigLedgerPeers" .= numberOfKnownBigLedgerPeers
            , "coldBigLedgerPeersPromotions" .= numberOfColdBigLedgerPeersPromotions
            , "establishedBigLedgerPeers" .= numberOfEstablishedBigLedgerPeers
            , "warmBigLedgerPeersDemotions" .= numberOfWarmBigLedgerPeersDemotions
            , "warmBigLedgerPeersPromotions" .= numberOfWarmBigLedgerPeersPromotions
            , "activeBigLedgerPeers" .= numberOfActiveBigLedgerPeers
            , "activeBigLedgerPeersDemotions" .= numberOfActiveBigLedgerPeersDemotions

            , "knownLocalRootPeers" .= numberOfKnownLocalRootPeers
            , "establishedLocalRootPeers" .= numberOfEstablishedLocalRootPeers
            , "warmLocalRootPeersPromotions" .= numberOfWarmLocalRootPeersPromotions
            , "activeLocalRootPeers" .= numberOfActiveLocalRootPeers
            , "activeLocalRootPeersDemotions" .= numberOfActiveLocalRootPeersDemotions

            , "knownNonRootPeers" .= numberOfKnownNonRootPeers
            , "coldNonRootPeersPromotions" .= numberOfColdNonRootPeersPromotions
            , "establishedNonRootPeers" .= numberOfEstablishedNonRootPeers
            , "warmNonRootPeersDemotions" .= numberOfWarmNonRootPeersDemotions
            , "warmNonRootPeersPromotions" .= numberOfWarmNonRootPeersPromotions
            , "activeNonRootPeers" .= numberOfActiveNonRootPeers
            , "activeNonRootPeersDemotions" .= numberOfActiveNonRootPeersDemotions

            , "knownBootstrapPeers" .= snd (Cardano.viewKnownBootstrapPeers extraCounters)
            , "coldBootstrapPeersPromotions" .= snd (Cardano.viewColdBootstrapPeersPromotions extraCounters)
            , "establishedBootstrapPeers" .= snd (Cardano.viewEstablishedBootstrapPeers extraCounters)
            , "warmBootstrapPeersDemotions" .= snd (Cardano.viewWarmBootstrapPeersDemotions extraCounters)
            , "warmBootstrapPeersPromotions" .= snd (Cardano.viewWarmBootstrapPeersPromotions extraCounters)
            , "activeBootstrapPeers" .= snd (Cardano.viewActiveBootstrapPeers extraCounters)
            , "activeBootstrapPeersDemotions" .= snd (Cardano.viewActiveBootstrapPeersDemotions extraCounters)
            ]

instance ToJSON ProtocolLimitFailure where
  toJSON (ExceededSizeLimit tok) =
    Aeson.object [ "kind" .= String "ProtocolLimitFailure"
                 , "agency" .= show tok
                 ]
  toJSON (ExceededTimeLimit tok) =
    Aeson.object [ "kind" .= String "ProtocolLimitFailure"
                 , "agency" .= show tok
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
  toJSON QueryNotSupported =
    Aeson.object [ "kind" .= String "QueryNotSupported"
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
  toJSON NodeToNodeV_13  = Number 13
  toJSON NodeToNodeV_14  = Number 14

instance FromJSON NodeToNodeVersion where
  parseJSON (Number 13) = return NodeToNodeV_13
  parseJSON (Number 14) = return NodeToNodeV_14
  parseJSON (Number x) = fail ("FromJSON.NodeToNodeVersion: unsupported node-to-node protocol version " ++ show x)
  parseJSON x          = fail ("FromJSON.NodeToNodeVersion: error parsing NodeToNodeVersion: " ++ show x)

instance ToJSON NodeToClientVersion where
  toJSON NodeToClientV_16 = Number 16
  toJSON NodeToClientV_17 = Number 17
  toJSON NodeToClientV_18 = Number 18
  toJSON NodeToClientV_19 = Number 19
  toJSON NodeToClientV_20 = Number 20
  -- NB: When adding a new version here, update FromJSON below as well!

instance FromJSON NodeToClientVersion where
  parseJSON (Number 16) = return NodeToClientV_16
  parseJSON (Number 17) = return NodeToClientV_17
  parseJSON (Number 18) = return NodeToClientV_18
  parseJSON (Number 19) = return NodeToClientV_19
  parseJSON (Number x) = fail ("FromJSON.NodeToClientVersion: unsupported node-to-client protocol version " ++ show x)
  parseJSON x          = fail ("FromJSON.NodeToClientVersion: error parsing NodeToClientVersion: " ++ show x)

instance ToJSON NodeToNodeVersionData where
  toJSON (NodeToNodeVersionData (NetworkMagic m) dm ps q) =
    Aeson.object [ "networkMagic" .= toJSON m
                 , "diffusionMode" .= show dm
                 , "peerSharing" .= show ps
                 , "query" .= toJSON q
                 ]

instance ToJSON NodeToClientVersionData where
  toJSON (NodeToClientVersionData (NetworkMagic m) q) =
    Aeson.object [ "networkMagic" .= toJSON m
                 , "query" .= toJSON q
                 ]

instance (Show versionNumber, ToJSON versionNumber, ToJSON agreedOptions)
  => ToObject (ConnectionHandlerTrace versionNumber agreedOptions) where
  toObject _verb (TrHandshakeSuccess versionNumber agreedOptions) =
    mconcat
      [ "kind" .= String "HandshakeSuccess"
      , "versionNumber" .= toJSON versionNumber
      , "agreedOptions" .= toJSON agreedOptions
      ]
  toObject _verb (TrHandshakeQuery vMap) =
    mconcat
      [ "kind" .= String "HandshakeQuery"
      , "versions" .= toJSON ((\(k,v) -> Aeson.object [
          "versionNumber" .= k
        , "options" .= v
        ]) <$> Map.toList vMap)
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
  toObject _verb (TrConnectionHandlerError e err cerr) =
    mconcat
      [ "kind" .= String "Error"
      , "context" .= show e
      , "reason" .= show err
      , "command" .= show cerr
      ]

instance ToJSON addr => ToJSON (LocalAddr addr) where
  toJSON (LocalAddr addr) = toJSON addr
  toJSON UnknownLocalAddr = Null

instance ToJSON NtN.DiffusionMode where
  toJSON = String . pack . show

instance ToJSON ConnStateId where
  toJSON (ConnStateId connStateId) = toJSON connStateId

instance ToObject ConnStateId where
  toObject _ connStateId = mconcat [ "connStateId" .= toJSON connStateId ]

instance (Show addr, Show versionNumber, Show agreedOptions, ToObject addr,
          ToJSON addr, ToJSON versionNumber, ToJSON agreedOptions)
      => ToObject (ConnMgr.Trace addr (ConnectionHandlerTrace versionNumber agreedOptions)) where
  toObject verb ev =
    case ev of
      TrIncludeConnection prov peerAddr ->
        mconcat $ reverse
          [ "kind" .= String "IncludeConnection"
          , "remoteAddress" .= toObject verb peerAddr
          , "provenance" .= String (pack . show $ prov)
          ]
      TrReleaseConnection prov connId ->
        mconcat $ reverse
          [ "kind" .= String "UnregisterConnection"
          , "remoteAddress" .= toJSON connId
          , "provenance" .= String (pack . show $ prov)
          ]
      TrConnect (Just localAddress) remoteAddress diffusionMode ->
        mconcat
          [ "kind" .= String "Connect"
          , "connectionId" .= toJSON ConnectionId { localAddress, remoteAddress }
          , "diffusionMode" .= toJSON diffusionMode
          ]
      TrConnect Nothing remoteAddress diffusionMode ->
        mconcat
          [ "kind" .= String "Connect"
          , "remoteAddress" .= toObject verb remoteAddress
          , "diffusionMode" .= toJSON diffusionMode
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
          , "choiceSet" .= toJSON (toJSON `Set.map` chosenPeers)
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
          , "state" .= listValue (\(remoteAddr, inner) ->
                                         Aeson.object
                                           [ "connections" .=
                                             listValue (\(localAddr, connState) ->
                                                Aeson.object
                                                  [ "localAddress" .= localAddr
                                                  , "state" .= toJSON connState
                                                  ]
                                             )
                                             (Map.toList inner)
                                           , "remoteAddress" .= toJSON remoteAddr
                                           ]
                                 )
                                 (Map.toList (getConnMap cmState))
          ]
      ConnMgr.TrUnexpectedlyFalseAssertion info ->
        mconcat
          [ "kind" .= String "UnexpectedlyFalseAssertion"
          , "info" .= String (pack . show $ info)
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
      => ToObject (Server.Trace addr) where
  toObject _verb (Server.TrAcceptConnection connId)     =
    mconcat [ "kind" .= String "AcceptConnection"
             , "connectionId" .= toJSON connId
             ]
  toObject _verb (Server.TrAcceptError exception)         =
    mconcat [ "kind" .= String "AcceptErroor"
             , "reason" .= show exception
             ]
  toObject verb (Server.TrAcceptPolicyTrace policyTrace) =
    mconcat [ "kind" .= String "AcceptPolicyServer.Trace"
             , "policy" .= toObject verb policyTrace
             ]
  toObject verb (Server.TrServerStarted peerAddrs)       =
    mconcat [ "kind" .= String "AcceptPolicyServer.Trace"
             , "addresses" .= toJSON (toObject verb `map` peerAddrs)
             ]
  toObject _verb Server.TrServerStopped                   =
    mconcat [ "kind" .= String "ServerStopped"
             ]
  toObject _verb (Server.TrServerError exception)         =
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

instance ToJSON Time where
  toJSON = String . pack . show

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
      => ToObject (InboundGovernor.Trace addr) where
  toObject _verb (InboundGovernor.TrNewConnection p connId)            =
    mconcat [ "kind" .= String "NewConnection"
             , "provenance" .= show p
             , "connectionId" .= toJSON connId
             ]
  toObject _verb (InboundGovernor.TrResponderRestarted connId m)       =
    mconcat [ "kind" .= String "ResponderStarted"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             ]
  toObject _verb (InboundGovernor.TrResponderStartFailure connId m s)  =
    mconcat [ "kind" .= String "ResponderStartFailure"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             , "reason" .= show s
             ]
  toObject _verb (InboundGovernor.TrResponderErrored connId m s)       =
    mconcat [ "kind" .= String "ResponderErrored"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             , "reason" .= show s
             ]
  toObject _verb (InboundGovernor.TrResponderStarted connId m)         =
    mconcat [ "kind" .= String "ResponderStarted"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             ]
  toObject _verb (InboundGovernor.TrResponderTerminated connId m)      =
    mconcat [ "kind" .= String "ResponderTerminated"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             ]
  toObject _verb (InboundGovernor.TrPromotedToWarmRemote connId opRes) =
    mconcat [ "kind" .= String "PromotedToWarmRemote"
             , "connectionId" .= toJSON connId
             , "result" .= toJSON opRes
             ]
  toObject _verb (InboundGovernor.TrPromotedToHotRemote connId)        =
    mconcat [ "kind" .= String "PromotedToHotRemote"
             , "connectionId" .= toJSON connId
             ]
  toObject _verb (InboundGovernor.TrDemotedToColdRemote connId od)     =
    mconcat [ "kind" .= String "DemotedToColdRemote"
             , "connectionId" .= toJSON connId
             , "result" .= show od
             ]
  toObject _verb (InboundGovernor.TrDemotedToWarmRemote connId)     =
    mconcat [ "kind" .= String "DemotedToWarmRemote"
             , "connectionId" .= toJSON connId
             ]
  toObject _verb (InboundGovernor.TrWaitIdleRemote connId opRes) =
    mconcat [ "kind" .= String "WaitIdleRemote"
             , "connectionId" .= toJSON connId
             , "result" .= toJSON opRes
             ]
  toObject _verb (InboundGovernor.TrMuxCleanExit connId)               =
    mconcat [ "kind" .= String "MuxCleanExit"
             , "connectionId" .= toJSON connId
             ]
  toObject _verb (InboundGovernor.TrMuxErrored connId s)               =
    mconcat [ "kind" .= String "MuxErrored"
             , "connectionId" .= toJSON connId
             , "reason" .= show s
             ]
  toObject _verb (InboundGovernor.TrInboundGovernorCounters counters) =
    mconcat [ "kind" .= String "InboundGovernorCounters"
             , "idlePeers" .= InboundGovernor.idlePeersRemote counters
             , "coldPeers" .= InboundGovernor.coldPeersRemote counters
             , "warmPeers" .= InboundGovernor.warmPeersRemote counters
             , "hotPeers" .= InboundGovernor.hotPeersRemote counters
             ]
  toObject _verb (InboundGovernor.TrRemoteState st) =
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
  toObject _verb (InboundGovernor.TrMaturedConnections matured fresh) =
    mconcat [ "kind" .= String "MaturedConnections"
            , "matured" .= toJSON matured
            , "fresh" .= toJSON fresh
            ]
  toObject _verb (InboundGovernor.TrInactive fresh) =
    mconcat [ "kind" .= String "Inactive"
            , "fresh" .= toJSON fresh
            ]

instance ToJSON addr
      => ToObject (Server.RemoteTransitionTrace addr) where
    toObject _verb (ConnMgr.TransitionTrace addr tr) =
      mconcat [ "kind"    .= String "InboundGovernorTransition"
               , "address" .= toJSON addr
               , "from"    .= toJSON (ConnMgr.fromState tr)
               , "to"      .= toJSON (ConnMgr.toState   tr)
               ]

instance FromJSON PeerSharing where
  parseJSON = Aeson.withBool "PeerSharing" $ \b ->
    pure $ if b then PeerSharingEnabled
                else PeerSharingDisabled

instance ToJSON PeerSharing where
  toJSON PeerSharingEnabled = Bool True
  toJSON PeerSharingDisabled = Bool False

instance FromJSON UseLedgerPeers where
  parseJSON (Number slot) = return $
    case compare slot 0 of
      GT -> UseLedgerPeers (After (SlotNo (floor slot)))
      EQ -> UseLedgerPeers Always
      LT -> DontUseLedgerPeers
  parseJSON invalid = fail $ "Parsing of slot number failed due to type mismatch. "
                            <> "Encountered: " <> show invalid

instance ToJSON LedgerStateJudgement where
  toJSON YoungEnough = String "YoungEnough"
  toJSON TooOld      = String "TooOld"

instance FromJSON LedgerStateJudgement where
  parseJSON (String "YoungEnough") = pure YoungEnough
  parseJSON (String "TooOld")      = pure TooOld
  parseJSON _                      = fail "Invalid JSON for LedgerStateJudgement"

instance ToJSON AssociationMode where
  toJSON LocalRootsOnly = String "LocalRootsOnly"
  toJSON Unrestricted   = String "Unrestricted"

instance FromJSON AssociationMode where
  parseJSON (String "LocalRootsOnly") = pure LocalRootsOnly
  parseJSON (String "Unrestricted")   = pure Unrestricted
  parseJSON _                      = fail "Invalid JSON for AssociationMode"

instance ToJSON UseLedgerPeers where
  toJSON DontUseLedgerPeers                  = Number (-1)
  toJSON (UseLedgerPeers Always)             = Number 0
  toJSON (UseLedgerPeers (After (SlotNo s))) = Number (fromIntegral s)

instance ToJSON UseBootstrapPeers where
  toJSON DontUseBootstrapPeers   = Null
  toJSON (UseBootstrapPeers dps) = toJSON dps

instance FromJSON UseBootstrapPeers where
  parseJSON Null = pure DontUseBootstrapPeers
  parseJSON v    = UseBootstrapPeers <$> parseJSON v

instance FromJSON PeerTrustable where
  parseJSON = Aeson.withBool "PeerTrustable" $ \b ->
    pure $ if b then IsTrustable
                else IsNotTrustable

instance ToJSON PeerTrustable where
  toJSON IsTrustable = Bool True
  toJSON IsNotTrustable = Bool False
