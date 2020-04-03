{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE NamedFieldPuns        #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Tracing.ToObjectOrphans
  ( ) where

import           Cardano.Prelude hiding (atomically, show)
import           Prelude (String, show, id)

import           Data.Aeson (Value (..), ToJSON, toJSON, (.=))
import           Data.Text (pack)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Network.Socket as Socket (SockAddr)
import           Network.Mux (WithMuxBearer (..), MuxTrace (..))

import           Cardano.BM.Tracing
import           Cardano.BM.Data.Tracer (HasTextFormatter (..), emptyObject,
                   mkObject, trStructured, trStructuredText)
import qualified Cardano.Chain.Block as Block
import qualified Cardano.Chain.Delegation as Dlg
import qualified Cardano.Crypto.Signing as Crypto

import           Ouroboros.Consensus.Block
                   (Header, headerPoint,
                    RealPoint, realPointSlot, realPointHash)
import           Ouroboros.Consensus.Byron.Ledger
                   (ByronBlock(..), byronHeaderRaw)
import           Ouroboros.Network.Point (withOrigin)
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                   (TraceBlockFetchServerEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                   (TraceChainSyncClientEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
                   (TraceChainSyncServerEvent(..))
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                   (LedgerSupportsProtocol)
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Protocol.BFT as BFT
import qualified Ouroboros.Consensus.Protocol.PBFT as PBFT
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mempool.API (GenTx, GenTxId,
                   HasTxId, HasTxs(..), TraceEventMempool (..), ApplyTxErr,
                   MempoolSize(..), TxId, extractTxs, txId)
import qualified Ouroboros.Consensus.Mock.Ledger as Mock
import qualified Ouroboros.Consensus.Mock.Protocol.Praos as Praos
import           Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                   (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import qualified Ouroboros.Consensus.Byron.Ledger as Byron

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch.ClientState
                   (TraceFetchClientState (..), TraceLabelPeer (..))
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision)
import           Ouroboros.Network.Codec (AnyMessage (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.NodeToNode
                   (WithAddr(..), ErrorPolicyTrace(..), TraceSendRecv (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch, Message(..))
import           Ouroboros.Network.Protocol.TxSubmission.Type
                   (Message (..), TxSubmission)
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.Subscription (ConnectResult (..), DnsTrace (..),
                   SubscriptionTrace (..), SubscriberError (..),
                   WithDomainName (..), WithIPList (..))
import           Ouroboros.Network.TxSubmission.Inbound
                   (TraceTxSubmissionInbound(..))
import           Ouroboros.Network.TxSubmission.Outbound
                   (TraceTxSubmissionOutbound (..))

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB

showTip :: Condense (HeaderHash blk)
        => TracingVerbosity
        -> Tip blk
        -> String
showTip verb = showPoint verb . getTipPoint

showPoint :: Condense (HeaderHash blk)
          => TracingVerbosity
          -> Point blk
          -> String
showPoint verb pt =
  case pt of
    GenesisPoint -> "genesis (origin)"
    BlockPoint slot h -> trim (condense h) ++ "@" ++ condense slot
 where
  trim :: [a] -> [a]
  trim = case verb of
    MinimalVerbosity -> take 7
    NormalVerbosity -> take 7
    MaximalVerbosity -> id

--
-- * instances of @HasPrivacyAnnotation@ and @HasSeverityAnnotation@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

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

instance HasPrivacyAnnotation (ChainDB.TraceEvent blk)
instance HasSeverityAnnotation (ChainDB.TraceEvent blk) where
  getSeverityAnnotation (ChainDB.TraceAddBlockEvent ev) = case ev of
    ChainDB.IgnoreBlockOlderThanK {} -> Info
    ChainDB.IgnoreBlockAlreadyInVolDB {} -> Info
    ChainDB.IgnoreInvalidBlock {} -> Info
    ChainDB.AddedBlockToQueue {} -> Debug
    ChainDB.BlockInTheFuture {} -> Info
    ChainDB.StoreButDontChange {} -> Debug
    ChainDB.TryAddToCurrentChain {} -> Debug
    ChainDB.TrySwitchToAFork {} -> Info
    ChainDB.AddedToCurrentChain {} -> Notice
    ChainDB.SwitchedToAFork {} -> Notice
    ChainDB.AddBlockValidation ev' -> case ev' of
      ChainDB.InvalidBlock {} -> Error
      ChainDB.InvalidCandidate {} -> Error
      ChainDB.ValidCandidate {} -> Info
      ChainDB.CandidateExceedsRollback {} -> Error
    ChainDB.AddedBlockToVolDB {} -> Debug
    ChainDB.ChainChangedInBg {} -> Info
    ChainDB.ScheduledChainSelection {} -> Debug
    ChainDB.RunningScheduledChainSelection {} -> Debug

  getSeverityAnnotation (ChainDB.TraceLedgerReplayEvent ev) = case ev of
    LedgerDB.ReplayFromGenesis {} -> Info
    LedgerDB.ReplayFromSnapshot {} -> Info
    LedgerDB.ReplayedBlock {} -> Info

  getSeverityAnnotation (ChainDB.TraceLedgerEvent ev) = case ev of
    LedgerDB.TookSnapshot {} -> Info
    LedgerDB.DeletedSnapshot {} -> Debug
    LedgerDB.InvalidSnapshot {} -> Error

  getSeverityAnnotation (ChainDB.TraceCopyToImmDBEvent ev) = case ev of
    ChainDB.CopiedBlockToImmDB {} -> Debug
    ChainDB.NoBlocksToCopyToImmDB -> Debug

  getSeverityAnnotation (ChainDB.TraceGCEvent ev) = case ev of
    ChainDB.PerformedGC {} -> Debug
    ChainDB.ScheduledGC {} -> Debug

  getSeverityAnnotation (ChainDB.TraceOpenEvent ev) = case ev of
    ChainDB.OpenedDB {} -> Info
    ChainDB.ClosedDB {} -> Info
    ChainDB.OpenedImmDB {} -> Info
    ChainDB.OpenedVolDB -> Info
    ChainDB.OpenedLgrDB -> Info

  getSeverityAnnotation (ChainDB.TraceReaderEvent ev) = case ev of
    ChainDB.NewReader {} -> Info
    ChainDB.ReaderNoLongerInMem {} -> Info
    ChainDB.ReaderSwitchToMem {} -> Info
    ChainDB.ReaderNewImmIterator {} -> Info
  getSeverityAnnotation (ChainDB.TraceInitChainSelEvent ev) = case ev of
    ChainDB.InitChainSelValidation {} -> Debug
  getSeverityAnnotation (ChainDB.TraceIteratorEvent ev) = case ev of
    ChainDB.StreamFromVolDB {} -> Debug
    _ -> Debug
  getSeverityAnnotation (ChainDB.TraceImmDBEvent _ev) = Debug
  getSeverityAnnotation (ChainDB.TraceVolDBEvent _ev) = Debug

instance HasPrivacyAnnotation (TraceBlockFetchServerEvent blk)
instance HasSeverityAnnotation (TraceBlockFetchServerEvent blk) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (TraceChainSyncClientEvent blk)
instance HasSeverityAnnotation (TraceChainSyncClientEvent blk) where
  getSeverityAnnotation (TraceDownloadedHeader _) = Info
  getSeverityAnnotation (TraceFoundIntersection _ _ _) = Info
  getSeverityAnnotation (TraceRolledBack _) = Notice
  getSeverityAnnotation (TraceException _) = Warning

instance HasPrivacyAnnotation (TraceChainSyncServerEvent blk b)
instance HasSeverityAnnotation (TraceChainSyncServerEvent blk b) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (TraceEventMempool blk)
instance HasSeverityAnnotation (TraceEventMempool blk) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (TraceFetchClientState header)
instance HasSeverityAnnotation (TraceFetchClientState header) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (TraceForgeEvent blk tx)
instance HasSeverityAnnotation (TraceForgeEvent blk tx) where
  getSeverityAnnotation TraceForgedBlock {}            = Info
  getSeverityAnnotation TraceStartLeadershipCheck {}   = Info
  getSeverityAnnotation TraceNodeNotLeader {}          = Info
  getSeverityAnnotation TraceNodeIsLeader {}           = Info
  getSeverityAnnotation TraceNoLedgerState {}          = Error
  getSeverityAnnotation TraceNoLedgerView {}           = Error
  getSeverityAnnotation TraceBlockFromFuture {}        = Error
  getSeverityAnnotation TraceSlotIsImmutable {}        = Error
  getSeverityAnnotation TraceAdoptedBlock {}           = Info
  getSeverityAnnotation TraceDidntAdoptBlock {}        = Error
  getSeverityAnnotation TraceForgedInvalidBlock {}     = Error

instance HasPrivacyAnnotation (TraceLocalTxSubmissionServerEvent blk)
instance HasSeverityAnnotation (TraceLocalTxSubmissionServerEvent blk) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (TraceSendRecv a)
instance HasSeverityAnnotation (TraceSendRecv a) where
  getSeverityAnnotation _ = Debug

instance HasPrivacyAnnotation a => HasPrivacyAnnotation (TraceLabelPeer peer a)
instance HasSeverityAnnotation a => HasSeverityAnnotation (TraceLabelPeer peer a)

instance HasPrivacyAnnotation [TraceLabelPeer peer (FetchDecision [Point header])]
instance HasSeverityAnnotation [TraceLabelPeer peer (FetchDecision [Point header])] where
  getSeverityAnnotation [] = Debug
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))
instance HasSeverityAnnotation (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk)) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))
instance HasSeverityAnnotation (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)) where
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
  formatText _ = pack . show . toList

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

--
-- | instances of @Transformable@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance Transformable Text IO NtN.HandshakeTr where
  trTransformer = trStructuredText
instance HasTextFormatter NtN.HandshakeTr where
  formatText _ = pack . show . toList

instance Transformable Text IO NtC.HandshakeTr where
  trTransformer = trStructuredText
instance HasTextFormatter NtC.HandshakeTr where
  formatText _ = pack . show . toList

instance Transformable Text IO NtN.AcceptConnectionsPolicyTrace where
  trTransformer = trStructuredText
instance HasTextFormatter NtN.AcceptConnectionsPolicyTrace where
  formatText _ = pack . show . toList

instance ( HasPrivacyAnnotation (ChainDB.TraceAddBlockEvent blk)
         , HasSeverityAnnotation (ChainDB.TraceAddBlockEvent blk)
         , LedgerSupportsProtocol blk
         , ToObject (ChainDB.TraceAddBlockEvent blk))
 => Transformable Text IO (ChainDB.TraceAddBlockEvent blk) where
   trTransformer = trStructuredText
instance (LedgerSupportsProtocol blk)
 => HasTextFormatter (ChainDB.TraceAddBlockEvent blk) where
  formatText _ = pack . show . toList

instance Transformable Text IO (TraceBlockFetchServerEvent blk) where
  trTransformer = trStructuredText
instance HasTextFormatter (TraceBlockFetchServerEvent blk) where
  formatText _ = pack . show . toList

instance (Condense (HeaderHash blk), LedgerSupportsProtocol blk)
 => Transformable Text IO (TraceChainSyncClientEvent blk) where
  trTransformer verb tr = trStructured verb tr

instance Condense (HeaderHash blk)
 => Transformable Text IO (TraceChainSyncServerEvent blk b) where
  trTransformer verb tr = trStructured verb tr

instance (ToObject (GenTx blk), ToJSON (GenTxId blk), Show (ApplyTxErr blk))
 => Transformable Text IO (TraceEventMempool blk) where
  trTransformer verb tr = trStructured verb tr

condenseT :: Condense a => a -> Text
condenseT = pack . condense
showT :: Show a => a -> Text
showT = pack . show

instance ( Condense (HeaderHash blk)
         , HasTxId tx
         , LedgerSupportsProtocol blk
         , Show (TxId tx)
         , ToObject (LedgerError blk)
         , ToObject (ValidationErr (BlockProtocol blk)))
 => Transformable Text IO (TraceForgeEvent blk tx) where
  trTransformer = trStructuredText
instance ( Condense (HeaderHash blk)
         , HasTxId tx
         , LedgerSupportsProtocol blk
         , Show (TxId tx) )
 => HasTextFormatter (TraceForgeEvent blk tx) where
   formatText = \case
        TraceAdoptedBlock slotNo blk txs -> const $
          "Adopted forged block for slot " <> showT (unSlotNo slotNo) <> ": " <> condenseT (blockHash blk) <> "; TxIds: " <> showT (map txId txs)
        TraceBlockFromFuture currentSlot tip -> const $
          "Forged block from future: current slot " <> showT (unSlotNo currentSlot) <> ", tip being " <> condenseT tip
        TraceSlotIsImmutable slotNo tipPoint tipBlkNo -> const $
          "Forged for immutable slot " <> showT (unSlotNo slotNo) <> ", tip: " <> pack (showPoint MaximalVerbosity tipPoint) <> ", block no: " <> showT (unBlockNo tipBlkNo)
        TraceDidntAdoptBlock slotNo _ -> const $
          "Didn't adopt forged block at slot " <> showT (unSlotNo slotNo)
        TraceForgedBlock slotNo _ _ _ -> const $
          "Forged block for slot " <> showT (unSlotNo slotNo)
        TraceForgedInvalidBlock slotNo _ reason -> const $
          "Forged invalid block for slot " <> showT (unSlotNo slotNo) <> ", reason: " <> showT reason
        TraceNodeIsLeader slotNo -> const $
          "Leading slot " <> showT (unSlotNo slotNo)
        TraceNodeNotLeader slotNo -> const $
          "Not leading slot " <> showT (unSlotNo slotNo)
        TraceNoLedgerState slotNo _blk -> const $
          "No ledger state at slot " <> showT (unSlotNo slotNo)
        TraceNoLedgerView slotNo _ -> const $
          "No ledger view at slot " <> showT (unSlotNo slotNo)
        TraceStartLeadershipCheck slotNo -> const $
          "Testing for leadership at slot " <> showT (unSlotNo slotNo)

instance Show peer
 => Transformable Text IO [TraceLabelPeer peer (FetchDecision [Point header])] where
  trTransformer = trStructuredText
instance HasTextFormatter [TraceLabelPeer peer (FetchDecision [Point header])] where
  formatText _ = pack . show . toList

instance (Show peer, HasPrivacyAnnotation a, HasSeverityAnnotation a, ToObject a)
 => Transformable Text IO (TraceLabelPeer peer a) where
  trTransformer = trStructuredText
instance HasTextFormatter (TraceLabelPeer peer a) where
  formatText _ = pack . show . toList

instance Transformable Text IO (TraceLocalTxSubmissionServerEvent blk) where
  trTransformer = trStructured

instance Transformable Text IO (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk)) where
  trTransformer = trStructuredText
instance HasTextFormatter (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk)) where
  formatText _ = pack . show . toList

instance (Show (GenTxId blk), Show (GenTx blk))
 => Transformable Text IO (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)) where
  trTransformer = trStructuredText
instance HasTextFormatter (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)) where
  formatText _ = pack . show . toList

instance Show addr => Transformable Text IO (WithAddr addr ErrorPolicyTrace) where
  trTransformer = trStructuredText
instance HasTextFormatter (WithAddr addr ErrorPolicyTrace) where
  formatText _ = pack . show . toList

instance Transformable Text IO (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  trTransformer = trStructuredText
instance HasTextFormatter (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  formatText _ = pack . show . toList

instance Transformable Text IO (WithDomainName DnsTrace) where
  trTransformer = trStructuredText
instance HasTextFormatter (WithDomainName DnsTrace) where
  formatText _ = pack . show . toList

instance Transformable Text IO (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  trTransformer = trStructuredText
instance HasTextFormatter (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  formatText _ = pack . show . toList

instance (Show peer)
 => Transformable Text IO (WithMuxBearer peer MuxTrace) where
  trTransformer = trStructuredText
instance (Show peer)
 => HasTextFormatter (WithMuxBearer peer MuxTrace) where
  formatText (WithMuxBearer peer ev) = \_o -> "Bearer on " <> (pack $ show peer) <> " event: " <> (pack $ show ev)

instance ( Condense (HeaderHash blk)
         , LedgerSupportsProtocol blk
         , ToObject (Header blk))
 => Transformable Text IO (ChainDB.TraceEvent blk) where
  trTransformer = trStructuredText

instance (Condense (HeaderHash blk), LedgerSupportsProtocol blk)
 => HasTextFormatter (ChainDB.TraceEvent blk) where
   formatText = \case
      ChainDB.TraceAddBlockEvent ev -> case ev of
        ChainDB.IgnoreBlockOlderThanK pt -> const $
          "Ignoring block older than K: " <> condenseT pt
        ChainDB.IgnoreBlockAlreadyInVolDB pt -> \_o ->
          "Ignoring block already in DB: " <> condenseT pt
        ChainDB.IgnoreInvalidBlock pt _reason -> \_o ->
          "Ignoring previously seen invalid block: " <> condenseT pt
        ChainDB.AddedBlockToQueue pt sz -> \_o ->
          "Block added to queue: " <> condenseT pt <> " queue size " <> condenseT sz
        ChainDB.BlockInTheFuture pt slot -> \_o ->
          "Ignoring block from future: " <> condenseT pt <> ", slot " <> condenseT slot
        ChainDB.StoreButDontChange pt -> \_o ->
          "Ignoring block: " <> condenseT pt
        ChainDB.TryAddToCurrentChain pt -> \_o ->
          "Block fits onto the current chain: " <> condenseT pt
        ChainDB.TrySwitchToAFork pt _ -> \_o ->
          "Block fits onto some fork: " <> condenseT pt
        ChainDB.AddedToCurrentChain _ _ c -> \_o ->
          "Chain extended, new tip: " <> condenseT (AF.headPoint c)
        ChainDB.SwitchedToAFork _ _ c -> \_o ->
          "Switched to a fork, new tip: " <> condenseT (AF.headPoint c)
        ChainDB.AddBlockValidation ev' -> case ev' of
          ChainDB.InvalidBlock err pt -> \_o ->
            "Invalid block " <> condenseT pt <> ": " <> showT err
          ChainDB.InvalidCandidate c -> \_o ->
            "Invalid candidate " <> condenseT (AF.headPoint c)
          ChainDB.ValidCandidate c -> \_o ->
            "Valid candidate " <> condenseT (AF.headPoint c)
          ChainDB.CandidateExceedsRollback _ _ c -> \_o ->
            "Exceeds rollback " <> condenseT (AF.headPoint c)
        ChainDB.AddedBlockToVolDB pt _ _ -> \_o ->
          "Chain added block " <> condenseT pt
        ChainDB.ChainChangedInBg c1 c2 -> \_o ->
          "Chain changed in bg, from " <> condenseT (AF.headPoint c1) <> " to "  <> condenseT (AF.headPoint c2)
        ChainDB.ScheduledChainSelection pt slot _n -> \_o ->
          "Chain selection scheduled for future: " <> condenseT pt
                                      <> ", slot " <> condenseT slot
        ChainDB.RunningScheduledChainSelection pts slot _n -> \_o ->
          "Running scheduled chain selection: " <> condenseT (NonEmpty.toList pts)
                                  <> ", slot " <> condenseT slot
      ChainDB.TraceLedgerReplayEvent ev -> case ev of
        LedgerDB.ReplayFromGenesis _replayTo -> \_o ->
          "Replaying ledger from genesis"
        LedgerDB.ReplayFromSnapshot snap tip' _replayTo -> \_o ->
          "Replaying ledger from snapshot " <> showT snap <> " at " <>
            condenseT tip'
        LedgerDB.ReplayedBlock pt replayTo -> \_o ->
          "Replayed block: slot " <> showT (realPointSlot pt) <> " of " <> showT (pointSlot replayTo)
      ChainDB.TraceLedgerEvent ev -> case ev of
        LedgerDB.TookSnapshot snap pt -> \_o ->
          "Took ledger snapshot " <> showT snap <> " at " <> condenseT pt
        LedgerDB.DeletedSnapshot snap -> \_o ->
          "Deleted old snapshot " <> showT snap
        LedgerDB.InvalidSnapshot snap failure -> \_o ->
          "Invalid snapshot " <> showT snap <> showT failure
      ChainDB.TraceCopyToImmDBEvent ev -> case ev of
        ChainDB.CopiedBlockToImmDB pt -> \_o ->
          "Copied block " <> condenseT pt <> " to the ImmutableDB"
        ChainDB.NoBlocksToCopyToImmDB -> \_o ->
          "There are no blocks to copy to the ImmutableDB"
      ChainDB.TraceGCEvent ev -> case ev of
        ChainDB.PerformedGC slot -> \_o -> 
          "Performed a garbage collection for " <> condenseT slot
        ChainDB.ScheduledGC slot _difft -> \_o ->
          "Scheduled a garbage collection for " <> condenseT slot
      ChainDB.TraceOpenEvent ev -> case ev of
        ChainDB.OpenedDB immTip tip' -> \_o ->
          "Opened db with immutable tip at " <> condenseT immTip <>
          " and tip " <> condenseT tip'
        ChainDB.ClosedDB immTip tip' -> \_o ->
          "Closed db with immutable tip at " <> condenseT immTip <>
          " and tip " <> condenseT tip'
        ChainDB.OpenedImmDB immTip epoch -> \_o ->
          "Opened imm db with immutable tip at " <> condenseT immTip <>
          " and epoch " <> showT epoch
        ChainDB.OpenedVolDB -> \_o -> "Opened vol db"
        ChainDB.OpenedLgrDB -> \_o -> "Opened lgr db"
      ChainDB.TraceReaderEvent ev -> case ev of
        ChainDB.NewReader -> \_o -> "New reader was created"
        ChainDB.ReaderNoLongerInMem _ -> \_o -> "ReaderNoLongerInMem"
        ChainDB.ReaderSwitchToMem _ _ -> \_o -> "ReaderSwitchToMem"
        ChainDB.ReaderNewImmIterator _ _ -> \_o -> "ReaderNewImmIterator"
      ChainDB.TraceInitChainSelEvent ev -> case ev of
        ChainDB.InitChainSelValidation _ -> \_o -> "InitChainSelValidation"
      ChainDB.TraceIteratorEvent ev -> case ev of
        ChainDB.UnknownRangeRequested _ -> \_o -> "UnknownRangeRequested"
        ChainDB.BlockMissingFromVolDB _ -> \_o -> "BlockMissingFromVolDB"
        ChainDB.StreamFromImmDB _ _ -> \_o -> "StreamFromImmDB"
        ChainDB.StreamFromBoth _ _ _ -> \_o -> "StreamFromBoth"
        ChainDB.StreamFromVolDB _ _ _ -> \_o -> "StreamFromVolDB"
        ChainDB.BlockWasCopiedToImmDB _ -> \_o -> "BlockWasCopiedToImmDB"
        ChainDB.BlockGCedFromVolDB _ -> \_o -> "BlockGCedFromVolDB"
        ChainDB.SwitchBackToVolDB -> \_o -> "SwitchBackToVolDB"
      ChainDB.TraceImmDBEvent _ev -> \_o -> "TraceImmDBEvent"
      ChainDB.TraceVolDBEvent _ev -> \_o -> "TraceVolDBEvent"


--
-- | instances of @ToObject@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance ( Condense (HeaderHash blk)
         , Condense (TxId (GenTx blk))
         , HasHeader blk
         , HasTxs blk
         , HasTxId (GenTx blk)
         )
 => ToObject (AnyMessage (BlockFetch blk)) where
  toObject _verb (AnyMessage (MsgBlock blk)) =
    mkObject
      [ "kind" .= String "MsgBlock"
      , "blkid" .= String (pack . condense $ blockHash blk)
      , "txids" .= toJSON (presentTx <$> extractTxs blk)
      ]
   where
     presentTx :: GenTx blk -> Value
     presentTx =  String . pack . condense . txId
  toObject _v (AnyMessage MsgRequestRange{}) =
    mkObject [ "kind" .= String "MsgRequestRange" ]
  toObject _v (AnyMessage MsgStartBatch{}) =
    mkObject [ "kind" .= String "MsgStartBatch" ]
  toObject _v (AnyMessage MsgNoBlocks{}) =
    mkObject [ "kind" .= String "MsgNoBlocks" ]
  toObject _v (AnyMessage MsgBatchDone{}) =
    mkObject [ "kind" .= String "MsgBatchDone" ]
  toObject _v (AnyMessage MsgClientDone{}) =
    mkObject [ "kind" .= String "MsgClientDone" ]

{-
instance ToObject Byron.ApplyMempoolPayloadErr where
  toObject = TODO
-}

instance ToObject BFT.BftValidationErr where
  toObject _verb (BFT.BftInvalidSignature err) =
    mkObject
      [ "kind" .= String "BftInvalidSignature"
      , "error" .= String (pack err)
      ]

instance ToObject (GenTx Byron.ByronBlock) where
  toObject verb tx =
    mkObject $
        [ "txid" .= txId tx ]
     ++ [ "tx"   .= condense tx | verb == MaximalVerbosity ]

instance ToJSON (TxId (GenTx Byron.ByronBlock)) where
  toJSON (Byron.ByronTxId             i) = toJSON (condense i)
  toJSON (Byron.ByronDlgId            i) = toJSON (condense i)
  toJSON (Byron.ByronUpdateProposalId i) = toJSON (condense i)
  toJSON (Byron.ByronUpdateVoteId     i) = toJSON (condense i)

instance ToObject Block.ChainValidationError where
  toObject _verb Block.ChainValidationBoundaryTooLarge =
    mkObject
      [ "kind" .= String "ChainValidationBoundaryTooLarge" ]
  toObject _verb Block.ChainValidationBlockAttributesTooLarge =
    mkObject
      [ "kind" .= String "ChainValidationBlockAttributesTooLarge" ]
  toObject _verb (Block.ChainValidationBlockTooLarge _ _) =
    mkObject
      [ "kind" .= String "ChainValidationBlockTooLarge" ]
  toObject _verb Block.ChainValidationHeaderAttributesTooLarge =
    mkObject
      [ "kind" .= String "ChainValidationHeaderAttributesTooLarge" ]
  toObject _verb (Block.ChainValidationHeaderTooLarge _ _) =
    mkObject
      [ "kind" .= String "ChainValidationHeaderTooLarge" ]
  toObject _verb (Block.ChainValidationDelegationPayloadError err) =
    mkObject
      [ "kind" .= String err ]
  toObject _verb (Block.ChainValidationInvalidDelegation _ _) =
    mkObject
      [ "kind" .= String "ChainValidationInvalidDelegation" ]
  toObject _verb (Block.ChainValidationGenesisHashMismatch _ _) =
    mkObject
      [ "kind" .= String "ChainValidationGenesisHashMismatch" ]
  toObject _verb (Block.ChainValidationExpectedGenesisHash _ _) =
    mkObject
      [ "kind" .= String "ChainValidationExpectedGenesisHash" ]
  toObject _verb (Block.ChainValidationExpectedHeaderHash _ _) =
    mkObject
      [ "kind" .= String "ChainValidationExpectedHeaderHash" ]
  toObject _verb (Block.ChainValidationInvalidHash _ _) =
    mkObject
      [ "kind" .= String "ChainValidationInvalidHash" ]
  toObject _verb (Block.ChainValidationMissingHash _) =
    mkObject
      [ "kind" .= String "ChainValidationMissingHash" ]
  toObject _verb (Block.ChainValidationUnexpectedGenesisHash _) =
    mkObject
      [ "kind" .= String "ChainValidationUnexpectedGenesisHash" ]
  toObject _verb (Block.ChainValidationInvalidSignature _) =
    mkObject
      [ "kind" .= String "ChainValidationInvalidSignature" ]
  toObject _verb (Block.ChainValidationDelegationSchedulingError _) =
    mkObject
      [ "kind" .= String "ChainValidationDelegationSchedulingError" ]
  toObject _verb (Block.ChainValidationProtocolMagicMismatch _ _) =
    mkObject
      [ "kind" .= String "ChainValidationProtocolMagicMismatch" ]
  toObject _verb Block.ChainValidationSignatureLight =
    mkObject
      [ "kind" .= String "ChainValidationSignatureLight" ]
  toObject _verb (Block.ChainValidationTooManyDelegations _) =
    mkObject
      [ "kind" .= String "ChainValidationTooManyDelegations" ]
  toObject _verb (Block.ChainValidationUpdateError _ _) =
    mkObject
      [ "kind" .= String "ChainValidationUpdateError" ]
  toObject _verb (Block.ChainValidationUTxOValidationError _) =
    mkObject
      [ "kind" .= String "ChainValidationUTxOValidationError" ]
  toObject _verb (Block.ChainValidationProofValidationError _) =
    mkObject
      [ "kind" .= String "ChainValidationProofValidationError" ]

instance ToObject LedgerDB.DiskSnapshot where
  toObject MinimalVerbosity snap = toObject NormalVerbosity snap
  toObject NormalVerbosity _ = mkObject [ "kind" .= String "snapshot" ]
  toObject MaximalVerbosity snap =
    mkObject [ "kind" .= String "snapshot"
             , "snapshot" .= String (pack $ show snap) ]

instance ( StandardHash blk
         , ToObject (LedgerError blk)
         , ToObject (ValidationErr (BlockProtocol blk)))
 => ToObject (ExtValidationError blk) where
  toObject verb (ExtValidationErrorLedger err) = toObject verb err
  toObject verb (ExtValidationErrorHeader err) = toObject verb err

instance ToObject (FetchDecision [Point header]) where
  toObject _verb (Left decline) =
    mkObject [ "kind" .= String "FetchDecision declined"
             , "declined" .= String (pack $ show $ decline) ]
  toObject _verb (Right results) =
    mkObject [ "kind" .= String "FetchDecision results"
             , "length" .= String (pack $ show $ length results) ]

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

instance ( Mock.SimpleCrypto c
         , Typeable a)
 => ToObject (Header (Mock.SimpleBlock c a)) where
  toObject _verb b =
    mkObject $
        [ "kind" .= String "SimpleBlockHeader"
        , "hash" .= condense (blockHash b)
        , "prevhash" .= condense (blockPrevHash b)
        , "slotNo" .= condense (blockSlot b)
        , "blockNo" .= condense (blockNo b) ]

instance ToObject (Header ByronBlock) where
  toObject _verb b =
    mkObject $
        [ "kind" .= String "ByronBlock"
        , "hash" .= condense (blockHash b)
        , "prevhash" .= condense (blockPrevHash b)
        , "slotNo" .= condense (blockSlot b)
        , "blockNo" .= condense (blockNo b)
        ] <>
        case byronHeaderRaw b of
          Block.ABOBBoundaryHdr{} -> []
          Block.ABOBBlockHdr h ->
            [ "delegate" .= condense (headerSignerVk h) ]
   where
     headerSignerVk :: Block.AHeader ByteString -> Crypto.VerificationKey
     headerSignerVk =
       Dlg.delegateVK . Block.delegationCertificate . Block.headerSignature

instance (StandardHash blk)
 => ToObject (HeaderEnvelopeError blk) where
  toObject _verb (UnexpectedBlockNo expect act) =
    mkObject
      [ "kind" .= String "UnexpectedBlockNo"
      , "expected" .= condense expect
      , "actual" .= condense act
      ]
  toObject _verb (UnexpectedSlotNo expect act) =
    mkObject
      [ "kind" .= String "UnexpectedSlotNo"
      , "expected" .= condense expect
      , "actual" .= condense act
      ]
  toObject _verb (UnexpectedPrevHash expect act) =
    mkObject
      [ "kind" .= String "UnexpectedPrevHash"
      , "expected" .= String (pack $ show expect)
      , "actual" .= String (pack $ show act)
      ]
  toObject _verb (OtherEnvelopeError text) =
    mkObject
      [ "kind" .= String "OtherEnvelopeError"
      , "error" .= String text
      ]

instance ( StandardHash blk
         , ToObject (ValidationErr (BlockProtocol blk)))
 => ToObject (HeaderError blk) where
  toObject verb (HeaderProtocolError err) =
    mkObject
      [ "kind" .= String "HeaderProtocolError"
      , "error" .= toObject verb err
      ]
  toObject verb (HeaderEnvelopeError err) =
    mkObject
      [ "kind" .= String "HeaderEnvelopeError"
      , "error" .= toObject verb err
      ]

instance ( Condense (HeaderHash blk)
         , StandardHash blk
         , ToObject (LedgerError blk)
         , ToObject (ValidationErr (BlockProtocol blk)))
 => ToObject (ChainDB.InvalidBlockReason blk) where
  toObject verb (ChainDB.ValidationError extvalerr) =
    mkObject
      [ "kind" .= String "ValidationError"
      , "error" .= toObject verb extvalerr
      ]
  toObject verb (ChainDB.InChainAfterInvalidBlock point extvalerr) =
    mkObject
      [ "kind" .= String "InChainAfterInvalidBlock"
      , "point" .= toObject verb point
      , "error" .= toObject verb extvalerr
      ]

instance (Show txid, Show tx)
 => ToObject (AnyMessage (TxSubmission txid tx)) where
  toObject _verb (AnyMessage (MsgRequestTxs txids)) =
    mkObject
      [ "kind" .= String "MsgRequestTxs"
      , "txIds" .= String (pack $ show txids)
      ]
  toObject _verb (AnyMessage (MsgReplyTxs txs)) =
    mkObject
      [ "kind" .= String "MsgReplyTxs"
      , "txs" .= String (pack $ show txs)
      ]
  toObject _verb (AnyMessage (MsgRequestTxIds _ _ _)) =
    mkObject
      [ "kind" .= String "MsgRequestTxIds"
      ]
  toObject _verb (AnyMessage (MsgReplyTxIds _)) =
    mkObject
      [ "kind" .= String "MsgReplyTxIds"
      ]
  toObject _verb (AnyMessage MsgDone) =
    mkObject
      [ "kind" .= String "MsgDone"
      ]
instance StandardHash blk
 => ToObject (Mock.MockError blk) where
  toObject _verb (Mock.MockUtxoError e) =
    mkObject
      [ "kind" .= String "MockUtxoError"
      , "error" .= String (pack $ show e)
      ]
  toObject _verb (Mock.MockInvalidHash expect act) =
    mkObject
      [ "kind" .= String "MockInvalidHash"
      , "expected" .= String (pack $ show expect)
      , "actual" .= String (pack $ show act)
      ]
  toObject _verb (Mock.MockExpired expiredSlot validatedSlot) =
      mkObject
        [ "kind" .= String "MockExpired"
        , "error" .= String (pack msg)
        ]
    where
      msg =
        "transaction expired in slot " <> condense expiredSlot <>
        ", current slot " <> condense validatedSlot

instance (Show (PBFT.PBftVerKeyHash c))
 => ToObject (PBFT.PBftValidationErr c) where
  toObject _verb (PBFT.PBftInvalidSignature text) =
    mkObject
      [ "kind" .= String "PBftInvalidSignature"
      , "error" .= String text
      ]
  toObject _verb (PBFT.PBftNotGenesisDelegate vkhash _ledgerView) =
    mkObject
      [ "kind" .= String "PBftNotGenesisDelegate"
      , "vk" .= String (pack $ show vkhash)
      ]
  toObject _verb (PBFT.PBftExceededSignThreshold vkhash n) =
    mkObject
      [ "kind" .= String "PBftExceededSignThreshold"
      , "vk" .= String (pack $ show vkhash)
      , "n" .= String (pack $ show n)
      ]
  toObject _verb PBFT.PBftInvalidSlot =
    mkObject
      [ "kind" .= String "PBftInvalidSlot"
      ]

instance Condense (HeaderHash blk)
 => ToObject (Point blk) where
  toObject MinimalVerbosity p = toObject NormalVerbosity p
  toObject verb p =
    mkObject [ "kind" .= String "Tip" --TODO: why is this a Tip not a Point?
             , "tip" .= showPoint verb p ]

instance ToObject (Praos.PraosValidationError c) where
  toObject _verb (Praos.PraosInvalidSlot expect act) =
    mkObject
      [ "kind" .= String "PraosInvalidSlot"
      , "expected" .= String (pack $ show expect)
      , "actual" .= String (pack $ show act)
      ]
  toObject _verb (Praos.PraosUnknownCoreId cid) =
    mkObject
      [ "kind" .= String "PraosUnknownCoreId"
      , "error" .= String (pack $ show cid)
      ]
  toObject _verb (Praos.PraosInvalidSig str _ _ _) =
    mkObject
      [ "kind" .= String "PraosInvalidSig"
      , "error" .= String (pack str)
      ]
  toObject _verb (Praos.PraosInvalidCert _vkvrf y nat _vrf) =
    mkObject
      [ "kind" .= String "PraosInvalidCert"
      , "y" .= String (pack $ show y)
      , "nat" .= String (pack $ show nat)
      ]
  toObject _verb (Praos.PraosInsufficientStake t y) =
    mkObject
      [ "kind" .= String "PraosInsufficientStake"
      , "t" .= String (pack $ show t)
      , "y" .= String (pack $ show y)
      ]

instance Condense (HeaderHash blk)
 => ToObject (RealPoint blk) where
  toObject verb p =
    mkObject $
        [ "kind" .= String "Point"
        , "slot" .= unSlotNo (realPointSlot p) ]
     ++ [ "hash" .= condense (realPointHash p) | verb == MaximalVerbosity ]

instance ToObject (GenTx (Mock.SimpleBlock c ext)) where
  toObject verb tx =
    mkObject $
        [ "txid" .= txId tx ]
     ++ [ "tx"   .= condense tx | verb == MaximalVerbosity ]

instance ToJSON (TxId (GenTx (Mock.SimpleBlock c ext))) where
  toJSON txid = toJSON (condense txid)

instance ToObject SlotNo where
  toObject _verb slot =
    mkObject [ "kind" .= String "SlotNo"
             , "slot" .= toJSON (unSlotNo slot) ]


instance ( Condense (HeaderHash blk)
         , LedgerSupportsProtocol blk
         , ToObject (Header blk))
 => ToObject (ChainDB.TraceEvent blk) where
  toObject verb (ChainDB.TraceAddBlockEvent ev) = case ev of
    ChainDB.IgnoreBlockOlderThanK pt ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.IgnoreBlockOlderThanK"
               , "block" .= toObject verb pt ]
    ChainDB.IgnoreBlockAlreadyInVolDB pt ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.IgnoreBlockAlreadyInVolDB"
               , "block" .= toObject verb pt ]
    ChainDB.IgnoreInvalidBlock pt reason ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.IgnoreInvalidBlock"
               , "block" .= toObject verb pt
               , "reason" .= show reason ]
    ChainDB.AddedBlockToQueue pt sz ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.AddedBlockToQueue"
               , "block" .= toObject verb pt
               , "queueSize" .= toJSON sz ]
    ChainDB.BlockInTheFuture pt slot ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.BlockInTheFuture"
               , "block" .= toObject verb pt
               , "slot" .= toObject verb slot ]
    ChainDB.StoreButDontChange pt ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.StoreButDontChange"
               , "block" .= toObject verb pt ]
    ChainDB.TryAddToCurrentChain pt ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.TryAddToCurrentChain"
               , "block" .= toObject verb pt ]
    ChainDB.TrySwitchToAFork pt _ ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.TrySwitchToAFork"
               , "block" .= toObject verb pt ]
    ChainDB.AddedToCurrentChain _ base extended  ->
      mkObject $
               [ "kind" .= String "TraceAddBlockEvent.AddedToCurrentChain"
               , "newtip" .= showPoint verb (AF.headPoint extended)
               ] ++
               [ "headers" .= toJSON (toObject verb `map` addedHdrsNewChain base extended)
               | verb == MaximalVerbosity ]
    ChainDB.SwitchedToAFork _ old new ->
      mkObject $
               [ "kind" .= String "TraceAddBlockEvent.SwitchedToAFork"
               , "newtip" .= showPoint verb (AF.headPoint new)
               ] ++
               [ "headers" .= toJSON (toObject verb `map` addedHdrsNewChain old new)
               | verb == MaximalVerbosity ]
    ChainDB.AddBlockValidation ev' -> case ev' of
      ChainDB.InvalidBlock err pt ->
        mkObject [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.InvalidBlock"
                 , "block" .= toObject verb pt
                 , "error" .= show err ]
      ChainDB.InvalidCandidate c ->
        mkObject [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.InvalidCandidate"
                 , "block" .= showPoint verb (AF.headPoint c) ]
      ChainDB.ValidCandidate c ->
        mkObject [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.ValidCandidate"
                 , "block" .= showPoint verb (AF.headPoint c) ]
      ChainDB.CandidateExceedsRollback supported actual c ->
        mkObject [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.CandidateExceedsRollback"
                 , "block" .= showPoint verb (AF.headPoint c)
                 , "supported" .= show supported
                 , "actual" .= show actual ]
    ChainDB.AddedBlockToVolDB pt (BlockNo bn) _ ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.AddedBlockToVolDB"
               , "block" .= toObject verb pt
               , "blockNo" .= show bn ]
    ChainDB.ChainChangedInBg c1 c2 ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.ChainChangedInBg"
               , "prev" .= showPoint verb (AF.headPoint c1)
               , "new" .= showPoint verb (AF.headPoint c2) ]
    ChainDB.ScheduledChainSelection pt slot n ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.ScheduledChainSelection"
               , "block" .= toObject verb pt
               , "slot" .= toObject verb slot
               , "scheduled" .= n ]
    ChainDB.RunningScheduledChainSelection pts slot n ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.RunningScheduledChainSelection"
               , "blocks" .= map (toObject verb) (NonEmpty.toList pts)
               , "slot" .= toObject verb slot
               , "scheduled" .= n ]
   where
     addedHdrsNewChain
       :: (AF.AnchoredFragment (Header blk))
       -> (AF.AnchoredFragment (Header blk))
       -> [Header blk]
     addedHdrsNewChain fro to_ =
       case AF.intersect fro to_ of
         Just (_, _, _, s2 :: AF.AnchoredFragment (Header blk)) ->
           AF.toOldestFirst s2
         Nothing -> [] -- No sense to do validation here.
  toObject MinimalVerbosity (ChainDB.TraceLedgerReplayEvent _ev) = emptyObject -- no output
  toObject verb (ChainDB.TraceLedgerReplayEvent ev) = case ev of
    LedgerDB.ReplayFromGenesis _replayTo ->
      mkObject [ "kind" .= String "TraceLedgerReplayEvent.ReplayFromGenesis" ]
    LedgerDB.ReplayFromSnapshot snap tip' _replayTo ->
      mkObject [ "kind" .= String "TraceLedgerReplayEvent.ReplayFromSnapshot"
               , "snapshot" .= toObject verb snap
               , "tip" .= show tip' ]
    LedgerDB.ReplayedBlock pt replayTo ->
      mkObject [ "kind" .= String "TraceLedgerReplayEvent.ReplayedBlock"
               , "slot" .= unSlotNo (realPointSlot pt)
               , "tip"  .= withOrigin 0 unSlotNo (pointSlot replayTo) ]

  toObject MinimalVerbosity (ChainDB.TraceLedgerEvent _ev) = emptyObject -- no output
  toObject verb (ChainDB.TraceLedgerEvent ev) = case ev of
    LedgerDB.TookSnapshot snap pt ->
      mkObject [ "kind" .= String "TraceLedgerEvent.TookSnapshot"
               , "snapshot" .= toObject verb snap
               , "tip" .= show pt ]
    LedgerDB.DeletedSnapshot snap ->
      mkObject [ "kind" .= String "TraceLedgerEvent.DeletedSnapshot"
               , "snapshot" .= toObject verb snap ]
    LedgerDB.InvalidSnapshot snap failure ->
      mkObject [ "kind" .= String "TraceLedgerEvent.InvalidSnapshot"
               , "snapshot" .= toObject verb snap
               , "failure" .= show failure ]

  toObject verb (ChainDB.TraceCopyToImmDBEvent ev) = case ev of
    ChainDB.CopiedBlockToImmDB pt ->
      mkObject [ "kind" .= String "TraceCopyToImmDBEvent.CopiedBlockToImmDB"
               , "slot" .= toObject verb pt ]
    ChainDB.NoBlocksToCopyToImmDB ->
      mkObject [ "kind" .= String "TraceCopyToImmDBEvent.NoBlocksToCopyToImmDB" ]

  toObject verb (ChainDB.TraceGCEvent ev) = case ev of
    ChainDB.PerformedGC slot ->
      mkObject [ "kind" .= String "TraceGCEvent.PerformedGC"
               , "slot" .= toObject verb slot ]
    ChainDB.ScheduledGC slot difft ->
      mkObject $ [ "kind" .= String "TraceGCEvent.ScheduledGC"
                 , "slot" .= toObject verb slot ] <>
                 [ "difft" .= String ((pack . show) difft) | verb >= MaximalVerbosity]

  toObject verb (ChainDB.TraceOpenEvent ev) = case ev of
    ChainDB.OpenedDB immTip tip' ->
      mkObject [ "kind" .= String "TraceOpenEvent.OpenedDB"
               , "immtip" .= toObject verb immTip
               , "tip" .= toObject verb tip' ]
    ChainDB.ClosedDB immTip tip' ->
      mkObject [ "kind" .= String "TraceOpenEvent.ClosedDB"
               , "immtip" .= toObject verb immTip
               , "tip" .= toObject verb tip' ]
    ChainDB.OpenedImmDB immTip epoch ->
      mkObject [ "kind" .= String "TraceOpenEvent.OpenedImmDB"
               , "immtip" .= toObject verb immTip
               , "epoch" .= String ((pack . show) epoch) ]
    ChainDB.OpenedVolDB ->
      mkObject [ "kind" .= String "TraceOpenEvent.OpenedVolDB" ]
    ChainDB.OpenedLgrDB ->
      mkObject [ "kind" .= String "TraceOpenEvent.OpenedLgrDB" ]

  toObject _verb (ChainDB.TraceReaderEvent ev) = case ev of
    ChainDB.NewReader ->
      mkObject [ "kind" .= String "TraceReaderEvent.NewReader" ]
    ChainDB.ReaderNoLongerInMem _ ->
      mkObject [ "kind" .= String "TraceReaderEvent.ReaderNoLongerInMem" ]
    ChainDB.ReaderSwitchToMem _ _ ->
      mkObject [ "kind" .= String "TraceReaderEvent.ReaderSwitchToMem" ]
    ChainDB.ReaderNewImmIterator _ _ ->
      mkObject [ "kind" .= String "TraceReaderEvent.ReaderNewImmIterator" ]
  toObject _verb (ChainDB.TraceInitChainSelEvent ev) = case ev of
    ChainDB.InitChainSelValidation _ ->
      mkObject [ "kind" .= String "InitChainSelValidation" ]
  toObject _verb (ChainDB.TraceIteratorEvent ev) = case ev of
    ChainDB.StreamFromVolDB _ _ _ ->
      mkObject [ "kind" .= String "StreamFromVolDB" ]
    _ -> emptyObject  -- TODO add more iterator events
  toObject _verb (ChainDB.TraceImmDBEvent _ev) =
    mkObject [ "kind" .= String "TraceImmDBEvent" ]
  toObject _verb (ChainDB.TraceVolDBEvent _ev) =
    mkObject [ "kind" .= String "TraceVolDBEvent" ]

instance ToObject (TraceBlockFetchServerEvent blk) where
  toObject _verb _ =
    mkObject [ "kind" .= String "TraceBlockFetchServerEvent" ]

instance (Condense (HeaderHash blk), LedgerSupportsProtocol blk)
 => ToObject (TraceChainSyncClientEvent blk) where
  toObject verb ev = case ev of
    TraceDownloadedHeader pt ->
      mkObject [ "kind" .= String "ChainSyncClientEvent.TraceDownloadedHeader"
               , "block" .= toObject verb (headerPoint pt) ]
    TraceRolledBack tip ->
      mkObject [ "kind" .= String "ChainSyncClientEvent.TraceRolledBack"
               , "tip" .= toObject verb tip ]
    TraceException exc ->
      mkObject [ "kind" .= String "ChainSyncClientEvent.TraceException"
               , "exception" .= String (pack $ show exc) ]
    TraceFoundIntersection _ _ _ ->
      mkObject [ "kind" .= String "ChainSyncClientEvent.TraceFoundIntersection" ]

instance Condense (HeaderHash blk)
 => ToObject (TraceChainSyncServerEvent blk b) where
    toObject verb ev = case ev of
      TraceChainSyncServerRead tip (AddBlock hdr) ->
        mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerRead.AddBlock"
                 , "tip" .= (String (pack $ showTip verb tip))
                 , "addedBlock" .= (String (pack $ condense hdr)) ]
      TraceChainSyncServerRead tip (RollBack pt) ->
        mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerRead.RollBack"
                 , "tip" .= (String (pack $ showTip verb tip))
                 , "rolledBackBlock" .= (String (pack $ showPoint verb pt)) ]
      TraceChainSyncServerReadBlocked tip (AddBlock hdr) ->
        mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.AddBlock"
                 , "tip" .= (String (pack $ showTip verb tip))
                 , "addedBlock" .= (String (pack $ condense hdr)) ]
      TraceChainSyncServerReadBlocked tip (RollBack pt) ->
        mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.RollBack"
                 , "tip" .= (String (pack $ showTip verb tip))
                 , "rolledBackBlock" .= (String (pack $ showPoint verb pt)) ]

instance (ToObject (GenTx blk), ToJSON (GenTxId blk), Show (ApplyTxErr blk))
 => ToObject (TraceEventMempool blk) where
  toObject verb (TraceMempoolAddedTx tx _mpSzBefore mpSzAfter) =
    mkObject
      [ "kind" .= String "TraceMempoolAddedTx"
      , "tx" .= toObject verb tx
      , "mempoolSize" .= toObject verb mpSzAfter
      ]
  toObject verb (TraceMempoolRejectedTx tx err mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolRejectedTx"
      , "err" .= show err --TODO: provide a proper ToObject instance
      , "tx" .= toObject verb tx
      , "mempoolSize" .= toObject verb mpSz
      ]
  toObject verb (TraceMempoolRemoveTxs txs mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolRemoveTxs"
      , "txs" .= map (toObject verb) txs
      , "mempoolSize" .= toObject verb mpSz
      ]
  toObject verb (TraceMempoolManuallyRemovedTxs txs0 txs1 mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolManuallyRemovedTxs"
      , "txsRemoved" .= txs0
      , "txsInvalidated" .= map (toObject verb) txs1
      , "mempoolSize" .= toObject verb mpSz
      ]

instance ToObject MempoolSize where
  toObject _verb MempoolSize{msNumTxs, msNumBytes} =
    mkObject
      [ "numTxs" .= msNumTxs
      , "bytes" .= msNumBytes
      ]

instance ToObject (TraceFetchClientState header) where
  toObject _verb (AddedFetchRequest {}) =
    mkObject [ "kind" .= String "AddedFetchRequest" ]
  toObject _verb (AcknowledgedFetchRequest {}) =
    mkObject [ "kind" .= String "AcknowledgedFetchRequest" ]
  toObject _verb (CompletedBlockFetch {}) =
    mkObject [ "kind" .= String "CompletedBlockFetch" ]
  toObject _verb (CompletedFetchBatch {}) =
    mkObject [ "kind" .= String "CompletedFetchBatch" ]
  toObject _verb (StartedFetchBatch {}) =
    mkObject [ "kind" .= String "StartedFetchBatch" ]
  toObject _verb (RejectedFetchBatch {}) =
    mkObject [ "kind" .= String "RejectedFetchBatch" ]

instance ( Condense (HeaderHash blk)
         , HasTxId tx
         , LedgerSupportsProtocol blk
         , Show (TxId tx)
         , ToObject (LedgerError blk)
         , ToObject (ValidationErr (BlockProtocol blk)))
 => ToObject (TraceForgeEvent blk tx) where
  toObject MaximalVerbosity (TraceAdoptedBlock slotNo blk txs) =
    mkObject
      [ "kind" .= String "TraceAdoptedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "block hash" .=  (condense $ blockHash blk)
      , "tx ids" .= (show $ map txId txs)
      ]
  toObject _verb (TraceAdoptedBlock slotNo blk _txs) =
    mkObject
      [ "kind" .= String "TraceAdoptedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "block hash" .=  (condense $ blockHash blk)
      ]
  toObject _verb (TraceBlockFromFuture currentSlot tip) =
    mkObject
      [ "kind" .= String "TraceBlockFromFuture"
      , "current slot" .= toJSON (unSlotNo currentSlot)
      , "tip" .= toJSON (unSlotNo tip)
      ]
  toObject verb (TraceSlotIsImmutable slotNo tipPoint tipBlkNo) =
    mkObject
      [ "kind" .= String "TraceSlotIsImmutable"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "tip" .= showPoint verb tipPoint
      , "tipBlockNo" .= toJSON (unBlockNo tipBlkNo)
      ]
  toObject _verb (TraceDidntAdoptBlock slotNo _) =
    mkObject
      [ "kind" .= String "TraceDidntAdoptBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceForgedBlock slotNo _ _ _) =
    mkObject
      [ "kind" .= String "TraceForgedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject verb (TraceForgedInvalidBlock slotNo _ reason) =
    mkObject
      [ "kind" .= String "TraceForgedInvalidBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= toObject verb reason
      ]
  toObject _verb (TraceNodeIsLeader slotNo) =
    mkObject
      [ "kind" .= String "TraceNodeIsLeader"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceNodeNotLeader slotNo) =
    mkObject
      [ "kind" .= String "TraceNodeNotLeader"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceNoLedgerState slotNo _blk) =
    mkObject
      [ "kind" .= String "TraceNoLedgerState"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceNoLedgerView slotNo _) =
    mkObject
      [ "kind" .= String "TraceNoLedgerView"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceStartLeadershipCheck slotNo) =
    mkObject
      [ "kind" .= String "TraceStartLeadershipCheck"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]

instance Show peer
 => ToObject [TraceLabelPeer peer (FetchDecision [Point header])] where
  toObject MinimalVerbosity _ = emptyObject
  toObject _ [] = emptyObject
  toObject _ (lbl : r) = toObject MaximalVerbosity lbl <>
                                        toObject MaximalVerbosity r

instance (Show peer, ToObject a) => ToObject (TraceLabelPeer peer a) where
  toObject verb (TraceLabelPeer peerid a) =
    mkObject [ "peer" .= show peerid ] <> toObject verb a

instance ToObject (TraceLocalTxSubmissionServerEvent blk) where
  toObject _verb _ =
    mkObject [ "kind" .= String "TraceLocalTxSubmissionServerEvent" ]

instance ToObject (AnyMessage ps)
 => ToObject (TraceSendRecv ps) where
  toObject verb (TraceSendMsg m) = mkObject
    [ "kind" .= String "Send" , "msg" .= toObject verb m ]
  toObject verb (TraceRecvMsg m) = mkObject
    [ "kind" .= String "Recv" , "msg" .= toObject verb m ]

instance ToObject (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk)) where
  toObject _verb TraceTxSubmissionInbound =
    mkObject [ "kind" .= String "TraceTxSubmissionInbound" ]

instance (Show (GenTx blk), Show (GenTxId blk))
 => ToObject (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)) where
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

instance (Show peer)
 => ToObject (WithMuxBearer peer MuxTrace) where
  toObject _verb (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "MuxTrace"
             , "bearer" .= show b
             , "event" .= show ev ]

