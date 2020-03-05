{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Tracing.ToObjectOrphans
  ( WithTip (..)
  , showTip
  , showWithTip
  ) where

import           Cardano.Prelude hiding (atomically, show)
import           Prelude (String, show, id)

import           Data.Aeson (Value (..), toJSON, (.=))
import           Data.Text (pack)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Network.Socket as Socket (SockAddr)
import           Network.Mux (WithMuxBearer (..), MuxTrace (..))

import           Cardano.BM.Data.LogItem (LOContent (..), LogObject (..),
                   mkLOMeta)
import           Cardano.BM.Tracing
import           Cardano.BM.Data.Tracer (trStructured, emptyObject, mkObject)
import qualified Cardano.Chain.Block as Block

import           Ouroboros.Consensus.Block
                   (Header, headerPoint,
                    RealPoint, realPointSlot, realPointHash)
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
                   HasTxId, TraceEventMempool (..), TxId, txId)
import qualified Ouroboros.Consensus.Mock.Ledger as Mock
import qualified Ouroboros.Consensus.Mock.Protocol.Praos as Praos
import           Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                   (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch.ClientState
                   (TraceFetchClientState (..), TraceLabelPeer (..))
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision)
import           Ouroboros.Network.Codec (AnyMessage (..))
import           Ouroboros.Network.NodeToNode
                   (WithAddr(..), ErrorPolicyTrace(..), TraceSendRecv (..))
import           Ouroboros.Network.Protocol.TxSubmission.Type
                   (Message (..), TxSubmission)
import           Ouroboros.Network.Subscription (ConnectResult (..), DnsTrace (..),
                   SubscriptionTrace (..),
                   WithDomainName (..), WithIPList (..))
import           Ouroboros.Network.TxSubmission.Inbound
                   (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound
                   (TraceTxSubmissionOutbound (..))

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB

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

showWithTip :: Condense (HeaderHash blk)
            => (a -> String)
            -> WithTip blk a
            -> String
showWithTip customShow (WithTip tip a) =
  "[" ++ showPoint MinimalVerbosity tip ++ "] " ++ customShow a

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

instance ( Show a
         , Condense (HeaderHash blk)
         ) => Show (WithTip blk a) where

  show = showWithTip show

-- instances of @DefinePrivacyAnnotation@ and @DefineSeverity@
instance DefinePrivacyAnnotation (WithIPList (SubscriptionTrace Socket.SockAddr))
instance DefineSeverity (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  defineSeverity (WithIPList _ _ ev) = case ev of
    SubscriptionTraceConnectStart _ -> Info
    SubscriptionTraceConnectEnd _ connectResult -> case connectResult of
      ConnectSuccess -> Info
      ConnectSuccessLast -> Notice
      ConnectValencyExceeded -> Warning
    SubscriptionTraceConnectException {} -> Error
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
    SubscriptionTraceApplicationException {} -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Info

instance DefinePrivacyAnnotation (WithDomainName (SubscriptionTrace Socket.SockAddr))
instance DefineSeverity (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  defineSeverity (WithDomainName _ ev) = case ev of
    SubscriptionTraceConnectStart {} -> Info
    SubscriptionTraceConnectEnd {} -> Info
    SubscriptionTraceConnectException {} -> Error
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
    SubscriptionTraceApplicationException {} -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Debug

instance DefinePrivacyAnnotation (WithDomainName DnsTrace)
instance DefineSeverity (WithDomainName DnsTrace) where
  defineSeverity (WithDomainName _ ev) = case ev of
    DnsTraceLookupException {} -> Error
    DnsTraceLookupAError {} -> Error
    DnsTraceLookupAAAAError {} -> Error
    DnsTraceLookupIPv6First -> Info
    DnsTraceLookupIPv4First -> Info
    DnsTraceLookupAResult {} -> Debug
    DnsTraceLookupAAAAResult {} -> Debug

instance DefinePrivacyAnnotation (WithAddr Socket.SockAddr ErrorPolicyTrace)
instance DefineSeverity (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  defineSeverity (WithAddr _ ev) = case ev of
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

instance DefinePrivacyAnnotation (WithMuxBearer peer MuxTrace)
instance DefineSeverity (WithMuxBearer peer MuxTrace) where
  defineSeverity (WithMuxBearer _ ev) = case ev of
    MuxTraceRecvHeaderStart -> Debug
    MuxTraceRecvHeaderEnd {} -> Debug
    MuxTraceRecvPayloadStart {} -> Debug
    MuxTraceRecvPayloadEnd {} -> Debug
    MuxTraceRecvStart {} -> Debug
    MuxTraceRecvEnd {} -> Debug
    MuxTraceSendStart {} -> Debug
    MuxTraceSendEnd -> Debug
    MuxTraceState {} -> Info
    MuxTraceCleanExit {} -> Info
    MuxTraceExceptionExit {} -> Info
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
    MuxTraceRecvDeltaQSample {} -> Info

instance DefinePrivacyAnnotation (WithTip blk (ChainDB.TraceEvent blk))
instance DefineSeverity (WithTip blk (ChainDB.TraceEvent blk)) where
  defineSeverity (WithTip _tip ev) = defineSeverity ev

defaultTextTransformer
  :: ( MonadIO m
     , DefinePrivacyAnnotation b
     , DefineSeverity b
     , Show b
     , ToObject b)
  => TracingFormatting
  -> TracingVerbosity
  -> Trace m Text
  -> Tracer m b
defaultTextTransformer TextualRepresentation _verb tr =
  Tracer $ \s -> do
    meta <- mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
    traceWith tr (mempty, LogObject mempty meta (LogMessage $ pack $ show s))
defaultTextTransformer _ verb tr =
  trStructured verb tr

instance ( DefinePrivacyAnnotation (ChainDB.TraceAddBlockEvent blk)
         , DefineSeverity (ChainDB.TraceAddBlockEvent blk)
         , LedgerSupportsProtocol blk
         , Show (Ouroboros.Consensus.Block.Header blk)
         , ToObject (ChainDB.TraceAddBlockEvent blk))
 => Transformable Text IO (ChainDB.TraceAddBlockEvent blk) where
   trTransformer = defaultTextTransformer

instance DefineSeverity (ChainDB.TraceEvent blk) where
  defineSeverity (ChainDB.TraceAddBlockEvent ev) = case ev of
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

  defineSeverity (ChainDB.TraceLedgerReplayEvent ev) = case ev of
    LedgerDB.ReplayFromGenesis {} -> Info
    LedgerDB.ReplayFromSnapshot {} -> Info
    LedgerDB.ReplayedBlock {} -> Info

  defineSeverity (ChainDB.TraceLedgerEvent ev) = case ev of
    LedgerDB.TookSnapshot {} -> Info
    LedgerDB.DeletedSnapshot {} -> Debug
    LedgerDB.InvalidSnapshot {} -> Error

  defineSeverity (ChainDB.TraceCopyToImmDBEvent ev) = case ev of
    ChainDB.CopiedBlockToImmDB {} -> Notice
    ChainDB.NoBlocksToCopyToImmDB -> Debug

  defineSeverity (ChainDB.TraceGCEvent ev) = case ev of
    ChainDB.PerformedGC {} -> Debug
    ChainDB.ScheduledGC {} -> Debug

  defineSeverity (ChainDB.TraceOpenEvent ev) = case ev of
    ChainDB.OpenedDB {} -> Info
    ChainDB.ClosedDB {} -> Info
    ChainDB.ReopenedDB {} -> Debug
    ChainDB.OpenedImmDB {} -> Info
    ChainDB.OpenedVolDB -> Info
    ChainDB.OpenedLgrDB -> Info

  defineSeverity (ChainDB.TraceReaderEvent ev) = case ev of
    ChainDB.NewReader {} -> Info
    ChainDB.ReaderNoLongerInMem {} -> Info
    ChainDB.ReaderSwitchToMem {} -> Info
    ChainDB.ReaderNewImmIterator {} -> Info
  defineSeverity (ChainDB.TraceInitChainSelEvent ev) = case ev of
    ChainDB.InitChainSelValidation {} -> Debug
  defineSeverity (ChainDB.TraceIteratorEvent ev) = case ev of
    ChainDB.StreamFromVolDB {} -> Debug
    _ -> Debug
  defineSeverity (ChainDB.TraceImmDBEvent _ev) = Debug
  defineSeverity (ChainDB.TraceVolDBEvent _ev) = Debug

instance DefinePrivacyAnnotation (TraceChainSyncClientEvent blk)
instance DefineSeverity (TraceChainSyncClientEvent blk) where
  defineSeverity (TraceDownloadedHeader _) = Info
  defineSeverity (TraceFoundIntersection _ _ _) = Info
  defineSeverity (TraceRolledBack _) = Notice
  defineSeverity (TraceException _) = Warning

instance DefinePrivacyAnnotation (TraceChainSyncServerEvent blk b)
instance DefineSeverity (TraceChainSyncServerEvent blk b) where
  defineSeverity _ = Info

instance DefinePrivacyAnnotation [TraceLabelPeer peer
                                  (FetchDecision [Point header])]
instance DefineSeverity [TraceLabelPeer peer
                         (FetchDecision [Point header])] where
  defineSeverity [] = Debug
  defineSeverity _ = Info

instance DefinePrivacyAnnotation (TraceLabelPeer peer
                                  (TraceFetchClientState header))
instance DefineSeverity (TraceLabelPeer peer
                         (TraceFetchClientState header)) where
  defineSeverity _ = Info

instance DefinePrivacyAnnotation (TraceLabelPeer peer
                                  (TraceSendRecv (TxSubmission txid tx)))
instance DefineSeverity (TraceLabelPeer peer
                         (TraceSendRecv (TxSubmission txid tx))) where
  defineSeverity _ = Debug

instance DefinePrivacyAnnotation (TraceBlockFetchServerEvent blk)
instance DefineSeverity (TraceBlockFetchServerEvent blk) where
  defineSeverity _ = Info

instance DefinePrivacyAnnotation (TraceTxSubmissionInbound
                                  (GenTxId blk) (GenTx blk))
instance DefineSeverity (TraceTxSubmissionInbound
                         (GenTxId blk) (GenTx blk)) where
  defineSeverity _ = Info

instance DefinePrivacyAnnotation (TraceTxSubmissionOutbound
                                  (GenTxId blk) (GenTx blk))
instance DefineSeverity (TraceTxSubmissionOutbound
                         (GenTxId blk) (GenTx blk)) where
  defineSeverity _ = Info

instance DefinePrivacyAnnotation (TraceLocalTxSubmissionServerEvent blk)
instance DefineSeverity (TraceLocalTxSubmissionServerEvent blk) where
  defineSeverity _ = Info

instance DefinePrivacyAnnotation (TraceEventMempool blk)
instance DefineSeverity (TraceEventMempool blk) where
  defineSeverity _ = Info

instance DefinePrivacyAnnotation (TraceForgeEvent blk tx)
instance DefineSeverity (TraceForgeEvent blk tx) where
  defineSeverity TraceForgedBlock {}            = Info
  defineSeverity TraceStartLeadershipCheck {}   = Info
  defineSeverity TraceNodeNotLeader {}          = Info
  defineSeverity TraceNodeIsLeader {}           = Info
  defineSeverity TraceNoLedgerState {}          = Error
  defineSeverity TraceNoLedgerView {}           = Error
  defineSeverity TraceBlockFromFuture {}        = Error
  defineSeverity TraceSlotIsImmutable {}        = Error
  defineSeverity TraceAdoptedBlock {}           = Info
  defineSeverity TraceDidntAdoptBlock {}        = Error
  defineSeverity TraceForgedInvalidBlock {}     = Error

-- | instances of @Transformable@

-- transform @ChainSyncClient@
instance (Condense (HeaderHash blk), LedgerSupportsProtocol blk)
          => Transformable Text IO (TraceChainSyncClientEvent blk) where
  trTransformer _ verb tr = trStructured verb tr

-- transform @ChainSyncServer@
instance Condense (HeaderHash blk) => Transformable Text IO (TraceChainSyncServerEvent blk b) where
  trTransformer _ verb tr = trStructured verb tr

-- transform @BlockFetchDecision@
instance Show peer => Transformable Text IO [TraceLabelPeer peer
                                (FetchDecision [Point header])] where
  trTransformer _ verb tr = trStructured verb tr

-- transform @BlockFetchDecision@
instance Show peer => Transformable Text IO (TraceLabelPeer peer
                                (TraceFetchClientState header)) where
  trTransformer _ verb tr = trStructured verb tr

instance (Show peer, Show txid, Show tx)
      => Transformable Text IO (TraceLabelPeer peer
           (TraceSendRecv (TxSubmission txid tx))) where
  trTransformer = defaultTextTransformer

-- transform @BlockFetchServerEvent@
instance Transformable Text IO (TraceBlockFetchServerEvent blk) where
  trTransformer = defaultTextTransformer

instance Transformable Text IO (TraceTxSubmissionInbound
                                (GenTxId blk) (GenTx blk)) where
  trTransformer = defaultTextTransformer

instance (Show (GenTxId blk), Show (GenTx blk))
      => Transformable Text IO (TraceTxSubmissionOutbound
                                (GenTxId blk) (GenTx blk)) where
  trTransformer = defaultTextTransformer

instance Transformable Text IO (TraceLocalTxSubmissionServerEvent blk) where
  trTransformer _ verb tr = trStructured verb tr

instance ( Condense (HeaderHash blk)
         , HasTxId tx
         , LedgerSupportsProtocol blk
         , Show (TxId tx)
         , ToObject (LedgerError blk)
         , ToObject (ValidationErr (BlockProtocol blk)))
           => Transformable Text IO (TraceForgeEvent blk tx) where
  trTransformer TextualRepresentation _verb tr = readableForgeEventTracer $ Tracer $ \s -> do
    meta <- mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
    traceWith tr (mempty, LogObject mempty meta (LogMessage $ pack s))
  -- user defined formatting of log output
  trTransformer _ verb tr = trStructured verb tr

instance (Show (GenTx blk), Show (GenTxId blk))
      => Transformable Text IO (TraceEventMempool blk) where
  trTransformer _ verb tr = trStructured verb tr

-- transform @SubscriptionTrace@
instance Transformable Text IO (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  trTransformer = defaultTextTransformer


instance Transformable Text IO (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  trTransformer = defaultTextTransformer

-- transform @DnsTrace@
instance Transformable Text IO (WithDomainName DnsTrace) where
  trTransformer = defaultTextTransformer

-- transform @ErrorPolicyTrace@
instance Transformable Text IO (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  trTransformer = defaultTextTransformer

-- transform @MuxTrace@
instance (Show peer)
           => Transformable Text IO (WithMuxBearer peer MuxTrace) where
  trTransformer = defaultTextTransformer

-- transform @TraceEvent@
instance (Condense (HeaderHash blk), LedgerSupportsProtocol blk)
            => Transformable Text IO (WithTip blk (ChainDB.TraceEvent blk)) where
  -- structure required, will call 'toObject'
  trTransformer StructuredLogging verb tr = trStructured verb tr
  -- textual output based on the readable ChainDB tracer
  trTransformer TextualRepresentation _verb tr = readableChainDBTracer $ Tracer $ \s -> do
    meta <- mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
    traceWith tr (mempty, LogObject mempty meta (LogMessage $ pack s))
  -- user defined formatting of log output
  trTransformer UserdefinedFormatting verb tr = trStructured verb tr
  -- trTransformer _ verb tr = trStructured verb tr

-- | tracer transformer to text messages for TraceEvents
-- Converts the trace events from the ChainDB that we're interested in into
-- human-readable trace messages.
readableChainDBTracer
  :: forall m blk.
     (Monad m, Condense (HeaderHash blk), LedgerSupportsProtocol blk)
  => Tracer m String
  -> Tracer m (WithTip blk (ChainDB.TraceEvent blk))
readableChainDBTracer tracer = Tracer $ \case
  WithTip tip (ChainDB.TraceAddBlockEvent ev) -> case ev of
    ChainDB.IgnoreBlockOlderThanK pt -> tr $ WithTip tip $
      "Ignoring block older than K: " <> condense pt
    ChainDB.IgnoreBlockAlreadyInVolDB pt -> tr $ WithTip tip $
      "Ignoring block already in DB: " <> condense pt
    ChainDB.IgnoreInvalidBlock pt _reason -> tr $ WithTip tip $
      "Ignoring previously seen invalid block: " <> condense pt
    ChainDB.AddedBlockToQueue pt sz -> tr $ WithTip tip $
      "Block added to queue: " <> condense pt <> " queue size " <> condense sz
    ChainDB.BlockInTheFuture pt slot -> tr $ WithTip tip $
      "Ignoring block from future: " <> condense pt <> ", slot " <> condense slot
    ChainDB.StoreButDontChange pt -> tr $ WithTip tip $
      "Ignoring block: " <> condense pt
    ChainDB.TryAddToCurrentChain pt -> tr $ WithTip tip $
      "Block fits onto the current chain: " <> condense pt
    ChainDB.TrySwitchToAFork pt _ -> tr $ WithTip tip $
      "Block fits onto some fork: " <> condense pt
    ChainDB.AddedToCurrentChain _ _ c -> tr $ WithTip tip $
      "Chain extended, new tip: " <> condense (AF.headPoint c)
    ChainDB.SwitchedToAFork _ _ c -> tr $ WithTip tip $
      "Switched to a fork, new tip: " <> condense (AF.headPoint c)
    ChainDB.AddBlockValidation ev' -> case ev' of
      ChainDB.InvalidBlock err pt -> tr $ WithTip tip $
        "Invalid block " <> condense pt <> ": " <> show err
      ChainDB.InvalidCandidate c -> tr $ WithTip tip $
        "Invalid candidate " <> condense (AF.headPoint c)
      ChainDB.ValidCandidate c -> tr $ WithTip tip $
        "Valid candidate " <> condense (AF.headPoint c)
      ChainDB.CandidateExceedsRollback _ _ c -> tr $ WithTip tip $
        "Exceeds rollback " <> condense (AF.headPoint c)
    ChainDB.AddedBlockToVolDB pt _ _ -> tr $ WithTip tip $
      "Chain added block " <> condense pt
    ChainDB.ChainChangedInBg c1 c2 -> tr $ WithTip tip $
      "Chain changed in bg, from " <> condense (AF.headPoint c1) <> " to "  <> condense (AF.headPoint c2)
    ChainDB.ScheduledChainSelection pt slot _n -> tr $ WithTip tip $
      "Chain selection scheduled for future: " <> condense pt
                                  <> ", slot " <> condense slot
    ChainDB.RunningScheduledChainSelection pts slot _n -> tr $ WithTip tip $
      "Running scheduled chain selection: " <> condense (NonEmpty.toList pts)
                               <> ", slot " <> condense slot
  WithTip tip (ChainDB.TraceLedgerReplayEvent ev) -> case ev of
    LedgerDB.ReplayFromGenesis _replayTo -> tr $ WithTip tip
      "Replaying ledger from genesis"
    LedgerDB.ReplayFromSnapshot snap tip' _replayTo -> tr $ WithTip tip $
      "Replaying ledger from snapshot " <> show snap <> " at " <>
        condense tip'
    LedgerDB.ReplayedBlock pt replayTo -> tr $ WithTip tip $
      "Replayed block: slot " <> show (realPointSlot pt) ++ " of " ++ show (pointSlot replayTo)
  WithTip tip (ChainDB.TraceLedgerEvent ev) -> case ev of
    LedgerDB.TookSnapshot snap pt -> tr $ WithTip tip $
      "Took ledger snapshot " <> show snap <> " at " <> condense pt
    LedgerDB.DeletedSnapshot snap -> tr $ WithTip tip $
      "Deleted old snapshot " <> show snap
    LedgerDB.InvalidSnapshot snap failure -> tr $ WithTip tip $
      "Invalid snapshot " <> show snap <> show failure
  WithTip tip (ChainDB.TraceCopyToImmDBEvent ev) -> case ev of
    ChainDB.CopiedBlockToImmDB pt -> tr $ WithTip tip $
      "Copied block " <> condense pt <> " to the ImmutableDB"
    ChainDB.NoBlocksToCopyToImmDB -> tr $ WithTip tip
      "There are no blocks to copy to the ImmutableDB"
  WithTip tip (ChainDB.TraceGCEvent ev) -> case ev of
    ChainDB.PerformedGC slot -> tr $ WithTip tip $
      "Performed a garbage collection for " <> condense slot
    ChainDB.ScheduledGC slot _difft -> tr $ WithTip tip $
      "Scheduled a garbage collection for " <> condense slot
  WithTip tip (ChainDB.TraceOpenEvent ev) -> case ev of
    ChainDB.OpenedDB immTip tip' -> tr $ WithTip tip $
      "Opened db with immutable tip at " <> condense immTip <>
      " and tip " <> condense tip'
    ChainDB.ClosedDB immTip tip' -> tr $ WithTip tip $
      "Closed db with immutable tip at " <> condense immTip <>
      " and tip " <> condense tip'
    ChainDB.ReopenedDB immTip tip' -> tr $ WithTip tip $
      "Reopened db with immutable tip at " <> condense immTip <>
      " and tip " <> condense tip'
    ChainDB.OpenedImmDB immTip epoch -> tr $ WithTip tip $
      "Opened imm db with immutable tip at " <> condense immTip <>
      " and epoch " <> show epoch
    ChainDB.OpenedVolDB -> tr $ WithTip tip "Opened vol db"
    ChainDB.OpenedLgrDB -> tr $ WithTip tip "Opened lgr db"
  WithTip tip (ChainDB.TraceReaderEvent ev) -> case ev of
    ChainDB.NewReader -> tr $ WithTip tip $ "New reader was created"
    ChainDB.ReaderNoLongerInMem _ -> tr $ WithTip tip "ReaderNoLongerInMem"
    ChainDB.ReaderSwitchToMem _ _ -> tr $ WithTip tip "ReaderSwitchToMem"
    ChainDB.ReaderNewImmIterator _ _ -> tr $ WithTip tip "ReaderNewImmIterator"
  WithTip tip (ChainDB.TraceInitChainSelEvent ev) -> case ev of
    ChainDB.InitChainSelValidation _ -> tr $ WithTip tip "InitChainSelValidation"
  WithTip tip (ChainDB.TraceIteratorEvent ev) -> case ev of
    ChainDB.StreamFromVolDB _ _ _ -> tr $ WithTip tip "StreamFromVolDB"
    _ -> pure ()  -- TODO add more iterator events
  WithTip tip (ChainDB.TraceImmDBEvent _ev) -> tr $ WithTip tip "TraceImmDBEvent"
  WithTip tip (ChainDB.TraceVolDBEvent _ev) -> tr $ WithTip tip "TraceVolDBEvent"

 where
  tr :: WithTip blk String -> m ()
  tr = traceWith (contramap (showWithTip id) tracer)

-- | Tracer transformer for making TraceForgeEvents human-readable.
readableForgeEventTracer
  :: forall m blk tx.
     ( Condense (HeaderHash blk)
     , HasTxId tx
     , Show (TxId tx)
     , LedgerSupportsProtocol blk)
  => Tracer m String
  -> Tracer m (TraceForgeEvent blk tx)
readableForgeEventTracer tracer = Tracer $ \case
  TraceAdoptedBlock slotNo blk txs -> tr $
    "Adopted forged block for slot " <> show (unSlotNo slotNo) <> ": " <> condense (blockHash blk) <> "; TxIds: " <> show (map txId txs)
  TraceBlockFromFuture currentSlot tip -> tr $
    "Forged block from future: current slot " <> show (unSlotNo currentSlot) <> ", tip being " <> condense tip
  TraceSlotIsImmutable slotNo tipPoint tipBlkNo -> tr $
    "Forged for immutable slot " <> show (unSlotNo slotNo) <> ", tip: " <> showPoint MaximalVerbosity tipPoint <> ", block no: " <> show (unBlockNo tipBlkNo)
  TraceDidntAdoptBlock slotNo _ -> tr $
    "Didn't adopt forged block at slot " <> show (unSlotNo slotNo)
  TraceForgedBlock slotNo _ _ _ -> tr $
    "Forged block for slot " <> show (unSlotNo slotNo)
  TraceForgedInvalidBlock slotNo _ reason -> tr $
    "Forged invalid block for slot " <> show (unSlotNo slotNo) <> ", reason: " <> show reason
      -- , "reason" .= toObject verb reason
  TraceNodeIsLeader slotNo -> tr $
    "Leading slot " <> show (unSlotNo slotNo)
  TraceNodeNotLeader slotNo -> tr $
    "Not leading slot " <> show (unSlotNo slotNo)
  TraceNoLedgerState slotNo _blk -> tr $
    "No ledger state at slot " <> show (unSlotNo slotNo)
  TraceNoLedgerView slotNo _ -> tr $
    "No ledger view at slot " <> show (unSlotNo slotNo)
  TraceStartLeadershipCheck slotNo -> tr $
    "Testing for leadership at slot " <> show (unSlotNo slotNo)
 where
   tr :: String -> m ()
   tr = traceWith tracer

-- | instances of @ToObject@

instance ToObject (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  toObject _verb (WithIPList localAddresses dests ev) =
    mkObject [ "kind" .= String "WithIPList SubscriptionTrace"
             , "localAddresses" .= show localAddresses
             , "dests" .= show dests
             , "event" .= show ev ]

instance ToObject (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  toObject _verb (WithDomainName dom ev) =
    mkObject [ "kind" .= String "SubscriptionTrace"
             , "domain" .= show dom
             , "event" .= show ev ]

instance ToObject (WithDomainName DnsTrace) where
  toObject _verb (WithDomainName dom ev) =
    mkObject [ "kind" .= String "DnsTrace"
             , "domain" .= show dom
             , "event" .= show ev ]

instance ToObject (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  toObject _verb (WithAddr addr ev) =
    mkObject [ "kind" .= String "ErrorPolicyTrace"
             , "address" .= show addr
             , "event" .= show ev ]

instance (Show peer)
      => ToObject (WithMuxBearer peer MuxTrace) where
  toObject _verb (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "MuxTrace"
             , "bearer" .= show b
             , "event" .= show ev ]

instance (Condense (HeaderHash blk), LedgerSupportsProtocol blk)
      => ToObject (WithTip blk (ChainDB.TraceEvent blk)) where
  -- example: turn off any tracing of @TraceEvent@s when minimal verbosity level is set
  -- toObject MinimalVerbosity _ = emptyObject -- no output
  toObject verb (WithTip tip ev) =
    let evobj = toObject verb ev
    in
    if evobj == emptyObject
    then emptyObject
    else mkObject [ "kind" .= String "TraceEvent"
                  , "tip" .= showPoint MinimalVerbosity tip
                  , "event" .= evobj
                  ]

instance ToObject SlotNo where
  toObject _verb slot =
    mkObject [ "kind" .= String "SlotNo"
             , "slot" .= toJSON (unSlotNo slot) ]

instance Condense (HeaderHash blk)
      => ToObject (Point blk) where
  toObject MinimalVerbosity p = toObject NormalVerbosity p
  toObject verb p =
    mkObject [ "kind" .= String "Tip" --TODO: why is this a Tip not a Point?
             , "tip" .= showPoint verb p ]

instance Condense (HeaderHash blk)
      => ToObject (RealPoint blk) where
  toObject verb p =
    mkObject $
        [ "kind" .= String "Point"
        , "slot" .= unSlotNo (realPointSlot p) ]
     ++ [ "hash" .= condense (realPointHash p) | verb == MaximalVerbosity ]

instance (Condense (HeaderHash blk), LedgerSupportsProtocol blk)
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
    ChainDB.AddedToCurrentChain _ _ c ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.AddedToCurrentChain"
               , "newtip" .= showPoint verb (AF.headPoint c) ]
    ChainDB.SwitchedToAFork _ _ c ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.SwitchedToAFork"
               , "newtip" .= showPoint verb (AF.headPoint c) ]
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
    ChainDB.ReopenedDB immTip tip' ->
      mkObject [ "kind" .= String "TraceOpenEvent.ReopenedDB"
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

instance ToObject LedgerDB.DiskSnapshot where
  toObject MinimalVerbosity snap = toObject NormalVerbosity snap
  toObject NormalVerbosity _ = mkObject [ "kind" .= String "snapshot" ]
  toObject MaximalVerbosity snap =
    mkObject [ "kind" .= String "snapshot"
             , "snapshot" .= String (pack $ show snap) ]

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

instance Condense (HeaderHash blk) => ToObject (TraceChainSyncServerEvent blk b) where
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


instance Show peer => ToObject [TraceLabelPeer peer
                        (FetchDecision [Point header])] where
  toObject MinimalVerbosity _ = emptyObject
  toObject NormalVerbosity lbls = mkObject [ "kind" .= String "TraceLabelPeer"
                                         , "length" .= String (pack $ show $ length lbls) ]
  toObject MaximalVerbosity [] = emptyObject
  toObject MaximalVerbosity (lbl : r) = toObject MaximalVerbosity lbl <>
                                        toObject MaximalVerbosity r

instance Show peer => ToObject (TraceLabelPeer peer
                        (FetchDecision [Point header])) where
  toObject verb (TraceLabelPeer peerid a) =
    mkObject [ "kind" .= String "FetchDecision"
             , "peer" .= show peerid
             , "decision" .= toObject verb a ]

instance ToObject (FetchDecision [Point header]) where
  toObject _verb (Left decline) =
    mkObject [ "kind" .= String "FetchDecision declined"
             , "declined" .= String (pack $ show $ decline) ]
  toObject _verb (Right results) =
    mkObject [ "kind" .= String "FetchDecision results"
             , "length" .= String (pack $ show $ length results) ]

instance (Show peer, Show txid, Show tx)
    => ToObject (TraceLabelPeer peer
         (TraceSendRecv (TxSubmission txid tx))) where
  toObject verb (TraceLabelPeer peerid (TraceSendMsg (AnyMessage msg))) =
    mkObject
      [ "kind" .= String "TraceSendMsg"
      , "peer" .= show peerid
      , "message" .= toObject verb msg
      ]
  toObject verb (TraceLabelPeer peerid (TraceRecvMsg (AnyMessage msg))) =
    mkObject
      [ "kind" .= String "TraceRecvMsg"
      , "peer" .= show peerid
      , "message" .= toObject verb msg
      ]

instance (Show txid, Show tx) => ToObject (Message
                                   (TxSubmission txid tx) from to) where
  toObject _verb (MsgRequestTxs txids) =
    mkObject
      [ "kind" .= String "MsgRequestTxs"
      , "txIds" .= String (pack $ show txids)
      ]
  toObject _verb (MsgReplyTxs txs) =
    mkObject
      [ "kind" .= String "MsgReplyTxs"
      , "txs" .= String (pack $ show txs)
      ]
  toObject _verb (MsgRequestTxIds _ _ _) =
    mkObject
      [ "kind" .= String "MsgRequestTxIds"
      ]
  toObject _verb (MsgReplyTxIds _) =
    mkObject
      [ "kind" .= String "MsgReplyTxIds"
      ]
  toObject _verb MsgDone =
    mkObject
      [ "kind" .= String "MsgDone"
      ]

instance Show peer => ToObject (TraceLabelPeer peer
                        (TraceFetchClientState header)) where
  toObject verb (TraceLabelPeer peerid a) =
    mkObject [ "kind" .= String "TraceFetchClientState"
           , "peer" .= show peerid
           , "state" .= toObject verb a ]

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

instance ToObject (TraceBlockFetchServerEvent blk) where
  toObject _verb _ =
    mkObject [ "kind" .= String "TraceBlockFetchServerEvent" ]

instance ToObject (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk)) where
  toObject _verb _ =
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

instance ToObject (TraceLocalTxSubmissionServerEvent blk) where
  toObject _verb _ =
    mkObject [ "kind" .= String "TraceLocalTxSubmissionServerEvent" ]

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

instance (Show (GenTx blk), Show (GenTxId blk))
      => ToObject (TraceEventMempool blk) where
  toObject _verb (TraceMempoolAddedTx tx _mpSzBefore mpSzAfter) =
    mkObject
      [ "kind" .= String "TraceMempoolAddedTx"
      , "txAdded" .= String (pack $ show tx)
      , "mempoolSize" .= String (pack $ show mpSzAfter)
      ]
  toObject _verb (TraceMempoolRejectedTx txAndErrs _mpSzBefore mpSzAfter) =
    mkObject
      [ "kind" .= String "TraceMempoolRejectedTxs"
      , "txRejected" .= String (pack $ show txAndErrs)
      , "mempoolSize" .= String (pack $ show mpSzAfter)
      ]
  toObject _verb (TraceMempoolRemoveTxs txs mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolRemoveTxs"
      , "txsRemoved" .= String (pack $ show txs)
      , "mempoolSize" .= String (pack $ show mpSz)
      ]
  toObject _verb (TraceMempoolManuallyRemovedTxs txs0 txs1 mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolManuallyRemovedTxs"
      , "txsManuallyRemoved" .= String (pack $ show txs0)
      , "txsNoLongerValidRemoved" .= String (pack $ show txs1)
      , "mempoolSize" .= String (pack $ show mpSz)
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

instance ( StandardHash blk
         , ToObject (LedgerError blk)
         , ToObject (ValidationErr (BlockProtocol blk))
         )
      => ToObject (ExtValidationError blk) where
  toObject verb (ExtValidationErrorLedger err) = toObject verb err
  toObject verb (ExtValidationErrorHeader err) = toObject verb err

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

instance StandardHash blk => ToObject (Mock.MockError blk) where
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

instance ToObject BFT.BftValidationErr where
  toObject _verb (BFT.BftInvalidSignature err) =
    mkObject
      [ "kind" .= String "BftInvalidSignature"
      , "error" .= String (pack err)
      ]

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
