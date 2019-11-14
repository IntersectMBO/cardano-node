{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Tracing.ToObjectOrphans
  (
    WithTip (..)
  , showTip
  , showWithTip
  ) where

import           Cardano.Prelude hiding (atomically, show)
import           Prelude (String, show, id)

import           Data.Aeson (Value (..), toJSON, (.=))
import           Data.Text (pack)
import qualified Network.Socket as Socket (SockAddr)
import           Network.Mux.Types (WithMuxBearer (..), MuxTrace (..))

import           Cardano.BM.Data.LogItem (LOContent (..), LogObject (..),
                   mkLOMeta)
import           Cardano.BM.Tracing
import           Cardano.BM.Data.Tracer (trStructured, emptyObject, mkObject)

import           Ouroboros.Consensus.Block (SupportedBlock, headerPoint)
import           Ouroboros.Consensus.BlockFetchServer
                   (TraceBlockFetchServerEvent)
import           Ouroboros.Consensus.ChainSyncClient
                   (TraceChainSyncClientEvent (..))
import           Ouroboros.Consensus.ChainSyncServer
                   (TraceChainSyncServerEvent(..))
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API (GenTx, GenTxId)
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.TxSubmission
                   (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch.ClientState
                   (TraceFetchClientState (..), TraceLabelPeer (..))
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision)
import           Ouroboros.Network.Point (WithOrigin (..))
import           Ouroboros.Network.Subscription (ConnectResult (..), DnsTrace (..),
                   SubscriptionTrace (..),
                   WithDomainName (..), WithIPList (..))
import           Ouroboros.Network.TxSubmission.Inbound
                   (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound
                   (TraceTxSubmissionOutbound)

import qualified Ouroboros.Storage.ChainDB as ChainDB
import qualified Ouroboros.Storage.LedgerDB.OnDisk as LedgerDB


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
showWithTip customShow (WithTip tip a) = "[" ++ showTip MinimalVerbosity tip ++ "] " ++ customShow a

showTip :: Condense (HeaderHash blk)
        => TracingVerbosity
        -> Point blk
        -> String
showTip verb tip =
  case pointHash tip of
    GenesisHash -> "genesis"
    BlockHash h -> trim $ condense h
  ++
  case pointSlot tip of
    Origin -> "(origin)"
    At slot -> "@" ++ condense slot
 where
  trim :: [a] -> [a]
  trim = case verb of
    MinimalVerbosity -> take 7
    NormalVerbosity  -> take 7
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
      ConnectSuccess         -> Info
      ConnectSuccessLast     -> Notice
      ConnectValencyExceeded -> Warning
    SubscriptionTraceConnectException {} -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceConnectCleanup {} -> Debug
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Error
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Notice
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Info
    SubscriptionTraceConnectionExist {} -> Notice
    SubscriptionTraceUnsupportedRemoteAddr {} -> Error
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionApplicationException {} -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Info

instance DefinePrivacyAnnotation (WithDomainName (SubscriptionTrace Socket.SockAddr))
instance DefineSeverity (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  defineSeverity (WithDomainName _ ev) = case ev of
    SubscriptionTraceConnectStart {} -> Info
    SubscriptionTraceConnectEnd {} -> Info
    SubscriptionTraceConnectException {} -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceConnectCleanup {} -> Debug
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Warning
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Debug
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Debug
    SubscriptionTraceConnectionExist {} -> Info
    SubscriptionTraceUnsupportedRemoteAddr {} -> Warning
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionApplicationException {} -> Error
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

instance DefinePrivacyAnnotation (WithMuxBearer peer (MuxTrace ptcl))
instance DefineSeverity (WithMuxBearer peer (MuxTrace ptcl)) where
  defineSeverity (WithMuxBearer _ ev) = case ev of
    MuxTraceRecvHeaderStart        -> Debug
    MuxTraceRecvHeaderEnd {}       -> Debug
    MuxTraceRecvPayloadStart {}    -> Debug
    MuxTraceRecvPayloadEnd {}      -> Debug
    MuxTraceRecvStart {}           -> Debug
    MuxTraceRecvEnd {}             -> Debug
    MuxTraceSendStart {}           -> Debug
    MuxTraceSendEnd                -> Debug
    MuxTraceStateChange {}         -> Info
    MuxTraceCleanExit {}           -> Info
    MuxTraceExceptionExit {}       -> Info
    MuxTraceChannelRecvStart {}    -> Debug
    MuxTraceChannelRecvEnd {}      -> Debug
    MuxTraceChannelSendStart {}    -> Debug
    MuxTraceChannelSendEnd {}      -> Debug
    MuxTraceHandshakeStart         -> Debug
    MuxTraceHandshakeEnd           -> Debug
    MuxTraceHandshakeClientError _ -> Error
    MuxTraceHandshakeServerError _ -> Error

instance DefinePrivacyAnnotation (WithTip blk (ChainDB.TraceEvent blk))
instance DefineSeverity (WithTip blk (ChainDB.TraceEvent blk)) where
  defineSeverity (WithTip _tip ev) = defineSeverity ev

instance DefineSeverity (ChainDB.TraceEvent blk) where
  defineSeverity (ChainDB.TraceAddBlockEvent ev) = case ev of
    ChainDB.StoreButDontChange {} -> Debug
    ChainDB.TryAddToCurrentChain {} -> Debug
    ChainDB.TrySwitchToAFork {} -> Info
    ChainDB.SwitchedToChain {} -> Notice
    ChainDB.AddBlockValidation ev' -> case ev' of
      ChainDB.InvalidBlock {} -> Error
      ChainDB.InvalidCandidate {} -> Error
      ChainDB.ValidCandidate {} -> Info
      ChainDB.CandidateExceedsRollback {} -> Error
    ChainDB.AddedBlockToVolDB {} -> Debug
    ChainDB.ChainChangedInBg {} -> Info

  defineSeverity (ChainDB.TraceLedgerReplayEvent ev) = case ev of
    LedgerDB.ReplayFromGenesis {} -> Info
    LedgerDB.ReplayFromSnapshot {} -> Info
    _ -> Debug
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
    ChainDB.OpenedImmDB {} -> Debug
    ChainDB.OpenedVolDB -> Debug
    ChainDB.OpenedLgrDB -> Debug

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

instance DefinePrivacyAnnotation (TraceChainSyncClientEvent blk tip)
instance DefineSeverity (TraceChainSyncClientEvent blk tip) where
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

instance DefinePrivacyAnnotation (Consensus.TraceForgeEvent blk)
instance DefineSeverity (Consensus.TraceForgeEvent blk) where
  defineSeverity _ = Info

-- | instances of @Transformable@

-- transform @ChainSyncClient@
instance (Condense (HeaderHash blk), ProtocolLedgerView blk, SupportedBlock blk, Show tip)
          => Transformable Text IO (TraceChainSyncClientEvent blk tip) where
  trTransformer _ verb tr = trStructured verb tr

-- transform @ChainSyncServer@
instance (Condense (HeaderHash blk), Condense b) => Transformable Text IO (TraceChainSyncServerEvent blk b) where
  trTransformer _ verb tr = trStructured verb tr

-- transform @BlockFetchDecision@
instance Show peer => Transformable Text IO [TraceLabelPeer peer
                                (FetchDecision [Point header])] where
  trTransformer _ verb tr = trStructured verb tr

-- transform @BlockFetchDecision@
instance Show peer => Transformable Text IO (TraceLabelPeer peer
                                (TraceFetchClientState header)) where
  trTransformer _ verb tr = trStructured verb tr

-- transform @BlockFetchServerEvent@
instance Transformable Text IO (TraceBlockFetchServerEvent blk) where
  trTransformer _ verb tr = trStructured verb tr

instance Transformable Text IO (TraceTxSubmissionInbound
                                (GenTxId blk) (GenTx blk)) where
  trTransformer _ verb tr = trStructured verb tr

instance Transformable Text IO (TraceTxSubmissionOutbound
                                (GenTxId blk) (GenTx blk)) where
  trTransformer _ verb tr = trStructured verb tr

instance Transformable Text IO (TraceLocalTxSubmissionServerEvent blk) where
  trTransformer _ verb tr = trStructured verb tr

instance Transformable Text IO (Consensus.TraceForgeEvent blk) where
  trTransformer _ verb tr = trStructured verb tr

-- transform @SubscriptionTrace@
instance Transformable Text IO (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  trTransformer StructuredLogging verb tr = trStructured verb tr
  trTransformer TextualRepresentation _verb tr = Tracer $ \s ->
    traceWith tr =<< LogObject <$> pure mempty
                               <*> mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
                               <*> pure (LogMessage $ pack $ show s)
  trTransformer UserdefinedFormatting verb tr = trStructured verb tr


instance Transformable Text IO (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  trTransformer StructuredLogging verb tr = trStructured verb tr
  trTransformer TextualRepresentation _verb tr = Tracer $ \s ->
    traceWith tr =<< LogObject <$> pure mempty
                               <*> mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
                               <*> pure (LogMessage $ pack $ show s)
  trTransformer UserdefinedFormatting verb tr = trStructured verb tr

-- transform @DnsTrace@
instance Transformable Text IO (WithDomainName DnsTrace) where
  trTransformer StructuredLogging verb tr = trStructured verb tr
  trTransformer TextualRepresentation _verb tr = Tracer $ \s ->
    traceWith tr =<< LogObject <$> pure mempty
                               <*> mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
                               <*> pure (LogMessage $ pack $ show s)
  trTransformer UserdefinedFormatting verb tr = trStructured verb tr

-- transform @MuxTrace@
instance (Show ptcl, Show peer)
           => Transformable Text IO (WithMuxBearer peer (MuxTrace ptcl)) where
  trTransformer StructuredLogging verb tr = trStructured verb tr
  trTransformer TextualRepresentation _verb tr = Tracer $ \s ->
    traceWith tr =<< LogObject <$> pure mempty
                               <*> mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
                               <*> pure (LogMessage $ pack $ show s)
  trTransformer UserdefinedFormatting verb tr = trStructured verb tr

-- transform @TraceEvent@
instance (Condense (HeaderHash blk), ProtocolLedgerView blk)
            => Transformable Text IO (WithTip blk (ChainDB.TraceEvent blk)) where
  -- structure required, will call 'toObject'
  trTransformer StructuredLogging verb tr = trStructured verb tr
  -- textual output based on the readable ChainDB tracer
  trTransformer TextualRepresentation _verb tr = readableChainDBTracer $ Tracer $ \s ->
    traceWith tr =<< LogObject <$> pure mempty
                               <*> mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
                               <*> pure (LogMessage $ pack s)
  -- user defined formatting of log output
  trTransformer UserdefinedFormatting verb tr = trStructured verb tr

-- | tracer transformer to text messages for TraceEvents
-- Converts the trace events from the ChainDB that we're interested in into
-- human-readable trace messages.
readableChainDBTracer
  :: forall m blk.
     (Monad m, Condense (HeaderHash blk), ProtocolLedgerView blk)
  => Tracer m String
  -> Tracer m (WithTip blk (ChainDB.TraceEvent blk))
readableChainDBTracer tracer = Tracer $ \case
  WithTip tip (ChainDB.TraceAddBlockEvent ev) -> case ev of
    ChainDB.StoreButDontChange pt   -> tr $ WithTip tip $
      "Ignoring block: " <> condense pt
    ChainDB.TryAddToCurrentChain pt -> tr $ WithTip tip $
      "Block fits onto the current chain: " <> condense pt
    ChainDB.TrySwitchToAFork pt _   -> tr $ WithTip tip $
      "Block fits onto some fork: " <> condense pt
    ChainDB.SwitchedToChain _ c     -> tr $ WithTip tip $
      "Chain changed, new tip: " <> condense (AF.headPoint c)
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
    ChainDB.ChainChangedInBg c1 c2     -> tr $ WithTip tip $
      "Chain changed in bg, from " <> condense (AF.headPoint c1) <> " to "  <> condense (AF.headPoint c2)
  WithTip tip (ChainDB.TraceLedgerReplayEvent ev) -> case ev of
    LedgerDB.ReplayFromGenesis _replayTo -> tr $ WithTip tip
      "Replaying ledger from genesis"
    LedgerDB.ReplayFromSnapshot snap tip' _replayTo -> tr $ WithTip tip $
      "Replaying ledger from snapshot " <> show snap <> " at " <>
        condense tip'
    LedgerDB.ReplayedBlock {} -> pure ()
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
    ChainDB.PerformedGC slot        -> tr $ WithTip tip $
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
    ChainDB.NewReader readerid -> tr $ WithTip tip $
      "New reader with id: " <> condense readerid
    ChainDB.ReaderNoLongerInMem _ -> tr $ WithTip tip "ReaderNoLongerInMem"
    ChainDB.ReaderSwitchToMem _ _ -> tr $ WithTip tip "ReaderSwitchToMem"
    ChainDB.ReaderNewImmIterator _ _ -> tr $ WithTip tip "ReaderNewImmIterator"
  WithTip tip (ChainDB.TraceInitChainSelEvent ev) -> case ev of
    ChainDB.InitChainSelValidation _ -> tr $ WithTip tip "InitChainSelValidation"
  WithTip tip (ChainDB.TraceIteratorEvent ev) -> case ev of
    ChainDB.StreamFromVolDB _ _ _ -> tr $ WithTip tip "StreamFromVolDB"
    _ -> pure ()  -- TODO add more iterator events
  WithTip tip (ChainDB.TraceImmDBEvent _ev) -> tr $ WithTip tip "TraceImmDBEvent"

 where
  tr :: WithTip blk String -> m ()
  tr = traceWith (contramap (showWithTip id) tracer)


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

instance (Show ptcl, Show peer)
      => ToObject (WithMuxBearer peer (MuxTrace ptcl)) where
  toObject _verb (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "MuxTrace"
             , "bearer" .= show b
             , "event" .= show ev ]

instance (Condense (HeaderHash blk), ProtocolLedgerView blk)
      => ToObject (WithTip blk (ChainDB.TraceEvent blk)) where
  -- example: turn off any tracing of @TraceEvent@s when minimal verbosity level is set
  -- toObject MinimalVerbosity _ = emptyObject -- no output
  toObject verb (WithTip tip ev) =
    let evobj = toObject verb ev
    in
    if evobj == emptyObject
    then emptyObject
    else mkObject [ "kind" .= String "TraceEvent"
                  , "tip" .= showTip MinimalVerbosity tip
                  , "event" .= evobj
                  ]

instance ToObject SlotNo where
  toObject _verb slot =
    mkObject [ "kind" .= String "SlotNo"
             , "slot" .= toJSON (unSlotNo slot) ]

instance (Condense (HeaderHash blk), ProtocolLedgerView blk)
      => ToObject (Point blk) where
  toObject MinimalVerbosity p = toObject NormalVerbosity p
  toObject verb p =
    mkObject [ "kind" .= String "Tip"
             , "tip" .= showTip verb p ]

instance (Condense (HeaderHash blk), ProtocolLedgerView blk)
      => ToObject (ChainDB.TraceEvent blk) where
  toObject verb (ChainDB.TraceAddBlockEvent ev) = case ev of
    ChainDB.StoreButDontChange pt ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.StoreButDontChange"
               , "block" .= toObject verb pt ]
    ChainDB.TryAddToCurrentChain pt ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.TryAddToCurrentChain"
               , "block" .= toObject verb pt ]
    ChainDB.TrySwitchToAFork pt _ ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.TrySwitchToAFork"
               , "block" .= toObject verb pt ]
    ChainDB.SwitchedToChain _ c ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.SwitchedToChain"
               , "newtip" .= showTip verb (AF.headPoint c) ]
    ChainDB.AddBlockValidation ev' -> case ev' of
      ChainDB.InvalidBlock err pt ->
        mkObject [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.InvalidBlock"
                 , "block" .= toObject verb pt
                 , "error" .= show err ]
      ChainDB.InvalidCandidate c ->
        mkObject [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.InvalidCandidate"
                 , "block" .= showTip verb (AF.headPoint c) ]
      ChainDB.ValidCandidate c ->
        mkObject [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.ValidCandidate"
                 , "block" .= showTip verb (AF.headPoint c) ]
      ChainDB.CandidateExceedsRollback supported actual c ->
        mkObject [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.CandidateExceedsRollback"
                 , "block" .= showTip verb (AF.headPoint c)
                 , "supported" .= show supported
                 , "actual"    .= show actual ]
    ChainDB.AddedBlockToVolDB pt (BlockNo bn) _ ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.AddedBlockToVolDB"
               , "block" .= toObject verb pt
               , "blockNo" .= show bn ]
    ChainDB.ChainChangedInBg c1 c2     ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.ChainChangedInBg"
               , "prev" .= showTip verb (AF.headPoint c1)
               , "new" .= showTip verb (AF.headPoint c2) ]

  toObject MinimalVerbosity (ChainDB.TraceLedgerReplayEvent _ev) = emptyObject -- no output
  toObject verb (ChainDB.TraceLedgerReplayEvent ev) = case ev of
    LedgerDB.ReplayFromGenesis _replayTo ->
      mkObject [ "kind" .= String "TraceLedgerReplayEvent.ReplayFromGenesis" ]
    LedgerDB.ReplayFromSnapshot snap tip' _replayTo ->
      mkObject [ "kind" .= String "TraceLedgerReplayEvent.ReplayFromSnapshot"
               , "snapshot" .= toObject verb snap
               , "tip" .= show tip' ]
    _ -> emptyObject

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
    ChainDB.NewReader readerid ->
      mkObject [ "kind" .= String "TraceGCEvent.PerformedGC"
               , "readerid" .= String ((pack . condense) readerid) ]
    ChainDB.ReaderNoLongerInMem _ ->
      mkObject [ "kind" .= String "ReaderNoLongerInMem" ]
    ChainDB.ReaderSwitchToMem _ _ ->
      mkObject [ "kind" .= String "ReaderSwitchToMem" ]
    ChainDB.ReaderNewImmIterator _ _ ->
      mkObject [ "kind" .= String "ReaderNewImmIterator" ]
  toObject _verb (ChainDB.TraceInitChainSelEvent ev) = case ev of
    ChainDB.InitChainSelValidation _ ->
      mkObject [ "kind" .= String "InitChainSelValidation" ]
  toObject _verb (ChainDB.TraceIteratorEvent ev) = case ev of
    ChainDB.StreamFromVolDB _ _ _ ->
      mkObject [ "kind" .= String "StreamFromVolDB" ]
    _ -> emptyObject  -- TODO add more iterator events
  toObject _verb (ChainDB.TraceImmDBEvent _ev) =
    mkObject [ "kind" .= String "TraceImmDBEvent" ]

instance ToObject LedgerDB.DiskSnapshot where
  toObject MinimalVerbosity snap = toObject NormalVerbosity snap
  toObject NormalVerbosity _ = mkObject [ "kind" .= String "snapshot" ]
  toObject MaximalVerbosity snap =
    mkObject [ "kind" .= String "snapshot"
             , "snapshot" .= String (pack $ show snap) ]

instance (Condense (HeaderHash blk), ProtocolLedgerView blk, SupportedBlock blk, Show tip)
          => ToObject (TraceChainSyncClientEvent blk tip) where
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

instance (Condense (HeaderHash blk), Condense b) => ToObject (TraceChainSyncServerEvent blk b) where
    toObject verb ev = case ev of
      TraceChainSyncServerRead tip (AddBlock hdr) ->
        mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerRead.AddBlock"
                 , "tip" .= (String (pack . showTip verb $ tipPoint tip))
                 , "addedBlock" .= (String (pack $ condense hdr)) ]
      TraceChainSyncServerRead tip (RollBack pt) ->
        mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerRead.RollBack"
                 , "tip" .= (String (pack . showTip verb $ tipPoint tip))
                 , "rolledBackBlock" .= (String (pack $ showTip verb pt)) ]
      TraceChainSyncServerReadBlocked tip (AddBlock hdr) ->
        mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.AddBlock"
                 , "tip" .= (String (pack . showTip verb $ tipPoint tip))
                 , "addedBlock" .= (String (pack $ condense hdr)) ]
      TraceChainSyncServerReadBlocked tip (RollBack pt) ->
        mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.RollBack"
                 , "tip" .= (String (pack . showTip verb $ tipPoint tip))
                 , "rolledBackBlock" .= (String (pack $ showTip verb pt)) ]


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
    mkObject [" kind" .= String "CompletedFetchBatch" ]
  toObject _verb (StartedFetchBatch {}) =
    mkObject [" kind" .= String "StartedFetchBatch" ]
  toObject _verb (RejectedFetchBatch {}) =
    mkObject [" kind" .= String "RejectedFetchBatch" ]

instance ToObject (TraceBlockFetchServerEvent blk) where
  toObject _verb _ =
    mkObject [ "kind" .= String "TraceBlockFetchServerEvent" ]

instance ToObject (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk)) where
  toObject _verb _ =
    mkObject [ "kind" .= String "TraceTxSubmissionInbound" ]

instance ToObject (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)) where
  toObject _verb _ =
    mkObject [ "kind" .= String "TraceTxSubmissionOutbound" ]

instance ToObject (TraceLocalTxSubmissionServerEvent blk) where
  toObject _verb _ =
    mkObject [ "kind" .= String "TraceLocalTxSubmissionServerEvent" ]

instance ToObject (Consensus.TraceForgeEvent blk) where
  toObject _verb _ =
    mkObject [ "kind" .= String "TraceForgeEvent" ]
