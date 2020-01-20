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
  ( WithTip (..)
  , showTip
  , showWithTip
  ) where

import           Cardano.Prelude hiding (atomically, show)
import           Prelude (String, show, id)

import           Data.Aeson (Value (..), toJSON, (.=))
import           Data.Text (pack)

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
import           Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..))
import           Ouroboros.Consensus.TxSubmission
                   (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch.ClientState
                   (TraceFetchClientState (..), TraceLabelPeer (..))
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision)
import           Ouroboros.Network.Point (WithOrigin (..))
import           Ouroboros.Network.TxSubmission.Inbound
                   (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound
                   (TraceTxSubmissionOutbound)

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

instance DefinePrivacyAnnotation (TraceForgeEvent blk tx)
instance DefineSeverity (TraceForgeEvent blk tx) where
  defineSeverity TraceForgedBlock {}            = Info
  defineSeverity TraceStartLeadershipCheck {}   = Info
  defineSeverity TraceNodeNotLeader {}          = Info
  defineSeverity TraceNodeIsLeader {}           = Info
  defineSeverity TraceNoLedgerState {}          = Warning
  defineSeverity TraceNoLedgerView {}           = Warning
  defineSeverity TraceBlockFromFuture {}        = Warning
  defineSeverity TraceAdoptedBlock {}           = Info
  defineSeverity TraceDidntAdoptBlock {}        = Warning
  defineSeverity TraceForgedInvalidBlock {}     = Alert

-- | instances of @Transformable@

-- transform @ChainSyncClient@
instance (Condense (HeaderHash blk), ProtocolLedgerView blk, SupportedBlock blk)
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

instance (Show blk, Show tx, ProtocolLedgerView blk) => Transformable Text IO (TraceForgeEvent blk tx) where
  trTransformer StructuredLogging verb tr = trStructured verb tr
  trTransformer TextualRepresentation _verb tr = Tracer $ \s ->
    traceWith tr =<< LogObject <$> pure mempty
                               <*> mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
                               <*> pure (LogMessage $ pack $ show s)
  trTransformer UserdefinedFormatting verb tr = trStructured verb tr

-- | instances of @ToObject@
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

instance ToObject LedgerDB.DiskSnapshot where
  toObject MinimalVerbosity snap = toObject NormalVerbosity snap
  toObject NormalVerbosity _ = mkObject [ "kind" .= String "snapshot" ]
  toObject MaximalVerbosity snap =
    mkObject [ "kind" .= String "snapshot"
             , "snapshot" .= String (pack $ show snap) ]

instance (Condense (HeaderHash blk), ProtocolLedgerView blk, SupportedBlock blk)
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

instance ToObject (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)) where
  toObject _verb _ =
    mkObject [ "kind" .= String "TraceTxSubmissionOutbound" ]

instance ToObject (TraceLocalTxSubmissionServerEvent blk) where
  toObject _verb _ =
    mkObject [ "kind" .= String "TraceLocalTxSubmissionServerEvent" ]

instance ProtocolLedgerView blk => ToObject (TraceForgeEvent blk tx) where
  toObject _verb (TraceAdoptedBlock slotNo _blk _txs) =
    mkObject
        [ "kind"    .= String "TraceAdoptedBlock"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
  toObject _verb (TraceBlockFromFuture currentSlot tip) =
    mkObject
        [ "kind" .= String "TraceBlockFromFuture"
        , "current slot" .= toJSON (unSlotNo currentSlot)
        , "tip" .= toJSON (unSlotNo tip)
        ]
  toObject _verb (TraceDidntAdoptBlock slotNo _) =
    mkObject
        [ "kind"    .= String "TraceDidntAdoptBlock"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
  toObject _verb (TraceForgedBlock slotNo _ _) =
    mkObject
        [ "kind"    .= String "TraceForgedBlock"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
  toObject _verb (TraceForgedInvalidBlock slotNo _ _) =
    mkObject
        [ "kind"    .= String "TraceForgedInvalidBlock"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
  toObject _verb (TraceNodeIsLeader slotNo) =
    mkObject
        [ "kind"    .= String "TraceNodeIsLeader"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
  toObject _verb (TraceNodeNotLeader slotNo) =
    mkObject
        [ "kind"    .= String "TraceNodeNotLeader"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
  toObject _verb (TraceNoLedgerState slotNo _blk) =
    mkObject
        [ "kind"    .= String "TraceNoLedgerState"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
  toObject _verb (TraceNoLedgerView slotNo _) =
    mkObject
        [ "kind"    .= String "TraceNoLedgerView"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
  toObject _verb (TraceStartLeadershipCheck slotNo) =
    mkObject
        [ "kind"    .= String "TraceStartLeadershipCheck"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
