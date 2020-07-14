{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Tracing.OrphanInstances.Consensus () where

import           Cardano.Prelude hiding (show)
import           Prelude (show)

import qualified Data.Text as Text
import           Data.Text (pack)

import           Cardano.Tracing.OrphanInstances.Common
import           Cardano.Tracing.OrphanInstances.Network (showTip, showPoint)

import           Ouroboros.Consensus.Block
                   (BlockProtocol, Header, getHeader, headerPoint,
                    RealPoint, realPointSlot, realPointHash,
                    ForgeState(..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                   (TraceBlockFetchServerEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                   (TraceChainSyncClientEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
                   (TraceChainSyncServerEvent (..))
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                   (LedgerSupportsProtocol)
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Protocol.BFT  as BFT
import qualified Ouroboros.Consensus.Protocol.PBFT as PBFT
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
                   (InspectLedger, LedgerEvent (..), LedgerUpdate, LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx, GenTxId,
                   HasTxId, TxId, txId)
import           Ouroboros.Consensus.Mempool.API (TraceEventMempool (..), MempoolSize(..))
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                   (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Node.Run (RunNode (..))
import           Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..))
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Point (withOrigin)
import           Ouroboros.Network.Block
                   (BlockNo(..), SlotNo(..), ChainUpdate(..),
                    HeaderHash, StandardHash, blockHash, pointSlot)

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB



--
-- * instances of @HasPrivacyAnnotation@ and @HasSeverityAnnotation@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance HasPrivacyAnnotation (ChainDB.TraceEvent blk)
instance HasSeverityAnnotation (ChainDB.TraceEvent blk) where
  getSeverityAnnotation (ChainDB.TraceAddBlockEvent ev) = case ev of
    ChainDB.IgnoreBlockOlderThanK {} -> Info
    ChainDB.IgnoreBlockAlreadyInVolDB {} -> Info
    ChainDB.IgnoreInvalidBlock {} -> Info
    ChainDB.AddedBlockToQueue {} -> Debug
    ChainDB.BlockInTheFuture {} -> Info
    ChainDB.AddedBlockToVolDB {} -> Debug
    ChainDB.TryAddToCurrentChain {} -> Debug
    ChainDB.TrySwitchToAFork {} -> Info
    ChainDB.StoreButDontChange {} -> Debug
    ChainDB.AddedToCurrentChain events _ _ _ ->
      maximumDef Notice (map getSeverityAnnotation events)
    ChainDB.SwitchedToAFork events _ _ _ ->
      maximumDef Notice (map getSeverityAnnotation events)
    ChainDB.AddBlockValidation ev' -> case ev' of
      ChainDB.InvalidBlock {} -> Error
      ChainDB.InvalidCandidate {} -> Error
      ChainDB.ValidCandidate {} -> Info
      ChainDB.CandidateContainsFutureBlocks{} -> Debug
      ChainDB.CandidateContainsFutureBlocksExceedingClockSkew{} -> Error
    ChainDB.ChainSelectionForFutureBlock{} -> Debug

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
    ChainDB.NewReader {} -> Debug
    ChainDB.ReaderNoLongerInMem {} -> Debug
    ChainDB.ReaderSwitchToMem {} -> Debug
    ChainDB.ReaderNewImmIterator {} -> Debug
  getSeverityAnnotation (ChainDB.TraceInitChainSelEvent ev) = case ev of
    ChainDB.InitChainSelValidation {} -> Debug
  getSeverityAnnotation (ChainDB.TraceIteratorEvent ev) = case ev of
    ChainDB.StreamFromVolDB {} -> Debug
    _ -> Debug
  getSeverityAnnotation (ChainDB.TraceImmDBEvent _ev) = Debug
  getSeverityAnnotation (ChainDB.TraceVolDBEvent _ev) = Debug

instance HasSeverityAnnotation (LedgerEvent blk) where
  getSeverityAnnotation (LedgerUpdate _)  = Notice
  getSeverityAnnotation (LedgerWarning _) = Critical

instance HasPrivacyAnnotation (TraceBlockFetchServerEvent blk)
instance HasSeverityAnnotation (TraceBlockFetchServerEvent blk) where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation (TraceChainSyncClientEvent blk)
instance HasSeverityAnnotation (TraceChainSyncClientEvent blk) where
  getSeverityAnnotation (TraceDownloadedHeader _) = Info
  getSeverityAnnotation (TraceFoundIntersection _ _ _) = Info
  getSeverityAnnotation (TraceRolledBack _) = Notice
  getSeverityAnnotation (TraceException _) = Warning


instance HasPrivacyAnnotation (TraceChainSyncServerEvent blk)
instance HasSeverityAnnotation (TraceChainSyncServerEvent blk) where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation (TraceEventMempool blk)
instance HasSeverityAnnotation (TraceEventMempool blk) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (ForgeState c)
instance HasSeverityAnnotation (ForgeState c) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation ()
instance HasSeverityAnnotation () where
  getSeverityAnnotation () = Info

instance HasPrivacyAnnotation (TraceForgeEvent blk)
instance HasSeverityAnnotation (TraceForgeEvent blk) where
  getSeverityAnnotation TraceForgedBlock {}            = Info
  getSeverityAnnotation TraceStartLeadershipCheck {}   = Info
  getSeverityAnnotation TraceNodeNotLeader {}          = Info
  getSeverityAnnotation TraceNodeCannotLead {}         = Error
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


--
-- | instances of @Transformable@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

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
  trTransformer = trStructured


instance Condense (HeaderHash blk)
      => Transformable Text IO (TraceChainSyncServerEvent blk) where
  trTransformer = trStructured


instance ( ToObject (ApplyTxErr blk), Show (ApplyTxErr blk), ToObject (GenTx blk),
           ToJSON (GenTxId blk)) => Transformable Text IO (TraceEventMempool blk) where
  trTransformer = trStructured


condenseT :: Condense a => a -> Text
condenseT = pack . condense

showT :: Show a => a -> Text
showT = pack . show


instance ( tx ~ GenTx blk
         , Condense (HeaderHash blk)
         , HasTxId tx
         , RunNode blk
         , Show (TxId tx)
         , ToObject (LedgerError blk)
         , ToObject (OtherHeaderEnvelopeError blk)
         , ToObject (ValidationErr (BlockProtocol blk))
         , ToObject (CannotLead (BlockProtocol blk)))
      => Transformable Text IO (TraceForgeEvent blk) where
  trTransformer = trStructuredText

instance HasTextFormatter (ForgeState blk) where
  formatText _ = pack . show . toList

instance ToObject (ChainIndepState (BlockProtocol blk))
      => Transformable Text IO (ForgeState blk) where
  trTransformer = trStructuredText

instance ( tx ~ GenTx blk
         , Condense (HeaderHash blk)
         , HasTxId tx
         , LedgerSupportsProtocol blk
         , Show (TxId tx))
      => HasTextFormatter (TraceForgeEvent blk) where
  formatText = \case
    TraceAdoptedBlock slotNo blk txs -> const $
      "Adopted block forged in slot "
        <> showT (unSlotNo slotNo)
        <> ": " <> condenseT (blockHash blk)
        <> ", TxIds: " <> showT (map txId txs)
    TraceBlockFromFuture currentSlot tipSlot -> const $
      "Couldn't forge block because current tip is in the future: "
        <> "current tip slot: " <> showT (unSlotNo tipSlot)
        <> ", current slot: " <> showT (unSlotNo currentSlot)
    TraceSlotIsImmutable slotNo immutableTipPoint immutableTipBlkNo -> const $
      "Couldn't forge block because current slot is immutable: "
        <> "immutable tip: " <> pack (showPoint MaximalVerbosity immutableTipPoint)
        <> ", immutable tip block no: " <> showT (unBlockNo immutableTipBlkNo)
        <> ", current slot: " <> showT (unSlotNo slotNo)
    TraceDidntAdoptBlock slotNo _ -> const $
      "Didn't adopt forged block in slot " <> showT (unSlotNo slotNo)
    TraceForgedBlock slotNo _ _ _ -> const $
      "Forged block in slot " <> showT (unSlotNo slotNo)
    TraceForgedInvalidBlock slotNo _ reason -> const $
      "Forged invalid block in slot "
        <> showT (unSlotNo slotNo)
        <> ", reason: " <> showT reason
    TraceNodeIsLeader slotNo -> const $
      "Leading slot " <> showT (unSlotNo slotNo)
    TraceNodeNotLeader slotNo -> const $
      "Not leading slot " <> showT (unSlotNo slotNo)
    TraceNodeCannotLead slotNo reason -> const $
      "We are the leader in slot "
        <> showT (unSlotNo slotNo)
        <> ", but we cannot lead because: "
        <> showT reason
    TraceNoLedgerState slotNo pt -> const $
      "Could not obtain ledger state for point "
        <> pack (showPoint MaximalVerbosity pt)
        <> ", current slot: "
        <> showT (unSlotNo slotNo)
    TraceNoLedgerView slotNo _ -> const $
      "Could not obtain ledger view for slot " <> showT (unSlotNo slotNo)
    TraceStartLeadershipCheck slotNo -> const $
      "Checking for leadership in slot " <> showT (unSlotNo slotNo)


instance Transformable Text IO (TraceLocalTxSubmissionServerEvent blk) where
  trTransformer = trStructured


instance ( Condense (HeaderHash blk)
         , LedgerSupportsProtocol blk
         , InspectLedger blk
         , ToObject (Header blk)
         , ToObject (LedgerEvent blk))
      => Transformable Text IO (ChainDB.TraceEvent blk) where
  trTransformer = trStructuredText





instance ( Condense (HeaderHash blk)
         , LedgerSupportsProtocol blk
         , InspectLedger blk)
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
        ChainDB.AddedToCurrentChain es _ _ c -> \_o ->
          "Chain extended, new tip: " <> condenseT (AF.headPoint c) <>
          Text.concat [ "\nEvent: " <> showT e | e <- es ]
        ChainDB.SwitchedToAFork es _ _ c -> \_o ->
          "Switched to a fork, new tip: " <> condenseT (AF.headPoint c) <>
          Text.concat [ "\nEvent: " <> showT e | e <- es ]
        ChainDB.AddBlockValidation ev' -> case ev' of
          ChainDB.InvalidBlock err pt -> \_o ->
            "Invalid block " <> condenseT pt <> ": " <> showT err
          ChainDB.InvalidCandidate c -> \_o ->
            "Invalid candidate " <> condenseT (AF.headPoint c)
          ChainDB.ValidCandidate c -> \_o ->
            "Valid candidate " <> condenseT (AF.headPoint c)
          ChainDB.CandidateContainsFutureBlocks c hdrs -> \_o ->
            "Candidate contains blocks from near future:  " <>
            condenseT (AF.headPoint c) <> ", slots " <>
            Text.intercalate ", " (map (condenseT . headerPoint) hdrs)
          ChainDB.CandidateContainsFutureBlocksExceedingClockSkew c hdrs -> \_o ->
            "Candidate contains blocks from future exceeding clock skew limit: " <>
            condenseT (AF.headPoint c) <> ", slots " <>
            Text.intercalate ", " (map (condenseT . headerPoint) hdrs)
        ChainDB.AddedBlockToVolDB pt _ _ -> \_o ->
          "Chain added block " <> condenseT pt
        ChainDB.ChainSelectionForFutureBlock pt -> \_o ->
          "Chain selection run for block previously from future: " <> condenseT pt
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

instance ToObject BFT.BftValidationErr where
  toObject _verb (BFT.BftInvalidSignature err) =
    mkObject
      [ "kind" .= String "BftInvalidSignature"
      , "error" .= String (pack err)
      ]


instance ToObject LedgerDB.DiskSnapshot where
  toObject MinimalVerbosity snap = toObject NormalVerbosity snap
  toObject NormalVerbosity _ = mkObject [ "kind" .= String "snapshot" ]
  toObject MaximalVerbosity snap =
    mkObject [ "kind" .= String "snapshot"
             , "snapshot" .= String (pack $ show snap) ]


instance ( StandardHash blk
         , ToObject (LedgerError blk)
         , ToObject (OtherHeaderEnvelopeError blk)
         , ToObject (ValidationErr (BlockProtocol blk)))
      => ToObject (ExtValidationError blk) where
  toObject verb (ExtValidationErrorLedger err) = toObject verb err
  toObject verb (ExtValidationErrorHeader err) = toObject verb err


instance ( StandardHash blk
         , ToObject (OtherHeaderEnvelopeError blk)
         )
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
  toObject verb (OtherHeaderEnvelopeError err) =
    toObject verb err


instance ( StandardHash blk
         , ToObject (ValidationErr (BlockProtocol blk))
         , ToObject (OtherHeaderEnvelopeError blk)
         )
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
         , ToObject (OtherHeaderEnvelopeError blk)
         , ToObject (ValidationErr (BlockProtocol blk)))
      => ToObject (ChainDB.InvalidBlockReason blk) where
  toObject verb (ChainDB.ValidationError extvalerr) =
    mkObject
      [ "kind" .= String "ValidationError"
      , "error" .= toObject verb extvalerr
      ]
  toObject verb (ChainDB.InFutureExceedsClockSkew point) =
    mkObject
      [ "kind" .= String "InFutureExceedsClockSkew"
      , "point" .= toObject verb point
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
  toObject _verb (PBFT.PBftExceededSignThreshold vkhash numForged) =
    mkObject
      [ "kind" .= String "PBftExceededSignThreshold"
      , "vk" .= String (pack $ show vkhash)
      , "numForged" .= String (pack (show numForged))
      ]
  toObject _verb PBFT.PBftInvalidSlot =
    mkObject
      [ "kind" .= String "PBftInvalidSlot"
      ]


instance (Show (PBFT.PBftVerKeyHash c))
      => ToObject (PBFT.PBftCannotLead c) where
  toObject _verb (PBFT.PBftCannotLeadInvalidDelegation vkhash) =
    mkObject
      [ "kind" .= String "PBftCannotLeadInvalidDelegation"
      , "vk" .= String (pack $ show vkhash)
      ]
  toObject _verb (PBFT.PBftCannotLeadThresholdExceeded numForged) =
    mkObject
      [ "kind" .= String "PBftCannotLeadThresholdExceeded"
      , "numForged" .= numForged
      ]


instance Condense (HeaderHash blk)
      => ToObject (RealPoint blk) where
  toObject verb p =
    mkObject $
        [ "kind" .= String "Point"
        , "slot" .= unSlotNo (realPointSlot p) ]
     ++ [ "hash" .= condense (realPointHash p) | verb == MaximalVerbosity ]


instance (ToObject (LedgerUpdate blk), ToObject (LedgerWarning blk))
      => ToObject (LedgerEvent blk) where
  toObject verb = \case
    LedgerUpdate  update  -> toObject verb update
    LedgerWarning warning -> toObject verb warning


instance ( Condense (HeaderHash blk)
         , LedgerSupportsProtocol blk
         , ToObject (Header blk)
         , ToObject (LedgerEvent blk))
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
    ChainDB.AddedToCurrentChain events _ base extended ->
      mkObject $
               [ "kind" .= String "TraceAddBlockEvent.AddedToCurrentChain"
               , "newtip" .= showPoint verb (AF.headPoint extended)
               ]
            ++ [ "headers" .= toJSON (toObject verb `map` addedHdrsNewChain base extended)
               | verb == MaximalVerbosity ]
            ++ [ "events" .= toJSON (map (toObject verb) events)
               | not (null events) ]
    ChainDB.SwitchedToAFork events _ old new ->
      mkObject $
               [ "kind" .= String "TraceAddBlockEvent.SwitchedToAFork"
               , "newtip" .= showPoint verb (AF.headPoint new)
               ]
            ++ [ "headers" .= toJSON (toObject verb `map` addedHdrsNewChain old new)
               | verb == MaximalVerbosity ]
            ++ [ "events" .= toJSON (map (toObject verb) events)
               | not (null events) ]
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
      ChainDB.CandidateContainsFutureBlocks c hdrs ->
        mkObject [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocks"
                 , "block"   .= showPoint verb (AF.headPoint c)
                 , "headers" .= map (showPoint verb . headerPoint) hdrs ]
      ChainDB.CandidateContainsFutureBlocksExceedingClockSkew c hdrs ->
        mkObject [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocksExceedingClockSkew"
                 , "block"   .= showPoint verb (AF.headPoint c)
                 , "headers" .= map (showPoint verb . headerPoint) hdrs ]
    ChainDB.AddedBlockToVolDB pt (BlockNo bn) _ ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.AddedBlockToVolDB"
               , "block" .= toObject verb pt
               , "blockNo" .= show bn ]
    ChainDB.ChainSelectionForFutureBlock pt ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.ChainSelectionForFutureBlock"
               , "block" .= toObject verb pt ]
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
      => ToObject (TraceChainSyncServerEvent blk) where
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


instance ( Show (ApplyTxErr blk), ToObject (ApplyTxErr blk), ToObject (GenTx blk),
           ToJSON (GenTxId blk)
         ) => ToObject (TraceEventMempool blk) where
  toObject verb (TraceMempoolAddedTx tx _mpSzBefore mpSzAfter) =
    mkObject
      [ "kind" .= String "TraceMempoolAddedTx"
      , "tx" .= toObject verb tx
      , "mempoolSize" .= toObject verb mpSzAfter
      ]
  toObject verb (TraceMempoolRejectedTx tx txApplyErr mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolRejectedTx"
      , "err" .= toObject verb txApplyErr
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

instance HasTextFormatter () where
  formatText _ = pack . show . toList

-- ForgeState default value = ()
instance Transformable Text IO () where
  trTransformer = trStructuredText

instance ToObject (ChainIndepState (BlockProtocol blk))
      => ToObject (ForgeState blk) where
  toObject verb ForgeState { chainIndepState, extraForgeState = _ } =
    -- We assume there's nothing interesting in the extraForgeState
    toObject verb chainIndepState

instance ( tx ~ GenTx blk
         , Condense (HeaderHash blk)
         , HasTxId tx
         , RunNode blk
         , Show (TxId tx)
         , ToObject (LedgerError blk)
         , ToObject (OtherHeaderEnvelopeError blk)
         , ToObject (ValidationErr (BlockProtocol blk))
         , ToObject (CannotLead (BlockProtocol blk)))
      => ToObject (TraceForgeEvent blk) where
  toObject MaximalVerbosity (TraceAdoptedBlock slotNo blk txs) =
    mkObject
      [ "kind" .= String "TraceAdoptedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "block hash" .=  (condense $ blockHash blk)
      , "block size" .= toJSON (nodeBlockFetchSize (getHeader blk))
      , "tx ids" .= toJSON (map (show . txId) txs)
      ]
  toObject _verb (TraceAdoptedBlock slotNo blk _txs) =
    mkObject
      [ "kind" .= String "TraceAdoptedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "block hash" .=  (condense $ blockHash blk)
      , "block size" .= toJSON (nodeBlockFetchSize (getHeader blk))
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
  toObject verb (TraceNodeCannotLead slotNo reason) =
    mkObject
      [ "kind" .= String "TraceNodeCannotLead"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= toObject verb reason
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


instance ToObject (TraceLocalTxSubmissionServerEvent blk) where
  toObject _verb _ =
    mkObject [ "kind" .= String "TraceLocalTxSubmissionServerEvent" ]
