{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cardano.Tracing.ToObjectOrphans.ChainDBTrace () where

import Cardano.Tracing.ToObjectOrphans

import           Cardano.Prelude hiding (show)
import           Prelude (String, show)

import           Data.Aeson hiding (Error)
import qualified Data.List.NonEmpty as NonEmpty

import           Cardano.BM.Data.LogItem (LOContent (..), LogObject(..))
import           Cardano.BM.Data.Tracer ( TracingVerbosity(..), definePrivacyAnnotation
                                        , emptyObject, mkObject, trStructured)
import           Cardano.BM.Tracing ( DefinePrivacyAnnotation
                                    , DefineSeverity(..), ToObject(..)
                                    , Severity(..), Tracer(..)
                                    , TracingFormatting(..), Transformable(..)
                                    , contramap, traceWith, mkLOMeta)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (BlockNo(..), HeaderHash, SlotNo(..))
import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.Common (EpochNo (..))
import           Ouroboros.Consensus.Ledger.Abstract (ProtocolLedgerView(..))
import qualified Ouroboros.Storage.LedgerDB.OnDisk as LedgerDB
import           Ouroboros.Consensus.Util.Condense (Condense(..))







instance DefinePrivacyAnnotation (WithTip blk (ChainDB.TraceEvent blk))
instance DefineSeverity (WithTip blk (ChainDB.TraceEvent blk)) where
  defineSeverity (WithTip _tip ev) = defineSeverity ev

instance DefineSeverity (ChainDB.TraceEvent blk) where
  defineSeverity (ChainDB.TraceAddBlockEvent ev) = case ev of
    ChainDB.IgnoreBlockOlderThanK {} -> Info
    ChainDB.IgnoreBlockAlreadyInVolDB {} -> Info
    ChainDB.IgnoreInvalidBlock {} -> Info
    ChainDB.BlockInTheFuture {} -> Info
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

-- transform @TraceEvent@
instance (Condense (HeaderHash blk), ProtocolLedgerView blk)
            => Transformable Text IO (WithTip blk (ChainDB.TraceEvent blk)) where
  -- structure required, will call 'toObject'
  trTransformer StructuredLogging verb tr = trStructured verb tr
  -- textual output based on the readable ChainDB tracer
  trTransformer TextualRepresentation _verb tr = readableChainDBTracer $ Tracer $ \s ->
    traceWith tr =<< LogObject <$> pure mempty
                               <*> mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
                               <*> pure (LogMessage $ toS s)
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
    ChainDB.IgnoreBlockOlderThanK pt -> tr $ WithTip tip $
      "Ignoring block older than K: " <> condense pt
    ChainDB.IgnoreBlockAlreadyInVolDB pt -> tr $ WithTip tip $
      "Ignoring block already in DB: " <> condense pt
    ChainDB.IgnoreInvalidBlock pt _reason -> tr $ WithTip tip $
      "Ignoring previously seen invalid block: " <> condense pt
    ChainDB.BlockInTheFuture pt slot -> tr $ WithTip tip $
      "Ignoring block from future: " <> condense pt <> ", slot " <> condense slot
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
    LedgerDB.ReplayedBlock _r (epochno, slotno) _replayTo -> tr $ WithTip tip $
      "Replayed block: " <> show (unSlotNo slotno) <> " in epoch: " <> show (unEpochNo epochno)
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
  tr = traceWith (contramap (showWithTip identity) tracer)


--tr :: forall m blk.
--  (Monad m, Condense (HeaderHash blk), ProtocolLedgerView blk)
--  => Tracer m String -> WithTip blk String -> m ()
--tr trcr wTip = traceWith (contramap (showWithTip identity) trcr) wTip

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

instance (Condense (HeaderHash blk), ProtocolLedgerView blk)
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
    LedgerDB.ReplayedBlock _ (epochno, slotno) _replayTo ->
      mkObject [ "kind" .= String "TraceLedgerReplayEvent.ReplayedBlock"
               , "epoch" .= show (unEpochNo epochno)
               , "slot" .= show (unSlotNo slotno) ]

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
                 [ "difft" .= String ((toS . show) difft) | verb >= MaximalVerbosity]

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
               , "epoch" .= String ((toS . show) epoch) ]
    ChainDB.OpenedVolDB ->
      mkObject [ "kind" .= String "TraceOpenEvent.OpenedVolDB" ]
    ChainDB.OpenedLgrDB ->
      mkObject [ "kind" .= String "TraceOpenEvent.OpenedLgrDB" ]

  toObject _verb (ChainDB.TraceReaderEvent ev) = case ev of
    ChainDB.NewReader readerid ->
      mkObject [ "kind" .= String "TraceGCEvent.PerformedGC"
               , "readerid" .= String ((toS . condense) readerid) ]
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
