
module Cardano.TraceDispatcher.ChainDBTracer.Combinators
  ( withSeverityChainDB
  , withNamesChainDB
  ) where

import           Cardano.Logging
import           Cardano.Prelude

import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent (..))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types as ImmDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolDB
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl as VolDb

withSeverityChainDB :: Monad m
  => Trace m (ChainDB.TraceEvent blk)
  -> Trace m (ChainDB.TraceEvent blk)
withSeverityChainDB = withSeverity gsTraceEvent

gsTraceEvent :: ChainDB.TraceEvent blk -> SeverityS
gsTraceEvent (ChainDB.TraceAddBlockEvent v)          = gsTraceAddBlockEvent v
gsTraceEvent (ChainDB.TraceFollowerEvent v)          = gsTraceFollowerEvent v
gsTraceEvent (ChainDB.TraceCopyToImmutableDBEvent v) = gsTraceCopyToImmutableDBEvent v
gsTraceEvent (ChainDB.TraceGCEvent v)                = gsTraceGCEvent v
gsTraceEvent (ChainDB.TraceInitChainSelEvent v)      = gsTraceInitChainSelEvent v
gsTraceEvent (ChainDB.TraceOpenEvent v)              = gsTraceOpenEvent v
gsTraceEvent (ChainDB.TraceIteratorEvent v)          = gsTraceIteratorEvent v
gsTraceEvent (ChainDB.TraceLedgerEvent v)            = gsTraceLedgerEvent v
gsTraceEvent (ChainDB.TraceLedgerReplayEvent v)      = gsTraceLedgerReplayEvent v
gsTraceEvent (ChainDB.TraceImmutableDBEvent v)       = gsTraceImmutableDBEvent v
gsTraceEvent (ChainDB.TraceVolatileDBEvent v)        = gsTraceVolatileDBEvent v

gsTraceAddBlockEvent :: ChainDB.TraceAddBlockEvent blk -> SeverityS
gsTraceAddBlockEvent ChainDB.IgnoreBlockOlderThanK {} = Info
gsTraceAddBlockEvent ChainDB.IgnoreBlockAlreadyInVolatileDB {} = Info
gsTraceAddBlockEvent ChainDB.IgnoreInvalidBlock {} = Info
gsTraceAddBlockEvent ChainDB.AddedBlockToQueue {} = Debug
gsTraceAddBlockEvent ChainDB.BlockInTheFuture {} = Info
gsTraceAddBlockEvent ChainDB.AddedBlockToVolatileDB {} = Debug
gsTraceAddBlockEvent ChainDB.TryAddToCurrentChain {} = Debug
gsTraceAddBlockEvent ChainDB.TrySwitchToAFork {} = Info
gsTraceAddBlockEvent ChainDB.StoreButDontChange {} = Debug
gsTraceAddBlockEvent (ChainDB.AddedToCurrentChain events _ _ _) =
      maximumDef Notice (map gsLedgerEvent events)
gsTraceAddBlockEvent (ChainDB.SwitchedToAFork events _ _ _) =
      maximumDef Notice (map gsLedgerEvent events)
gsTraceAddBlockEvent (ChainDB.AddBlockValidation ev') = gsTraceValidationEvent ev'
gsTraceAddBlockEvent ChainDB.ChainSelectionForFutureBlock{} = Debug

gsTraceValidationEvent :: ChainDB.TraceValidationEvent blk -> SeverityS
gsTraceValidationEvent ChainDB.InvalidBlock {} = Error
gsTraceValidationEvent ChainDB.InvalidCandidate {} = Error
gsTraceValidationEvent ChainDB.ValidCandidate {} = Info
gsTraceValidationEvent ChainDB.CandidateContainsFutureBlocks {} = Debug
gsTraceValidationEvent ChainDB.CandidateContainsFutureBlocksExceedingClockSkew{} = Error

gsTraceFollowerEvent :: ChainDB.TraceFollowerEvent blk -> SeverityS
gsTraceFollowerEvent ChainDB.NewFollower {}            = Debug
gsTraceFollowerEvent ChainDB.FollowerNoLongerInMem {}  = Debug
gsTraceFollowerEvent ChainDB.FollowerSwitchToMem {}    = Debug
gsTraceFollowerEvent ChainDB.FollowerNewImmIterator {} = Debug

gsLedgerEvent :: LedgerEvent blk -> SeverityS
gsLedgerEvent (LedgerUpdate _)  = Notice
gsLedgerEvent (LedgerWarning _) = Critical

gsTraceCopyToImmutableDBEvent :: ChainDB.TraceCopyToImmutableDBEvent blk -> SeverityS
gsTraceCopyToImmutableDBEvent ChainDB.CopiedBlockToImmutableDB {} = Debug
gsTraceCopyToImmutableDBEvent ChainDB.NoBlocksToCopyToImmutableDB = Debug

gsTraceGCEvent :: ChainDB.TraceGCEvent blk -> SeverityS
gsTraceGCEvent ChainDB.PerformedGC {} = Debug
gsTraceGCEvent ChainDB.ScheduledGC {} = Debug

gsTraceInitChainSelEvent :: ChainDB.TraceInitChainSelEvent blk -> SeverityS
gsTraceInitChainSelEvent ChainDB.InitChainSelValidation {} = Debug

gsTraceOpenEvent :: ChainDB.TraceOpenEvent blk -> SeverityS
gsTraceOpenEvent ChainDB.OpenedDB {}          = Info
gsTraceOpenEvent ChainDB.ClosedDB {}          = Info
gsTraceOpenEvent ChainDB.OpenedImmutableDB {} = Info
gsTraceOpenEvent ChainDB.OpenedVolatileDB     = Info
gsTraceOpenEvent ChainDB.OpenedLgrDB          = Info

gsTraceIteratorEvent :: ChainDB.TraceIteratorEvent blk -> SeverityS
gsTraceIteratorEvent ChainDB.StreamFromVolatileDB {} = Debug
gsTraceIteratorEvent _                               = Debug

gsTraceLedgerEvent :: LedgerDB.TraceEvent blk -> SeverityS
gsTraceLedgerEvent LedgerDB.TookSnapshot {}    = Info
gsTraceLedgerEvent LedgerDB.DeletedSnapshot {} = Debug
gsTraceLedgerEvent LedgerDB.InvalidSnapshot {} = Error

gsTraceLedgerReplayEvent :: LedgerDB.TraceReplayEvent blk replayTo -> SeverityS
gsTraceLedgerReplayEvent LedgerDB.ReplayFromGenesis {}  = Info
gsTraceLedgerReplayEvent LedgerDB.ReplayFromSnapshot {} = Info
gsTraceLedgerReplayEvent LedgerDB.ReplayedBlock {}      = Info

gsTraceImmutableDBEvent :: ImmDB.TraceEvent blk -> SeverityS
gsTraceImmutableDBEvent _ = Debug

gsTraceVolatileDBEvent :: VolDB.TraceEvent blk -> SeverityS
gsTraceVolatileDBEvent _ = Debug

withNamesChainDB :: Monad m
  => Trace m (ChainDB.TraceEvent blk)
  -> Trace m (ChainDB.TraceEvent blk)
withNamesChainDB = withNamesAppended namesForTraceEvents

namesForTraceEvents :: ChainDB.TraceEvent blk -> [Text]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.IgnoreBlockOlderThanK _)) =
      ["AddBlockEvent","IgnoreBlockOlderThanK"]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.IgnoreBlockAlreadyInVolatileDB _)) =
      ["AddBlockEvent", "IgnoreBlockAlreadyInVolatileDB"]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.IgnoreInvalidBlock {})) =
      ["AddBlockEvent", "IgnoreBlockAlreadyInVolatileDB"]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.AddedBlockToQueue {})) =
      ["AddBlockEvent", "AddedBlockToQueue"]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.BlockInTheFuture {})) =
      ["AddBlockEvent","BlockInTheFuture"]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.AddedBlockToVolatileDB {})) =
      ["AddBlockEvent", "AddedBlockToVolatileDB"]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.TryAddToCurrentChain {})) =
      ["AddBlockEvent", "TryAddToCurrentChain"]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.TrySwitchToAFork {})) =
      ["AddBlockEvent", "TrySwitchToAFork"]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.StoreButDontChange {})) =
      ["AddBlockEvent", "StoreButDontChange"]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.AddedToCurrentChain {})) =
      ["AddBlockEvent", "AddedToCurrentChain"]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.SwitchedToAFork {})) =
      ["AddBlockEvent", "SwitchedToAFork"]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.AddBlockValidation (ChainDB.InvalidBlock {}))) =
      ["AddBlockEvent", "AddBlockValidation", "InvalidBlock"]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.AddBlockValidation (ChainDB.InvalidCandidate {}))) =
      ["AddBlockEvent", "AddBlockValidation", "InvalidCandidate"]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.AddBlockValidation (ChainDB.ValidCandidate {}))) =
      ["AddBlockEvent", "AddBlockValidation", "ValidCandidate"]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.AddBlockValidation (ChainDB.CandidateContainsFutureBlocks {}))) =
      ["AddBlockEvent", "AddBlockValidation", "CandidateContainsFutureBlocks"]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.AddBlockValidation
    (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew {}))) =
      ["AddBlockEvent", "AddBlockValidation",
        "CandidateContainsFutureBlocksExceedingClockSkew"]
namesForTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.ChainSelectionForFutureBlock {})) =
      ["AddBlockEvent", "ChainSelectionForFutureBlock"]
namesForTraceEvents (ChainDB.TraceFollowerEvent
  ChainDB.NewFollower) =
      ["FollowerEvent", "NewFollower"]
namesForTraceEvents (ChainDB.TraceFollowerEvent
  (ChainDB.FollowerNoLongerInMem {})) =
      ["FollowerEvent", "FollowerNoLongerInMem"]
namesForTraceEvents (ChainDB.TraceFollowerEvent
  (ChainDB.FollowerSwitchToMem {})) =
      ["FollowerEvent", "FollowerSwitchToMem"]
namesForTraceEvents (ChainDB.TraceFollowerEvent
  (ChainDB.FollowerNewImmIterator {})) =
      ["FollowerEvent", "FollowerNewImmIterator"]
namesForTraceEvents (ChainDB.TraceCopyToImmutableDBEvent
  (ChainDB.CopiedBlockToImmutableDB {})) =
      ["CopyToImmutableDBEvent", "CopiedBlockToImmutableDB"]
namesForTraceEvents (ChainDB.TraceCopyToImmutableDBEvent
  (ChainDB.NoBlocksToCopyToImmutableDB)) =
      ["CopyToImmutableDBEvent", "NoBlocksToCopyToImmutableDB"]
namesForTraceEvents (ChainDB.TraceGCEvent
  (ChainDB.ScheduledGC {})) =
      ["GCEvent", "NoBlocksToCopyToImmutableDB"]
namesForTraceEvents (ChainDB.TraceGCEvent
  (ChainDB.PerformedGC {})) =
      ["GCEvent", "NoBlocksToCopyToImmutableDB"]
namesForTraceEvents (ChainDB.TraceInitChainSelEvent
  (ChainDB.InitChainSelValidation (ChainDB.InvalidBlock {}))) =
      ["InitChainSelEvent", "InitChainSelValidation", "InvalidBlock"]
namesForTraceEvents (ChainDB.TraceInitChainSelEvent
  (ChainDB.InitChainSelValidation (ChainDB.InvalidCandidate {}))) =
      ["InitChainSelEvent", "InitChainSelValidation", "InvalidCandidate"]
namesForTraceEvents (ChainDB.TraceInitChainSelEvent
  (ChainDB.InitChainSelValidation (ChainDB.ValidCandidate {}))) =
      ["InitChainSelEvent", "InitChainSelValidation", "ValidCandidate"]
namesForTraceEvents (ChainDB.TraceInitChainSelEvent
  (ChainDB.InitChainSelValidation (ChainDB.CandidateContainsFutureBlocks {}))) =
      ["InitChainSelEvent", "InitChainSelValidation",
        "CandidateContainsFutureBlocks"]
namesForTraceEvents (ChainDB.TraceInitChainSelEvent
  (ChainDB.InitChainSelValidation
      (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew {}))) =
      ["InitChainSelEvent", "InitChainSelValidation",
        "CandidateContainsFutureBlocksExceedingClockSkew"]
namesForTraceEvents (ChainDB.TraceOpenEvent
  (ChainDB.OpenedDB {})) =
      ["OpenEvent", "OpenedDB"]
namesForTraceEvents (ChainDB.TraceOpenEvent
  (ChainDB.ClosedDB {})) =
      ["OpenEvent", "ClosedDB"]
namesForTraceEvents (ChainDB.TraceOpenEvent
  (ChainDB.OpenedImmutableDB {})) =
      ["OpenEvent", "OpenedImmutableDB"]
namesForTraceEvents (ChainDB.TraceOpenEvent
  ChainDB.OpenedVolatileDB) =
      ["OpenEvent", "OpenedVolatileDB"]
namesForTraceEvents (ChainDB.TraceOpenEvent
  ChainDB.OpenedLgrDB) =
      ["OpenEvent", "OpenedLgrDB"]
namesForTraceEvents (ChainDB.TraceIteratorEvent
  (ChainDB.UnknownRangeRequested {})) =
      ["IteratorEvent", "UnknownRangeRequested"]
namesForTraceEvents (ChainDB.TraceIteratorEvent
  (ChainDB.StreamFromVolatileDB {})) =
      ["IteratorEvent", "StreamFromVolatileDB"]
namesForTraceEvents (ChainDB.TraceIteratorEvent
  (ChainDB.StreamFromImmutableDB {})) =
      ["IteratorEvent", "StreamFromImmutableDB"]
namesForTraceEvents (ChainDB.TraceIteratorEvent
  (ChainDB.StreamFromBoth {})) =
      ["IteratorEvent", "StreamFromBoth"]
namesForTraceEvents (ChainDB.TraceIteratorEvent
  (ChainDB.BlockMissingFromVolatileDB {})) =
      ["IteratorEvent", "BlockMissingFromVolatileDB"]
namesForTraceEvents (ChainDB.TraceIteratorEvent
  (ChainDB.BlockWasCopiedToImmutableDB {})) =
      ["IteratorEvent", "BlockWasCopiedToImmutableDB"]
namesForTraceEvents (ChainDB.TraceIteratorEvent
  (ChainDB.BlockGCedFromVolatileDB {})) =
      ["IteratorEvent", "BlockGCedFromVolatileDB"]
namesForTraceEvents (ChainDB.TraceIteratorEvent
  ChainDB.SwitchBackToVolatileDB) =
      ["IteratorEvent", "SwitchBackToVolatileDB"]
namesForTraceEvents (ChainDB.TraceLedgerEvent
  (LedgerDB.InvalidSnapshot {})) =
      ["TraceLedgerEvent", "InvalidSnapshot"]
namesForTraceEvents (ChainDB.TraceLedgerEvent
  (LedgerDB.TookSnapshot {})) =
      ["TraceLedgerEvent", "TookSnapshot"]
namesForTraceEvents (ChainDB.TraceLedgerEvent
  (LedgerDB.DeletedSnapshot {})) =
      ["TraceLedgerEvent", "DeletedSnapshot"]
namesForTraceEvents (ChainDB.TraceLedgerReplayEvent
  (LedgerDB.ReplayFromGenesis {})) =
      ["TraceLedgerEvent", "ReplayFromGenesis"]
namesForTraceEvents (ChainDB.TraceLedgerReplayEvent
  (LedgerDB.ReplayFromSnapshot {})) =
      ["TraceLedgerEvent", "ReplayFromSnapshot"]
namesForTraceEvents (ChainDB.TraceLedgerReplayEvent
  (LedgerDB.ReplayedBlock {})) =
      ["TraceLedgerEvent", "ReplayedBlock"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  ImmDB.NoValidLastLocation) =
      ["ImmutableDBEvent", "NoValidLastLocation"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.ValidatedLastLocation {})) =
      ["ImmutableDBEvent", "ValidatedLastLocation"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.ValidatingChunk {})) =
      ["ImmutableDBEvent", "ValidatingChunk"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.MissingChunkFile {})) =
      ["ImmutableDBEvent", "MissingChunkFile"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.InvalidChunkFile {})) =
      ["ImmutableDBEvent", "InvalidChunkFile"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.ChunkFileDoesntFit {})) =
      ["ImmutableDBEvent", "ChunkFileDoesntFit"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.MissingPrimaryIndex {})) =
      ["ImmutableDBEvent", "MissingPrimaryIndex"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.MissingSecondaryIndex {})) =
      ["ImmutableDBEvent", "MissingSecondaryIndex"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.InvalidPrimaryIndex {})) =
      ["ImmutableDBEvent", "InvalidPrimaryIndex"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.InvalidSecondaryIndex {})) =
      ["ImmutableDBEvent", "InvalidSecondaryIndex"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.RewritePrimaryIndex {})) =
      ["ImmutableDBEvent", "RewritePrimaryIndex"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.RewriteSecondaryIndex {})) =
      ["ImmutableDBEvent", "RewriteSecondaryIndex"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.Migrating {})) =
      ["ImmutableDBEvent", "Migrating"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.DeletingAfter {})) =
      ["ImmutableDBEvent", "DeletingAfter"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  ImmDB.DBAlreadyClosed) =
      ["ImmutableDBEvent", "DBAlreadyClosed"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent ImmDB.DBClosed) =
      ["ImmutableDBEvent", "DBClosed"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.TraceCacheEvent (ImmDB.TraceCurrentChunkHit {}))) =
      ["ImmutableDBEvent", "CacheEvent", "TraceCurrentChunkHit"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.TraceCacheEvent (ImmDB.TracePastChunkHit {}))) =
      ["ImmutableDBEvent", "CacheEvent", "TracePastChunkHit"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.TraceCacheEvent (ImmDB.TracePastChunkMiss {}))) =
      ["ImmutableDBEvent", "CacheEvent", "TracePastChunkMiss"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.TraceCacheEvent (ImmDB.TracePastChunkEvict {}))) =
      ["ImmutableDBEvent", "CacheEvent", "TracePastChunkEvict"]
namesForTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.TraceCacheEvent (ImmDB.TracePastChunksExpired {}))) =
      ["ImmutableDBEvent", "CacheEvent", "TracePastChunkEvict"]
namesForTraceEvents (ChainDB.TraceVolatileDBEvent
  VolDb.DBAlreadyClosed) =
    ["VolatileDbEvent", "DBAlreadyClosed"]
namesForTraceEvents (ChainDB.TraceVolatileDBEvent
  VolDb.DBAlreadyOpen) =
    ["VolatileDbEvent", "TruncateCurrentFile"]
namesForTraceEvents (ChainDB.TraceVolatileDBEvent
  (VolDb.Truncate {})) =
    ["VolatileDbEvent", "Truncate"]
namesForTraceEvents (ChainDB.TraceVolatileDBEvent
  (VolDb.InvalidFileNames {})) =
    ["VolatileDBEvent", "InvalidFileNames"]
namesForTraceEvents (ChainDB.TraceVolatileDBEvent
  (VolDb.BlockAlreadyHere {})) =
    ["VolatileDBEvent", "BlockAlreadyHere"]
namesForTraceEvents (ChainDB.TraceVolatileDBEvent
  (VolDb.TruncateCurrentFile {})) =
    ["VolatileDBEvent", "TruncateCurrentFile"]
