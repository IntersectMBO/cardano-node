
module Cardano.TraceDispatcher.ChainDBTracer.Combinators
  ( withSeverityChainDB
  ) where

import           Cardano.Logging
import           Cardano.Prelude

import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent (..))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolDB

withSeverityChainDB :: Monad m
  => Trace m (ChainDB.TraceEvent blk)
  -> Trace m (ChainDB.TraceEvent blk)
withSeverityChainDB = withSeverity gsTraceEvent

gsTraceEvent :: ChainDB.TraceEvent blk -> SeverityS
gsTraceEvent (ChainDB.TraceAddBlockEvent v) = gsTraceAddBlockEvent v
gsTraceEvent (ChainDB.TraceFollowerEvent v) = gsTraceFollowerEvent v
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

gsTraceImmutableDBEvent :: ImmDB.TraceEvent blk-> SeverityS
gsTraceImmutableDBEvent _ = Debug

gsTraceVolatileDBEvent :: VolDB.TraceEvent blk-> SeverityS
gsTraceVolatileDBEvent _ = Debug
