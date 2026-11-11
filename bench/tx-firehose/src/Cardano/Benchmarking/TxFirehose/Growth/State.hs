{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | Small journal: only funding lanes and pending batch recipes, never the
-- millions of retained outputs. Stable checkpoints may advance only from an
-- immutable ledger query. A later change output proves all earlier batches.
module Cardano.Benchmarking.TxFirehose.Growth.State where

import Cardano.Api qualified as Api
import Cardano.Benchmarking.TxFirehose.Tx (Fund (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.List (findIndices)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)

data SavedFund = SavedFund {reference :: Api.TxIn, lovelace :: Integer}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

saveFund :: Fund -> SavedFund
saveFund (Fund ref value) = SavedFund (Api.fromShelleyTxIn ref) value

restoreFund :: SavedFund -> Fund
restoreFund (SavedFund ref value) = Fund (Api.toShelleyTxIn ref) value

data Batch = Batch {count :: Int, fee :: Integer, change :: SavedFund}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Lane = Lane {anchor :: SavedFund, pending :: [Batch], finalizedAnchor :: Bool}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

reserved :: [Lane] -> Integer
reserved = sum . map (sum . map (toInteger . count) . pending)

laneFunds :: Lane -> [SavedFund]
laneFunds lane = anchor lane : map change (pending lane)

-- | Index 0 is the stable anchor; index n is change after n pending batches.
observedPrefix :: Set Api.TxIn -> Lane -> Either String Int
observedPrefix present lane = case findIndices ((`Set.member` present) . reference) (laneFunds lane) of
  [] ->
    Left
      "no known funding/change output exists at the queried ledger point; external spending or inconsistent journal"
  indices -> Right (last indices)

advance :: Set Api.TxIn -> Lane -> Either String (Integer, Lane)
advance present lane = case observedPrefix present lane of
  Left _ | not (finalizedAnchor lane) -> Right (0, lane)
  Left err -> Left err
  Right n ->
    let (done, rest) = splitAt n (pending lane)
        newAnchor = if null done then anchor lane else change (last done)
     in Right (sum (map (toInteger . count) done), Lane newAnchor rest True)

data Identity = Identity
  { schema :: Int
  , network :: String
  , era :: String
  , fundingAddress :: String
  , retainedAddress :: String
  , colour :: String
  , target :: Integer
  , lanesRequested :: Int
  , outputsRequested :: Int
  , retainedAmount :: Integer
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data SeedPlan = SeedPlan {seedInput :: SavedFund, seedFee :: Integer}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Journal = Journal
  { revision :: Integer
  , identity :: Identity
  , seed :: Maybe SeedPlan
  , seeded :: Bool
  , confirmed :: Integer
  , lanes :: [Lane]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

pendingGrowth :: Journal -> Integer
pendingGrowth j = reserved (lanes j) + if seeded j then 0 else toInteger (length (lanes j)) - 1

-- Each submission appends only its changed lane. Checkpoints compact the WAL
-- after finality advances; sequence numbers make either side of a crash during
-- compaction safe to replay. Keep the main file and .wal together.
data LaneDelta = LaneDelta
  { nextRevision :: Integer
  , laneIndex :: Int
  , nextLane :: Lane
  }
  deriving (Generic, FromJSON, ToJSON)

-- | Reapply a durable lane update exactly once after a checkpoint. Only a
-- single appended batch is valid: changing an anchor requires a checkpoint
-- justified by immutable ledger state, never a submission record.
applyDelta :: Journal -> LaneDelta -> Either String Journal
applyDelta journal delta
  | nextRevision delta <= revision journal = Right journal
  | nextRevision delta /= revision journal + 1 || index < 0 || index >= length (lanes journal) =
      Left "Invalid growth WAL sequence/lane"
  | anchor old /= anchor new
      || finalizedAnchor old /= finalizedAnchor new
      || length (pending new) /= length (pending old) + 1
      || take (length (pending old)) (pending new) /= pending old =
      Left "Invalid growth WAL lane transition"
  | otherwise =
      Right
        journal
          { revision = nextRevision delta
          , lanes = take index (lanes journal) ++ [new] ++ drop (index + 1) (lanes journal)
          }
 where
  index = laneIndex delta
  old = lanes journal !! index
  new = nextLane delta

-- | Reconcile only against immutable state. A later change proves its entire
-- preceding lane prefix, including a recorded seed transaction.
reconcile :: Set Api.TxIn -> Journal -> Either String Journal
reconcile stable journal = do
  let seedStillPending =
        not (seeded journal)
          && not (all (either (const False) (const True) . observedPrefix stable) (lanes journal))
  advanced <-
    if seedStillPending
      then pure [(0, lane) | lane <- lanes journal]
      else traverse (advance stable) (lanes journal)
  let seedGrowth =
        if not (seeded journal) && not seedStillPending
          then toInteger (length (lanes journal)) - 1
          else 0
  pure
    journal
      { confirmed = confirmed journal + seedGrowth + sum (map fst advanced)
      , seeded = not seedStillPending
      , lanes = map snd advanced
      }

-- | Drain must also wait for zero-net-growth (single-lane) initialization.
-- The mode is deliberately not stored in the journal or its identity.
finished :: Bool -> Journal -> Bool
finished draining journal =
  pendingGrowth journal == 0
    && if draining then seeded journal else confirmed journal == target (identity journal)

-- | Replaying recorded transactions is independent of allocating new batches.
-- Drain always leaves the entire unallocated remainder for a future normal run.
remainingToAllocate :: Bool -> Journal -> Integer
remainingToAllocate draining journal
  | draining = 0
  | otherwise = target (identity journal) - confirmed journal - pendingGrowth journal
