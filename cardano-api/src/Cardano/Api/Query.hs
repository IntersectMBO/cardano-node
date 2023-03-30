{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- The Shelley ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}


-- | Queries from local clients to the node.
--
module Cardano.Api.Query (

    -- * Queries
    AnyQuery(.., AnyQuerySbe),
    QueryInMode(..),
    QueryShelleyBasedEra(..),

    -- * Query errors
    AcquiringFailure(..),
    ShelleyBasedQueryError(..),
    AllQueryErrors(..),
    SimpleQueryError(..),

    -- * Internal conversion functions
    toConsensusQuery,
    fromConsensusQueryResult,
    toConsensusAnyQuery,
    fromConsensusQueryAnyResult,


    EraHistory(..),
    SystemStart(..),

    LedgerEpochInfo(..),
    toLedgerEpochInfo,

    SlotsInEpoch(..),
    SlotsToEpochEnd(..),

    slotToEpoch,

    LedgerState(..),

    getProgress,
    getSlotForRelativeTime,
  ) where


import           Data.Word (Word64)

import           Ouroboros.Network.Protocol.LocalStateQuery.Client (Some (..))

import           Ouroboros.Consensus.Cardano.Block (LedgerState (..))
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry
import qualified Ouroboros.Consensus.Ledger.Query as Consensus

import           Cardano.Slotting.Time (SystemStart (..))

import           Cardano.Api.Block
import           Cardano.Api.Convenience.Error
import           Cardano.Api.Eras
import           Cardano.Api.IPC.Error
import           Cardano.Api.IPC.Version
import           Cardano.Api.Modes
import           Cardano.Api.Query.Error
import           Cardano.Api.Query.InMode
import           Cardano.Api.Query.ShelleyBased

-- ----------------------------------------------------------------------------
-- Queries
--
-- 'QueryShelleyBasedEra' queries return 'Either EraMismatch'.
-- This means we have an additional failure in the query of the result, that does
-- not exists in `QueryInMode` queries. Because of this we make this distinction
-- so that we are not forced to handle errors that cannot occurr.
data AnyQuery mode result where
  AnyQueryAnyEra :: QueryInMode mode result -> AnyQuery mode result
  AnyQueryShelleyBasedEra :: QueryShelleyBasedEra mode result -> AnyQuery mode result

instance NodeToClientVersionOf (AnyQuery mode result) where
  nodeToClientVersionOf (AnyQueryAnyEra q) = nodeToClientVersionOf q
  nodeToClientVersionOf (AnyQueryShelleyBasedEra q) = nodeToClientVersionOf q

pattern AnyQuerySbe
  :: EraInMode era mode
  -> ShelleyBasedEra era
  -> QueryInShelleyBasedEra era result
  -> AnyQuery mode (Either EraMismatch result)
pattern AnyQuerySbe a sbe q = AnyQueryShelleyBasedEra (QueryShelleyBasedEra a (QueryInShelleyBasedEra sbe q))

{-# COMPLETE AnyQueryAnyEra, AnyQuerySbe #-}

--TODO: add support for these
--     QueryEraStart   :: ConsensusModeIsMultiEra mode
--                     -> EraInMode era mode
--                     -> QueryInMode mode (Maybe EraStart)

newtype SlotsInEpoch = SlotsInEpoch Word64

newtype SlotsToEpochEnd = SlotsToEpochEnd Word64

slotToEpoch :: SlotNo -> EraHistory mode -> Either Qry.PastHorizonException (EpochNo, SlotsInEpoch, SlotsToEpochEnd)
slotToEpoch slotNo (EraHistory _ interpreter) = case Qry.interpretQuery interpreter (Qry.slotToEpoch slotNo) of
  Right (epochNumber, slotsInEpoch, slotsToEpochEnd) -> Right (epochNumber, SlotsInEpoch slotsInEpoch, SlotsToEpochEnd slotsToEpochEnd)
  Left e -> Left e

toConsensusAnyQuery
  :: forall mode block result.
     ConsensusBlockForMode mode ~ block
  => AnyQuery mode result
  -> Some (Consensus.Query block)
toConsensusAnyQuery (AnyQueryShelleyBasedEra q) = toConsensusQuerySbe q
toConsensusAnyQuery (AnyQueryAnyEra q) = toConsensusQuery q


fromConsensusQueryAnyResult
  :: forall mode block result result'. ConsensusBlockForMode mode ~ block
  => AnyQuery mode result
  -> Consensus.Query block result'
  -> (result' -> result)
fromConsensusQueryAnyResult (AnyQueryShelleyBasedEra q) = fromConsensusQueryResultSbe q
fromConsensusQueryAnyResult (AnyQueryAnyEra q) = fromConsensusQueryResult q


