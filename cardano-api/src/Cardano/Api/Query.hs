{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Queries from local clients to the node.
--
module Cardano.Api.Query (

    -- * Queries
    QueryInMode(..),
    QueryInEra(..),
    QueryInShelleyBasedEra(..),

    -- * Internal conversion functions
    toConsensusQuery,
    fromConsensusQueryResult,
  ) where

import           Prelude
import           Data.Bifunctor (bimap)
import           Data.SOP.Strict (SListI)

import           Ouroboros.Network.Protocol.LocalStateQuery.Client (Some(..))

import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.Degenerate as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

import qualified Ouroboros.Consensus.Byron.Ledger       as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger     as Consensus
import qualified Ouroboros.Consensus.Cardano.Block      as Consensus

import qualified Cardano.Chain.Update.Validation.Interface as Byron.Update

import           Cardano.Api.Block
import           Cardano.Api.Eras
import           Cardano.Api.Modes


-- ----------------------------------------------------------------------------
-- Queries
--

data QueryInMode mode result where

     QueryCurrentEra :: ConsensusModeIsMultiEra mode
                     -> QueryInMode mode AnyCardanoEra

     QueryInEra      :: EraInMode era mode
                     -> QueryInEra era result
                     -> QueryInMode mode (Either EraMismatch result)

--TODO: add support for these
--     QueryEraStart   :: ConsensusModeIsMultiEra mode
--                     -> EraInMode era mode
--                     -> QueryInMode mode (Maybe EraStart)

--     QueryEraHistory :: QueryInMode mode EraHistory

deriving instance Show (QueryInMode mode result)


data QueryInEra era result where
     QueryByronUpdateState :: QueryInEra ByronEra ByronUpdateState

     QueryInShelleyBasedEra :: ShelleyBasedEra era
                            -> QueryInShelleyBasedEra result
                            -> QueryInEra era result

deriving instance Show (QueryInEra era result)


data QueryInShelleyBasedEra result where
     QueryChainPoint
       :: QueryInShelleyBasedEra ChainPoint

     QueryEpoch
       :: QueryInShelleyBasedEra EpochNo

--TODO: add support for these
--     QueryGenesisParameters
--       :: QueryInShelleyBasedEra GenesisParameters

--     QueryProtocolParameters
--       :: QueryInShelleyBasedEra ProtocolParameters

--     QueryProtocolParametersUpdate
--       :: QueryInShelleyBasedEra ProtocolParametersUpdate

--     QueryStakeDistribution
--       :: QueryInShelleyBasedEra StakeDistribution

--     QueryUTxO
--       :: Maybe (Set AddressAny)
--       -> QueryInShelleyBasedEra UTxO

--     QueryStakeAddresses
--       :: Set StakeAddress
--       -> QueryInShelleyBasedEra (Map StakeAddress Lovelace,
--                                  Map StakeAddress PoolId)

--     QueryPoolRanking
--       :: 
--       -> QueryInShelleyBasedEra 

--     QueryLedgerState
--       :: QueryInShelleyBasedEra LedgerState

--     QueryProtocolState
--       :: QueryInShelleyBasedEra ProtocolState

deriving instance Show (QueryInShelleyBasedEra result)


-- ----------------------------------------------------------------------------
-- Wrapper types used in queries
--

--TODO: provide appropriate instances for these types as needed, e.g. JSON

newtype ByronUpdateState = ByronUpdateState Byron.Update.State
  deriving Show


-- ----------------------------------------------------------------------------
-- Conversions of queries into the consensus types.
--

toConsensusQuery :: forall mode block result.
                    ConsensusBlockForMode mode ~ block
                 => QueryInMode mode result
                 -> Some (Consensus.Query block)
toConsensusQuery (QueryCurrentEra CardanoModeIsMultiEra) =
    Some (Consensus.QueryHardFork Consensus.GetCurrentEra)

toConsensusQuery (QueryInEra ByronEraInByronMode QueryByronUpdateState) =
    Some (Consensus.DegenQuery Consensus.GetUpdateInterfaceState)

toConsensusQuery (QueryInEra ByronEraInCardanoMode QueryByronUpdateState) =
    Some (Consensus.QueryIfCurrentByron Consensus.GetUpdateInterfaceState)

toConsensusQuery (QueryInEra erainmode (QueryInShelleyBasedEra era q)) =
    case erainmode of
      ByronEraInByronMode     -> case era of {}
      ShelleyEraInShelleyMode -> toConsensusQueryShelleyBased erainmode q
      ByronEraInCardanoMode   -> case era of {}
      ShelleyEraInCardanoMode -> toConsensusQueryShelleyBased erainmode q
      AllegraEraInCardanoMode -> toConsensusQueryShelleyBased erainmode q
      MaryEraInCardanoMode    -> toConsensusQueryShelleyBased erainmode q


toConsensusQueryShelleyBased
  :: forall era mode ledgerera block xs result.
     ConsensusBlockForEra era ~ Consensus.ShelleyBlock ledgerera
  => ConsensusBlockForMode mode ~ block
  => block ~ Consensus.HardForkBlock xs
  => EraInMode era mode
  -> QueryInShelleyBasedEra result
  -> Some (Consensus.Query block)
toConsensusQueryShelleyBased erainmode QueryChainPoint =
    Some (consensusQueryInEraInMode erainmode Consensus.GetLedgerTip)

toConsensusQueryShelleyBased erainmode QueryEpoch =
    Some (consensusQueryInEraInMode erainmode Consensus.GetEpochNo)


consensusQueryInEraInMode
  :: forall era mode erablock modeblock result result' xs.
     ConsensusBlockForEra era   ~ erablock
  => ConsensusBlockForMode mode ~ modeblock
  => modeblock ~ Consensus.HardForkBlock xs
  => Consensus.HardForkQueryResult xs result ~ result'
  => EraInMode era mode
  -> Consensus.Query erablock  result
  -> Consensus.Query modeblock result'
consensusQueryInEraInMode ByronEraInByronMode     = Consensus.DegenQuery
consensusQueryInEraInMode ShelleyEraInShelleyMode = Consensus.DegenQuery
consensusQueryInEraInMode ByronEraInCardanoMode   = Consensus.QueryIfCurrentByron
consensusQueryInEraInMode ShelleyEraInCardanoMode = Consensus.QueryIfCurrentShelley
consensusQueryInEraInMode AllegraEraInCardanoMode = Consensus.QueryIfCurrentAllegra
consensusQueryInEraInMode MaryEraInCardanoMode    = Consensus.QueryIfCurrentMary


-- ----------------------------------------------------------------------------
-- Conversions of query results from the consensus types.
--

fromConsensusQueryResult :: forall mode block result result'.
                            ConsensusBlockForMode mode ~ block
                         => QueryInMode mode result
                         -> Consensus.Query block result'
                         -> result'
                         -> result
fromConsensusQueryResult (QueryCurrentEra CardanoModeIsMultiEra) q' r' =
    case q' of
      Consensus.QueryHardFork Consensus.GetCurrentEra ->
        anyEraInModeToAnyEra (fromConsensusEraIndex CardanoMode r')
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ByronEraInByronMode
                                     QueryByronUpdateState) q' r' =
    case (q', r') of
      (Consensus.DegenQuery Consensus.GetUpdateInterfaceState,
       Consensus.DegenQueryResult r'') ->
        Right (ByronUpdateState r'')

fromConsensusQueryResult (QueryInEra ByronEraInCardanoMode
                                     QueryByronUpdateState) q' r' =
    case q' of
      Consensus.QueryIfCurrentByron Consensus.GetUpdateInterfaceState ->
        bimap fromConsensusEraMismatch ByronUpdateState r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ByronEraInByronMode
                                     (QueryInShelleyBasedEra era _)) _ _ =
    case era of {}

fromConsensusQueryResult (QueryInEra ShelleyEraInShelleyMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case (q', r') of
      (Consensus.DegenQuery q'', Consensus.DegenQueryResult r'') ->
        Right (fromConsensusQueryResultShelleyBased q q'' r'')

fromConsensusQueryResult (QueryInEra ByronEraInCardanoMode
                                     (QueryInShelleyBasedEra era _)) _ _ =
    case era of {}

fromConsensusQueryResult (QueryInEra ShelleyEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.QueryIfCurrentShelley q'' ->
        bimap fromConsensusEraMismatch
              (fromConsensusQueryResultShelleyBased q q'')
              r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra AllegraEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.QueryIfCurrentAllegra q'' ->
        bimap fromConsensusEraMismatch
              (fromConsensusQueryResultShelleyBased q q'')
              r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra MaryEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.QueryIfCurrentMary q'' ->
        bimap fromConsensusEraMismatch
              (fromConsensusQueryResultShelleyBased q q'')
              r'
      _ -> fromConsensusQueryResultMismatch


fromConsensusQueryResultShelleyBased
  :: forall ledgerera result result'.
     Consensus.ShelleyBasedEra ledgerera
  => QueryInShelleyBasedEra result
  -> Consensus.Query (Consensus.ShelleyBlock ledgerera) result'
  -> result'
  -> result
fromConsensusQueryResultShelleyBased QueryChainPoint q' point =
    case q' of
      Consensus.GetLedgerTip -> fromConsensusPoint point
      _                      -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased QueryEpoch q' epoch =
    case q' of
      Consensus.GetEpochNo -> epoch
      _                    -> fromConsensusQueryResultMismatch


-- | This should /only/ happen if we messed up the mapping in 'toConsensusQuery'
-- and 'fromConsensusQueryResult' so they are inconsistent with each other.
--
-- If we do encounter this error it means that 'toConsensusQuery' maps a
-- API query constructor to a certain consensus query constructor but that
-- 'fromConsensusQueryResult' apparently expects a different pairing.
--
-- For example, imagine if 'toConsensusQuery would (incorrectly) map
-- 'QueryChainPoint' to 'Consensus.GetEpochNo' but 'fromConsensusQueryResult'
-- (correctly) expected to find 'Consensus.GetLedgerTip'. This mismatch would
-- trigger this error.
--
-- Such mismatches should be preventable with an appropriate property test.
--
fromConsensusQueryResultMismatch :: a
fromConsensusQueryResultMismatch =
    error "fromConsensusQueryResult: internal query mismatch"


fromConsensusEraMismatch :: SListI xs
                         => Consensus.MismatchEraInfo xs -> EraMismatch
fromConsensusEraMismatch = Consensus.mkEraMismatch

