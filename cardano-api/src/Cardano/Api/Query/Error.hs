module Cardano.Api.Query.Error
  ( SimpleQueryError(..)
  , ShelleyBasedQueryError(..)
  , fromConsensusQueryResultMismatch
  ) where

import           Cardano.Api.Eras
import           Cardano.Api.IPC.Error
import           Cardano.Api.IPC.Version
import           Cardano.Api.Modes

import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

data SimpleQueryError
  = SimpleQueryErrorAcquiringFail AcquiringFailure
  | SimpleQueryErrorUnsupportedVer UnsupportedNtcVersionError
  deriving Show

data ShelleyBasedQueryError
  = SbqeQueryEraMismatch EraMismatch
  | SbqeEraInMode AnyCardanoEra AnyConsensusMode
  | SbqeSimpleQueryError SimpleQueryError
  deriving Show

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
