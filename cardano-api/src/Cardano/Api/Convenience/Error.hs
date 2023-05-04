module Cardano.Api.Convenience.Error
  ( QueryConvenienceError(..)
  , AllQueryErrors(..)
  ) where

import           Cardano.Api.Environment (EnvSocketError (..))
import           Cardano.Api.Eras
import           Cardano.Api.IPC.Error
import           Cardano.Api.Modes
import           Cardano.Api.Query.Error

import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

data QueryConvenienceError
  = AcqFailure AcquiringFailure -- TODO to be obsolete
  | SockErr EnvSocketError
  | QueryEraMismatch EraMismatch -- TODO to be obsolete
  | QueryConvenienceError AllQueryErrors
  | ByronEraNotSupported -- TODO to be obsolete
  | EraConsensusModeMismatch !AnyConsensusMode !AnyCardanoEra -- TODO to be obsolete

data AllQueryErrors
  = AllQueryErrorSimple SimpleQueryError
  | AllQueryErrorSbe ShelleyBasedQueryError
  | AllQueryEraExpectedSbe
  deriving Show
