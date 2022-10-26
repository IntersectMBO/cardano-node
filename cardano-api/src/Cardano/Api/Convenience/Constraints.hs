{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Constraint satisfaction functions. These are used to avoid propagating constraints.
--
module Cardano.Api.Convenience.Constraints (
    getIsCardanoEraConstraint
  ) where


import           Cardano.Api.Eras

getIsCardanoEraConstraint :: CardanoEra era -> (IsCardanoEra era => a) -> a
getIsCardanoEraConstraint ByronEra f = f
getIsCardanoEraConstraint ShelleyEra f = f
getIsCardanoEraConstraint AllegraEra f = f
getIsCardanoEraConstraint MaryEra f = f
getIsCardanoEraConstraint AlonzoEra f = f
getIsCardanoEraConstraint BabbageEra f = f
