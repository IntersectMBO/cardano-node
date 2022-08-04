{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Api.EraCast
  ( EraCast(..)
  , EraCastError(..)
  ) where

import           Cardano.Api.Eras (CardanoEra (..), IsCardanoEra)
import           Data.Either (Either)
import           Data.Kind (Type)
import           Text.Show (Show (..))

data EraCastError = forall fromEra toEra value.
  ( IsCardanoEra fromEra
  , IsCardanoEra toEra
  , Show value
  ) =>
    EraCastError
    { originalValue :: value
    , fromEra :: CardanoEra fromEra
    , toEra :: CardanoEra toEra
    }

class EraCast (f :: Type -> Type) where
  eraCast :: (IsCardanoEra fromEra, IsCardanoEra toEra)
          => CardanoEra toEra
          -> f fromEra
          -> Either EraCastError (f toEra)
