{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Api.EraCast
  ( EraCast(..)
  , EraCastError(..)
  ) where

import           Cardano.Api.Eras (CardanoEra (..), IsCardanoEra)
import           Data.Either (Either)
import           Data.Kind (Type)
import           Data.Text (Text)

data EraCastError = forall fromEra toEra.
  ( IsCardanoEra fromEra
  , IsCardanoEra toEra
  ) =>
    EraCastError
    { typeName :: Text
    , fromEra :: CardanoEra fromEra
    , toEra :: CardanoEra toEra
    }

class EraCast (f :: Type -> Type) where
  eraCast :: (IsCardanoEra fromEra, IsCardanoEra toEra)
          => CardanoEra toEra
          -> f fromEra
          -> Either EraCastError (f toEra)
