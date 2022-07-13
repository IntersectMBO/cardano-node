{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Api.EraCast
  ( EraCast(..)
  , EraCastError(..)
  ) where

import           Prelude

import           Cardano.Api.Eras (CardanoEra (..), IsCardanoEra)

import           Data.Kind (Type)



data EraCastError = forall toEra thing.
  ( IsCardanoEra toEra
  , Show thing
  ) =>
    EraCastError
    { typeName :: thing
    , toEra :: CardanoEra toEra
    }

class EraCast (f :: Type -> Type) where
  eraCast :: (IsCardanoEra fromEra, IsCardanoEra toEra)
          => CardanoEra toEra
          -> f fromEra
          -> Either EraCastError (f toEra)
