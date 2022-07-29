{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.EraCast
  ( EraCast(..)
  , EraCastError(..)
  , renderEraCastError
  ) where

import           Prelude

import           Cardano.Api.Eras (CardanoEra (..), IsCardanoEra)

import           Data.Kind (Type)

class EraCast (f :: Type -> Type) where
  eraCast :: (IsCardanoEra fromEra, IsCardanoEra toEra)
          => CardanoEra toEra
          -> f fromEra
          -> Either EraCastError (f toEra)


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

deriving instance Show EraCastError


renderEraCastError :: EraCastError -> String
renderEraCastError (EraCastError value fromEra' toEra') =
      "Unable to cast era from " <> show fromEra' <> " to " <>
      show toEra' <> " the value " <> show value
