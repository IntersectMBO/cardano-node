{-# LANGUAGE KindSignatures #-}

module Cardano.Api.EraCast
  ( EraCast(..)
  ) where

import           Cardano.Api.Eras (CardanoEra (..), IsCardanoEra)
import           Data.Either (Either)
import           Data.Kind (Type)
import           Data.Text (Text)

class EraCast (f :: Type -> Type) where
  eraCast :: (IsCardanoEra fromEra, IsCardanoEra toEra)
          => CardanoEra toEra
          -> f fromEra
          -> Either Text (f toEra)
