{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Api.EraCast
  ( EraCast(..)
  ) where

import           Cardano.Api.Eras (CardanoEra (..), IsCardanoEra)
import           Data.Either (Either)
import           Data.Kind (Type)

class EraCast (f :: Type -> Type) error where
  eraCast :: (IsCardanoEra fromEra, IsCardanoEra toEra)
          => f fromEra
          -> CardanoEra toEra
          -> Either error (f toEra)
