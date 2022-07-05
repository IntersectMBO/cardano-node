{-# LANGUAGE KindSignatures #-}

module Cardano.Api.EraCast
  ( EraCast(..)
  ) where

import           Cardano.Api.Eras (IsCardanoEra)
import           Data.Either (Either)
import           Data.Kind (Type)
import           Data.Text (Text)

class EraCast (f :: Type -> Type) where
  eraCast :: (IsCardanoEra era1, IsCardanoEra era2) => f era1 -> Either Text (f era2)
