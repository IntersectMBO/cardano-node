{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Api.EraCast
  ( EraCast(..)
  , EraCastError(..)
  , eraCastList
  ) where

import           Cardano.Api.Eras (CardanoEra (..), IsCardanoEra)
import           Control.Monad (forM)
import           Data.Kind (Type)

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

eraCastList :: ()
  => Traversable t
  => EraCast f
  => IsCardanoEra fromEra
  => IsCardanoEra toEra
  => CardanoEra toEra
  -> t (f fromEra)
  -> Either EraCastError (t (f toEra))
eraCastList era as = forM as $ eraCast era
