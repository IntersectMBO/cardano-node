{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.HasTypeProxy
  ( HasTypeProxy(AsType, proxyToAsType)
  , Proxy(..)
  ) where

import           Data.Proxy (Proxy (..))

class HasTypeProxy t where
  -- | A family of singleton types used in this API to indicate which type to
  -- use where it would otherwise be ambiguous or merely unclear.
  --
  -- Values of this type are passed to deserialisation functions for example.
  --
  data AsType t

  proxyToAsType :: Proxy t -> AsType t
