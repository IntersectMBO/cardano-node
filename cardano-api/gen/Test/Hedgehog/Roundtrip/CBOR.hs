{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Hedgehog.Roundtrip.CBOR
  ( trippingCbor
  ) where

import           Cardano.Api

import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}

-- | Assert that CBOR serialisation and deserialisation roundtrips.
trippingCbor :: ()
  => HasCallStack
  => H.MonadTest m
  => Show a
  => Eq a
  => SerialiseAsCBOR a
  => AsType a
  -> a
  -> m ()
trippingCbor typeProxy v = GHC.withFrozenCallStack $
  H.tripping v serialiseToCBOR (deserialiseFromCBOR typeProxy)
