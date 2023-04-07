{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Hedgehog.Roundtrip.CBOR
  ( roundtrip_CBOR
  -- , roundtrip_CDDL_Tx
  ) where

import           Cardano.Api

import           Data.Proxy (Proxy (..))
import           Data.Typeable (typeRep)
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           Hedgehog (Gen, Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H

{- HLINT ignore "Use camelCase" -}

roundtrip_CBOR
  :: forall a. (SerialiseAsCBOR a, Eq a, Show a, HasCallStack)
  => AsType a
  -> Gen a
  -> Property
roundtrip_CBOR typeProxy gen =
  H.property $ do
    GHC.withFrozenCallStack $ H.noteShow_ $ typeRep $ Proxy @a
    val <- H.forAll gen
    H.tripping val serialiseToCBOR (deserialiseFromCBOR typeProxy)
