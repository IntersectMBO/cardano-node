{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.KeysByron
  ( tests
  ) where

import           Cardano.Api
import           Cardano.Prelude
import           Hedgehog (Gen, Property, discover)
import           Test.Cardano.Api.Typed.Orphans ()
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog.Group (fromGroup)

import qualified Hedgehog as H
import qualified Test.Cardano.Crypto.Seed.Gen as Gen

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_byron_key_CBOR :: Property
prop_roundtrip_byron_key_CBOR =
  roundtrip_CBOR (AsSigningKey AsByronKey) (deterministicSigningKey AsByronKey <$> Gen.genSeedForKey AsByronKey)

roundtrip_CBOR
  :: (SerialiseAsCBOR a, Eq a, Show a)
  => AsType a -> Gen a -> Property
roundtrip_CBOR typeProxy gen =
  H.property $ do
    val <- H.forAll gen
    H.tripping val serialiseToCBOR (deserialiseFromCBOR typeProxy)

tests :: TestTree
tests = fromGroup $$discover
