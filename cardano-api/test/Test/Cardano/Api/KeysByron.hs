{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.Api.KeysByron
  ( tests
  ) where

import           Cardano.Api (AsType (AsByronKey, AsSigningKey), Key (deterministicSigningKey))

import           Hedgehog (Property)
import           Test.Cardano.Api.Typed.Orphans ()
import           Test.Hedgehog.Roundtrip.CBOR (trippingCbor)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testPropertyNamed)

import qualified Hedgehog as H
import qualified Test.Gen.Cardano.Crypto.Seed as Gen

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_byron_key_CBOR :: Property
prop_roundtrip_byron_key_CBOR = H.property $ do
  seed <- H.forAll $ deterministicSigningKey AsByronKey <$> Gen.genSeedForKey AsByronKey
  trippingCbor (AsSigningKey AsByronKey) seed

tests :: TestTree
tests = testGroup "Test.Cardano.Api.KeysByron"
  [ testPropertyNamed "roundtrip byron key CBOR" "roundtrip byron key CBOR" prop_roundtrip_byron_key_CBOR
  ]
