{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.Api.KeysByron
  ( tests
  ) where

import           Cardano.Api (AsType(AsByronKey, AsSigningKey), Key(deterministicSigningKey))
import           Cardano.Prelude ((<$>))
import           Gen.Hedgehog.Roundtrip.CBOR (roundtrip_CBOR)
import           Hedgehog (Property)
import           Test.Cardano.Api.Typed.Orphans ()
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testPropertyNamed)

import qualified Gen.Cardano.Crypto.Seed as Gen

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_byron_key_CBOR :: Property
prop_roundtrip_byron_key_CBOR =
  roundtrip_CBOR (AsSigningKey AsByronKey) (deterministicSigningKey AsByronKey <$> Gen.genSeedForKey AsByronKey)

tests :: TestTree
tests = testGroup "Test.Cardano.Api.KeysByron"
  [ testPropertyNamed "roundtrip byron key CBOR" "roundtrip byron key CBOR" prop_roundtrip_byron_key_CBOR
  ]
