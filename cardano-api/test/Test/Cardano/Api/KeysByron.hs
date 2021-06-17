{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.KeysByron
  ( tests
  ) where

import           Cardano.Api
import           Cardano.Prelude
import           Gen.Hedgehog.Roundtrip.CBOR (roundtrip_CBOR)
import           Gen.Tasty.Hedgehog.Group (fromGroup)
import           Hedgehog (Property, discover)
import           Test.Cardano.Api.Typed.Orphans ()
import           Test.Tasty (TestTree)

import qualified Gen.Cardano.Crypto.Seed as Gen

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_byron_key_CBOR :: Property
prop_roundtrip_byron_key_CBOR =
  roundtrip_CBOR (AsSigningKey AsByronKey) (deterministicSigningKey AsByronKey <$> Gen.genSeedForKey AsByronKey)

tests :: TestTree
tests = fromGroup $$discover
