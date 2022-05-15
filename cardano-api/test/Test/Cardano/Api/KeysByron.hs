{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-deprecations #-} -- TODO Fix deprecations

module Test.Cardano.Api.KeysByron
  ( tests
  ) where

import           Cardano.Prelude

import           Hedgehog (Property)
import           Test.Cardano.Api.Typed.Orphans ()
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Cardano.Api
import qualified Gen.Cardano.Crypto.Seed as Gen
import           Gen.Hedgehog.Roundtrip.CBOR (roundtrip_CBOR)

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_byron_key_CBOR :: Property
prop_roundtrip_byron_key_CBOR =
  roundtrip_CBOR (AsSigningKey AsByronKey) (deterministicSigningKey AsByronKey <$> Gen.genSeedForKey AsByronKey)

tests :: TestTree
tests = $testGroupGenerator
