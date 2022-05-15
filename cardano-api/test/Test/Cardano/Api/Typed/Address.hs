{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-deprecations #-} -- TODO Fix deprecations

module Test.Cardano.Api.Typed.Address
  ( tests
  ) where

import           Cardano.Prelude

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Cardano.Api
import           Gen.Cardano.Api.Typed
import           Test.Cardano.Api.Typed.Orphans ()

{- HLINT ignore "Use camelCase" -}

-- Address CBOR round trips

prop_roundtrip_shelley_address :: Property
prop_roundtrip_shelley_address =
  roundtrip_serialise_address AsShelleyAddress genAddressShelley


prop_roundtrip_byron_address :: Property
prop_roundtrip_byron_address =
  roundtrip_serialise_address AsByronAddress genAddressByron


-- -----------------------------------------------------------------------------

roundtrip_serialise_address
  :: ( SerialiseAddress a
     , Eq a
     , Show a) => AsType a -> H.Gen a -> Property
roundtrip_serialise_address asType g =
  H.property $ do
    v <- H.forAll g
    H.tripping v serialiseAddress (deserialiseAddress asType)


-- -----------------------------------------------------------------------------

tests :: TestTree
tests = $testGroupGenerator
