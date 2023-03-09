{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.Api.Typed.Address
  ( tests
  ) where

import           Cardano.Api

import           Hedgehog (Property)
import           Test.Cardano.Api.Typed.Orphans ()
import           Test.Gen.Cardano.Api.Typed (genAddressByron, genAddressShelley)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testPropertyNamed)

import qualified Data.Aeson as Aeson
import qualified Hedgehog as H

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

prop_roundtrip_byron_address_JSON :: Property
prop_roundtrip_byron_address_JSON =
  H.property $ do
    mss <- H.forAll genAddressByron
    H.tripping mss Aeson.encode Aeson.eitherDecode

prop_roundtrip_shelley_address_JSON :: Property
prop_roundtrip_shelley_address_JSON =
  H.property $ do
    mss <- H.forAll genAddressShelley
    H.tripping mss Aeson.encode Aeson.eitherDecode

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Test.Cardano.Api.Typed.Address"
  [ testPropertyNamed "roundtrip shelley address" "roundtrip shelley address" prop_roundtrip_shelley_address
  , testPropertyNamed "roundtrip byron address"   "roundtrip byron address" prop_roundtrip_byron_address
  , testPropertyNamed "roundtrip byron address JSON"   "roundtrip byron address JSON" prop_roundtrip_byron_address_JSON
  , testPropertyNamed "roundtrip shelley address JSON"   "roundtrip shelley address JSON" prop_roundtrip_shelley_address_JSON
  ]
