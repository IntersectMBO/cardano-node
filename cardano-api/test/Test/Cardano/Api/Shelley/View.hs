{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api.Shelley.View
  ( tests
  ) where

import           Cardano.Prelude

import           Cardano.Api

import           Hedgehog (Property, discover)
import qualified Hedgehog as H

import           Test.Cardano.Api.Gen
import           Test.Cardano.Api.Orphans ()


prop_roundtrip_ShelleyAddress_view :: Property
prop_roundtrip_ShelleyAddress_view =
  H.property $ do
    addr <- H.forAll genShelleyAddress
    H.tripping addr renderShelleyAddressView parseShelleyAddressView

prop_roundtrip_ShelleyKeyPair_view :: Property
prop_roundtrip_ShelleyKeyPair_view =
  H.property $ do
    kp <- H.forAll genKeyPairShelley
    H.tripping kp renderShelleyKeyPairView parseShelleyKeyPairView


prop_roundtrip_ShelleyPublicKey_view :: Property
prop_roundtrip_ShelleyPublicKey_view =
  H.property $ do
    pk <- H.forAll genPublicKeyShelley
    H.tripping pk renderShelleyPublicKeyView parseShelleyPublicKeyView

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
