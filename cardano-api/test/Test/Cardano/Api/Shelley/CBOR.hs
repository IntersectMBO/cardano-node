{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Shelley.CBOR
  ( tests
  ) where


import           Cardano.Prelude

import           Cardano.Api

import           Hedgehog (Property, discover)
import qualified Hedgehog as H

import           Test.Cardano.Api.Gen
import           Test.Cardano.Api.Orphans ()

prop_ShelleyAddress_CBOR :: Property
prop_ShelleyAddress_CBOR =
  H.property $ do
    addr <- H.forAll genShelleyAddress
    H.tripping addr shelleyAddressToCBOR shelleyAddressFromCBOR


prop_ShelleyKeyPair_CBOR :: Property
prop_ShelleyKeyPair_CBOR =
  H.property $ do
    kp <- H.forAll genKeyPairShelley
    H.tripping kp shelleyKeyPairToCBOR shelleyKeyPairFromCBOR

prop_ShelleyPublicKey_CBOR :: Property
prop_ShelleyPublicKey_CBOR =
  H.property $ do
    kp <- H.forAll genPublicKeyShelley
    H.tripping kp renderShelleyPublicKeyToCBOR shelleyPublicKeyFromCBOR

prop_ShelleyGenesisVerificationKey_CBOR :: Property
prop_ShelleyGenesisVerificationKey_CBOR =
  H.property $ do
    svk <- H.forAll genGenesisShelleyVerificationKey
    H.tripping svk shelleyGenesisVerificationKeyToCBOR shelleyGenesisVerificationKeyFromCBOR

prop_ShelleyVerificationKey_CBOR :: Property
prop_ShelleyVerificationKey_CBOR =
  H.property $ do
    svk <- H.forAll genRegularShelleyVerificationKey
    H.tripping svk shelleyVerificationKeyToCBOR shelleyVerificationKeyFromCBOR

tests :: IO Bool
tests =
  H.checkParallel $$discover