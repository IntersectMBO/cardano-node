{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api.Byron.CBOR
  ( tests
  ) where

import           Cardano.Prelude

import           Cardano.Api

import           Hedgehog (Property, discover)
import qualified Hedgehog as H

import           Test.Cardano.Api.Gen
import           Test.Cardano.Api.Orphans ()


prop_ByronAddress_CBOR :: Property
prop_ByronAddress_CBOR =
  H.property $ do
    addr <- H.forAll genByronAddress
    H.tripping addr byronAddressToCBOR byronAddressFromCBOR

prop_ByronKeyPair_CBOR :: Property
prop_ByronKeyPair_CBOR =
  H.property $ do
    kp <- H.forAll genKeyPairByron
    H.tripping kp byronKeyPairToCBOR byronKeyPairFromCBOR

prop_ByronPublicKey_CBOR :: Property
prop_ByronPublicKey_CBOR =
  H.property $ do
    kp <- H.forAll genPublicKeyByron
    H.tripping kp renderByronPublicKeyToCBOR byronPublicKeyFromCBOR

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
