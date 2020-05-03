{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api.View
  ( tests
  ) where

import           Cardano.Api

import           Cardano.Prelude

import           Hedgehog (Property, discover)
import qualified Hedgehog as H

import           Test.Cardano.Api.Gen
import           Test.Cardano.Api.Orphans ()


prop_roundtrip_AddressByron_view :: Property
prop_roundtrip_AddressByron_view =
  H.property $ do
    addr <- H.forAll genByronVerificationKeyAddress
    H.tripping addr renderAddressView parseAddressView

prop_roundtrip_AddressShelley_view :: Property
prop_roundtrip_AddressShelley_view =
  H.property $ do
    addr <- H.forAll genShelleyVerificationKeyAddress
    H.tripping addr renderAddressView parseAddressView

prop_roundtrip_KeyPair_view :: Property
prop_roundtrip_KeyPair_view =
  H.property $ do
    kp <- H.forAll genKeyPair
    H.tripping kp renderKeyPairView parseKeyPairView

prop_roundtrip_VerificationKey_view :: Property
prop_roundtrip_VerificationKey_view =
  H.property $ do
    pk <- H.forAll genVerificationKey
    H.tripping pk renderVerificationKeyView parseVerificationKeyView

prop_roundtrip_TxSigned_view :: Property
prop_roundtrip_TxSigned_view =
  H.property $ do
    pk <- H.forAll genTxSigned
    H.tripping pk renderTxSignedView parseTxSignedView

prop_roundtrip_TxUnsigned_view :: Property
prop_roundtrip_TxUnsigned_view =
  H.property $ do
    pk <- H.forAll genTxUnsigned
    H.tripping pk renderTxUnsignedView parseTxUnsignedView

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
