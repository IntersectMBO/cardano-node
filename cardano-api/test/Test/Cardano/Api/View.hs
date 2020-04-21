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


prop_roundtrip_Address_view :: Property
prop_roundtrip_Address_view =
  H.property $ do
    addr <- byronPubKeyAddress <$> H.forAll genPublicKeyByron
    H.tripping addr renderAddressView parseAddressView

prop_roundtrip_KeyPair_view :: Property
prop_roundtrip_KeyPair_view =
  H.property $ do
    kp <- H.forAll genKeyPair
    H.tripping kp renderKeyPairView parseKeyPairView

prop_roundtrip_PublicKey_view :: Property
prop_roundtrip_PublicKey_view =
  H.property $ do
    pk <- H.forAll genPublicKey
    H.tripping pk renderPublicKeyView parsePublicKeyView

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
