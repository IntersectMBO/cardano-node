{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api.CBOR
  ( tests
  ) where

import           Cardano.Api

import           Cardano.Prelude

import           Hedgehog (Property, discover)
import qualified Hedgehog as H

import           Test.Cardano.Api.Gen
import           Test.Cardano.Api.Orphans ()


prop_AddressByron_CBOR :: Property
prop_AddressByron_CBOR =
  H.property $ do
    addr <- H.forAll genByronVerificationKeyAddress
    H.tripping addr addressToCBOR addressFromCBOR

prop_AddressShelley_CBOR :: Property
prop_AddressShelley_CBOR =
  H.property $ do
    addr <- H.forAll genShelleyVerificationKeyAddress
    H.tripping addr addressToCBOR addressFromCBOR

prop_KeyPair_CBOR :: Property
prop_KeyPair_CBOR =
  H.property $ do
    kp <- H.forAll genKeyPair
    H.tripping kp keyPairToCBOR keyPairFromCBOR

prop_VerificationKey_CBOR :: Property
prop_VerificationKey_CBOR =
  H.property $ do
    kp <- H.forAll genVerificationKey
    H.tripping kp verificationKeyToCBOR verificationKeyFromCBOR

prop_TxSigned_CBOR :: Property
prop_TxSigned_CBOR =
  H.property $ do
    txs <- H.forAll genTxSignedByron
    H.tripping txs txSignedToCBOR txSignedFromCBOR

prop_TxUnsigned_CBOR :: Property
prop_TxUnsigned_CBOR =
  H.property $ do
    txu <- H.forAll genTxUnsigned
    H.tripping txu txUnsignedToCBOR txUnsignedFromCBOR

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
