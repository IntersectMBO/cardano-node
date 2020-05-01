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


prop_Address_CBOR :: Property
prop_Address_CBOR =
  H.property $ do
    nw <- H.forAll genNetwork
    bpk <- H.forAll genPublicKeyByron
    let addr = byronPubKeyAddress bpk nw
    H.tripping addr addressToCBOR addressFromCBOR

prop_KeyPair_CBOR :: Property
prop_KeyPair_CBOR =
  H.property $ do
    kp <- H.forAll genKeyPair
    H.tripping kp keyPairToCBOR keyPairFromCBOR

prop_PublicKey_CBOR :: Property
prop_PublicKey_CBOR =
  H.property $ do
    kp <- H.forAll genPublicKey
    H.tripping kp publicKeyToCBOR publicKeyFromCBOR

prop_ShelleyVerificationKey_CBOR :: Property
prop_ShelleyVerificationKey_CBOR =
  H.property $ do
    svk <- H.forAll genShelleyVerificationKey
    H.tripping svk shelleyVerificationKeyToCBOR shelleyVerificationKeyFromCBOR

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
