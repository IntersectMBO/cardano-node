{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.CBOR
  ( tests
  ) where

import           Cardano.Api

import           Cardano.Prelude

import           Hedgehog (Property, discover)
import qualified Hedgehog as H

import           Cardano.Api.Shelley.KES (decodeKESVerificationKey, encodeKESVerificationKey) -- to remove
import           Cardano.Api.Shelley.VRF (decodeVRFVerificationKey, encodeVRFVerificationKey) -- to remove

import           Test.Cardano.Api.Gen
import           Test.Cardano.Api.Orphans ()


prop_AddressByron_CBOR :: Property
prop_AddressByron_CBOR =
  H.property $ do
    addr <- H.forAll genVerificationKeyAddressByron
    H.tripping addr addressToCBOR addressFromCBOR

prop_AddressShelley_CBOR :: Property
prop_AddressShelley_CBOR =
  H.property $ do
    addr <- H.forAll genVerificationKeyAddressShelley
    H.tripping addr addressToCBOR addressFromCBOR

prop_Certificate_CBOR :: Property
prop_Certificate_CBOR =
  H.property $ do
    addr <- H.forAll genCertificate
    H.tripping addr certificateToCBOR certificateFromCBOR

prop_GenesisVerificationKey_CBOR :: Property
prop_GenesisVerificationKey_CBOR =
  H.property $ do
    kp <- H.forAll genGenesisVerificationKey
    H.tripping kp genesisVerificationKeyToCBOR genesisVerificationKeyFromCBOR

prop_SigningKey_CBOR :: Property
prop_SigningKey_CBOR =
  H.property $ do
    kp <- H.forAll genSigningKey
    H.tripping kp signingKeyToCBOR signingKeyFromCBOR

prop_Update_CBOR :: Property
prop_Update_CBOR =
  H.property $ do
    kp <- H.forAll genUpdate
    H.tripping kp updateToCBOR updateFromCBOR


prop_PaymentVerificationKey_CBOR :: Property
prop_PaymentVerificationKey_CBOR =
  H.property $ do
    kp <- H.forAll genPaymentVerificationKey
    H.tripping kp paymentVerificationKeyToCBOR paymentVerificationKeyFromCBOR

prop_StakingVerificationKey_CBOR :: Property
prop_StakingVerificationKey_CBOR =
  H.property $ do
    kp <- H.forAll genStakingVerificationKey
    H.tripping kp stakingVerificationKeyToCBOR stakingVerificationKeyFromCBOR

prop_VerificationKeyStakePool_CBOR :: Property
prop_VerificationKeyStakePool_CBOR =
  H.property $ do
    kp <- H.forAll genVerificationKeyShelleyStakePool
    H.tripping kp verificationKeyStakePoolToCBOR verificationKeyStakePoolFromCBOR

prop_VerificationKeyStaking_CBOR :: Property
prop_VerificationKeyStaking_CBOR =
  H.property $ do
    kp <- H.forAll genVerificationKeyShelleyStaking
    H.tripping kp shelleyVerificationKeyStakingToCBOR shelleyVerificationKeyStakingFromCBOR

prop_VerificationKeyVRF_CBOR :: Property
prop_VerificationKeyVRF_CBOR =
  H.property $ do
    (_, vKey) <- H.forAll genVRFKeyPair
    H.tripping vKey verificationKeyVRFToCBOR verificationKeyVRFFromCBOR

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


-- To Remove

prop_roundtrip_VKeyES_TPraosStandardCrypto_CBOR :: Property
prop_roundtrip_VKeyES_TPraosStandardCrypto_CBOR =
  H.withTests 20 . H.property $ do
    vKeyES <- fst <$> H.forAll genKESKeyPair
    H.tripping vKeyES encodeKESVerificationKey decodeKESVerificationKey

prop_roundtrip_VerKeyVRF_SimpleVRF_CBOR :: Property
prop_roundtrip_VerKeyVRF_SimpleVRF_CBOR =
  H.withTests 50 . H.property $ do
    vKeyVRF <- snd <$> H.forAll genVRFKeyPair
    H.tripping vKeyVRF encodeVRFVerificationKey decodeVRFVerificationKey


-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
