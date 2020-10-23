{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.CBOR
  ( tests
  ) where

import           Cardano.Api.Typed
import           Cardano.Prelude
import           Hedgehog (Gen, Property, discover)
import           Test.Cardano.Api.Typed.Gen
import           Test.Cardano.Api.Typed.Orphans ()
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog.Group (fromGroup)

import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}

-- TODO: Need to add PaymentExtendedKey roundtrip tests however
-- we can't derive an Eq instance for Crypto.HD.XPrv

prop_roundtrip_txbody_byron_CBOR :: Property
prop_roundtrip_txbody_byron_CBOR =
  roundtrip_CBOR AsByronTxBody genTxBodyByron

prop_roundtrip_txbody_shelley_CBOR :: Property
prop_roundtrip_txbody_shelley_CBOR =
  roundtrip_CBOR AsShelleyTxBody genTxBodyShelley

prop_roundtrip_tx_byron_CBOR :: Property
prop_roundtrip_tx_byron_CBOR =
  roundtrip_CBOR AsByronTx genTxByron

prop_roundtrip_tx_shelley_CBOR :: Property
prop_roundtrip_tx_shelley_CBOR =
  roundtrip_CBOR AsShelleyTx genTxShelley

prop_roundtrip_witness_shelley_CBOR :: Property
prop_roundtrip_witness_shelley_CBOR =
  roundtrip_CBOR AsShelleyWitness genShelleyWitness

prop_roundtrip_witness_byron_CBOR :: Property
prop_roundtrip_witness_byron_CBOR =
  roundtrip_CBOR AsByronWitness genByronKeyWitness

prop_roundtrip_operational_certificate_CBOR :: Property
prop_roundtrip_operational_certificate_CBOR =
  roundtrip_CBOR AsOperationalCertificate genOperationalCertificate

prop_roundtrip_operational_certificate_issue_counter_CBOR :: Property
prop_roundtrip_operational_certificate_issue_counter_CBOR =
  roundtrip_CBOR AsOperationalCertificateIssueCounter genOperationalCertificateIssueCounter

prop_roundtrip_verification_key_byron_CBOR :: Property
prop_roundtrip_verification_key_byron_CBOR =
  roundtrip_CBOR (AsVerificationKey AsByronKey) (genVerificationKey AsByronKey)

prop_roundtrip_signing_key_byron_CBOR :: Property
prop_roundtrip_signing_key_byron_CBOR =
  roundtrip_CBOR (AsSigningKey AsByronKey) (genSigningKey AsByronKey)

prop_roundtrip_verification_key_payment_CBOR :: Property
prop_roundtrip_verification_key_payment_CBOR =
  roundtrip_CBOR (AsVerificationKey AsPaymentKey) (genVerificationKey AsPaymentKey)

prop_roundtrip_signing_key_payment_CBOR :: Property
prop_roundtrip_signing_key_payment_CBOR =
  roundtrip_CBOR (AsSigningKey AsPaymentKey) (genSigningKey AsPaymentKey)

prop_roundtrip_verification_key_stake_CBOR :: Property
prop_roundtrip_verification_key_stake_CBOR =
  roundtrip_CBOR (AsVerificationKey AsStakeKey) (genVerificationKey AsStakeKey)

prop_roundtrip_signing_key_stake_CBOR :: Property
prop_roundtrip_signing_key_stake_CBOR =
  roundtrip_CBOR (AsSigningKey AsStakeKey) (genSigningKey AsStakeKey)

prop_roundtrip_verification_key_genesis_CBOR :: Property
prop_roundtrip_verification_key_genesis_CBOR =
  roundtrip_CBOR (AsVerificationKey AsGenesisKey) (genVerificationKey AsGenesisKey)

prop_roundtrip_signing_key_genesis_CBOR :: Property
prop_roundtrip_signing_key_genesis_CBOR =
  roundtrip_CBOR (AsSigningKey AsGenesisKey) (genSigningKey AsGenesisKey)

prop_roundtrip_verification_key_genesis_delegate_CBOR :: Property
prop_roundtrip_verification_key_genesis_delegate_CBOR =
  roundtrip_CBOR (AsVerificationKey AsGenesisDelegateKey) (genVerificationKey AsGenesisDelegateKey)

prop_roundtrip_signing_key_genesis_delegate_CBOR :: Property
prop_roundtrip_signing_key_genesis_delegate_CBOR =
  roundtrip_CBOR (AsSigningKey AsGenesisDelegateKey) (genSigningKey AsGenesisDelegateKey)

prop_roundtrip_verification_key_stake_pool_CBOR :: Property
prop_roundtrip_verification_key_stake_pool_CBOR =
  roundtrip_CBOR (AsVerificationKey AsStakePoolKey) (genVerificationKey AsStakePoolKey)

prop_roundtrip_signing_key_stake_pool_CBOR :: Property
prop_roundtrip_signing_key_stake_pool_CBOR =
  roundtrip_CBOR (AsSigningKey AsStakePoolKey) (genSigningKey AsStakePoolKey)

prop_roundtrip_verification_key_vrf_CBOR :: Property
prop_roundtrip_verification_key_vrf_CBOR =
  roundtrip_CBOR (AsVerificationKey AsVrfKey) (genVerificationKey AsVrfKey)

prop_roundtrip_signing_key_vrf_CBOR :: Property
prop_roundtrip_signing_key_vrf_CBOR =
  roundtrip_CBOR (AsSigningKey AsVrfKey) (genSigningKey AsVrfKey)

prop_roundtrip_verification_key_kes_CBOR :: Property
prop_roundtrip_verification_key_kes_CBOR =
  roundtrip_CBOR (AsVerificationKey AsKesKey) (genVerificationKey AsKesKey)

prop_roundtrip_signing_key_kes_CBOR :: Property
prop_roundtrip_signing_key_kes_CBOR =
  roundtrip_CBOR (AsSigningKey AsKesKey) (genSigningKey AsKesKey)

prop_roundtrip_script_CBOR :: Property
prop_roundtrip_script_CBOR =
  roundtrip_CBOR AsScript genScript

-- -----------------------------------------------------------------------------

roundtrip_CBOR
  :: (SerialiseAsCBOR a, Eq a, Show a)
  => AsType a -> Gen a -> Property
roundtrip_CBOR typeProxy gen =
  H.property $ do
    val <- H.forAll gen
    H.tripping val serialiseToCBOR (deserialiseFromCBOR typeProxy)



-- -----------------------------------------------------------------------------

tests :: TestTree
tests = fromGroup $$discover
