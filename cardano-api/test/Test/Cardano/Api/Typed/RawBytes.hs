{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.RawBytes
  ( tests
  ) where

import           Cardano.Api.Typed

import           Cardano.Prelude

import           Hedgehog (Property, discover)
import qualified Hedgehog as H

import           Test.Cardano.Api.Typed.Gen
import           Test.Cardano.Api.Typed.Orphans ()

-- Address CBOR round trips

prop_roundtrip_shelley_address_raw :: Property
prop_roundtrip_shelley_address_raw = H.property $ do
  addr <- H.forAll genAddressShelley
  H.tripping addr serialiseToRawBytes (deserialiseFromRawBytes AsShelleyAddress)

prop_roundtrip_byron_address_raw :: Property
prop_roundtrip_byron_address_raw = H.property $ do
  addr <- H.forAll genAddressByron
  H.tripping addr serialiseToRawBytes (deserialiseFromRawBytes AsByronAddress)

{-
--TODO: Follow up
This property will fail due to:
 Shelley.deserialiseRewardAcnt not being available

prop_roundtrip_stake_address_raw :: Property
prop_roundtrip_stake_address_raw = H.property $ do
  addr <- H.forAll genStakeAddress
  H.tripping addr serialiseToRawBytes (deserialiseFromRawBytes AsStakeAddress)
-}


prop_roundtrip_verification_ByronKey_hash_raw :: Property
prop_roundtrip_verification_ByronKey_hash_raw =
  roundtrip_verification_key_hash_raw AsByronKey

prop_roundtrip_verification_PaymentKey_hash_raw :: Property
prop_roundtrip_verification_PaymentKey_hash_raw =
  roundtrip_verification_key_hash_raw AsPaymentKey

prop_roundtrip_verification_StakeKey_hash_raw :: Property
prop_roundtrip_verification_StakeKey_hash_raw =
  roundtrip_verification_key_hash_raw AsStakeKey

prop_roundtrip_verification_StakePoolKey_hash_raw :: Property
prop_roundtrip_verification_StakePoolKey_hash_raw =
  roundtrip_verification_key_hash_raw AsStakePoolKey

prop_roundtrip_verification_GenesisKey_hash_raw :: Property
prop_roundtrip_verification_GenesisKey_hash_raw =
  roundtrip_verification_key_hash_raw AsGenesisKey

prop_roundtrip_verification_GenesisDelegateKey_hash_raw :: Property
prop_roundtrip_verification_GenesisDelegateKey_hash_raw =
  roundtrip_verification_key_hash_raw AsGenesisDelegateKey

prop_roundtrip_verification_KesKey_hash_raw :: Property
prop_roundtrip_verification_KesKey_hash_raw =
  roundtrip_verification_key_hash_raw AsKesKey

prop_roundtrip_verification_VrfKey_hash_raw :: Property
prop_roundtrip_verification_VrfKey_hash_raw =
  roundtrip_verification_key_hash_raw AsVrfKey

prop_roundtrip_verification_GenesisUTxOKey_hash_raw :: Property
prop_roundtrip_verification_GenesisUTxOKey_hash_raw =
  roundtrip_verification_key_hash_raw AsGenesisUTxOKey

-- -----------------------------------------------------------------------------

roundtrip_verification_key_hash_raw
  :: (Key keyrole, Eq (Hash keyrole), Show (Hash keyrole))
  => AsType keyrole -> Property
roundtrip_verification_key_hash_raw roletoken =
  H.property $ do
    vKey <- H.forAll $ genVerificationKey roletoken
    let vKeyHash = verificationKeyHash vKey
    H.tripping vKeyHash serialiseToRawBytes (deserialiseFromRawBytes (AsHash roletoken))

-- -----------------------------------------------------------------------------


tests :: IO Bool
tests =
  H.checkParallel $$discover
