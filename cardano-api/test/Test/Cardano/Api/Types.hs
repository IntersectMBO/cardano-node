{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api.Types
  ( tests
  ) where

import           Cardano.Prelude

import           Data.Aeson (encode, fromJSON, decode, toJSON)

import qualified Data.ByteString.Lazy.Char8 as LBS

import           Cardano.Api.Shelley.KES (decodeKESVerificationKey, encodeKESVerificationKey)
import           Cardano.Api.Shelley.VRF (decodeVRFVerificationKey, encodeVRFVerificationKey)
import           Shelley.Spec.Ledger.Address (serialiseAddr, deserialiseAddr)

import           Hedgehog (Property, discover)
import qualified Hedgehog

import           Test.Cardano.Api.ConfigGen
import           Test.Cardano.Api.Examples
import           Test.Cardano.Prelude


prop_golden_ShelleyGenesis :: Property
prop_golden_ShelleyGenesis = goldenTestJSON exampleShelleyGenesis "test/Test/Cardano/Api/Golden/ShelleyGenesis"

-- Keep this here to make sure serialiseAddr/deserialiseAddr are working.
-- They are defined in the Shelley executable spec and have been wrong at
-- least once.
prop_roundtrip_Address_CBOR :: Property
prop_roundtrip_Address_CBOR =
  -- If this fails, FundPair and ShelleyGenesis can also fail.
  Hedgehog.property $ do
    addr <- Hedgehog.forAll genAddress
    Hedgehog.tripping addr serialiseAddr deserialiseAddr

prop_roundtrip_Address_JSON :: Property
prop_roundtrip_Address_JSON =
  -- If this fails, FundPair and ShelleyGenesis can also fail.
  Hedgehog.property $ do
    addr <- Hedgehog.forAll genAddress
    Hedgehog.tripping addr toJSON fromJSON
    Hedgehog.tripping addr encode decode

prop_roundtrip_GenesisDelegationPair_JSON :: Property
prop_roundtrip_GenesisDelegationPair_JSON =
  -- If this fails, ShelleyGenesis can also fail.
  Hedgehog.property $ do
    dp <- Hedgehog.forAll genGenesisDelegationPair
    Hedgehog.tripping dp toJSON fromJSON
    Hedgehog.tripping dp encode decode

prop_roundtrip_FundPair_JSON :: Property
prop_roundtrip_FundPair_JSON =
  -- If this fails, ShelleyGenesis can also fail.
  Hedgehog.property $ do
    fp <- Hedgehog.forAll genGenesisFundPair
    Hedgehog.tripping fp toJSON fromJSON
    Hedgehog.tripping fp encode decode

prop_roundtrip_ShelleyGenesis_JSON :: Property
prop_roundtrip_ShelleyGenesis_JSON =
  Hedgehog.property $ do
    sg <- Hedgehog.forAll genShelleyGenesis
    Hedgehog.tripping sg toJSON fromJSON
    Hedgehog.tripping sg encode decode

prop_roundtrip_VerKeyVRF_SimpleVRF_CBOR :: Property
prop_roundtrip_VerKeyVRF_SimpleVRF_CBOR =
  Hedgehog.withTests 50 . Hedgehog.property $ do
    vKeyVRF <- snd <$> Hedgehog.forAll genVRFKeyPair
    Hedgehog.tripping vKeyVRF encodeVRFVerificationKey decodeVRFVerificationKey

prop_roundtrip_VKeyES_TPraosStandardCrypto_CBOR :: Property
prop_roundtrip_VKeyES_TPraosStandardCrypto_CBOR =
  Hedgehog.withTests 20 . Hedgehog.property $ do
    vKeyES <- fst <$> Hedgehog.forAll genKESKeyPair
    Hedgehog.tripping vKeyES encodeKESVerificationKey decodeKESVerificationKey

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests = do
  -- Set to True when the struct or the JSON encoding changes and you need the new
  -- version.
  if False
    then LBS.writeFile "test/Golden/ShelleyGenesis" (encode exampleShelleyGenesis)
    else pure ()
  Hedgehog.checkParallel $$discover
