{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Config.Types
  ( tests
  ) where

import           Cardano.Prelude

import           Data.Aeson (encode)

import qualified Data.ByteString.Lazy.Char8 as LBS

import           Cardano.Config.Shelley.KES (decodeKESVerificationKey, encodeKESVerificationKey)
import           Cardano.Config.Shelley.VRF (decodeVRFVerificationKey, encodeVRFVerificationKey)
import           Shelley.Spec.Ledger.Address (serialiseAddr, deserialiseAddr)

import           Hedgehog (Property, discover)
import qualified Hedgehog

import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import qualified Test.Shelley.Spec.Ledger.Genesis.Properties as Ledger
import           Test.Shelley.Spec.Ledger.Generator.Genesis

import           Test.Cardano.Config.Examples
import           Test.Cardano.Config.Gen
import           Test.Cardano.Prelude


prop_golden_ShelleyGenesis :: Property
prop_golden_ShelleyGenesis = goldenTestJSON exampleShelleyGenesis "test/Golden/ShelleyGenesis"

-- Keep this here to make sure serialiseAddr/deserialiseAddr are working.
-- They are defined in the Shelley executable spec and have been wrong at
-- least once.
prop_roundtrip_Address_CBOR :: Property
prop_roundtrip_Address_CBOR =
  -- If this fails, FundPair and ShelleyGenesis can also fail.
  Hedgehog.property $ do
    addr <- Hedgehog.forAll (genAddress @TPraosStandardCrypto)
    Hedgehog.tripping addr serialiseAddr deserialiseAddr

-- If this fails, FundPair and ShelleyGenesis can also fail.
prop_roundtrip_Address_JSON :: Property
prop_roundtrip_Address_JSON =
  Ledger.prop_roundtrip_Address_JSON @TPraosStandardCrypto Proxy

prop_roundtrip_GenesisDelegationPair_JSON :: Property
prop_roundtrip_GenesisDelegationPair_JSON =
  Ledger.prop_roundtrip_GenesisDelegationPair_JSON @TPraosStandardCrypto Proxy

prop_roundtrip_FundPair_JSON :: Property
prop_roundtrip_FundPair_JSON =
  Ledger.prop_roundtrip_FundPair_JSON @TPraosStandardCrypto Proxy

prop_roundtrip_ShelleyGenesis_JSON :: Property
prop_roundtrip_ShelleyGenesis_JSON =
  Ledger.prop_roundtrip_ShelleyGenesis_JSON @TPraosStandardCrypto Proxy

prop_roundtrip_VerKeyVRF_SimpleVRF_CBOR :: Property
prop_roundtrip_VerKeyVRF_SimpleVRF_CBOR =
  Hedgehog.withTests 50 . Hedgehog.property $ do
    vKeyVRF <- snd <$> Hedgehog.forAll (genVRFKeyPair @TPraosStandardCrypto)
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
