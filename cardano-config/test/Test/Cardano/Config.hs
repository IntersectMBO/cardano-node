{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Config
  ( tests
  ) where

import           Cardano.Prelude
import           Test.Cardano.Prelude

import           Data.Aeson (encode, fromJSON, decode, toJSON)

import           Shelley.Spec.Ledger.Address (serialiseAddr, deserialiseAddr)

import           Hedgehog (Property, discover)
import qualified Hedgehog

import           Test.Cardano.Config.Examples
import           Test.Cardano.Config.Gen

prop_golden_ShelleyGenesis :: Property
prop_golden_ShelleyGenesis = goldenTestJSON exampleShelleyGenesis "test/Golden/ShelleyGenesis"

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

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
