{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Config
  ( tests
  ) where

import           Cardano.Config.TextView
import           Cardano.Prelude

import           Data.Aeson (encode, fromJSON, decode, toJSON)
import qualified Data.ByteString.Char8 as BS

import           Shelley.Spec.Ledger.Address (serialiseAddr, deserialiseAddr)

import           Hedgehog (Property, discover)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

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

-- Test this first. If this fails, others are likely to fail.
prop_roundtrip_multiline_hex :: Property
prop_roundtrip_multiline_hex =
  Hedgehog.property $ do
    bs <- BS.pack <$> Hedgehog.forAll (Gen.string (Range.linear 0 500) (Gen.element ['\0' .. '\xff']))
    Hedgehog.tripping bs (BS.unlines . rawToMultilineHex) unRawToMultilineHex

prop_roundtrip_TextView :: Property
prop_roundtrip_TextView =
  Hedgehog.property $ do
    tv <- Hedgehog.forAll genTextView
    Hedgehog.tripping tv renderTextView parseTextView

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
