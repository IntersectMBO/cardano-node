{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.Ledger
  ( tests
  ) where

import           Cardano.Prelude

import           Data.Aeson (encode)

import qualified Data.ByteString.Lazy.Char8 as LBS

import           Shelley.Spec.Ledger.Address (deserialiseAddr, serialiseAddr)

import           Hedgehog (Property, discover)
import qualified Hedgehog

import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Test.Shelley.Spec.Ledger.Serialisation.Generators.Genesis (genAddress)

import           Test.Cardano.Api.Examples
import           Test.Cardano.Prelude


prop_golden_ShelleyGenesis :: Property
prop_golden_ShelleyGenesis = goldenTestJSONPretty exampleShelleyGenesis "test/Golden/ShelleyGenesis"

-- Keep this here to make sure serialiseAddr/deserialiseAddr are working.
-- They are defined in the Shelley executable spec and have been wrong at
-- least once.
prop_roundtrip_Address_CBOR :: Property
prop_roundtrip_Address_CBOR =
  -- If this fails, FundPair and ShelleyGenesis can also fail.
  Hedgehog.property $ do
    addr <- Hedgehog.forAll (genAddress @TPraosStandardCrypto)
    Hedgehog.tripping addr serialiseAddr deserialiseAddr

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests = do
  -- Set to True when the struct or the JSON encoding changes and you need the new
  -- version.
  if False
    then LBS.writeFile "test/Golden/ShelleyGenesis" (encode exampleShelleyGenesis)
    else pure ()
  Hedgehog.checkParallel $$discover
