{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.Ledger
  ( tests
  ) where

import           Cardano.Prelude

import           Shelley.Spec.Ledger.Address (deserialiseAddr, serialiseAddr)

import           Hedgehog (Property, discover)
import qualified Hedgehog
import           Test.Tasty (TestTree)

import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)

import           Test.Shelley.Spec.Ledger.Serialisation.Generators.Genesis (genAddress)

import           Test.Cardano.Api.Genesis
import           Test.Cardano.Prelude
import           Test.Tasty.Hedgehog.Group (fromGroup)


prop_golden_ShelleyGenesis :: Property
prop_golden_ShelleyGenesis = goldenTestJSONPretty exampleShelleyGenesis "test/Golden/ShelleyGenesis"

-- Keep this here to make sure serialiseAddr/deserialiseAddr are working.
-- They are defined in the Shelley executable spec and have been wrong at
-- least once.
prop_roundtrip_Address_CBOR :: Property
prop_roundtrip_Address_CBOR =
  -- If this fails, FundPair and ShelleyGenesis can also fail.
  Hedgehog.property $ do
    addr <- Hedgehog.forAll (genAddress @StandardCrypto)
    Hedgehog.tripping addr serialiseAddr deserialiseAddr

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = fromGroup $$discover
