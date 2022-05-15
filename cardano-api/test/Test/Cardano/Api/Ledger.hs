{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-deprecations #-} -- TODO Fix deprecations

module Test.Cardano.Api.Ledger
  ( tests
  ) where

import           Cardano.Prelude

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Aeson as H
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Cardano.Ledger.Address (deserialiseAddr, serialiseAddr)
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import           Test.Cardano.Api.Genesis
import           Test.Cardano.Ledger.Shelley.Serialisation.Generators.Genesis (genAddress)

prop_golden_ShelleyGenesis :: Property
prop_golden_ShelleyGenesis = H.goldenTestJsonValuePretty exampleShelleyGenesis "test/Golden/ShelleyGenesis"

-- Keep this here to make sure serialiseAddr/deserialiseAddr are working.
-- They are defined in the Shelley executable spec and have been wrong at
-- least once.
prop_roundtrip_Address_CBOR :: Property
prop_roundtrip_Address_CBOR = H.property $ do
  -- If this fails, FundPair and ShelleyGenesis can also fail.
  addr <- H.forAll (genAddress @StandardCrypto)
  H.tripping addr serialiseAddr deserialiseAddr

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = $testGroupGenerator
