{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.Ledger
  ( tests
  ) where

import           Cardano.Ledger.Address (deserialiseAddr, serialiseAddr)
import           Cardano.Prelude (($))
import           Hedgehog (Property)
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import           Test.Cardano.Api.Genesis (exampleShelleyGenesis)
import           Test.Cardano.Ledger.Shelley.Serialisation.Generators.Genesis (genAddress)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testPropertyNamed)

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Aeson as H

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
tests = testGroup "Test.Cardano.Api.Ledger"
  [ testPropertyNamed "golden ShelleyGenesis"  "golden ShelleyGenesis"  prop_golden_ShelleyGenesis
  , testPropertyNamed "roundtrip Address CBOR" "roundtrip Address CBOR" prop_roundtrip_Address_CBOR
  ]
