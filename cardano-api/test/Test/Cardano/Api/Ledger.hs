{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.Ledger
  ( tests
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Control.Monad.Identity

import           Cardano.Ledger.Address (deserialiseAddr, serialiseAddr)
import qualified Cardano.Ledger.Api as L
import           Cardano.Ledger.Crypto
import           Cardano.Ledger.SafeHash

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Aeson as H
import           Hedgehog.Internal.Property
import           Test.Cardano.Api.Genesis (exampleShelleyGenesis)
import           Test.Cardano.Ledger.Core.Arbitrary ()
import           Test.Gen.Cardano.Api.Typed
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testPropertyNamed)
import           Hedgehog.Gen.QuickCheck (arbitrary)

prop_golden_ShelleyGenesis :: Property
prop_golden_ShelleyGenesis = H.goldenTestJsonValuePretty exampleShelleyGenesis "test/Golden/ShelleyGenesis"

-- Keep this here to make sure serialiseAddr/deserialiseAddr are working.
-- They are defined in the Shelley executable spec and have been wrong at
-- least once.
prop_roundtrip_Address_CBOR :: Property
prop_roundtrip_Address_CBOR = H.property $ do
  -- If this fails, FundPair and ShelleyGenesis can also fail.
  addr <- H.forAll (arbitrary @(L.Addr StandardCrypto))
  H.tripping addr serialiseAddr deserialiseAddr

-- prop_original_scriptdata_bytes_preserved and prop_roundtrip_scriptdata_plutusdata
-- allow us to generate a 'HashableScriptData' value from JSON with the original bytes being
-- derived from a JSON 'Value'. We serialize the 'ScriptData' (derived from the 'Value')
-- to CBOR and take those as the original bytes. Under the hood ScriptData is converted to PlutusData
-- before serializing.

prop_original_scriptdata_bytes_preserved :: Property
prop_original_scriptdata_bytes_preserved = H.property $ do
  schema <- forAll genScriptDataSchema
  sDataValue <- scriptDataToJson schema <$> forAll genHashableScriptData
  case scriptDataJsonToHashable schema sDataValue of
    Left e -> failWith Nothing $ show e
    Right hScriptData -> do
      let ScriptDataHash apiHash = hashScriptDataBytes hScriptData
          ledgerAlonzoData = toAlonzoData hScriptData :: L.Data L.Alonzo
      -- We check that our hashScriptDataBytes is equivalent to `L.hashData`
      -- This test will let us know if our 'hashScriptDataBytes' is ever broken
      L.hashData ledgerAlonzoData === apiHash

      -- We also check that the original bytes are the same after the calling
      -- toAlonzoData :: HashableScriptData -> L.Data ledgerera.
      originalBytes ledgerAlonzoData === getOriginalScriptDataBytes hScriptData

prop_roundtrip_scriptdata_plutusdata :: Property
prop_roundtrip_scriptdata_plutusdata = H.property $ do
  sd <- getScriptData <$> forAll genHashableScriptData
  H.tripping sd toPlutusData (Identity . fromPlutusData)

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Test.Cardano.Api.Ledger"
  [ testPropertyNamed "golden ShelleyGenesis"  "golden ShelleyGenesis"  prop_golden_ShelleyGenesis
  , testPropertyNamed "roundtrip Address CBOR" "roundtrip Address CBOR" prop_roundtrip_Address_CBOR
  , testPropertyNamed "roundtrip ScriptData" "roundtrip ScriptData" prop_roundtrip_scriptdata_plutusdata
  , testPropertyNamed "script data bytes preserved" "script data bytes preserved" prop_original_scriptdata_bytes_preserved
  ]
