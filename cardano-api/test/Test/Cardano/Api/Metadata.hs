{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-deprecations #-} -- TODO Fix deprecations

module Test.Cardano.Api.Metadata
  ( tests
  , genTxMetadata
  , genTxMetadataValue
  ) where

import           Cardano.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import           Hedgehog (Property, property, (===))
import qualified Hedgehog
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Cardano.Api
import           Gen.Cardano.Api.Metadata

-- ----------------------------------------------------------------------------
-- Golden / unit tests
--

prop_golden_1 :: Property
prop_golden_1 = matchMetadata
                  "{\"0\": 1}"
                  (TxMetadata (Map.fromList [(0, TxMetaNumber 1)]))

prop_golden_2 :: Property
prop_golden_2 = matchMetadata
                  "{\"0\": \"deadbeef\"}"
                  (txMetadataSingleton 0 (TxMetaText "deadbeef"))

prop_golden_3 :: Property
prop_golden_3 = matchMetadata
                  "{\"0\": \"0xDEADBEEF\"}"
                  (txMetadataSingleton 0 (TxMetaText "0xDEADBEEF"))

prop_golden_4 :: Property
prop_golden_4 = matchMetadata
                  "{\"0\": \"0xdeadbeef\"}"
                  (txMetadataSingleton 0 (TxMetaBytes "\xde\xad\xbe\xef"))

prop_golden_5 :: Property
prop_golden_5 = matchMetadata
                  "{\"0\": [] }"
                  (txMetadataSingleton 0 (TxMetaList []))

prop_golden_6 :: Property
prop_golden_6 = matchMetadata
                  "{\"0\": [1, \"a\", \"0x42\"] }"
                  (txMetadataSingleton 0
                    (TxMetaList [TxMetaNumber 1
                                ,TxMetaText "a"
                                ,TxMetaBytes "\x42"]))

prop_golden_7 :: Property
prop_golden_7 = matchMetadata
                  "{\"0\": {} }"
                  (txMetadataSingleton 0 (TxMetaMap []))

prop_golden_8 :: Property
prop_golden_8 = matchMetadata
                  "{\"0\": { \"0x41\": \"0x42\", \"1\": 2, \"a\" : \"b\" }}"
                  (txMetadataSingleton 0
                    (TxMetaMap [(TxMetaBytes "\x41", TxMetaBytes "\x42")
                               ,(TxMetaNumber 1,     TxMetaNumber 2)
                               ,(TxMetaText  "a",    TxMetaText "b")]))

txMetadataSingleton :: Word64 -> TxMetadataValue -> TxMetadata
txMetadataSingleton n v = TxMetadata (Map.fromList [(n, v)])

matchMetadata :: ByteString -> TxMetadata -> Property
matchMetadata jsonStr metadata =
  Hedgehog.withTests 1 $ Hedgehog.property $ Hedgehog.test $
    case Aeson.decodeStrict' jsonStr of
      Nothing -> Hedgehog.failure
      Just json -> do
        Hedgehog.annotateShow json
        metadataFromJson TxMetadataJsonNoSchema json === Right metadata


-- ----------------------------------------------------------------------------
-- Round trip properties
--

-- | Any JSON (within the supported subset) can be converted to tx metadata and
-- back, to give the same original JSON.
--
-- This uses the \"no schema\" mapping. Note that with this mapping it is /not/
-- the case that any tx metadata can be converted to JSON and back to give the
-- original value.
--
prop_noschema_json_roundtrip_via_metadata :: Property
prop_noschema_json_roundtrip_via_metadata = Hedgehog.property $ do
    json <- Hedgehog.forAll (genJsonForTxMetadata TxMetadataJsonNoSchema)
    Right json === (fmap (metadataToJson   TxMetadataJsonNoSchema)
                        . metadataFromJson TxMetadataJsonNoSchema) json

-- | Any JSON (fitting the detailed schema) can be converted to tx metadata and
-- back, to give the same original JSON.
--
prop_schema_json_roundtrip_via_metadata :: Property
prop_schema_json_roundtrip_via_metadata = Hedgehog.property $ do
    json <- Hedgehog.forAll (genJsonForTxMetadata TxMetadataJsonDetailedSchema)
    Right json === (fmap (metadataToJson   TxMetadataJsonDetailedSchema)
                        . metadataFromJson TxMetadataJsonDetailedSchema) json


-- | Any tx metadata can be converted to JSON (using the detailed schema) and
-- back, to give the same original tx metadata.
--
prop_metadata_roundtrip_via_schema_json :: Property
prop_metadata_roundtrip_via_schema_json = Hedgehog.property $ do
    md <- Hedgehog.forAll genTxMetadata
    Right md === (metadataFromJson TxMetadataJsonDetailedSchema
                . metadataToJson   TxMetadataJsonDetailedSchema) md

-- ----------------------------------------------------------------------------
-- Automagically collecting all the tests
--

tests :: TestTree
tests = $testGroupGenerator
