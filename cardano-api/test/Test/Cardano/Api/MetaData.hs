{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Test.Cardano.Api.MetaData
  ( tests
  ) where

import           Cardano.Prelude hiding (MetaData)

import           Cardano.Api.MetaData
import           Cardano.Api.Typed (serialiseToCBOR)

import           Data.Aeson (ToJSON (..))
import qualified Data.Aeson as Json
import           Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Hedgehog (Gen, Property, assert, discover, label, property, test, (===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Range as Range

-- This test is fragile to changes in the way Metadata is converted to/from JSON.
prop_round_trip_json_metadatum :: Property
prop_round_trip_json_metadatum = do
  Hedgehog.withTests 2000 . Hedgehog.property $ do
    json <- Hedgehog.forAll genMetaData
    case jsonToMetadata json of
      Left _   -> label "Left"  >> assert (not $ isValidMetadata json)
      Right md -> label "Right" >> jsonFromMetadata md === json

prop_golden_1 :: Property
prop_golden_1 = [aesonQQ|{"0": "0xff"}|] `matchCBOR` "a10041ff"

prop_golden_2 :: Property
prop_golden_2 = [aesonQQ|{"0": 1}|] `matchCBOR` "a10001"

prop_golden_3 :: Property
prop_golden_3 = [aesonQQ|{"0": { "hex": "ff"}}|] `matchCBOR` "a100a163686578626666"

prop_golden_4 :: Property
prop_golden_4 = [aesonQQ|{"0": [] }|] `matchCBOR` "a10080"

prop_golden_5 :: Property
prop_golden_5 = [aesonQQ|{"0": [["a", 1], ["a", 2]]}|] `matchCBOR` "a100828261610182616102"

prop_golden_6 :: Property
prop_golden_6 = [aesonQQ|{"0": {"a":1}}|] `matchCBOR` "a100a1616101"

matchCBOR :: Json.Value -> Text -> Property
matchCBOR json cbor = do
  let cborFromJson = Text.decodeUtf8 . Base16.encode . serialiseToCBOR <$> jsonToMetadata json
  Hedgehog.withTests 1 $ Hedgehog.property $ Hedgehog.test $ cborFromJson === Right cbor

--- Generate 'TxMetadata' that will round trip correctly.
--
-- We only expect metadata to roundtrip if they come from any valid JSON representation. So,
-- we expect the composition @fromJSON . toJSON@ to be roughly the identity function, with a few
-- gotchas:
--
-- - No boolean values
-- - No null values
genMetaData :: Gen Json.Value
genMetaData = do
  idxs <- List.nub <$> Gen.list (Range.linear 1 5) (Gen.word64 Range.constantBounded)
  toJSON . Map.fromList <$> mapM (\i -> (i,) <$> genTxMetadataValue) idxs

genTxMetadataValue :: Gen Json.Value
genTxMetadataValue =
  -- This shinks towards the head of the list, so have TxMetaMap at the end.
  Gen.frequency
    [ (10, toJSON <$> Gen.integral (Range.linear (0 :: Integer) 10000))
    , (10, toJSON <$> genText)
    , (10, toJSON <$> Gen.list (Range.linear 1 3) genTxMetadataValue)
    , (10, toJSON . Map.toList <$> Gen.map (Range.linear 1 3) ((,) <$> genText <*> genTxMetadataValue))
    , (1, toJSON <$> Gen.bool)
    , (1, pure Json.Null)
    ]

genText :: Gen Text
genText = Gen.choice
  [ Gen.ensure (not . Text.isPrefixOf bytesPrefix)
      (Text.pack <$> Gen.list (Range.linear 0 100) Gen.alphaNum)
  , (bytesPrefix<>) . Text.decodeUtf8 . Base16.encode <$> genByteString
  ]

genByteString :: Gen ByteString
genByteString = BS.pack <$> Gen.list (Range.linear 0 64) Gen.latin1

isValidMetadata :: Json.Value -> Bool
isValidMetadata json = case json of
  Json.Bool _   -> False
  Json.Null     -> False
  Json.Number _ -> True
  Json.Object o -> all isValidMetadata o
  Json.Array xs -> all isValidMetadata xs
  Json.String t -> or
    [ BS.length (Text.encodeUtf8 t) <= txMetadataTextStringMaxByteLength
    , case Base16.decode . Text.encodeUtf8 <$> Text.stripPrefix bytesPrefix t of
        Just (bytes, "") -> BS.length bytes <= txMetadataByteStringMaxLength
        _ -> False
    ]

tests :: IO Bool
tests = Hedgehog.checkParallel $$discover
