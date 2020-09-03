{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Test.Cardano.Api.MetaData
  ( tests
  ) where

import           Cardano.Prelude hiding (MetaData)

import           Cardano.Api.MetaData

import           Data.Aeson (ToJSON (..))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Hedgehog (Gen, Property, assert, discover, (===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Range as Range

-- This test is fragile to changes in the way Metadata is converted to/from JSON.
prop_round_trip_json_metadatum :: Property
prop_round_trip_json_metadatum = do
  Hedgehog.withTests 1000 . Hedgehog.property $ do
    json <- Hedgehog.forAll genMetaData
    case jsonToMetadata json of
      Left _   -> assert $ not $ isValidMetadata json
      Right md -> jsonFromMetadata md === json

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
  Gen.choice
    [ toJSON <$> Gen.integral (Range.linear (0 :: Integer) 10000)
    , toJSON <$> genText
    , toJSON <$> Gen.list (Range.linear 1 3) genTxMetadataValue
    , toJSON . Map.toList <$> Gen.map (Range.linear 1 3) ((,) <$> genText <*> genTxMetadataValue)
    , toJSON <$> Gen.bool
    , pure Json.Null
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
tests =
  Hedgehog.checkParallel $$discover
