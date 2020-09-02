{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Test.Cardano.Api.MetaData
  ( tests
  ) where

import           Cardano.Prelude hiding (MetaData)

import           Cardano.Api.MetaData
import           Cardano.Api.Typed

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Hedgehog (Gen, Property, discover)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Range as Range

-- This test is fragile to changes in the way Metadata is converted to/from JSON.
prop_round_trip_json_metadatum :: Property
prop_round_trip_json_metadatum = do
  Hedgehog.withTests 1000 . Hedgehog.property $ do
    json <- jsonFromMetadata <$> Hedgehog.forAll genMetaData
    Hedgehog.tripping json identity (fmap jsonFromMetadata . jsonToMetadata)

--- Generate 'TxMetadata' that will round trip correctly.
--
-- We only expect metadata to roundtrip if they come from any valid JSON representation. So,
-- we expect the composition @fromJSON . toJSON@ to be roughly the identity function, with a few
-- gotchas:
--
-- - No boolean values
-- - No null values
genMetaData :: Gen TxMetadata
genMetaData = do
  idxs <- List.nub <$> Gen.list (Range.linear 1 5) (Gen.word64 Range.constantBounded)
  makeTransactionMetadata . Map.fromList <$> mapM (\i -> (i,) <$> genTxMetadataValue) idxs

genTxMetadataValue :: Gen TxMetadataValue
genTxMetadataValue =
  -- This shinks towards the head of the list, so have TxMetaMap at the end.
  Gen.choice
    [ TxMetaNumber <$> Gen.integral (Range.linear 0 10000)
    , TxMetaText <$> genText
    , TxMetaList <$> Gen.list (Range.linear 1 2) genTxMetadataValue
    , TxMetaMap . Map.toList <$> Gen.map (Range.linear 1 4) ((,) <$> genValidJSONKey <*> genTxMetadataValue)
    ]

genValidJSONKey :: Gen TxMetadataValue
genValidJSONKey = TxMetaText <$> genText

genText :: Gen Text
genText = Gen.choice
  [ Gen.ensure (not . Text.isPrefixOf bytesPrefix)
      (Text.pack <$> Gen.list (Range.linear 0 64) Gen.alphaNum)
  , (bytesPrefix<>) . Text.decodeUtf8 . Base16.encode <$> genByteString
  ]

genByteString :: Gen ByteString
genByteString = BS.pack <$> Gen.list (Range.linear 0 64) Gen.latin1

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
