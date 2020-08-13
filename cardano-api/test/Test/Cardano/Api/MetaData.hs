{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Test.Cardano.Api.MetaData
  ( tests
  ) where

import           Cardano.Prelude hiding (MetaData)

import           Cardano.Api.Typed
import           Cardano.Api.MetaData

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Text as Text

import           Hedgehog (Gen, Property, discover)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Range (Range)
import qualified Hedgehog.Range as Range

-- This test is fragile to changes in the way Metadata is converted to/from JSON.
prop_round_trip_json_metadatum :: Property
prop_round_trip_json_metadatum = do
  Hedgehog.withTests 1000 . Hedgehog.property $ do
    md <- Hedgehog.forAll genMetaData
    Hedgehog.tripping md jsonFromMetadata jsonToMetadata

-- -----------------------------------------------------------------------------

-- Generate 'TxMetadata' that will round trip correctly.
-- The only valid value that is known to not round trip correctly is a list of
-- lists where all the sublists are of length 2.
-- The reason this does not round trip is because when decoding the JSON, a list
-- of pairs is assumed to be a map.
genMetaData :: Gen TxMetadata
genMetaData = do
  idxs <- List.nub <$> Gen.list (Range.linear 1 5) (Gen.word64 Range.constantBounded)
  makeTransactionMetadata . Map.fromList <$> mapM (\i -> (i,) <$> genTxMetadataValue) idxs

genTxMetadataValue :: Gen TxMetadataValue
genTxMetadataValue =
  -- This shinks towards the head of the list, so have TxMetaMap at the end.
  Gen.choice
    [ TxMetaNumber <$> Gen.integral (Range.linear 0 10000)
    , TxMetaBytes <$> genByteString
    , TxMetaText <$> genText
    -- Only generate list of flat values to avoid generating a valid HashMap.
    , TxMetaList <$> Gen.list (Range.linear 1 8) genFlatTxMetadataValue
    , TxMetaMap <$> genPairList (Range.linear 1 8)
    ]

-- As above, but without nested values (List and Map).
genFlatTxMetadataValue :: Gen TxMetadataValue
genFlatTxMetadataValue =
  Gen.choice
    [ TxMetaNumber <$> Gen.integral (Range.linear 0 10000)
    , TxMetaBytes <$> genByteString
    , TxMetaText <$> genText
    ]

genPairList :: Range Int -> Gen [(TxMetadataValue, TxMetadataValue)]
genPairList len = do
  keys <- Gen.list len $
            Gen.frequency
              [ (3, TxMetaText <$> genText)
              , (2, TxMetaBytes <$> genByteString)
              , (1, genFlatTxMetadataValue)
              ]
  -- Keys need to be unique and sorted.
  -- They need to be unque because they are inserted into a HashMap and they
  -- need to be sorted so they round trip correctly.
  mapM (\k -> (k,) <$> genTxMetadataValue) (List.sort $ List.nub keys)

genByteString :: Gen ByteString
genByteString = BS.pack <$> Gen.list (Range.linear 0 64) Gen.latin1

genText :: Gen Text
genText = Text.pack <$> Gen.list (Range.linear 0 64) Gen.alphaNum

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
