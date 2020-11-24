{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Api.Gen
  ( genMetaData
  ) where


import           Cardano.Prelude hiding (MetaData)

import qualified Data.Map.Strict as Map

import           Shelley.Spec.Ledger.MetaData (MetaData (..), MetaDatum (..))

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genMetaData :: Gen MetaData
genMetaData = do
  numberOfIndicies <- Gen.integral (Range.linear 1 15)
  let indexes = map (\i -> fromIntegral i :: Word64) [1..numberOfIndicies]
  mDatums <- Gen.list (Range.singleton numberOfIndicies) genMetaDatum
  return . MetaData . Map.fromList $ zip indexes mDatums

genMetaDatum :: Gen MetaDatum
genMetaDatum = do
    int <- Gen.list (Range.linear 1 5) (I <$> Gen.integral (Range.linear 1 100))
    bytes <- Gen.list (Range.linear 1 5) (B <$> Gen.bytes (Range.linear 1 20))
    str <- Gen.list (Range.linear 1 5) (S <$> Gen.text (Range.linear 1 20) Gen.alphaNum)
    let mDatumList = int ++ bytes ++ str

    singleMetaDatum <- Gen.element mDatumList

    Gen.choice
      [ return $ List mDatumList
      , return $ Map [(singleMetaDatum, singleMetaDatum)]
      , return $ Map [(List mDatumList, singleMetaDatum)]
      , return $ Map [(singleMetaDatum, List mDatumList)]
      ]
