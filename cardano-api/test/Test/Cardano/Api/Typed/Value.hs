{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-deprecations #-} -- TODO Fix deprecations

module Test.Cardano.Api.Typed.Value
  ( tests
  ) where

import           Prelude

import           Data.Aeson (eitherDecode, encode)
import           Data.List (groupBy, sort)
import qualified Data.Map.Strict as Map
import           Hedgehog (Property, forAll, property, tripping, (===))
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Cardano.Api (ValueNestedBundle (ValueNestedBundle, ValueNestedBundleAda),
                   ValueNestedRep (..), valueFromNestedRep, valueToNestedRep)

import           Gen.Cardano.Api.Typed (genAssetName, genValueDefault, genValueNestedRep)

prop_roundtrip_Value_JSON :: Property
prop_roundtrip_Value_JSON =
  property $ do v <- forAll genValueDefault
                tripping v encode eitherDecode


prop_roundtrip_Value_flatten_unflatten :: Property
prop_roundtrip_Value_flatten_unflatten =
  property $ do v <- forAll genValueDefault
                valueFromNestedRep (valueToNestedRep v) === v

prop_roundtrip_Value_unflatten_flatten :: Property
prop_roundtrip_Value_unflatten_flatten =
    property $ do
      v <- forAll genValueNestedRep
      canonicalise v === valueToNestedRep (valueFromNestedRep v)

canonicalise :: ValueNestedRep -> ValueNestedRep
canonicalise =
    ValueNestedRep
  . filter (not . isZeroOrEmpty)
  . map (filterZeros . foldl1 mergeBundle)
  . groupBy samePolicyId
  . sort
  . (\(ValueNestedRep bundles) -> bundles)
  where
    samePolicyId ValueNestedBundleAda{}
                 ValueNestedBundleAda{} = True
    samePolicyId (ValueNestedBundle pid _)
                 (ValueNestedBundle pid' _) = pid == pid'
    samePolicyId _ _ = False

    -- Merge together bundles that have already been grouped by same PolicyId:
    mergeBundle (ValueNestedBundleAda q)
                (ValueNestedBundleAda q') =
      ValueNestedBundleAda (q <> q')

    mergeBundle (ValueNestedBundle pid  as)
                (ValueNestedBundle pid' as') | pid == pid' =
      ValueNestedBundle pid (Map.unionWith (<>) as as')

    mergeBundle _ _ = error "canonicalise.mergeBundle: impossible"

    filterZeros b@ValueNestedBundleAda{} = b
    filterZeros (ValueNestedBundle pid as) =
      ValueNestedBundle pid (Map.filter (/=0) as)

    isZeroOrEmpty (ValueNestedBundleAda q) = q == 0
    isZeroOrEmpty (ValueNestedBundle _ as) = Map.null as


prop_roundtrip_AssetName_JSON :: Property
prop_roundtrip_AssetName_JSON =
  property $ do
    v <- forAll genAssetName
    tripping v encode eitherDecode

prop_roundtrip_AssetName_JSONKey :: Property
prop_roundtrip_AssetName_JSONKey =
  property $ do
    v <- forAll genAssetName
    tripping (Map.singleton v ()) encode eitherDecode


-- -----------------------------------------------------------------------------

tests :: TestTree
tests = $testGroupGenerator
