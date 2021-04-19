{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.MultiSig.Shelley
  ( tests
  ) where

import           Cardano.Prelude

import           Data.Aeson
import           Hedgehog (Property, discover)
import qualified Hedgehog as H
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog.Group (fromGroup)

import           Cardano.Api

import           Hedgehog.Extras.Aeson
import           Test.Cardano.Api.Examples
import           Test.Cardano.Api.Typed.Gen

prop_golden_AllMultiSig :: Property
prop_golden_AllMultiSig =
  goldenTestJsonValuePretty exampleAllShelley "test/Golden/MultiSig/Shelley/all"

prop_golden_AnyMultiSig :: Property
prop_golden_AnyMultiSig =
  goldenTestJsonValuePretty exampleAnyShelley "test/Golden/MultiSig/Shelley/any"

prop_golden_MofNMultiSig :: Property
prop_golden_MofNMultiSig =
  goldenTestJsonValuePretty exampleMofNShelley "test/Golden/MultiSig/Shelley/atleast"

prop_roundtrip_MultiSigScript_JSON :: Property
prop_roundtrip_MultiSigScript_JSON =
  H.property $ do
    mss <- H.forAll (genScriptInEra ShelleyEra)
    H.tripping mss encode eitherDecode

tests :: TestTree
tests = fromGroup $$discover
