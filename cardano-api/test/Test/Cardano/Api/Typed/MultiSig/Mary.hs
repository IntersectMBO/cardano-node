{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.MultiSig.Mary
  ( tests
  ) where

import           Cardano.Prelude

import           Data.Aeson
import           Hedgehog (Property, discover)
import qualified Hedgehog as H
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog.Group (fromGroup)

import           Test.Cardano.Api.Examples
import           Test.Cardano.Api.Typed.Gen
import           Test.Cardano.Prelude (goldenTestJSONPretty)

prop_golden_AllMultiSig :: Property
prop_golden_AllMultiSig =
  goldenTestJSONPretty exampleMaryAll "test/Golden/MultiSig/Mary/all"

prop_golden_AnyMultiSig :: Property
prop_golden_AnyMultiSig =
  goldenTestJSONPretty exampleMaryAny "test/Golden/MultiSig/Mary/any"

prop_golden_MofNMultiSig :: Property
prop_golden_MofNMultiSig =
  goldenTestJSONPretty exampleMaryMofN "test/Golden/MultiSig/Mary/atleast"

prop_roundtrip_MaryMultiSigScript_JSON :: Property
prop_roundtrip_MaryMultiSigScript_JSON =
  H.property $ do
    mss <- H.forAll genMultiSigScriptMary
    H.tripping mss encode eitherDecode

tests :: TestTree
tests = fromGroup $$discover

