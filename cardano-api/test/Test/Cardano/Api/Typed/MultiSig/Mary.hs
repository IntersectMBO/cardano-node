{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.MultiSig.Mary
  ( tests
  ) where

import           Cardano.Api
import           Cardano.Prelude
import           Data.Aeson
import           Hedgehog (Property, discover)
import           Test.Cardano.Api.Examples
import           Test.Cardano.Api.Typed.Gen
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog.Group (fromGroup)

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Aeson as H

prop_golden_AllMultiSig :: Property
prop_golden_AllMultiSig =
  H.goldenTestJsonValuePretty exampleAllMary "test/Golden/MultiSig/Mary/all"

prop_golden_AnyMultiSig :: Property
prop_golden_AnyMultiSig =
  H.goldenTestJsonValuePretty exampleAnyMary "test/Golden/MultiSig/Mary/any"

prop_golden_MofNMultiSig :: Property
prop_golden_MofNMultiSig =
  H.goldenTestJsonValuePretty exampleMofNMary "test/Golden/MultiSig/Mary/atleast"

prop_roundtrip_MaryMultiSigScript_JSON :: Property
prop_roundtrip_MaryMultiSigScript_JSON =
  H.property $ do
    script <- H.forAll (genScriptInEra MaryEra)
    H.tripping (canonicaliseScriptVersion script) encode eitherDecode

tests :: TestTree
tests = fromGroup $$discover
