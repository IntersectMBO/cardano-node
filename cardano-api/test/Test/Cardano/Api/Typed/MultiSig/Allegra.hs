{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.MultiSig.Allegra
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
  goldenTestJSONPretty exampleAllegraAll "test/Golden/MultiSig/Allegra/all"

prop_golden_AnyMultiSig :: Property
prop_golden_AnyMultiSig =
  goldenTestJSONPretty exampleAllegraAny "test/Golden/MultiSig/Allegra/any"

prop_golden_MofNMultiSig :: Property
prop_golden_MofNMultiSig =
  goldenTestJSONPretty exampleAllegraMofN "test/Golden/MultiSig/Allegra/atleast"


prop_roundtrip_AllegraMultiSigScript_JSON :: Property
prop_roundtrip_AllegraMultiSigScript_JSON =
  H.property $ do
    mss <- H.forAll genMultiSigScriptAllegra
    H.tripping mss encode eitherDecode

tests :: TestTree
tests = fromGroup $$discover

