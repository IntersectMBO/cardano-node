{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.MultiSig.Allegra
  ( tests
  ) where

import           Cardano.Prelude

import           Cardano.Api
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
  H.goldenTestJsonValuePretty exampleAllAllegra "test/Golden/MultiSig/Allegra/all"

prop_golden_AnyMultiSig :: Property
prop_golden_AnyMultiSig =
  H.goldenTestJsonValuePretty exampleAnyAllegra "test/Golden/MultiSig/Allegra/any"

prop_golden_MofNMultiSig :: Property
prop_golden_MofNMultiSig =
  H.goldenTestJsonValuePretty exampleMofNAllegra "test/Golden/MultiSig/Allegra/atleast"

prop_roundtrip_AllegraMultiSigScript_JSON :: Property
prop_roundtrip_AllegraMultiSigScript_JSON =
  H.property $ do
    script <- H.forAll (genScriptInEra AllegraEra)
    H.tripping (canonicaliseScriptVersion script) encode eitherDecode

tests :: TestTree
tests = fromGroup $$discover

