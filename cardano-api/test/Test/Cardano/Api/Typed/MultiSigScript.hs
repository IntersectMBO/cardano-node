{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.MultiSigScript
  ( tests
  ) where

import           Cardano.Api.Typed

import           Cardano.Prelude

import           Data.Aeson
import           Hedgehog (Property, discover)
import qualified Hedgehog as H
import           Hedgehog.Internal.Property (failWith)

import           Test.Cardano.Api.Examples
import           Test.Cardano.Api.Typed.Gen
import           Test.Cardano.Api.Typed.Orphans ()
import           Test.Cardano.Prelude (goldenTestJSONPretty)

prop_generateMofNcorrectly :: Property
prop_generateMofNcorrectly = H.property $ do
  RequireMOf req sigs <- H.forAll genMofNRequiredSig
  if length sigs >= req
  then H.success
  else failWith Nothing
         $ "genMofNRequiredSig: Number of required \
           \signatures exceed number of available key hashes. \
           \m: " ++ show req ++ " n: " ++ show (length sigs)

prop_golden_AllMultiSig :: Property
prop_golden_AllMultiSig = goldenTestJSONPretty exampleAll "test/Golden/MultiSig/all"

prop_golden_AnyMultiSig :: Property
prop_golden_AnyMultiSig = goldenTestJSONPretty exampleAny "test/Golden/MultiSig/any"

prop_golden_MofNMultiSig :: Property
prop_golden_MofNMultiSig = goldenTestJSONPretty exampleMofN "test/Golden/MultiSig/mofn"

prop_roundtrip_MultiSigScript_JSON :: Property
prop_roundtrip_MultiSigScript_JSON =
  H.property $ do
    mss <- H.forAll genMultiSigScript
    H.tripping mss encode eitherDecode


tests :: IO Bool
tests =
  H.checkParallel $$discover
