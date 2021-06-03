{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Json
  ( tests
  ) where

import           Cardano.Api.Orphans ()
import           Cardano.Prelude ( ($) )
import           Data.Aeson ( eitherDecode, encode )
import           Hedgehog (Property, discover, forAll, tripping)
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog.Group (fromGroup)

import qualified Hedgehog as H
import qualified Test.Cardano.Api.Typed.Gen as Gen
import qualified Test.Cardano.Api.Gen as Gen

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_AlonzoGenesis :: Property
prop_roundtrip_AlonzoGenesis = H.property $ do
  genesis <- forAll Gen.genAlonzoGenesis
  tripping genesis encode eitherDecode

prop_roundtrip_ScriptData :: Property
prop_roundtrip_ScriptData = H.property $ do
  genesis <- forAll Gen.genScriptData
  tripping genesis encode eitherDecode

tests :: TestTree
tests = fromGroup $$discover
