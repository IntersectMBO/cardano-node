{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cli.MultiAssetParsing where

import           Cardano.Prelude hiding (filter)

import qualified Data.Text as Text
import qualified Text.Parsec as Parsec (parse)

import           Hedgehog (Property, checkSequential, discover, forAll, property, tripping)
import           Hedgehog.Gen (filter)

import           Cardano.Api (parseValue, renderValue, renderValuePretty, valueToList)

import           Gen.Cardano.Api.Typed (genValueDefault)

prop_roundtrip_Value_parse_render :: Property
prop_roundtrip_Value_parse_render =
  property $ do
    value <- forAll $ filter (not . null . valueToList) genValueDefault
    tripping
      value
      renderValue
      (Parsec.parse parseValue "" . Text.unpack)

prop_roundtrip_Value_parse_renderPretty :: Property
prop_roundtrip_Value_parse_renderPretty =
  property $ do
    value <- forAll $ filter (not . null . valueToList) genValueDefault
    tripping
      value
      renderValuePretty
      (Parsec.parse parseValue "" . Text.unpack)

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  checkSequential $$discover
