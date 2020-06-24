{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api.TextView
  ( tests
  ) where

import           Cardano.Prelude

import           Cardano.Api.TextView
import           Cardano.Api.Shelley.ColdKeys

import qualified Data.ByteString.Char8 as BS

import           Hedgehog (Property, discover)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Test.Cardano.Config.Gen


-- Test this first. If this fails, others are likely to fail.
prop_roundtrip_multiline_hex :: Property
prop_roundtrip_multiline_hex =
  Hedgehog.property $ do
    bs <- BS.pack <$> Hedgehog.forAll (Gen.string (Range.linear 0 500) (Gen.element ['\0' .. '\xff']))
    Hedgehog.tripping bs (BS.unlines . rawToMultilineHex) unRawToMultilineHex

-- Test this second. If this fails, others are likely to fail.
prop_roundtrip_TextView :: Property
prop_roundtrip_TextView =
  Hedgehog.property $ do
    tv <- Hedgehog.forAll genTextView
    Hedgehog.tripping tv renderTextView parseTextView

prop_roundtrip_shelley_SigningKey_view :: Property
prop_roundtrip_shelley_SigningKey_view =
  Hedgehog.property $ do
    kr <- Hedgehog.forAll genKeyRole
    sk <- Hedgehog.forAll genSigningKey
    Hedgehog.tripping sk (encodeSigningKey kr) (decodeSigningKey kr)

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
