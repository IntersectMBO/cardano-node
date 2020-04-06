{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api.View
  ( tests
  ) where

import           Cardano.Api

import           Cardano.Prelude

import qualified Data.ByteString.Char8 as BS

import           Hedgehog (Property, discover)
import qualified Hedgehog as H
import           Hedgehog.Gen as Gen
import           Hedgehog.Range as Range

import           Test.Cardano.Api.Gen
import           Test.Cardano.Api.Orphans ()


-- Test this first. If this fails, others are likely to fail.
prop_roundtrip_multiline_hex :: Property
prop_roundtrip_multiline_hex =
  H.property $ do
    bs <- BS.pack <$> H.forAll (Gen.string (Range.linear 0 500) (Gen.element ['\0' .. '\xff']))
    H.tripping bs (BS.unlines . rawToMultilineHex) unRawToMultilineHex

prop_roundtrip_AddressView :: Property
prop_roundtrip_AddressView =
  H.property $ do
    addr <- byronPubKeyAddress <$> H.forAll genPublicKeyByron
    H.tripping addr renderAddressView parseAddressView

prop_roundtrip_KeyPairView :: Property
prop_roundtrip_KeyPairView =
  H.property $ do
    kp <- H.forAll genKeyPair
    H.tripping kp renderKeyPairView parseKeyPairView

prop_roundtrip_PublicKeyView :: Property
prop_roundtrip_PublicKeyView =
  H.property $ do
    pk <- H.forAll genPublicKey
    H.tripping pk renderPublicKeyView parsePublicKeyView

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
