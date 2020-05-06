{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api.Convert
  ( tests
  ) where

import           Cardano.Prelude

import           Cardano.Api

import           Test.Cardano.Api.Orphans ()

import           Hedgehog (Property, discover)
import qualified Hedgehog as Hedgehog

import           Test.Cardano.Api.Gen

prop_roundtrip_AddressByron_hex :: Property
prop_roundtrip_AddressByron_hex =
  Hedgehog.withTests 500 . Hedgehog.property $ do
    addr <- Hedgehog.forAll genVerificationKeyAddressByron
    Hedgehog.tripping addr addressToHex addressFromHex

prop_roundtrip_AddressShelley_hex :: Property
prop_roundtrip_AddressShelley_hex =
  Hedgehog.withTests 500 . Hedgehog.property $ do
    addr <- Hedgehog.forAll genVerificationKeyAddressShelley
    Hedgehog.tripping addr addressToHex addressFromHex

prop_roundtrip_TxIn_hex :: Property
prop_roundtrip_TxIn_hex =
  Hedgehog.withTests 500 . Hedgehog.property $ do
    txin <- Hedgehog.forAll genTxIn
    Hedgehog.tripping txin renderTxIn parseTxIn

prop_roundtrip_TxOut_hex :: Property
prop_roundtrip_TxOut_hex =
  Hedgehog.withTests 500 . Hedgehog.property $ do
    txin <- Hedgehog.forAll genTxOut
    Hedgehog.tripping txin renderTxOut parseTxOut

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
