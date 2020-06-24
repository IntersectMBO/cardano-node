{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api.Convert
  ( tests
  ) where

import           Cardano.Prelude

import           Cardano.Api

import           Test.Cardano.Api.Orphans ()

import           Hedgehog (Property, discover)
import qualified Hedgehog as Hedgehog

import           Test.Cardano.Api.Gen hiding (genTxIn)
import           Test.Cardano.Api.Typed.Gen (genTxIn)

-- This test has been disabled because 'Shelley.Spec.Ledger.Address.getByron'
-- has not been implemented yet and just 'panic's.
_prop_roundtrip_AddressByron_hex :: Property
_prop_roundtrip_AddressByron_hex =
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
