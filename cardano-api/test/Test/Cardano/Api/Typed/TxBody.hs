{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.TxBody
  ( tests
  ) where

import           Cardano.Prelude

import           Hedgehog (Property, footnoteShow, failure, (===))
import qualified Hedgehog as H
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Cardano.Api
import           Gen.Cardano.Api.Typed
import           Test.Cardano.Api.Typed.Orphans ()

{- HLINT ignore "Use camelCase" -}

-- | Check the txOuts in a TxBodyContent after a ledger roundtrip.
prop_roundtrip_txbodycontent_txouts:: Property
prop_roundtrip_txbodycontent_txouts =
  H.property $ do
    content <- H.forAll $ genTxBodyContent BabbageEra
    body <- case makeTransactionBody content of
      Left err -> footnoteShow err >> failure
      Right body -> pure body
    -- NOTE: This tests 'getTxBodyContent' and 'fromLedgerTxBody'
    let (TxBody content') = body
    txOuts content === txOuts content'

tests :: TestTree
tests = $testGroupGenerator
