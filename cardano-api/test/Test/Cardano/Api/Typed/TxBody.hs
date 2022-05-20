{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.TxBody
  ( tests
  ) where

import           Cardano.Prelude

import           Hedgehog (Property, failure, footnoteShow, (===))
import qualified Hedgehog as H
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Cardano.Api
import           Cardano.Api.Shelley
import           Cardano.Ledger.BaseTypes (StrictMaybe (..))
import           Gen.Cardano.Api.Typed
import           Test.Cardano.Api.Typed.Orphans ()

{- HLINT ignore "Use camelCase" -}

-- | Check the txOuts in a TxBodyContent after a ledger roundtrip.
prop_roundtrip_txbodycontent_txouts:: Property
prop_roundtrip_txbodycontent_txouts =
  H.property $ do
    content <- H.forAll $ upgradeSimpleScripts ShelleyBasedEraBabbage <$> genTxBodyContent BabbageEra
    body <- case makeTransactionBody content of
      Left err -> footnoteShow err >> failure
      Right body -> pure body
    footnoteShow body
    -- NOTE: This tests 'getTxBodyContent' and 'fromLedgerTxBody'
    let (TxBody content') = body
    txOuts content === txOuts content'
 where
  -- FIXME: This vvv is not the only case in which the propery is unstable
  -- NOTE: SimpleV1 scripts are "interpreted" as SimpleV2 on the conversion
  -- back. So to be able to re-use the genTxBodyContent generator, we "upgrade"
  -- those scripts directly by doing the conversion once "a priori".
  upgradeSimpleScripts :: ShelleyBasedEra era -> TxBodyContent BuildTx era -> TxBodyContent BuildTx era
  upgradeSimpleScripts sbe content@TxBodyContent{txOuts} =
    content{txOuts = map (upgradeSimpleRefScript sbe) txOuts }

  upgradeSimpleRefScript sbe (TxOut address value datum refScript) =
    TxOut address value datum $
      case refScriptToShelleyScript (shelleyBasedToCardanoEra sbe) refScript of
        SJust ledgerScript -> fromShelleyScriptToReferenceScript sbe ledgerScript
        SNothing -> ReferenceScriptNone

tests :: TestTree
tests = $testGroupGenerator
