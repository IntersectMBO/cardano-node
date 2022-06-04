{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Cardano.Api.Typed.TxBody
  ( tests
  ) where

import           Cardano.Prelude

import           Hedgehog (Property, annotateShow, failure, (===), MonadTest)
import qualified Hedgehog as H
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Cardano.Api
import           Cardano.Api.Shelley (ReferenceScript (..), refScriptToShelleyScript)
import           Data.Type.Equality (TestEquality (testEquality))
import           Gen.Cardano.Api.Typed
import           Test.Cardano.Api.Typed.Orphans ()

{- HLINT ignore "Use camelCase" -}

-- | Check the txOuts in a TxBodyContent after a ledger roundtrip.
prop_roundtrip_txbodycontent_txouts:: Property
prop_roundtrip_txbodycontent_txouts =
  H.property $ do
    content <- H.forAll $ genTxBodyContent BabbageEra
    -- Create the ledger body & auxiliaries
    body <- case makeTransactionBody content of
      Left err -> annotateShow err >> failure
      Right body -> pure body
    annotateShow body
    -- Convert ledger body back via 'getTxBodyContent' and 'fromLedgerTxBody'
    let (TxBody content') = body
    matchTxOuts (txOuts content) (txOuts content')
 where
  matchTxOuts :: MonadTest m => [TxOut CtxTx BabbageEra] -> [TxOut CtxTx BabbageEra] -> m ()
  matchTxOuts as bs =
    mapM_ matchTxOut $ zip as bs

  matchTxOut :: MonadTest m => (TxOut CtxTx BabbageEra, TxOut CtxTx BabbageEra) -> m ()
  matchTxOut (a, b) = do
    let TxOut aAddress aValue aDatum aRefScript = a
    let TxOut bAddress bValue bDatum bRefScript = b
    aAddress === bAddress
    aValue === bValue
    matchDatum (aDatum, bDatum)
    matchRefScript (aRefScript, bRefScript)

  -- NOTE: We accept TxOutDatumInTx instead of TxOutDatumHash as it may be
  -- correctly resolved given a datum matching the hash was generated.
  matchDatum :: MonadTest m => (TxOutDatum CtxTx era, TxOutDatum CtxTx era) -> m ()
  matchDatum = \case
    (TxOutDatumHash _ dh, TxOutDatumInTx _ d) ->
      dh === hashScriptData d
    (a, b) ->
      a === b

  -- NOTE: After Allegra, all eras interpret SimpleScriptV1 as SimpleScriptV2
  -- because V2 is a superset of V1. So we accept that as a valid conversion.
  matchRefScript :: MonadTest m => (ReferenceScript BabbageEra, ReferenceScript BabbageEra) -> m ()
  matchRefScript (a, b)
    | isSimpleScriptV1 a && isSimpleScriptV2 b =
      refScriptToShelleyScript BabbageEra a === refScriptToShelleyScript BabbageEra b
    | otherwise =
      a === b

  isSimpleScriptV1 :: ReferenceScript era -> Bool
  isSimpleScriptV1 = isLang (SimpleScriptLanguage SimpleScriptV1)

  isSimpleScriptV2 :: ReferenceScript era -> Bool
  isSimpleScriptV2 = isLang (SimpleScriptLanguage SimpleScriptV2)

  isLang :: ScriptLanguage a -> ReferenceScript era -> Bool
  isLang expected = \case
    (ReferenceScript _ (ScriptInAnyLang actual _)) -> isJust $ testEquality expected actual
    _ -> False

tests :: TestTree
tests = $testGroupGenerator
