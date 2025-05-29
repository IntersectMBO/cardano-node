{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Api.TxReferenceInputDatum
  ( hprop_tx_refin_datum
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Network as Net
import qualified Cardano.Api.Network as Net.Tx
import           Cardano.Api.Shelley
import qualified Cardano.Api.Tx.UTxO as Utxo

import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Data.Default.Class
import           Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Proxy
import           Data.Set (Set)
import           GHC.Exts (IsList (..))
import           GHC.Stack
import           Lens.Micro

import           Testnet.Components.Query
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H


-- | Test providing public key resolved datum for reference inputs.
--
-- One can specify reference inputs which are pointing to UTXOs with just datum hashes. To use those datum in the
-- script, it's necessary to provide an actual datum value. One way to provide such datums is to just put it explicitly
-- in the 'TxBodyContent`, alongside the reference inputs in 'TxInsReference' value.
--
-- Reference inputs are the only way to supply additional datum to scripts from UTXO.
--
-- This test tests that such datums are made available to the script.
hprop_tx_refin_datum :: Property
hprop_tx_refin_datum = integrationRetryWorkspace 2 "api-tx-refin-dat" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf{tempAbsPath} <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath

  let ceo = ConwayEraOnwardsConway
      beo = convert ceo
      sbe = convert ceo
      eraProxy = proxyToAsType Proxy
      options = def{cardanoNodeEra = AnyShelleyBasedEra sbe}

  tr@TestnetRuntime
    { configurationFile
    , testnetNodes = node0 : _
    , wallets = wallet0@(PaymentKeyInfo _ addrTxt0) : wallet1 : _
    } <-
    cardanoTestnetDefault options def conf

  systemStart <- H.noteShowM $ getStartTime tempAbsPath' tr
  epochStateView <- getEpochStateView configurationFile (nodeSocketPath node0)
  connectionInfo <- nodeConnectionInfo tr 0
  pparams <- getProtocolParams epochStateView ceo

  -- prepare tx inputs and output address
  H.noteShow_ addrTxt0
  addr0 <- H.nothingFail $ deserialiseAddress (AsAddressInEra eraProxy) addrTxt0

  let (PaymentKeyInfo _ addrTxt1) = wallet1
  H.noteShow_ addrTxt1
  addr1 <- H.nothingFail $ deserialiseAddress (AsAddressInEra eraProxy) addrTxt1

  -- read key witnesses
  [wit0, wit1] :: [ShelleyWitnessSigningKey] <-
    forM [wallet0, wallet1] $ \wallet ->
      H.leftFailM . H.evalIO $
        readFileTextEnvelopeAnyOf
          [FromSomeType asType WitnessGenesisUTxOKey]
          (signingKey $ paymentKeyInfoPair wallet)

  -- query node for era history
  epochInfo <-
    (H.leftFail <=< H.leftFailM) . H.evalIO $
      executeLocalStateQueryExpr connectionInfo Net.VolatileTip $
        fmap toLedgerEpochInfo <$> queryEraHistory

  let scriptData1 = unsafeHashableScriptData $ ScriptDataBytes "HASH_1"
      scriptData2 = unsafeHashableScriptData $ ScriptDataBytes "INLINE_1"
      scriptData3 = unsafeHashableScriptData $ ScriptDataBytes "SUPPLEMENTAL_1"
  -- 4e62677c3b9f3b247502efe39a85aadcc2f2d3a32aec544d62175ed86c57fe9b
  H.noteShow_ $ hashScriptDataBytes scriptData1
  -- c93bae5c7cb737e16eb224d1884e7fbe14dc038caf1b511e34a43e67d3eb9f63
  H.noteShow_ $ hashScriptDataBytes scriptData2
  -- 74ea77567269646d49e072bd83e701ff7e43574522ad90833bcfa554658c65bb
  H.noteShow_ $ hashScriptDataBytes scriptData3
  let txDatum1 =
        TxOutDatumHash
          (convert beo)
          (hashScriptDataBytes scriptData1)
      txDatum2 = TxOutDatumInline beo scriptData2
      txDatum3 = TxOutSupplementalDatum (convert beo) scriptData3

  -- first transaction inserting txouts to UTXO set with datum in the outputs
  tx1Utxo <- do
    txIn <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

    -- prepare txout
    let txOutValue = lovelaceToTxOutValue sbe 100_000_000
        txOuts =
          [ TxOut addr1 txOutValue txDatum1 ReferenceScriptNone
          , TxOut addr1 txOutValue txDatum2 ReferenceScriptNone
          , TxOut addr1 txOutValue txDatum3 ReferenceScriptNone
          ]

        -- build a transaction
        content =
          defaultTxBodyContent sbe
            & setTxIns [(txIn, pure $ KeyWitness KeyWitnessForSpending)]
            & setTxOuts txOuts
            & setTxProtocolParams (pure $ pure pparams)

    utxo <- findAllUtxos epochStateView sbe

    BalancedTxBody _ txBody@(ShelleyTxBody _ lbody _ (TxBodyScriptData _ (L.TxDats' datums) _) _ _) _ fee <-
      H.leftFail $
        makeTransactionBodyAutoBalance
          sbe
          systemStart
          epochInfo
          pparams
          mempty
          mempty
          mempty
          utxo
          content
          addr0
          Nothing -- keys override
    H.noteShow_ fee

    H.noteShowPretty_ lbody

    -- sanity check that the integrity hash was calculated
    lbody ^. L.scriptIntegrityHashTxBodyL /== L.SNothing

    let bodyScriptData = fromList . map fromAlonzoData $ M.elems datums :: Set HashableScriptData

    -- Only supplemental datums are included here
    [ scriptData3 ] === bodyScriptData

    let tx = signShelleyTransaction sbe txBody [wit0]
    txId <- H.noteShow . getTxId $ getTxBody tx

    H.noteShowPretty_ tx

    expectTxSubmissionSuccess =<< submitTx sbe connectionInfo tx

    -- wait till transaction gets included in the block
    _ <- waitForBlocks epochStateView 1

    -- test if it's in UTxO set
    utxo1 <- findAllUtxos epochStateView sbe
    let txUtxo = Utxo.filterWithKey (\(TxIn txId' _) _ -> txId == txId') utxo1
    (length txOuts + 1) === length (toList txUtxo)

    let chainTxOuts =
          reverse
            . drop 1
            . reverse
            . map snd
            . toList
            $ Utxo.filterWithKey (\(TxIn txId' _) _ -> txId == txId') utxo1

    -- check that the transaction's outputs are the same as we've submitted them
    -- i.e. check the datums
    (toCtxUTxOTxOut <$> txOuts) === chainTxOuts

    pure txUtxo

  -- the second transaction using the outputs from the first one
  tx2Utxo <- do
    let txDatum3' = TxOutDatumHash (convert beo) (hashScriptDataBytes scriptData3)
    [(txIn1, _)] <- H.noteShowPretty $ filter (\(_, TxOut _ _ datum _) -> datum == txDatum1) $ toList tx1Utxo -- hash
    [(txIn2, _)] <- H.noteShowPretty $ filter (\(_, TxOut _ _ datum _) -> datum == txDatum2) $ toList tx1Utxo -- inline
    [(txIn3, _)] <- H.noteShowPretty $ filter (\(_, TxOut _ _ datum _) -> datum == txDatum3') $ toList tx1Utxo -- hash of supplemental

    let scriptData4 = unsafeHashableScriptData $ ScriptDataBytes "SUPPLEMENTAL_2"
        txDatum = TxOutSupplementalDatum (convert beo) scriptData4
        txFee = 500
        -- manually balance
        txOutValue = lovelaceToTxOutValue sbe (100_000_000 - txFee)
        txOut = TxOut addr0 txOutValue txDatum ReferenceScriptNone
        -- add actual datum values for the two reference inputs
        txInsReference = TxInsReference beo [txIn1, txIn3] $ pure [scriptData1, scriptData3]

    let content =
          defaultTxBodyContent sbe
            & setTxIns [(txIn2, pure $ KeyWitness KeyWitnessForSpending)]
            & setTxInsReference txInsReference
            & setTxFee (TxFeeExplicit sbe txFee)
            & setTxOuts [txOut]
            & setTxProtocolParams (pure $ pure pparams)

    txBody@(ShelleyTxBody _ lbody _ (TxBodyScriptData _ (L.TxDats' datums) _) _ _) <-
      H.leftFail $ createTransactionBody sbe content

    let bodyScriptData = fromList . map fromAlonzoData $ M.elems datums :: Set HashableScriptData
    -- only hashes (1 & 3) and supplemental (4) are present here
    [scriptData1, scriptData3, scriptData4] === bodyScriptData

    H.noteShowPretty_ txBody

    -- make sure that the script integrity hash was calculated
    lbody ^. L.scriptIntegrityHashTxBodyL /== L.SNothing

    let tx = signShelleyTransaction sbe txBody [wit1]
    txId <- H.noteShow . getTxId $ getTxBody tx

    expectTxSubmissionSuccess =<< submitTx sbe connectionInfo tx

    -- wait till transaction gets included in the block
    _ <- waitForBlocks epochStateView 1

    -- test if the transaction is visible in UTxO set
    utxo1 <- findAllUtxos epochStateView sbe
    let txUtxo = Utxo.filterWithKey (\(TxIn txId' _) _ -> txId == txId') utxo1
    [toCtxUTxOTxOut txOut] === (snd <$> toList txUtxo)
    pure txUtxo

  -- the third transaction using the unspent output from the second one and reference inputs from the first one
  -- tries to add actual datum not present in the reference input and fails on submission
  do
    [(txIn1, _)] <- H.noteShowPretty $ filter (\(_, TxOut _ _ datum _) -> datum == txDatum1) $ toList tx1Utxo -- hash
    (tx2In1, _) <- H.noteShowPrettyM . H.nothingFail . listToMaybe $ toList tx2Utxo

    let txFee = 500
        -- manually balance
        txOutValue = lovelaceToTxOutValue sbe (99_999_500 - txFee)
        txOut = TxOut addr0 txOutValue TxOutDatumNone ReferenceScriptNone
        -- add one reference input with datum hash and its datum, and one superfluous datum
        txInsReference = TxInsReference beo [txIn1] $ pure [scriptData1, scriptData3]

    let content =
          defaultTxBodyContent sbe
            & setTxIns [(tx2In1, pure $ KeyWitness KeyWitnessForSpending)]
            & setTxInsReference txInsReference
            & setTxFee (TxFeeExplicit sbe txFee)
            & setTxOuts [txOut]
            & setTxProtocolParams (pure $ pure pparams)

    txBody@(ShelleyTxBody _ lbody _ (TxBodyScriptData _ (L.TxDats' datums) _) _ _) <-
      H.leftFail $ createTransactionBody sbe content

    let bodyScriptData = fromList . map fromAlonzoData $ M.elems datums :: Set HashableScriptData
    -- all hashes of datums supplied to reference inputs (1 & 3) are present here
    [scriptData1, scriptData3] === bodyScriptData

    H.noteShowPretty_ txBody

    -- make sure that the script integrity hash was calculated
    lbody ^. L.scriptIntegrityHashTxBodyL /== L.SNothing

    let tx = signShelleyTransaction sbe txBody [wit0]
    -- H.noteShowPretty_ tx
    H.noteShow_ . getTxId $ getTxBody tx

    -- transaction contains not allowed supplemental datum, submission has to fail
    submitTx sbe connectionInfo tx >>= \case
      Right () -> do
        H.note_ "Transaction submission succeeded, but it should fail!"
        H.failure
      Left err
        | "NotAllowedSupplementalDatums" `isInfixOf` show err ->
          H.success
        | otherwise -> do
          H.note_ "Unexpected failure"
          H.noteShowPretty_ err
          H.failure


submitTx
  :: MonadTest m
  => MonadIO m
  => HasCallStack
  => ShelleyBasedEra era
  -> LocalNodeConnectInfo
  -> Tx era
  -> m (Either TxValidationErrorInCardanoMode ()) -- ^ 'Left' on ledger error, 'Right' on success
submitTx sbe connectionInfo tx =
  withFrozenCallStack $
    H.evalIO (submitTxToNodeLocal connectionInfo (TxInMode sbe tx)) >>= \case
      Net.Tx.SubmitFail reason -> pure . Left $ reason
      Net.Tx.SubmitSuccess -> pure $ Right ()


expectTxSubmissionSuccess :: HasCallStack
                          => MonadTest m
                          => Either TxValidationErrorInCardanoMode ()
                          -> m ()
expectTxSubmissionSuccess = withFrozenCallStack $ \case
  Left err -> H.noteShowPretty_ err >> H.failure
  Right () -> H.success
