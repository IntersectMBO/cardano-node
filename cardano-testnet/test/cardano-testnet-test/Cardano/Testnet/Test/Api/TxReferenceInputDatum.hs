{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Api.TxReferenceInputDatum
  ( hprop_tx_refin_datum
  )
where

import           Cardano.Api hiding (txId)
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Experimental.Tx as Exp
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Network as Net
import qualified Cardano.Api.UTxO as Utxo

import           Cardano.Ledger.Plutus.Data (hashData)
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Data.Default.Class
import           Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
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

  let era = Exp.ConwayEra
      sbe = convert era
      ceo = convert era
      beo = convert ceo
      eraProxy = proxyToAsType Proxy
      creationOptions = def{creationEra = AnyShelleyBasedEra sbe}

  tr@TestnetRuntime
    { configurationFile
    , testnetNodes = node0 : _
    , wallets = wallet0@(PaymentKeyInfo _ addrTxt0) : wallet1 : _
    } <-
    createAndRunTestnet creationOptions def conf

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
        txOuts :: [TxOut CtxTx ConwayEra]
        txOuts =
          [ TxOut addr1 txOutValue txDatum1 ReferenceScriptNone
          , TxOut addr1 txOutValue txDatum2 ReferenceScriptNone
          , TxOut addr1 txOutValue txDatum3 ReferenceScriptNone
          ]

        -- build a transaction
        expTxOuts = map (Exp.TxOut . toShelleyTxOut sbe . toCtxUTxOTxOut) txOuts
        -- toCtxUTxOTxOut strips the TxOutSupplementalDatum marker, so we must pass
        -- supplemental datums explicitly
        supplementalDatums = Exp.obtainCommonConstraints era $ M.fromList
          [ let ledgerData = toAlonzoData @(ShelleyLedgerEra ConwayEra) sd
            in (hashData ledgerData, ledgerData)
          | sd <- [scriptData3]
          ]
        content =
          Exp.defaultTxBodyContent
            & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
            & Exp.setTxOuts expTxOuts
            & Exp.setTxProtocolParams (unLedgerProtocolParameters pparams)
            & Exp.setTxSupplementalDatums supplementalDatums

    utxo <- findAllUtxos epochStateView sbe
    let ledgerUtxo = Utxo.toShelleyUTxO sbe utxo

    (unsignedTx@(Exp.UnsignedTx ledgerTx), _finalContent) <-
      H.leftFail $
        Exp.makeTransactionBodyAutoBalance @ConwayEra
          systemStart
          epochInfo
          (unLedgerProtocolParameters pparams)
          mempty
          mempty
          ledgerUtxo
          content
          addr0
          Nothing -- keys override

    let lbody = ledgerTx ^. L.bodyTxL
        fee = Exp.getUnsignedTxFee unsignedTx
    H.noteShow_ fee

    H.noteShowPretty_ lbody

    -- sanity check that the integrity hash was calculated
    lbody ^. L.scriptIntegrityHashTxBodyL /== L.SNothing

    let L.TxDats datums = ledgerTx ^. L.witsTxL . L.datsTxWitsL
        bodyScriptData = fromList . map fromAlonzoData $ M.elems datums :: Set HashableScriptData

    -- Only supplemental datums are included here
    [ scriptData3 ] === bodyScriptData

    let keyWit = Exp.makeKeyWitness era unsignedTx wit0
        Exp.SignedTx signedLedgerTx = Exp.signTx era [] [keyWit] unsignedTx
    txId <- H.noteShow . Exp.obtainCommonConstraints era . TxId $ Exp.hashTxBody (signedLedgerTx ^. L.bodyTxL)

    let signedTx = ShelleyTx sbe signedLedgerTx
    H.noteShowPretty_ signedTx

    expectTxSubmissionSuccess =<< submitTx sbe connectionInfo signedTx

    -- wait till transaction gets included in the block
    txUtxo <- retryUntilM epochStateView (WaitForBlocks 5)
      (Utxo.filterWithKey (\(TxIn txId' _) _ -> txId == txId') <$> findAllUtxos epochStateView sbe)
      (not . Utxo.null)
    (length txOuts + 1) === length (toList txUtxo)

    let chainTxOuts =
          reverse
            . drop 1
            . reverse
            . map snd
            . toList
            $ txUtxo

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
        -- add actual datum values for the two reference inputs via supplemental datums
        ledgerPparams = unLedgerProtocolParameters pparams
        supplementalDatums = Exp.obtainCommonConstraints era $ M.fromList
          [ let ledgerData = toAlonzoData @(ShelleyLedgerEra ConwayEra) sd
            in (hashData ledgerData, ledgerData)
          | sd <- [scriptData1, scriptData3, scriptData4]
          ]

    let content =
          Exp.defaultTxBodyContent
            & Exp.setTxIns [(txIn2, Exp.AnyKeyWitnessPlaceholder)]
            & Exp.setTxInsReference (Exp.TxInsReference [txIn1, txIn3] Set.empty)
            & Exp.setTxFee txFee
            & Exp.setTxOuts [Exp.TxOut . toShelleyTxOut sbe $ toCtxUTxOTxOut txOut]
            & Exp.setTxProtocolParams ledgerPparams
            & Exp.setTxSupplementalDatums supplementalDatums

    unsignedTx@(Exp.UnsignedTx ledgerTx) <-
      H.leftFail $ Exp.makeUnsignedTx era content

    let lbody = ledgerTx ^. L.bodyTxL
        L.TxDats datums = ledgerTx ^. L.witsTxL . L.datsTxWitsL
        bodyScriptData = fromList . map fromAlonzoData $ M.elems datums :: Set HashableScriptData
    -- only hashes (1 & 3) and supplemental (4) are present here
    [scriptData1, scriptData3, scriptData4] === bodyScriptData

    H.noteShowPretty_ lbody

    -- make sure that the script integrity hash was calculated
    lbody ^. L.scriptIntegrityHashTxBodyL /== L.SNothing

    let keyWit = Exp.makeKeyWitness era unsignedTx wit1
        Exp.SignedTx signedLedgerTx = Exp.signTx era [] [keyWit] unsignedTx
    txId <- H.noteShow . Exp.obtainCommonConstraints era . TxId $ Exp.hashTxBody (signedLedgerTx ^. L.bodyTxL)

    expectTxSubmissionSuccess =<< submitTx sbe connectionInfo (ShelleyTx sbe signedLedgerTx)

    -- wait till transaction gets included in the block
    txUtxo <- retryUntilM epochStateView (WaitForBlocks 5)
      (Utxo.filterWithKey (\(TxIn txId' _) _ -> txId == txId') <$> findAllUtxos epochStateView sbe)
      (not . Utxo.null)

    -- test if the transaction is visible in UTxO set
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
        ledgerPparams3 = unLedgerProtocolParameters pparams
        supplementalDatums3 = Exp.obtainCommonConstraints era $ M.fromList
          [ let ledgerData = toAlonzoData @(ShelleyLedgerEra ConwayEra) sd
            in (hashData ledgerData, ledgerData)
          | sd <- [scriptData1, scriptData3]
          ]

    let content =
          Exp.defaultTxBodyContent
            & Exp.setTxIns [(tx2In1, Exp.AnyKeyWitnessPlaceholder)]
            & Exp.setTxInsReference (Exp.TxInsReference [txIn1] Set.empty)
            & Exp.setTxFee txFee
            & Exp.setTxOuts [Exp.TxOut . toShelleyTxOut sbe $ toCtxUTxOTxOut txOut]
            & Exp.setTxProtocolParams ledgerPparams3
            & Exp.setTxSupplementalDatums supplementalDatums3

    unsignedTx3@(Exp.UnsignedTx ledgerTx3) <-
      H.leftFail $ Exp.makeUnsignedTx era content

    let lbody = ledgerTx3 ^. L.bodyTxL
        L.TxDats datums = ledgerTx3 ^. L.witsTxL . L.datsTxWitsL
        bodyScriptData = fromList . map fromAlonzoData $ M.elems datums :: Set HashableScriptData
    -- all hashes of datums supplied to reference inputs (1 & 3) are present here
    [scriptData1, scriptData3] === bodyScriptData

    H.noteShowPretty_ lbody

    -- make sure that the script integrity hash was calculated
    lbody ^. L.scriptIntegrityHashTxBodyL /== L.SNothing

    let keyWit = Exp.makeKeyWitness era unsignedTx3 wit0
        Exp.SignedTx signedLedgerTx = Exp.signTx era [] [keyWit] unsignedTx3
    Exp.obtainCommonConstraints era $
      H.noteShow_ . TxId $ Exp.hashTxBody (signedLedgerTx ^. L.bodyTxL)

    -- transaction contains not allowed supplemental datum, submission has to fail
    submitTx sbe connectionInfo (ShelleyTx sbe signedLedgerTx) >>= \case
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
      TxSubmitFail reason -> pure . Left $ reason
      TxSubmitSuccess -> pure $ Right ()
      TxSubmitError err -> do
        H.annotate $ "submitTxToNodeLocal connection error: " <> show err
        H.failure


expectTxSubmissionSuccess :: HasCallStack
                          => MonadTest m
                          => Either TxValidationErrorInCardanoMode ()
                          -> m ()
expectTxSubmissionSuccess = withFrozenCallStack $ \case
  Left err -> H.noteShowPretty_ err >> H.failure
  Right () -> H.success
