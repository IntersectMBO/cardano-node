{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Api.TxSupplementalDatum
  ( hprop_tx_supp_datum
  )
where

import           Cardano.Api
import           Cardano.Api.Experimental
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Network as Net
import qualified Cardano.Api.Network as Net.Tx
import           Cardano.Api.Shelley

import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Data.Default.Class
import qualified Data.Map.Strict as M
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

hprop_tx_supp_datum :: Property
hprop_tx_supp_datum = integrationRetryWorkspace 2 "api-tx-supp-dat" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
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
  connectionInfo <- node0ConnectionInfo tr
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
          [FromSomeType (proxyToAsType Proxy) WitnessGenesisUTxOKey]
          (signingKey $ paymentKeyInfoPair wallet)

  -- query node for era history
  epochInfo <-
    (H.leftFail <=< H.leftFailM) . H.evalIO $
      executeLocalStateQueryExpr connectionInfo Net.VolatileTip $
        fmap toLedgerEpochInfo <$> queryEraHistory

  let scriptData1 = unsafeHashableScriptData $ ScriptDataBytes "HASH_1" -- hash
      scriptData2 = unsafeHashableScriptData $ ScriptDataBytes "INLINE_1" -- inline
      scriptData3 = unsafeHashableScriptData $ ScriptDataBytes "SUPPLEMENTAL_1" -- supplemental
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

    utxo <- UTxO <$> findAllUtxos epochStateView sbe


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

    lbody ^. L.scriptIntegrityHashTxBodyL /== L.SNothing

    let bodyScriptData = fromList . map fromAlonzoData $ M.elems datums :: Set HashableScriptData

    -- Only supplemental datum are included here
    [ scriptData3 ] === bodyScriptData

    let tx = signShelleyTransaction sbe txBody [wit0]
    txId <- H.noteShow . getTxId $ getTxBody tx

    H.noteShowPretty_ tx

    submitTx sbe connectionInfo tx

    -- wait till transaction gets included in the block
    _ <- waitForBlocks epochStateView 1

    -- test if it's in UTxO set
    utxo1 <- findAllUtxos epochStateView sbe
    let txUtxo = M.filterWithKey (\(TxIn txId' _) _ -> txId == txId') utxo1
    -- H.noteShowPretty_ txUtxo
    (length txOuts + 1) === length txUtxo

    let chainTxOuts =
          reverse
            . drop 1
            . reverse
            . map snd
            . toList
            $ M.filterWithKey (\(TxIn txId' _) _ -> txId == txId') utxo1

    (toCtxUTxOTxOut <$> txOuts) === chainTxOuts

    pure txUtxo

  do
    let txDatum3' = TxOutDatumHash (convert beo) (hashScriptDataBytes scriptData3)
    [(txIn1, _)] <- H.noteShowPretty $ filter (\(_, TxOut _ _ datum _) -> datum == txDatum1) $ toList tx1Utxo
    [(txIn2, _)] <- H.noteShowPretty $ filter (\(_, TxOut _ _ datum _) -> datum == txDatum2) $ toList tx1Utxo
    [(txIn3, _)] <- H.noteShowPretty $ filter (\(_, TxOut _ _ datum _) -> datum == txDatum3') $ toList tx1Utxo

    let scriptData4 = unsafeHashableScriptData $ ScriptDataBytes "SUPPLEMENTAL_2"
        txDatum = TxOutSupplementalDatum (convert beo) scriptData4
        txOutValue = lovelaceToTxOutValue sbe 99_999_500
        txOut = TxOut addr0 txOutValue txDatum ReferenceScriptNone
        txInsReference = TxInsReference beo [txIn1, txIn3] $ pure [scriptData1, scriptData2, scriptData3, scriptData4]

    let content =
          defaultTxBodyContent sbe
            & setTxIns [(txIn2, pure $ KeyWitness KeyWitnessForSpending)]
            & setTxInsReference txInsReference
            & setTxFee (TxFeeExplicit sbe 500)
            & setTxOuts [txOut]
            & setTxProtocolParams (pure $ pure pparams)

    txBody@(ShelleyTxBody _ lbody _ (TxBodyScriptData _ (L.TxDats' datums) _) _ _) <-
      H.leftFail $ createTransactionBody sbe (UTxO tx1Utxo) content

    let bodyScriptData = fromList . map fromAlonzoData $ M.elems datums :: Set HashableScriptData
    [scriptData1, scriptData3, scriptData4] === bodyScriptData

    H.noteShowPretty_ txBody

    lbody ^. L.scriptIntegrityHashTxBodyL /== L.SNothing

    let tx = signShelleyTransaction sbe txBody [wit1]
    H.noteShowPretty_ tx
    txId <- H.noteShow . getTxId $ getTxBody tx

    submitTx sbe connectionInfo tx

    -- wait till transaction gets included in the block
    _ <- waitForBlocks epochStateView 1

    -- test if it's in UTxO set
    utxo1 <- findAllUtxos epochStateView sbe
    let txUtxo = M.filterWithKey (\(TxIn txId' _) _ -> txId == txId') utxo1
    [toCtxUTxOTxOut txOut] === M.elems txUtxo

  H.failure

submitTx
  :: MonadTest m
  => MonadIO m
  => HasCallStack
  => ShelleyBasedEra era
  -> LocalNodeConnectInfo
  -> Tx era
  -> m ()
submitTx sbe connectionInfo tx =
  withFrozenCallStack $
    H.evalIO (submitTxToNodeLocal connectionInfo (TxInMode sbe tx)) >>= \case
      Net.Tx.SubmitFail reason -> H.noteShowPretty_ reason >> H.failure
      Net.Tx.SubmitSuccess -> H.success
