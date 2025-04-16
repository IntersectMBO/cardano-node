{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Api.TxSupplementalDatum
  ( hprop_tx_supp_datum
  )
where

import           Cardano.Api
import qualified Cardano.Api.Network as Net
import qualified Cardano.Api.Network as Net.Tx
import           Cardano.Api.Shelley

import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Data.Default.Class
import qualified Data.Map.Strict as M
import           Data.Proxy
import           GHC.Exts (IsList (..))
import           Lens.Micro

import           Testnet.Components.Query
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

hprop_tx_supp_datum :: Property
hprop_tx_supp_datum = integrationRetryWorkspace 2 "api-tx-supp-dat" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf{tempAbsPath} <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath

  let ceo = ConwayEraOnwardsConway
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
  txIn <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  let (PaymentKeyInfo _ addrTxt1) = wallet1
  H.noteShow_ addrTxt1
  addr1 <- H.nothingFail $ deserialiseAddress (AsAddressInEra eraProxy) addrTxt1

  let txOutValue = lovelaceToTxOutValue sbe 1_000_000
      txOut =
        TxOut
          addr1
          txOutValue
          TxOutDatumNone
          ReferenceScriptNone

  -- read key witnesses
  wit <-
    H.leftFailM . H.evalIO $
      readFileTextEnvelopeAnyOf
        [FromSomeType (proxyToAsType Proxy) WitnessGenesisUTxOKey]
        (signingKey $ paymentKeyInfoPair wallet0)

  -- query node for era history
  epochInfo <-
    (H.leftFail <=< H.leftFailM) . H.evalIO $
      executeLocalStateQueryExpr connectionInfo Net.VolatileTip $
        fmap toLedgerEpochInfo <$> queryEraHistory

  -- build a transaction
  let content =
        defaultTxBodyContent sbe
          & setTxIns [(txIn, pure $ KeyWitness KeyWitnessForSpending)]
          & setTxOuts [txOut]
          & setTxProtocolParams (pure $ pure pparams)

  utxo <- UTxO <$> findAllUtxos epochStateView sbe

  BalancedTxBody _ txBody _ _ <-
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
  let tx = signShelleyTransaction sbe txBody [wit]
  txId <- H.noteShow . getTxId $ getTxBody tx

  H.evalIO (submitTxToNodeLocal connectionInfo (TxInMode sbe tx)) >>= \case
    Net.Tx.SubmitFail reason -> H.noteShow_ reason >> H.failure
    Net.Tx.SubmitSuccess -> H.success

  -- wait till transaction gets included in the block
  _ <- waitForBlocks epochStateView 1

  -- test if it's in UTxO set
  utxos1 <- findAllUtxos epochStateView sbe
  let txUtxo = M.filterWithKey (\(TxIn txId' _) _ -> txId == txId') utxos1
  2 === length txUtxo

  [(_, firstTxOut)] <-
    pure . toList $ M.filterWithKey (\(TxIn txId' txIx') _ -> txId == txId' && txIx' == TxIx 0) utxos1

  txOut === firstTxOut

  H.failure
