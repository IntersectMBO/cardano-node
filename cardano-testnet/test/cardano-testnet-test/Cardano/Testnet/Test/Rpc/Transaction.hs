{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Testnet.Test.Rpc.Transaction
  ( hprop_rpc_transaction
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L

import           Cardano.Rpc.Client (Proto)
import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as UtxoRpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as UtxoRpc
import           Cardano.Rpc.Server.Internal.UtxoRpc.Query ()
import           Cardano.Rpc.Server.Internal.UtxoRpc.Type
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Control.Monad.Fix
import           Data.Default.Class
import qualified Data.Text.Encoding as T
import           GHC.Stack
import           Lens.Micro

import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

import           RIO (threadDelay)

hprop_rpc_transaction :: Property
hprop_rpc_transaction = integrationRetryWorkspace 2 "rpc-tx" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf <- mkConf tempAbsBasePath'
  let (ceo, eraProxy) =
        (conwayBasedEra, asType) :: era ~ ConwayEra => (ConwayEraOnwards era, AsType era)
      sbe = convert ceo
      options = def{cardanoNodeEra = AnyShelleyBasedEra sbe, cardanoEnableRpc = True}

  TestnetRuntime
    { testnetNodes = node0 : _
    , wallets = wallet0@(PaymentKeyInfo _ addrTxt0) : (PaymentKeyInfo _ addrTxt1) : _
    } <-
    createAndRunTestnet options def conf

  rpcSocket <- H.note . unFile $ nodeRpcSocketPath node0

  -- prepare tx inputs and output address
  H.noteShow_ addrTxt0
  addr0 <- H.nothingFail $ deserialiseAddress (AsAddressInEra eraProxy) addrTxt0

  H.noteShow_ addrTxt1
  addr1 <- H.nothingFail $ deserialiseAddress (AsAddressInEra eraProxy) addrTxt1

  -- read key witnesses
  wit0 :: ShelleyWitnessSigningKey <-
    H.leftFailM . H.evalIO $
      readFileTextEnvelopeAnyOf
        [FromSomeType asType WitnessGenesisUTxOKey]
        (signingKey $ paymentKeyInfoPair wallet0)

  --------------
  -- RPC queries
  --------------
  let rpcServer = Rpc.ServerUnix rpcSocket
  (pparamsResponse, utxosResponse) <- H.noteShowM . H.evalIO . Rpc.withConnection def rpcServer $ \conn -> do
    pparams' <- do
      let req = def
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "readParams")) req

    utxos' <- do
      let req = def & #addresses . #items .~ [T.encodeUtf8 addrTxt0]
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "readUtxos")) req
    pure (pparams', utxos')

  pparams <- H.leftFail $ utxoRpcPParamsToProtocolParams (convert ceo) $ pparamsResponse ^. #values . #cardano

  txOut0 : _ <- H.noteShow $ utxosResponse ^. #items
  txIn0 <- txoRefToTxIn $ txOut0 ^. #txoRef

  let outputCoin = txOut0 ^. #cardano . #coin . to fromIntegral
      amount = 200_000_000
      fee = 500
      change = outputCoin - amount - fee
      txOut = TxOut addr1 (lovelaceToTxOutValue sbe $ L.Coin amount) TxOutDatumNone ReferenceScriptNone
      changeTxOut = TxOut addr0 (lovelaceToTxOutValue sbe $ L.Coin change) TxOutDatumNone ReferenceScriptNone
      content =
        defaultTxBodyContent sbe
          & setTxIns [(txIn0, pure $ KeyWitness KeyWitnessForSpending)]
          & setTxFee (TxFeeExplicit sbe 500)
          & setTxOuts [txOut, changeTxOut]
          & setTxProtocolParams (pure . pure $ LedgerProtocolParameters pparams)

  txBody <- H.leftFail $ createTransactionBody sbe content

  let signedTx = signShelleyTransaction sbe txBody [wit0]
  txId' <- H.noteShow . getTxId $ getTxBody signedTx

  H.noteShowPretty_ utxosResponse

  (utxos, submitResponse) <- H.noteShowM . H.evalIO . Rpc.withConnection def rpcServer $ \conn -> do
    submitResponse <-
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.SubmitService "submitTx")) $
        def & #tx .~ [def & #raw .~ serialiseToCBOR signedTx]

    fix $ \loop -> do
      resp <- Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "readParams")) def

      let previousBlockNo = pparamsResponse ^. #ledgerTip . #height
          currentBlockNo = resp ^. #ledgerTip . #height
      -- wait for 2 blocks
      when (previousBlockNo + 1 >= currentBlockNo) $ do
        threadDelay 500_000
        loop

    utxos <-
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "readUtxos")) $
        def & #addresses . #items .~ [T.encodeUtf8 addrTxt1]
    pure (utxos, submitResponse)

  submittedTxIds <- forM (submitResponse ^. #results) $ \res -> do
    let mErr = res ^. #maybe'errorMessage
        mTxId = res ^. #maybe'ref
    case (mErr, mTxId) of
      (Just err, Nothing) -> H.noteShow_ err >> H.failure
      (Nothing, Just txId'') ->
        H.leftFail $ deserialiseFromRawBytes AsTxId txId''
      _ -> do
        H.note_ $ "Protocol error: " <> show res
        H.failure

  H.note_ "Ensure that submitted transaction ID is in the submitted transactions list"
  [txId'] === submittedTxIds

  H.note_ $ "Enxure that there are 2 UTXOs in the address " <> show addrTxt1
  2 === length (utxos ^. #items)

  let outputsAmounts = map (^. #cardano . #coin) $ utxos ^. #items
  H.note_ $ "Ensure that the output sent is one of the utxos for the address " <> show addrTxt1
  H.assertWith outputsAmounts $ elem (fromIntegral amount)

txoRefToTxIn :: (HasCallStack, MonadTest m) => Proto UtxoRpc.TxoRef -> m TxIn
txoRefToTxIn r = withFrozenCallStack $ do
  txId' <- H.leftFail $ deserialiseFromRawBytes AsTxId $ r ^. #hash
  pure $ TxIn txId' (TxIx . fromIntegral $ r ^. #index)
