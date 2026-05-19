{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
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
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as U5c hiding (cardano, items, tx)
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as UtxoRpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as U5c
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as UtxoRpc
import           Cardano.Rpc.Server.Internal.UtxoRpc.Type
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Control.Monad.Trans.Control (liftBaseOp)
import           Data.Default.Class
import qualified Data.Text.Encoding as T
import           GHC.Stack
import           Lens.Micro

import           Testnet.Components.Query (TestnetWaitPeriod (..), getEpochStateView, retryUntilM)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

import           RIO (ByteString)

-- | Run with:
-- @TASTY_PATTERN='/RPC Transaction Submit/' cabal test cardano-testnet-test@
hprop_rpc_transaction :: Property
hprop_rpc_transaction = integrationRetryWorkspace 2 "rpc-tx" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf <- mkConf tempAbsBasePath'
  let (ceo, eraProxy) =
        (conwayBasedEra, asType) :: era ~ ConwayEra => (ConwayEraOnwards era, AsType era)
      sbe = convert ceo
      creationOptions = def{creationEra = AnyShelleyBasedEra sbe}
      runtimeOptions = def{runtimeEnableRpc = RpcEnabled}
      addrInEra = AsAddressInEra eraProxy

  TestnetRuntime
    { configurationFile
    , testnetNodes = node0 : _
    , wallets = wallet0@(PaymentKeyInfo _ addrTxt0) : (PaymentKeyInfo _ addrTxt1) : _
    } <-
    createAndRunTestnet creationOptions runtimeOptions conf

  epochStateView <- getEpochStateView configurationFile $ nodeSocketPath node0
  rpcSocket <- H.note . unFile $ nodeRpcSocketPath node0

  -- prepare tx inputs and output address
  H.noteShow_ addrTxt0
  addr0 <- H.nothingFail $ deserialiseAddress addrInEra addrTxt0

  H.noteShow_ addrTxt1
  addr1 <- H.nothingFail $ deserialiseAddress addrInEra addrTxt1

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
      let req = def -- & # U5c.keys .~ [T.encodeUtf8 addrTxt0]
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "readUtxos")) req
    pure (pparams', utxos')

  pparams <- H.leftFail $ utxoRpcPParamsToProtocolParams (convert ceo) $ pparamsResponse ^. U5c.values . U5c.cardano

  txOut0 : _ <- H.noteShowM . flip filterM (utxosResponse ^. U5c.items) $ \utxo -> do
    utxoAddress <- deserialiseAddressBs addrInEra $ utxo ^. U5c.cardano . U5c.address
    pure $ addr0 == utxoAddress
  txIn0 <- txoRefToTxIn $ txOut0 ^. U5c.txoRef

  outputCoin <- H.leftFail $ txOut0 ^. U5c.cardano . U5c.coin . to utxoRpcBigIntToInteger
  let amount = 200_000_000
      fee = 500
      change = outputCoin - amount - fee
      txOut = TxOut addr1 (lovelaceToTxOutValue sbe $ L.Coin amount) TxOutDatumNone ReferenceScriptNone
      changeTxOut = TxOut addr0 (lovelaceToTxOutValue sbe $ L.Coin change) TxOutDatumNone ReferenceScriptNone
      content =
        defaultTxBodyContent sbe
          & setTxIns [(txIn0, pure $ KeyWitness KeyWitnessForSpending)]
          & setTxFee (TxFeeExplicit sbe (L.Coin fee))
          & setTxOuts [txOut, changeTxOut]
          & setTxProtocolParams (pure . pure $ LedgerProtocolParameters pparams)

  txBody <- H.leftFail $ createTransactionBody sbe content

  let signedTx = signShelleyTransaction sbe txBody [wit0]
  txId' <- H.noteShow . getTxId $ getTxBody signedTx

  H.noteShowPretty_ utxosResponse

  liftBaseOp (Rpc.withConnection def rpcServer) $ \conn -> do
    submitResponse <- H.noteShowM . H.evalIO $
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.SubmitService "submitTx")) $
        def & U5c.tx .~ (def & U5c.raw .~ serialiseToCBOR signedTx)

    submittedTxId <- H.leftFail . deserialiseFromRawBytes AsTxId $ submitResponse ^. U5c.ref

    H.note_ "Ensure that submitTx returns the same transaction ID as the locally computed signed transaction ID"
    txId' === submittedTxId

    -- TODO use searchUtxos when available
    H.note_ $ "Ensure that there are 2 UTXOs in the address " <> show addrTxt1
    utxosForAddress <- retryUntilM epochStateView (WaitForBlocks 10)
      (do utxos <- H.evalIO $
            Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "readUtxos")) def
          flip filterM (utxos ^. U5c.items) $ \utxo -> do
            utxoAddress <- deserialiseAddressBs addrInEra $ utxo ^. U5c.cardano . U5c.address
            pure $ addr1 == utxoAddress
      )
      (\xs -> length xs == 2)

    let outputsAmounts = map (^. U5c.cardano . U5c.coin) utxosForAddress
    H.note_ $ "Ensure that the output sent is one of the utxos for the address " <> show addrTxt1
    H.assertWith outputsAmounts $ elem (inject amount)

txoRefToTxIn :: (HasCallStack, MonadTest m) => Proto UtxoRpc.TxoRef -> m TxIn
txoRefToTxIn r = withFrozenCallStack $ do
  txId' <- H.leftFail $ deserialiseFromRawBytes AsTxId $ r ^. U5c.hash
  pure $ TxIn txId' (TxIx . fromIntegral $ r ^. U5c.index)

deserialiseAddressBs :: (MonadTest m, SerialiseAddress c) => AsType c -> ByteString -> m c
deserialiseAddressBs addrInEra = H.nothingFail . deserialiseAddress addrInEra <=< H.leftFail . T.decodeUtf8'
