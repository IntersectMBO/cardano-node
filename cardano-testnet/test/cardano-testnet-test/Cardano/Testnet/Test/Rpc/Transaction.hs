{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import           Cardano.CLI.Type.Output (QueryTipLocalStateOutput (..))
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Binary.Version as L
import qualified Cardano.Ledger.Conway.Core as L
import qualified Cardano.Ledger.Conway.PParams as L
import qualified Cardano.Ledger.Plutus as L
import           Cardano.Rpc.Client (Proto)
import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as UtxoRpc
import           Cardano.Rpc.Server.Internal.UtxoRpc.Query ()
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import qualified Data.ByteString.Short as SBS
import           Data.Default.Class
import qualified Data.Map.Strict as M
import qualified Data.Text.Encoding as T
import           GHC.Stack
import           Lens.Micro

import           Testnet.Components.Query
import           Testnet.Process.Run
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

hprop_rpc_transaction :: Property
hprop_rpc_transaction = integrationRetryWorkspace 2 "rpc-tx" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf{tempAbsPath} <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath

  let (ceo, eraProxy) =
        (conwayBasedEra, asType) :: era ~ ConwayEra => (ConwayEraOnwards era, AsType era)
      sbe = convert ceo
      eraName = eraToString sbe
      options = def{cardanoNodeEra = AnyShelleyBasedEra sbe, cardanoEnableRpc = True}

  TestnetRuntime
    { testnetMagic
    , configurationFile
    , testnetNodes = node0@TestnetNode{nodeSprocket} : _
    , wallets = wallet0@(PaymentKeyInfo _ addrTxt0) : (PaymentKeyInfo _ addrTxt1) : _
    } <-
    createAndRunTestnet options def conf

  execConfig <- mkExecConfig tempAbsPath' nodeSprocket testnetMagic
  epochStateView <- getEpochStateView configurationFile (nodeSocketPath node0)

  -- H.noteShowPretty_ pparams
  utxos <- findAllUtxos epochStateView sbe
  H.noteShowPretty_ utxos
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
      let req = Rpc.defMessage
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "readParams")) req

    utxos' <- do
      let req = Rpc.defMessage & #addresses . #items .~ [T.encodeUtf8 addrTxt0]
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "readUtxos")) req
    pure (pparams', utxos')

  txIn0:_ <- mapM (txoRefToTxIn . (^. #txoRef)) $ utxosResponse ^. #items


  let txOut = TxOut addr1 (lovelaceToTxOutValue sbe 200_000_000) TxOutDatumNone ReferenceScriptNone
      content =
        defaultTxBodyContent sbe
          & setTxIns [(txIn0, pure $ KeyWitness KeyWitnessForSpending)]
          & setTxFee (TxFeeExplicit sbe 500)
          & setTxOuts [txOut]
          & setTxProtocolParams (pure $ pure undefined)

  txBody <- H.leftFail $ createTransactionBody sbe content

  let signedTx = signShelleyTransaction sbe txBody [wit0]
  txId <- H.noteShow . getTxId $ getTxBody signedTx

  H.noteShowPretty_ utxosResponse

  H.failure

txoRefToTxIn :: (HasCallStack, MonadTest m) => Proto UtxoRpc.TxoRef -> m TxIn
txoRefToTxIn r = withFrozenCallStack $ do
  txId' <- H.leftFail $ deserialiseFromRawBytes AsTxId $ r ^. #hash
  pure $ TxIn txId' (TxIx . fromIntegral $ r ^. #index)
