{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Testnet.Test.Rpc.SearchUtxos
  ( hprop_rpc_search_utxos
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L

import           Cardano.Rpc.Client (Proto)
import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as U5c
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as UtxoRpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as U5c
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as UtxoRpc
import           Cardano.Rpc.Server.Internal.UtxoRpc.Predicate (serialisePaymentCredential)
import           Cardano.Rpc.Server.Internal.UtxoRpc.Type
import           Cardano.Testnet

import           Prelude

import           Control.Monad.Trans.Control (liftBaseOp)
import           Data.ByteString (ByteString)
import           Data.Default.Class
import           GHC.Stack
import           Lens.Micro

import           Testnet.Components.Query (TestnetWaitPeriod (..), getEpochStateView, retryUntilM)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

-- | E2E test for the SearchUtxos gRPC method.
--
-- Spins up a testnet, submits a transaction to create UTxOs at a known address,
-- waits for them to appear in the UTxO set, then exercises SearchUtxos with
-- exact-address and payment-credential predicates.
--
-- Run with:
-- @TASTY_PATTERN='/RPC SearchUtxos/' cabal test cardano-testnet-test@
hprop_rpc_search_utxos :: Property
hprop_rpc_search_utxos = integrationRetryWorkspace 2 "rpc-search-utxos" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf <- mkConf tempAbsBasePath'
  let (ceo, eraProxy) =
        (conwayBasedEra, asType) :: era ~ ConwayEra => (ConwayEraOnwards era, AsType era)
      sbe = convert ceo
      options = def{cardanoNodeEra = AnyShelleyBasedEra sbe, cardanoEnableRpc = RpcEnabled}
      addressInEra = AsAddressInEra eraProxy

  TestnetRuntime
    { configurationFile
    , testnetNodes = node0 : _
    , wallets = wallet0@(PaymentKeyInfo _ addrTxt0) : (PaymentKeyInfo _ addrTxt1) : _
    } <-
    createAndRunTestnet options def conf

  epochStateView <- getEpochStateView configurationFile $ nodeSocketPath node0
  rpcSocket <- H.note . unFile $ nodeRpcSocketPath node0

  H.noteShow_ addrTxt0
  address0 <- H.nothingFail $ deserialiseAddress addressInEra addrTxt0

  H.noteShow_ addrTxt1
  address1 <- H.nothingFail $ deserialiseAddress addressInEra addrTxt1

  wit0 :: ShelleyWitnessSigningKey <-
    H.leftFailM . H.evalIO $
      readFileTextEnvelopeAnyOf
        [FromSomeType asType WitnessGenesisUTxOKey]
        (signingKey $ paymentKeyInfoPair wallet0)

  let rpcServer = Rpc.ServerUnix rpcSocket

  ----------------------
  -- Build and submit tx
  ----------------------
  (pparamsResponse, initialSearch) <- H.noteShowM . H.evalIO . Rpc.withConnection def rpcServer $ \conn -> do
    pparams' <-
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "readParams")) def

    search' <-
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "searchUtxos")) $
        def & U5c.predicate .~ addressPredicate address0
    pure (pparams', search')

  pparams <- H.leftFail $ utxoRpcPParamsToProtocolParams (convert ceo) $ pparamsResponse ^. U5c.values . U5c.cardano

  txOut0 : _ <- H.noteShow $ initialSearch ^. U5c.items
  txIn0 <- txoRefToTxIn $ txOut0 ^. U5c.txoRef

  outputCoin <- H.leftFail $ txOut0 ^. U5c.cardano . U5c.coin . to utxoRpcBigIntToInteger
  let amount = 200_000_000
      fee = 500
      change = outputCoin - amount - fee
      txOut = TxOut address1 (lovelaceToTxOutValue sbe $ L.Coin amount) TxOutDatumNone ReferenceScriptNone
      changeTxOut = TxOut address0 (lovelaceToTxOutValue sbe $ L.Coin change) TxOutDatumNone ReferenceScriptNone
      content =
        defaultTxBodyContent sbe
          & setTxIns [(txIn0, pure $ KeyWitness KeyWitnessForSpending)]
          & setTxFee (TxFeeExplicit sbe (L.Coin fee))
          & setTxOuts [txOut, changeTxOut]
          & setTxProtocolParams (pure . pure $ LedgerProtocolParameters pparams)

  txBody <- H.leftFail $ createTransactionBody sbe content
  let signedTx = signShelleyTransaction sbe txBody [wit0]

  liftBaseOp (Rpc.withConnection def rpcServer) $ \conn -> do
    _submitResponse <- H.noteShowM . H.evalIO $
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.SubmitService "submitTx")) $
        def & U5c.tx .~ (def & U5c.raw .~ serialiseToCBOR signedTx)

    -------------------------------------------
    -- Wait for UTxOs to appear at address1
    -------------------------------------------
    H.note_ $ "Wait for 2 UTxOs at address " <> show addrTxt1
    utxosAtAddress1 <- retryUntilM epochStateView (WaitForBlocks 10)
      (do searchResult <- H.evalIO $
            Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "searchUtxos")) $
              def & U5c.predicate .~ addressPredicate address1
          pure $ searchResult ^. U5c.items
      )
      (\xs -> length xs == 2)

    -------------------------------------------
    -- Test 1: exact address predicate returns correct amounts
    -------------------------------------------
    H.note_ "Test 1: Verify exact address search returns correct coin values"
    let outputAmounts = map (^. U5c.cardano . U5c.coin) utxosAtAddress1
    H.assertWith outputAmounts $ elem (inject amount)

    -------------------------------------------
    -- Test 2: payment credential predicate
    -------------------------------------------
    H.note_ "Test 2: Verify payment credential predicate matches same UTxOs"
    let paymentCredBytes :: ByteString
        paymentCredBytes = case address1 of
          AddressInEra ShelleyAddressInEra{} (ShelleyAddress _ payCred _) ->
            serialisePaymentCredential $ fromShelleyPaymentCredential payCred
          _ -> error "Expected a Shelley address"
        paymentPredicate :: Proto UtxoRpc.UtxoPredicate
        paymentPredicate =
          def
            & U5c.match
              .~ ( def
                     & U5c.cardano
                       .~ (def & U5c.address .~ (def & U5c.paymentPart .~ paymentCredBytes))
                 )
    payCredSearch <- H.noteShowM . H.evalIO $
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "searchUtxos")) $
        def & U5c.predicate .~ paymentPredicate

    let payCredUtxos = payCredSearch ^. U5c.items
    H.assertWith payCredUtxos $ \xs -> length xs == 2

    -------------------------------------------
    -- Test 3: search with non-matching address returns empty
    -------------------------------------------
    H.note_ "Test 3: Verify search with non-existent address returns empty"
    let bogusAddressPredicate :: Proto UtxoRpc.UtxoPredicate
        bogusAddressPredicate =
          def
            & U5c.match
              .~ ( def
                     & U5c.cardano
                       .~ (def & U5c.address .~ (def & U5c.exactAddress .~ "\x00\x01\x02\x03"))
                 )
    emptySearch <- H.noteShowM . H.evalIO $
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "searchUtxos")) $
        def & U5c.predicate .~ bogusAddressPredicate

    H.assertWith (emptySearch ^. U5c.items) null

    -------------------------------------------
    -- Test 4: search without predicate returns all UTxOs
    -------------------------------------------
    H.note_ "Test 4: Verify search without predicate returns all UTxOs"
    allUtxosSearch <- H.noteShowM . H.evalIO $
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "searchUtxos")) def

    H.assertWith (allUtxosSearch ^. U5c.items) $ \xs -> length xs > 2

txoRefToTxIn :: (HasCallStack, MonadTest m) => Proto UtxoRpc.TxoRef -> m TxIn
txoRefToTxIn r = withFrozenCallStack $ do
  txId' <- H.leftFail $ deserialiseFromRawBytes AsTxId $ r ^. U5c.hash
  pure $ TxIn txId' (TxIx . fromIntegral $ r ^. U5c.index)

addressPredicate :: IsCardanoEra era => AddressInEra era -> Proto UtxoRpc.UtxoPredicate
addressPredicate address =
  def
    & U5c.match
      .~ ( def
             & U5c.cardano
               .~ (def & U5c.address .~ (def & U5c.exactAddress .~ serialiseToRawBytes address))
         )
