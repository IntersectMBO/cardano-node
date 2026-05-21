{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Rpc.SearchUtxos
  ( hprop_rpc_search_utxos
  )
where

import           Cardano.Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Experimental.Tx as Exp
import qualified Cardano.Api.Ledger as L

import           Cardano.Rpc.Client (Proto)
import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as U5c hiding (cardano)
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as UtxoRpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as U5c
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as UtxoRpc
import           Cardano.Rpc.Server.Internal.UtxoRpc.Predicate (serialisePaymentCredential)
import           Cardano.Rpc.Server.Internal.UtxoRpc.Type
import           Cardano.Testnet

import           Prelude

import           Control.Exception (try)
import           Control.Monad.Trans.Control (liftBaseOp)
import           Data.ByteString (ByteString)
import           Data.Default.Class
import           GHC.Stack
import           Lens.Micro
import           Network.GRPC.Spec (GrpcError (..), GrpcException (..))

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
  let era = Exp.ConwayEra
      sbe = convert era
      creationOptions = def{creationEra = AnyShelleyBasedEra sbe}
      runtimeOptions = def{runtimeEnableRpc = RpcEnabled}
      addressInEra = asAddressInEra sbe

  TestnetRuntime
    { configurationFile
    , testnetNodes = node0 : _
    , wallets = wallet0@(PaymentKeyInfo _ addressText0) : (PaymentKeyInfo _ addressText1) : _
    } <-
    createAndRunTestnet creationOptions runtimeOptions conf

  epochStateView <- getEpochStateView configurationFile $ nodeSocketPath node0
  rpcSocket <- H.note . unFile $ nodeRpcSocketPath node0

  H.noteShow_ addressText0
  address0 <- H.nothingFail $ deserialiseAddress addressInEra addressText0

  H.noteShow_ addressText1
  address1 <- H.nothingFail $ deserialiseAddress addressInEra addressText1

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

  pparams <- H.leftFail $ utxoRpcPParamsToProtocolParams era $ pparamsResponse ^. U5c.values . U5c.cardano

  txOut0 : _ <- H.noteShow $ initialSearch ^. U5c.items
  txIn0 <- txoRefToTxIn $ txOut0 ^. U5c.txoRef

  outputCoin <- H.leftFail $ txOut0 ^. U5c.cardano . U5c.coin . to utxoRpcBigIntToInteger
  let amount = 200_000_000
      fee = 500
      change = outputCoin - amount - fee
      mkOut ledgerAddress coin = Exp.obtainCommonConstraints era $
        Exp.TxOut $ L.mkBasicTxOut ledgerAddress $ L.inject $ L.Coin coin
      content =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn0, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxFee (L.Coin fee)
          & Exp.setTxOuts [mkOut (toShelleyAddr address1) amount, mkOut (toShelleyAddr address0) change]
          & Exp.setTxProtocolParams pparams

  unsignedTx <- H.leftFail $ Exp.makeUnsignedTx era content
  let keyWit = Exp.makeKeyWitness era unsignedTx wit0
      Exp.SignedTx signedLedgerTx = Exp.signTx era [] [keyWit] unsignedTx

  liftBaseOp (Rpc.withConnection def rpcServer) $ \conn -> do
    _submitResponse <- H.noteShowM . H.evalIO $
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.SubmitService "submitTx")) $
        def & U5c.tx .~ (def & U5c.raw .~ serialiseToRawBytes (Exp.SignedTx signedLedgerTx))

    -------------------------------------------
    -- Wait for UTxOs to appear at address1
    -------------------------------------------
    H.note_ $ "Wait for 2 UTxOs at address " <> show addressText1
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
    H.note_ "Test 2: Verify exact address + payment credential predicate matches same UTxOs"
    paymentCredBytes <- case address1 of
      AddressInEra ShelleyAddressInEra{} (ShelleyAddress _ payCred _) ->
        pure $ serialisePaymentCredential $ fromShelleyPaymentCredential payCred
      _ -> do
        H.note_ "Expected a Shelley address"
        H.failure
    let paymentPredicate :: Proto UtxoRpc.UtxoPredicate
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
    -- Test 3: search with invalid address is rejected
    -------------------------------------------
    H.note_ "Test 3: Verify search with invalid address returns GrpcInvalidArgument"
    let bogusAddressPredicate :: Proto UtxoRpc.UtxoPredicate
        bogusAddressPredicate =
          def
            & U5c.match
              .~ ( def
                     & U5c.cardano
                       .~ (def & U5c.address .~ (def & U5c.exactAddress .~ "\x00\x01\x02\x03"))
                 )
    bogusResult <- H.evalIO . try @GrpcException $
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "searchUtxos")) $
        def & U5c.predicate .~ bogusAddressPredicate

    case bogusResult of
      Left err -> grpcError err === GrpcInvalidArgument
      Right _ -> do
        H.note_ "Expected GrpcInvalidArgument but search succeeded"
        H.failure

    -------------------------------------------
    -- Test 4: search without predicate returns all UTxOs
    -------------------------------------------
    H.note_ "Test 4: Verify search without predicate returns all UTxOs"
    allUtxosSearch <- H.noteShowM . H.evalIO $
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "searchUtxos")) def

    H.assertWith (allUtxosSearch ^. U5c.items) $ \xs -> length xs > 2

asAddressInEra :: ShelleyBasedEra era -> AsType (AddressInEra era)
asAddressInEra s = shelleyBasedEraConstraints s $ AsAddressInEra asType

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
