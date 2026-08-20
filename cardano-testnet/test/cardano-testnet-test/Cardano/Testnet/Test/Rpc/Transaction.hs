{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Rpc.Transaction
  ( hprop_rpc_transaction
  )
where

import           Cardano.Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Experimental.Tx as Exp
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as U5c hiding (cardano)
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as UtxoRpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as U5c
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as UtxoRpc
import           Cardano.Rpc.Server.Internal.UtxoRpc.Predicate (exactAddressPredicate)
import           Cardano.Rpc.Server.Internal.UtxoRpc.Type
import           Cardano.Testnet

import           Prelude

import           Control.Monad.Trans.Control (liftBaseOp)
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Lens.Micro

import           Testnet.Components.Query (TestnetWaitPeriod (..), getEpochStateView, retryUntilM)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

-- | Run with:
-- @TASTY_PATTERN='/RPC Transaction Submit/' cabal test cardano-testnet-test@
hprop_rpc_transaction :: Property
hprop_rpc_transaction = integrationRetryWorkspace 2 "rpc-tx" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf <- mkConf tempAbsBasePath'
  let era = Exp.ConwayEra
      sbe = convert era
      creationOptions = def{creationEra = AnyShelleyBasedEra sbe}
      runtimeOptions = def{runtimeEnableRpc = RpcEnabled}
      addressInEra = asAddressInEra sbe

  TestnetRuntime
    { configurationFile
    , testnetNodes = node0 :| _
    , wallets = wallet0@(PaymentKeyInfo _ addressText0) : (PaymentKeyInfo _ addressText1) : _
    } <-
    createAndRunTestnet creationOptions runtimeOptions conf

  epochStateView <- getEpochStateView configurationFile $ nodeSocketPath node0
  rpcSocket <- H.note . unFile $ nodeRpcSocketPath node0

  -- prepare tx inputs and output address
  H.noteShow_ addressText0
  address0 <- H.nothingFail $ deserialiseAddress addressInEra addressText0

  H.noteShow_ addressText1
  address1 <- H.nothingFail $ deserialiseAddress addressInEra addressText1

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
  (pparamsResponse, searchResponse) <- H.noteShowM . H.evalIO . Rpc.withConnection def rpcServer $ \conn -> do
    pparams' <-
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "readParams")) def

    search' <-
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "searchUtxos")) $
        def & U5c.predicate .~ exactAddressPredicate address0
    pure (pparams', search')

  pparams <- H.leftFail $ utxoRpcPParamsToProtocolParams era $ pparamsResponse ^. U5c.values . U5c.cardano

  txOut0 : _ <- H.noteShow $ searchResponse ^. U5c.items
  txIn0 <- H.leftFail . txoRefUtxoRpcToTxIn $ txOut0 ^. U5c.txoRef

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
  txId' <- H.noteShow . Exp.obtainCommonConstraints era . TxId $ Exp.hashTxBody (signedLedgerTx ^. L.bodyTxL)

  H.noteShowPretty_ searchResponse

  liftBaseOp (Rpc.withConnection def rpcServer) $ \conn -> do
    submitResponse <- H.noteShowM . H.evalIO $
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.SubmitService "submitTx")) $
        def & U5c.tx .~ (def & U5c.raw .~ serialiseToRawBytes (Exp.SignedTx signedLedgerTx))

    submittedTxId <- H.leftFail . deserialiseFromRawBytes AsTxId $ submitResponse ^. U5c.ref

    H.note_ "Ensure that submitTx returns the same transaction ID as the locally computed signed transaction ID"
    txId' === submittedTxId

    H.note_ $ "Ensure that there are 2 UTXOs in the address " <> show addressText1
    utxosForAddress <- retryUntilM epochStateView (WaitForBlocks 10)
      (do searchResult <- H.evalIO $
            Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "searchUtxos")) $
              def & U5c.predicate .~ exactAddressPredicate address1
          pure $ searchResult ^. U5c.items
      )
      (\xs -> length xs == 2)

    let outputsAmounts = map (^. U5c.cardano . U5c.coin) utxosForAddress
    H.note_ $ "Ensure that the output sent is one of the utxos for the address " <> show addressText1
    H.assertWith outputsAmounts $ elem (inject amount)

asAddressInEra :: ShelleyBasedEra era -> AsType (AddressInEra era)
asAddressInEra s = shelleyBasedEraConstraints s $ AsAddressInEra asType
