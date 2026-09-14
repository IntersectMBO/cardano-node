{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Rpc.WatchMempool
  ( hprop_rpc_watch_mempool
  )
where

import           Cardano.Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Experimental.Tx as Exp
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as Query
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as U5c
import           Cardano.Rpc.Server.Internal.UtxoRpc.Predicate (exactAddressPredicate)
import           Cardano.Rpc.Server.Internal.UtxoRpc.Type
import           Cardano.Testnet

import           Prelude

import           Control.Concurrent.Async (wait, withAsync)
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe (isJust)
import           Lens.Micro
import           Network.GRPC.Spec (NextElem (..), Proto (..))

import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | E2E test for the WatchMempool gRPC method (SubmitService).
--
-- Opens the WatchMempool stream with an absent predicate - which matches
-- every mempool entry, see 'Cardano.Rpc.Server.Internal.UtxoRpc.Predicate.matchesTxPredicate'
-- - /before/ submitting a transaction, then submits it and asserts the
-- stream delivers exactly that transaction at 'U5c.STAGE_MEMPOOL'. Opening
-- before submitting matters: WatchMempool only emits additions, so a
-- transaction submitted first could already have entered and left the
-- mempool (confirmed) before a late-opened stream ever saw it.
--
-- Run with:
-- @TASTY_PATTERN='/RPC WatchMempool/' cabal test cardano-testnet-test@
hprop_rpc_watch_mempool :: H.Property
hprop_rpc_watch_mempool = integrationRetryWorkspace 2 "rpc-watch-mempool" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf <- mkConf tempAbsBasePath'
  let era = Exp.ConwayEra
      sbe = convert era
      creationOptions = def{creationEra = AnyShelleyBasedEra sbe}
      runtimeOptions = def{runtimeEnableRpc = RpcEnabled}
      addressInEra = asAddressInEra sbe

  TestnetRuntime
    { testnetNodes = node0 :| _
    , wallets = wallet0@(PaymentKeyInfo _ addressText0) : (PaymentKeyInfo _ addressText1) : _
    } <-
    createAndRunTestnet creationOptions runtimeOptions conf

  rpcSocket <- H.note . unFile $ nodeRpcSocketPath node0
  let rpcServer = Rpc.ServerUnix rpcSocket

  address0 <- H.nothingFail $ deserialiseAddress addressInEra addressText0
  address1 <- H.nothingFail $ deserialiseAddress addressInEra addressText1

  wit0 :: ShelleyWitnessSigningKey <-
    H.leftFailM . H.evalIO $
      readFileTextEnvelopeAnyOf
        [FromSomeType asType WitnessGenesisUTxOKey]
        (signingKey $ paymentKeyInfoPair wallet0)

  ------------------------------------
  -- Build (but don't yet submit) a tx
  ------------------------------------
  (pparamsResponse, searchResponse) <- H.evalIO . Rpc.withConnection def rpcServer $ \conn -> do
    pparams' <-
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf Query.QueryService "readParams")) def
    search' <-
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf Query.QueryService "searchUtxos")) $
        def & Query.predicate .~ exactAddressPredicate address0
    pure (pparams', search')

  pparams <- H.leftFail $ utxoRpcPParamsToProtocolParams era $ pparamsResponse ^. Query.values . Query.cardano
  txOut0 : _ <- H.noteShow $ searchResponse ^. Query.items
  txIn0 <- H.leftFail . txoRefUtxoRpcToTxIn $ txOut0 ^. Query.txoRef
  outputCoin <- H.leftFail $ txOut0 ^. Query.cardano . Query.coin . to utxoRpcBigIntToInteger

  let amount = 200_000_000
      fee = 500
      change = outputCoin - amount - fee
      mkOut ledgerAddress coin = Exp.TxOut $ L.mkBasicTxOut ledgerAddress $ L.inject $ L.Coin coin
      content = Exp.obtainCommonConstraints era $
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn0, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxFee (L.Coin fee)
          & Exp.setTxOuts [mkOut (toShelleyAddr address1) amount, mkOut (toShelleyAddr address0) change]
          & Exp.setTxProtocolParams pparams

  unsignedTx <- H.leftFail $ Exp.makeUnsignedTx era content
  let keyWit = Exp.makeKeyWitness era unsignedTx wit0
      Exp.SignedTx signedLedgerTx = Exp.signTx era [] [keyWit] unsignedTx
  txId' <- H.noteShow . Exp.obtainCommonConstraints era . TxId $ Exp.hashTxBody (signedLedgerTx ^. L.bodyTxL)

  ------------------------------------------------------
  -- Open WatchMempool, THEN submit, and read one message
  ------------------------------------------------------
  H.note_ "Open the WatchMempool stream before submitting the transaction"
  (submitResponse, watchElem) <-
    H.evalIO $
      withAsync
        ( Rpc.withConnection def rpcServer $ \watchConn ->
            Rpc.serverStreaming watchConn (Rpc.rpc @(Rpc.Protobuf U5c.SubmitService "watchMempool")) def id
        )
        $ \watchAsync -> do
          submitResponse' <- Rpc.withConnection def rpcServer $ \submitConn ->
            Rpc.nonStreaming submitConn (Rpc.rpc @(Rpc.Protobuf U5c.SubmitService "submitTx")) $
              def & U5c.tx .~ (def & U5c.raw .~ serialiseToRawBytes (Exp.SignedTx signedLedgerTx))
          watchElem' <- wait watchAsync
          pure (submitResponse', watchElem')

  submittedTxId <- H.leftFail . deserialiseFromRawBytes AsTxId $ submitResponse ^. U5c.ref
  H.note_ "Ensure that submitTx returns the same transaction ID as the locally computed signed transaction ID"
  txId' === submittedTxId

  message <- case watchElem of
    NextElem message' -> pure message'
    NoNextElem -> do
      H.note_ "WatchMempool stream ended before delivering the submitted transaction"
      H.failure

  let txInMempool = message ^. U5c.tx
  H.note_ "Verify the delivered TxInMempool entry is the submitted transaction, at STAGE_MEMPOOL"
  H.assertWith (txInMempool ^. U5c.ref) (== serialiseToRawBytes txId')
  txInMempool ^. U5c.stage === Proto U5c.STAGE_MEMPOOL
  H.assertWith (txInMempool ^. U5c.maybe'cardano) isJust

asAddressInEra :: ShelleyBasedEra era -> AsType (AddressInEra era)
asAddressInEra s = shelleyBasedEraConstraints s $ AsAddressInEra asType
