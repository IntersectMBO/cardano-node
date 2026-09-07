{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import           Cardano.Testnet.Test.Utils (testEra)

import           Prelude

import           Control.Monad (void)
import           Data.Default.Class
import           Data.Foldable (find)
import           Data.Maybe (isJust)
import           Lens.Micro
import           Network.GRPC.Spec (NextElem (..), Proto (..))
import qualified System.Process as Process
import           System.Timeout (timeout)

import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Signal (hardKillProcess)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | E2E test for the WatchMempool gRPC method (SubmitService).
--
-- Stops the testnet's only block producer, so that nothing can leave the
-- mempool of the relay under test. Opens the WatchMempool stream on the relay
-- with an absent predicate - which matches every mempool entry, see
-- 'Cardano.Rpc.Server.Internal.UtxoRpc.Predicate.matchesTxPredicate' - submits
-- a transaction to the relay, and asserts the first message is that
-- transaction at 'U5c.STAGE_MEMPOOL'. The stream emits the mempool contents
-- first and additions after, so the order of opening and submitting is free.
--
-- Run with:
-- @TASTY_PATTERN='/RPC WatchMempool/' cabal test cardano-testnet-test@
hprop_rpc_watch_mempool :: H.Property
hprop_rpc_watch_mempool = integrationRetryWorkspace 2 "rpc-watch-mempool" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  let conf = mkConfig tempAbsBasePath'
  let era = testEra
      sbe = convert era
      creationOptions = def{creationEra = AnyShelleyBasedEra sbe}
      runtimeOptions = def
        { runtimeEnableRpc = RpcEnabledUnixSocket
        , runtimeEnableChainStallWatchdog = ChainStallWatchdogDisabled
        }
      addressInEra = asAddressInEra sbe

  TestnetRuntime
    { testnetNodes
    , wallets = wallet0@(PaymentKeyInfo _ addressText0) : (PaymentKeyInfo _ addressText1) : _
    } <-
    createAndRunTestnet creationOptions runtimeOptions conf

  relay <- H.evalMaybe $ find (not . isTestnetNodeSpo) testnetNodes
  spo <- H.evalMaybe $ find isTestnetNodeSpo testnetNodes
  rpcSocket <- H.note . unFile $ nodeRpcSocketPath relay
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

  H.note_ "Stop the only block producer, so that nothing can leave the mempool"
  H.evalIO $ do
    hardKillProcess $ nodeProcessHandle spo
    void . Process.waitForProcess $ nodeProcessHandle spo

  --------------------------------------------------------------
  -- Open WatchMempool, submit in the same thread, read one message
  --------------------------------------------------------------
  H.note_ "Fails if the WatchMempool stream does not deliver a message within 60 s"
  (submitResponse, watchElem) <-
    H.nothingFailM . H.evalIO . timeout 60_000_000 . Rpc.withConnection def rpcServer $ \watchConn ->
      Rpc.withRPC watchConn def (Proxy @(Rpc.Protobuf U5c.SubmitService "watchMempool")) $ \call -> do
        Rpc.expectCancellation call
        Rpc.sendFinalInput call def
        submitResponse' <- Rpc.withConnection def rpcServer $ \submitConn ->
          Rpc.nonStreaming submitConn (Rpc.rpc @(Rpc.Protobuf U5c.SubmitService "submitTx")) $
            def & U5c.tx .~ (def & U5c.raw .~ serialiseToRawBytes (Exp.SignedTx signedLedgerTx))
        watchElem' <- Rpc.recvNextOutputElem call
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
