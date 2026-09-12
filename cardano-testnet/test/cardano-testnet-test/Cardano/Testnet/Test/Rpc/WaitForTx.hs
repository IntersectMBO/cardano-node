{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Rpc.WaitForTx
  ( hprop_rpc_wait_for_tx
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
import           Control.Monad (forM_)
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Lens.Micro
import           Network.GRPC.Spec (NextElem (..), Proto (..))

import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | E2E test for the WaitForTx gRPC method (SubmitService).
--
-- Computes the transaction id of a signed-but-not-yet-submitted transaction,
-- opens WaitForTx for that ref /before/ submitting - the documented race
-- contract (see @WaitForTx@'s Haddock: a tx that reaches the chain between
-- submission and opening the stream is invisible to it) - then submits, and
-- asserts the stream eventually delivers 'U5c.STAGE_CONFIRMED' for the ref
-- and closes cleanly.
--
-- 'U5c.STAGE_MEMPOOL' is not asserted: on a single-node testnet the
-- transaction can confirm within a slot or two, so an intermediate
-- STAGE_MEMPOOL notification is racy and may or may not arrive. Its
-- presence is tolerated as long as it precedes STAGE_CONFIRMED.
--
-- Run with:
-- @TASTY_PATTERN='/RPC WaitForTx/' cabal test cardano-testnet-test@
hprop_rpc_wait_for_tx :: H.Property
hprop_rpc_wait_for_tx = integrationRetryWorkspace 2 "rpc-wait-for-tx" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
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
  let txIdBytes = serialiseToRawBytes txId'

  ------------------------------------------------------------
  -- Open WaitForTx for this ref, THEN submit, and drain the
  -- stream until it closes
  ------------------------------------------------------------
  H.note_ "Open WaitForTx for the not-yet-submitted transaction's ref before submitting"
  (submitResponse, messages) <-
    H.evalIO $
      withAsync
        ( Rpc.withConnection def rpcServer $ \watchConn ->
            Rpc.serverStreaming watchConn (Rpc.rpc @(Rpc.Protobuf U5c.SubmitService "waitForTx")) (def & U5c.ref .~ [txIdBytes]) $
              \recv -> drain recv []
        )
        $ \watchAsync -> do
          submitResponse' <- Rpc.withConnection def rpcServer $ \submitConn ->
            Rpc.nonStreaming submitConn (Rpc.rpc @(Rpc.Protobuf U5c.SubmitService "submitTx")) $
              def & U5c.tx .~ (def & U5c.raw .~ serialiseToRawBytes (Exp.SignedTx signedLedgerTx))
          messages' <- wait watchAsync
          pure (submitResponse', messages')

  submittedTxId <- H.leftFail . deserialiseFromRawBytes AsTxId $ submitResponse ^. U5c.ref
  H.note_ "Ensure that submitTx returns the same transaction ID as the locally computed signed transaction ID"
  txId' === submittedTxId

  H.note_ "The stream delivered at least one message and closed cleanly"
  H.assertWith messages $ not . null

  H.note_ "Every delivered message refers to the requested transaction"
  forM_ messages $ \msg -> H.assertWith (msg ^. U5c.ref) (== txIdBytes)

  let stages = map (^. U5c.stage) messages
  H.note_ $ "Observed stage sequence: " <> show stages
  case stages of
    [Proto U5c.STAGE_CONFIRMED] -> H.success
    [Proto U5c.STAGE_MEMPOOL, Proto U5c.STAGE_CONFIRMED] -> H.success
    _ -> do
      H.note_ "expected [STAGE_CONFIRMED] or [STAGE_MEMPOOL, STAGE_CONFIRMED]"
      H.failure
 where
  -- Read every message until the stream closes (WaitForTx closes once the
  -- ref has reached STAGE_CONFIRMED), collecting them in delivery order.
  drain recv acc = do
    next <- recv
    case next of
      NoNextElem -> pure (reverse acc)
      NextElem msg -> drain recv (msg : acc)

asAddressInEra :: ShelleyBasedEra era -> AsType (AddressInEra era)
asAddressInEra s = shelleyBasedEraConstraints s $ AsAddressInEra asType
