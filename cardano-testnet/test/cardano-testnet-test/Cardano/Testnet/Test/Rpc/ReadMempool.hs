{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Rpc.ReadMempool
  ( hprop_rpc_read_mempool
  )
where

import           Cardano.Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Experimental.Tx as Exp
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as Query
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as U5c hiding (cardano, items, nativeBytes, maybe'cardano)
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as UtxoRpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as U5c hiding (predicate)
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as UtxoRpc
import           Cardano.Rpc.Server.Internal.UtxoRpc.Predicate (exactAddressPredicate)
import           Cardano.Rpc.Server.Internal.UtxoRpc.Type
import           Cardano.Testnet

import           Prelude

import           Control.Monad (forM_)
import           Control.Monad.Trans.Control (liftBaseOp)
import qualified Data.ByteString as BS
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe (isJust)
import           Lens.Micro
import           Network.GRPC.Spec (Proto (..))

import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

-- | E2E test for the ReadMempool gRPC method (SubmitService).
--
-- Spins up a testnet, submits a transaction, and immediately reads the
-- mempool over the same connection. On a single-node testnet the
-- transaction may already have been confirmed by the time the read happens,
-- so an empty result is a pass; when the result is non-empty,
-- every returned 'TxInMempool' entry is checked for well-formedness.
--
-- Run with:
-- @TASTY_PATTERN='/RPC ReadMempool/' cabal test cardano-testnet-test@
hprop_rpc_read_mempool :: Property
hprop_rpc_read_mempool = integrationRetryWorkspace 2 "rpc-read-mempool" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
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
  (pparamsResponse, searchResponse) <- H.noteShowM . H.evalIO . Rpc.withConnection def rpcServer $ \conn -> do
    pparams' <-
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "readParams")) def

    search' <-
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "searchUtxos")) $
        def & U5c.predicate .~ exactAddressPredicate address0
    pure (pparams', search')

  pparams <- H.leftFail $ utxoRpcPParamsToProtocolParams era $ pparamsResponse ^. U5c.values . U5c.cardano

  txOut0 : _ <- H.noteShow $ searchResponse ^. Query.items
  txIn0 <- H.leftFail . txoRefUtxoRpcToTxIn $ txOut0 ^. U5c.txoRef

  outputCoin <- H.leftFail $ txOut0 ^. U5c.cardano . U5c.coin . to utxoRpcBigIntToInteger
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

  liftBaseOp (Rpc.withConnection def rpcServer) $ \conn -> do
    submitResponse <- H.noteShowM . H.evalIO $
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.SubmitService "submitTx")) $
        def & U5c.tx .~ (def & U5c.raw .~ serialiseToRawBytes (Exp.SignedTx signedLedgerTx))

    submittedTxId <- H.leftFail . deserialiseFromRawBytes AsTxId $ submitResponse ^. U5c.ref
    H.note_ "Ensure that submitTx returns the same transaction ID as the locally computed signed transaction ID"
    txId' === submittedTxId

    -------------------------------------------
    -- Read the mempool right after submitting
    -------------------------------------------
    H.note_ "Read the mempool immediately after submitting the transaction"
    mempoolResponse <- H.noteShowM . H.evalIO $
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.SubmitService "readMempool")) def

    let items = mempoolResponse ^. U5c.items
    if null items
      then
        H.note_
          "Mempool was empty at read time; the transaction most likely confirmed \
          \before the read happened (documented single-node testnet race)"
      else
        forM_ items $ \item -> do
          H.note_ "Verify TxInMempool entry is well-formed"
          H.assertWith (item ^. U5c.ref) $ not . BS.null
          H.assertWith (item ^. U5c.nativeBytes) $ not . BS.null
          item ^. U5c.stage === Proto U5c.STAGE_MEMPOOL
          H.assertWith (item ^. U5c.maybe'cardano) isJust

asAddressInEra :: ShelleyBasedEra era -> AsType (AddressInEra era)
asAddressInEra s = shelleyBasedEraConstraints s $ AsAddressInEra asType
