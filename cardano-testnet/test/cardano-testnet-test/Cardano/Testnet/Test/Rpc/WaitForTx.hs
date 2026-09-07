{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Cardano.Node.Testnet.Paths (defaultConfigFile)
import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as Query
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as U5c
import           Cardano.Rpc.Server.Internal.UtxoRpc.Predicate (exactAddressPredicate)
import           Cardano.Rpc.Server.Internal.UtxoRpc.Type
import           Cardano.Testnet
import           Cardano.Testnet.Test.Utils (testEra)

import           Prelude

import qualified Control.Lens as Lens
import           Control.Monad (forM_, void)
import           Control.Monad.Trans.Resource (register, release)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (_Object, key)
import           Data.Default.Class
import           Data.Foldable (find)
import           Lens.Micro
import           Network.GRPC.Common (StreamElem (..))
import           Network.GRPC.Spec (Proto (..))
import           System.FilePath ((</>))
import           System.IO.Error (tryIOError)
import           System.Timeout (timeout)

import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Signal (pauseProcess, resumeProcess)
import           Testnet.Start.Cardano (liftToIntegration)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | E2E test for the WaitForTx gRPC method (SubmitService).
--
-- Freezes the testnet's only block producer, so that the transaction stays in
-- the mempool of the relay under test. Submits the transaction to the relay and
-- opens WaitForTx for its ref: the first message must be 'U5c.STAGE_MEMPOOL',
-- which also shows that the server has positioned its chain follower. Then
-- resumes the block producer and asserts the stream delivers
-- 'U5c.STAGE_CONFIRMED' for the ref and closes cleanly (a gRPC error status
-- fails the test).
--
-- Run with:
-- @TASTY_PATTERN='/RPC WaitForTx/' cabal test cardano-testnet-test@
hprop_rpc_wait_for_tx :: H.Property
hprop_rpc_wait_for_tx = integrationRetryWorkspace 2 "rpc-wait-for-tx" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  let conf@Conf{tempAbsPath} = mkConfig tempAbsBasePath'
      configFile = unTmpAbsPath tempAbsPath </> defaultConfigFile
  let era = testEra
      sbe = convert era
      creationOptions = def{creationEra = AnyShelleyBasedEra sbe}
      runtimeOptions = def
        { runtimeEnableRpc = RpcEnabledUnixSocket
        , runtimeEnableChainStallWatchdog = ChainStallWatchdogDisabled
        }
      addressInEra = asAddressInEra sbe

  liftToIntegration $ createTestnetEnv creationOptions conf

  -- Relays forward transactions to their peers only after TxSubmissionInitDelay (60 s by default).
  configValue :: Aeson.Value <- H.leftFailM $ H.readJsonFile configFile
  let editedConfig =
        Lens.set (key "Configuration" . key "Network" . _Object . Lens.at "TxSubmissionInitDelay") (Just $ Aeson.Number 0) configValue
  editedConfig ^? key "Configuration" . key "Network" . key "TxSubmissionInitDelay" === Just (Aeson.Number 0)
  H.lbsWriteFile configFile $ Aeson.encode editedConfig

  TestnetRuntime
    { testnetNodes
    , wallets = wallet0@(PaymentKeyInfo _ addressText0) : (PaymentKeyInfo _ addressText1) : _
    } <-
    liftToIntegration $ cardanoTestnet (creationNodes creationOptions) runtimeOptions conf

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
  let txIdBytes = serialiseToRawBytes txId'

  H.note_ "Freeze the only block producer; resume it on teardown, as a stopped process ignores SIGTERM"
  resumeKey <- register . void . tryIOError . resumeProcess $ nodeProcessHandle spo
  H.evalIO . pauseProcess $ nodeProcessHandle spo

  submitResponse <- H.evalIO . Rpc.withConnection def rpcServer $ \submitConn ->
    Rpc.nonStreaming submitConn (Rpc.rpc @(Rpc.Protobuf U5c.SubmitService "submitTx")) $
      def & U5c.tx .~ (def & U5c.raw .~ serialiseToRawBytes (Exp.SignedTx signedLedgerTx))

  submittedTxId <- H.leftFail . deserialiseFromRawBytes AsTxId $ submitResponse ^. U5c.ref
  H.note_ "Ensure that submitTx returns the same transaction ID as the locally computed signed transaction ID"
  txId' === submittedTxId

  ----------------------------------------------------------------------
  -- Open WaitForTx, read the mempool message, resume block production
  ----------------------------------------------------------------------
  H.note_ "Fails if the WaitForTx stream does not complete within 180 s"
  messages <-
    H.nothingFailM . H.evalIO . timeout 180_000_000 . Rpc.withConnection def rpcServer $ \watchConn ->
      Rpc.withRPC watchConn def (Proxy @(Rpc.Protobuf U5c.SubmitService "waitForTx")) $ \call -> do
        Rpc.sendFinalInput call (def & U5c.ref .~ [txIdBytes])
        firstElem <- Rpc.recvOutput call
        release resumeKey
        case firstElem of
          StreamElem msg -> drain call [msg]
          FinalElem msg _ -> pure [msg]
          NoMoreElems _ -> pure []

  H.note_ "Every delivered message refers to the requested transaction"
  forM_ messages $ \msg -> H.assertWith (msg ^. U5c.ref) (== txIdBytes)

  H.note_ "The stream delivered the mempool message while the producer was frozen, then the confirmation, and closed cleanly"
  map (^. U5c.stage) messages === [Proto U5c.STAGE_MEMPOOL, Proto U5c.STAGE_CONFIRMED]
 where
  -- Read until the stream closes (after STAGE_CONFIRMED), in delivery order.
  -- 'Rpc.recvOutput' throws 'GrpcException' on a non-OK status, failing the test.
  drain call acc = do
    next <- Rpc.recvOutput call
    case next of
      StreamElem msg -> drain call (msg : acc)
      FinalElem msg _ -> pure (reverse (msg : acc))
      NoMoreElems _ -> pure (reverse acc)

asAddressInEra :: ShelleyBasedEra era -> AsType (AddressInEra era)
asAddressInEra s = shelleyBasedEraConstraints s $ AsAddressInEra asType
