{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Run with:
-- @TASTY_PATTERN='/RPC Eval Tx/' cabal test cardano-testnet-test@
module Cardano.Testnet.Test.Rpc.Eval
  ( hprop_rpc_eval_tx
  )
where

import           Cardano.Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Ledger as L

import           Cardano.Rpc.Client (Proto (..))
import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as U5c hiding (cardano)
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as U5c
import           Cardano.Rpc.Server.Internal.UtxoRpc.Type (utxoRpcBigIntToInteger)
import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Lens.Micro
import           System.FilePath ((</>))

import           Testnet.Components.Query (TestnetWaitPeriod (..), findLargestUtxoForPaymentKey,
                   findLargestUtxoWithAddress, getEpochStateView, retryUntilJustM)
import           Testnet.Defaults (plutusV3Script)
import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types (anyEraToString)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

-- | Evaluate a Plutus V3 spending transaction via the gRPC evalTx endpoint and
-- verify that the response contains a valid fee, non-zero execution units, one
-- redeemer, and no errors.
hprop_rpc_eval_tx :: Property
hprop_rpc_eval_tx = integrationRetryWorkspace 2 "rpc-eval-tx" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf{tempAbsPath} <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'
      era = Exp.ConwayEra
      sbe = convert era
      anyEra = AnyCardanoEra $ toCardanoEra sbe
      creationOptions = def{creationEra = AnyShelleyBasedEra sbe}
      runtimeOptions = def{runtimeEnableRpc = RpcEnabled}

  TestnetRuntime
    { configurationFile
    , testnetMagic
    , testnetNodes = node :| _
    , wallets = wallet0 : wallet1 : _
    } <-
    createAndRunTestnet creationOptions runtimeOptions conf

  poolSprocket <- H.noteShow $ nodeSprocket node
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket testnetMagic
  epochStateView <- getEpochStateView configurationFile $ nodeSocketPath node
  rpcSocket <- H.note . unFile $ nodeRpcSocketPath node

  let rpcServer = Rpc.ServerUnix rpcSocket
      utxoSKeyFile = signingKeyFp $ paymentKeyInfoPair wallet0
      utxoSKeyFile1 = signingKeyFp $ paymentKeyInfoPair wallet1

  ------------------------------------
  -- Write Plutus V3 always-succeeds script
  ------------------------------------
  plutusScriptFile <- H.note $ work </> "always-succeeds.plutusV3"
  H.writeFile plutusScriptFile $ T.unpack plutusV3Script

  plutusSpendingScriptAddr <-
    execCli'
      execConfig
      [ "latest"
      , "address"
      , "build"
      , "--payment-script-file" , plutusScriptFile
      ]

  scriptDatumHash <-
    filter (/= '\n')
      <$> execCli'
        execConfig
        [ "latest"
        , "transaction"
        , "hash-script-data"
        , "--script-data-value"
        , "0"
        ]

  -- Send ADA to the script address with a datum hash, creating a script-locked
  -- UTxO that the spending transaction can later reference with a redeemer.
  ------------------------------------
  -- 1. Fund the script address
  ------------------------------------
  txinFund <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  let fundTxBody = work </> "fund-script-tx-body"
      fundTx = work </> "fund-script-tx"

  void $
    execCli'
      execConfig
      [ anyEraToString anyEra
      , "transaction"
      , "build"
      , "--change-address" , T.unpack $ paymentKeyInfoAddr wallet0
      , "--tx-in" , T.unpack $ renderTxIn txinFund
      , "--tx-out" , plutusSpendingScriptAddr <> "+" <> show @Int 5_000_000
      , "--tx-out-datum-hash" , scriptDatumHash
      , "--out-file" , fundTxBody
      ]

  void $
    execCli'
      execConfig
      [ "latest"
      , "transaction"
      , "sign"
      , "--tx-body-file" , fundTxBody
      , "--signing-key-file" , utxoSKeyFile
      , "--out-file" , fundTx
      ]

  ------------------------------------
  -- 1b. EvalTx on the funding tx (no scripts)
  ------------------------------------
  (fundLedgerTx, fundTxEval) <- evalTxFile sbe rpcServer fundTx

  let fundCliFee = fundLedgerTx ^. L.bodyTxL . L.feeTxBodyL

  H.note_ "EvalTx minimum fee should not exceed the CLI-computed fee"
  fundEvalFee <- H.leftFail $ fundTxEval ^. U5c.fee . to utxoRpcBigIntToInteger
  H.assertWith fundEvalFee (<= L.unCoin fundCliFee)

  H.note_ "No execution units for a plain key-witnessed transaction"
  fundTxEval ^. U5c.exUnits . U5c.steps === 0
  fundTxEval ^. U5c.exUnits . U5c.memory === 0

  H.note_ "No redeemers for a plain key-witnessed transaction"
  fundTxEval ^. U5c.redeemers === []

  H.note_ "No evaluation errors"
  fundTxEval ^. U5c.errors === []

  void $
    execCli'
      execConfig
      [ "latest"
      , "transaction"
      , "submit"
      , "--tx-file" , fundTx
      ]

  ------------------------------------
  -- 2. Wait for the script UTxO, find collateral
  ------------------------------------
  plutusScriptTxIn <-
    fmap fst . retryUntilJustM epochStateView (WaitForBlocks 3) $
      findLargestUtxoWithAddress epochStateView sbe $
        T.pack plutusSpendingScriptAddr

  txinCollateral <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  ------------------------------------
  -- 3. Build and sign the spending tx
  ------------------------------------
  let spendTxBody = work </> "spend-script-tx-body"
      spendTx = work </> "spend-script-tx"

  void $
    execCli'
      execConfig
      [ anyEraToString anyEra
      , "transaction"
      , "build"
      , "--change-address" , T.unpack $ paymentKeyInfoAddr wallet1
      , "--tx-in-collateral" , T.unpack $ renderTxIn txinCollateral
      , "--tx-in" , T.unpack $ renderTxIn plutusScriptTxIn
      , "--tx-in-script-file" , plutusScriptFile
      , "--tx-in-datum-value" , "0"
      , "--tx-in-redeemer-value" , "0"
      , "--out-file" , spendTxBody
      ]

  void $
    execCli'
      execConfig
      [ "latest"
      , "transaction"
      , "sign"
      , "--tx-body-file" , spendTxBody
      , "--signing-key-file" , utxoSKeyFile1
      , "--out-file" , spendTx
      ]

  ------------------------------------
  -- 4. EvalTx on the spending tx (Plutus V3 always-succeeds)
  ------------------------------------
  (ledgerTx, txEval) <- evalTxFile sbe rpcServer spendTx

  let cliFee = ledgerTx ^. L.bodyTxL . L.feeTxBodyL

  H.note_ "EvalTx minimum fee should not exceed the CLI-computed fee"
  evalFee <- H.leftFail $ txEval ^. U5c.fee . to utxoRpcBigIntToInteger
  H.assertWith evalFee (<= L.unCoin cliFee)

  H.note_ "Execution units should match the transaction"
  (_, L.ExUnits txMem txSteps) <- H.headM . Map.elems . L.unRedeemers $ ledgerTx ^. L.witsTxL . L.rdmrsTxWitsL
  txEval ^. U5c.exUnits . U5c.steps === fromIntegral txSteps
  txEval ^. U5c.exUnits . U5c.memory === fromIntegral txMem

  H.note_ "One redeemer for the spend purpose at index 0"
  let redeemers = txEval ^. U5c.redeemers
  length redeemers === 1
  redeemer0 <- H.headM redeemers
  redeemer0 ^. U5c.purpose === Proto U5c.REDEEMER_PURPOSE_SPEND
  redeemer0 ^. U5c.index === 0
  redeemer0 ^. U5c.exUnits . U5c.steps === fromIntegral txSteps
  redeemer0 ^. U5c.exUnits . U5c.memory === fromIntegral txMem

  H.note_ "No evaluation errors"
  txEval ^. U5c.errors === []

  ------------------------------------
  -- 5. Failure path: always-fails script
  ------------------------------------
  let failScript = PlutusScript PlutusScriptV1 $ examplePlutusScriptAlwaysFails WitCtxTxIn
  failScriptFile <- H.note $ work </> "always-fails.plutusV1"
  H.leftFailM . H.evalIO $
    writeFileTextEnvelope (File failScriptFile) Nothing failScript

  failScriptAddr <-
    execCli'
      execConfig
      [ "latest"
      , "address"
      , "build"
      , "--payment-script-file" , failScriptFile
      ]

  txinFund2 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  let fundFailTxBody = work </> "fund-fail-script-tx-body"
      fundFailTx = work </> "fund-fail-script-tx"

  void $
    execCli'
      execConfig
      [ anyEraToString anyEra
      , "transaction"
      , "build"
      , "--change-address" , T.unpack $ paymentKeyInfoAddr wallet0
      , "--tx-in" , T.unpack $ renderTxIn txinFund2
      , "--tx-out" , failScriptAddr <> "+" <> show @Int 5_000_000
      , "--tx-out-datum-hash" , scriptDatumHash
      , "--out-file" , fundFailTxBody
      ]

  void $
    execCli'
      execConfig
      [ "latest"
      , "transaction"
      , "sign"
      , "--tx-body-file" , fundFailTxBody
      , "--signing-key-file" , utxoSKeyFile
      , "--out-file" , fundFailTx
      ]

  void $
    execCli'
      execConfig
      [ "latest"
      , "transaction"
      , "submit"
      , "--tx-file" , fundFailTx
      ]

  failScriptTxIn <-
    fmap fst . retryUntilJustM epochStateView (WaitForBlocks 3) $
      findLargestUtxoWithAddress epochStateView sbe $
        T.pack failScriptAddr

  txinCollateral2 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  protocolParamsFile <- H.note $ work </> "protocol-params.json"
  void $
    execCli'
      execConfig
      [ "latest"
      , "query"
      , "protocol-parameters"
      , "--out-file" , protocolParamsFile
      ]

  -- Use build-raw because `transaction build` would reject the always-fails script.
  let failSpendTxBody = work </> "fail-spend-tx-body"
      failSpendTx = work </> "fail-spend-tx"

  void $
    execCli'
      execConfig
      [ anyEraToString anyEra
      , "transaction"
      , "build-raw"
      , "--tx-in" , T.unpack $ renderTxIn failScriptTxIn
      , "--tx-in-collateral" , T.unpack $ renderTxIn txinCollateral2
      , "--tx-in-script-file" , failScriptFile
      , "--tx-in-datum-value" , "0"
      , "--tx-in-redeemer-value" , "0"
      , "--tx-in-execution-units" , "(10000000000,10000000)"
      , "--tx-out" , T.unpack (paymentKeyInfoAddr wallet1) <> "+4700000"
      , "--fee" , "300000"
      , "--protocol-params-file" , protocolParamsFile
      , "--out-file" , failSpendTxBody
      ]

  void $
    execCli'
      execConfig
      [ "latest"
      , "transaction"
      , "sign"
      , "--tx-body-file" , failSpendTxBody
      , "--signing-key-file" , utxoSKeyFile1
      , "--out-file" , failSpendTx
      ]

  ------------------------------------
  -- 5b. EvalTx on the always-fails spending tx
  ------------------------------------
  (_, failTxEval) <- evalTxFile sbe rpcServer failSpendTx

  H.note_ "Evaluation errors for always-fails script"
  failTxEval ^. U5c.errors /== []

  ------------------------------------
  -- 6. Unbalanced key-witnessed tx
  ------------------------------------
  txinUnbal <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  let unbalTxBody = work </> "unbal-tx-body"
      unbalTx = work </> "unbal-tx"

  void $
    execCli'
      execConfig
      [ anyEraToString anyEra
      , "transaction"
      , "build-raw"
      , "--tx-in" , T.unpack $ renderTxIn txinUnbal
      , "--tx-out" , T.unpack (paymentKeyInfoAddr wallet0) <> "+1000000"
      , "--fee" , "200000"
      , "--out-file" , unbalTxBody
      ]

  void $
    execCli'
      execConfig
      [ "latest"
      , "transaction"
      , "sign"
      , "--tx-body-file" , unbalTxBody
      , "--signing-key-file" , utxoSKeyFile
      , "--out-file" , unbalTx
      ]

  (_, unbalTxEval) <- evalTxFile sbe rpcServer unbalTx

  H.note_ "Balance error for unbalanced transaction"
  unbalTxEval ^. U5c.errors /== []
  unbalError <- H.headM $ unbalTxEval ^. U5c.errors
  H.assertWith (unbalError ^. U5c.msg) $ T.isInfixOf "not balanced"

  H.note_ "Non-zero fee is still returned"
  unbalEvalFee <- H.leftFail $ unbalTxEval ^. U5c.fee . to utxoRpcBigIntToInteger
  H.assertWith unbalEvalFee (> 0)

-- | Read a signed transaction from a file and evaluate it via the gRPC evalTx
-- endpoint, returning the ledger transaction and the TxEval result.
evalTxFile
  :: forall era m
   . (HasCallStack, MonadBaseControl IO m, MonadCatch m, MonadIO m, MonadTest m)
  => ShelleyBasedEra era
  -> Rpc.Server
  -> FilePath
  -> m (L.Tx L.TopTx (ShelleyLedgerEra era), Proto U5c.TxEval)
evalTxFile sbe' rpcServer txFile = withFrozenCallStack $ shelleyBasedEraConstraints sbe' $ do
  textEnvelope <- H.leftFailM . H.evalIO $ readTextEnvelopeFromFile txFile
  ShelleyTx _ ledgerTx <- H.leftFail $ deserialiseFromTextEnvelope @(Tx era) textEnvelope
  let request = def & U5c.tx . U5c.raw .~ textEnvelopeRawCBOR textEnvelope
  (response :: Proto U5c.EvalTxResponse) <-
    liftBaseOp (Rpc.withConnection def rpcServer) $ \conn ->
      H.noteShowPrettyM . H.evalIO $
        Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf U5c.SubmitService "evalTx")) request
  pure (ledgerTx, response ^. U5c.report . U5c.cardano)
