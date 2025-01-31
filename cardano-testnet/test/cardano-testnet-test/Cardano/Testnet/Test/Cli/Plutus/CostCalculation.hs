{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Cardano.Testnet.Test.Cli.Plutus.CostCalculation
  ( hprop_ref_plutus_cost_calculation
  , hprop_included_plutus_cost_calculation
    -- | Execute tests in this module with:
    -- @DISABLE_RETRIES=1 cabal run cardano-testnet-test -- -p "/Spec.hs.Spec.Ledger Events.Plutus.Cost Calc/"@
  )
where

import           Cardano.Api (AnyCardanoEra (AnyCardanoEra),
                   AnyShelleyBasedEra (AnyShelleyBasedEra), File (File), MonadIO (liftIO),
                   ShelleyBasedEra (ShelleyBasedEraConway), ToCardanoEra (toCardanoEra), renderTxIn,
                   unFile)
import           Cardano.Api.Experimental (Some (Some))
import           Cardano.Api.Ledger (EpochInterval (EpochInterval), unCoin)

import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import           Data.Default.Class (Default (def))
import qualified Data.Text as Text
import           System.Directory (makeAbsolute)
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Golden as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

import           Testnet.Components.Query (findLargestUtxoForPaymentKey, getEpochStateView, getTxIx,
                   watchEpochStateUpdate)
import           Testnet.Process.Cli.Transaction (TxOutAddress (..), mkSpendOutputsOnlyTx,
                   retrieveTransactionId, signTx, submitTx)
import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types (eraToString)
import           Testnet.Types (PaymentKeyInfo (paymentKeyInfoAddr), paymentKeyInfoPair)

-- @DISABLE_RETRIES=1 cabal run cardano-testnet-test -- -p "/Spec.hs.Spec.Ledger Events.Plutus.Cost Calc.Ref Script/"@
hprop_ref_plutus_cost_calculation :: Property
hprop_ref_plutus_cost_calculation = integrationRetryWorkspace 2 "reference plutus script" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  H.note_ SYS.os
  conf@Conf{tempAbsPath} <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let
    sbe = ShelleyBasedEraConway
    era = toCardanoEra sbe
    cEra = AnyCardanoEra era
    eraName = eraToString era
    tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'
    options = def{cardanoNodeEra = AnyShelleyBasedEra sbe}

  TestnetRuntime
    { configurationFile
    , testnetMagic
    , testnetNodes
    , wallets = wallet0 : wallet1 : _
    } <-
    cardanoTestnetDefault options def conf

  poolNode1 <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket poolNode1
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  epochStateView <- getEpochStateView configurationFile (nodeSocketPath poolNode1)

  refScriptSizeWork <- H.createDirectoryIfMissing $ work </> "ref-script-publish"
  plutusV3Script <-
    File <$> liftIO (makeAbsolute "test/cardano-testnet-test/files/plutus/v3/always-succeeds.plutus")

  let scriptPublishUTxOAmount = 10_000_000

  -- Submit a transaction to publish the reference script
  txBodyPublishRefScript <-
    mkSpendOutputsOnlyTx
      execConfig
      epochStateView
      sbe
      refScriptSizeWork
      "tx-body"
      wallet0
      [(ScriptAddress plutusV3Script, scriptPublishUTxOAmount, Just plutusV3Script)]
  signedTxPublishRefScript <-
    signTx
      execConfig
      cEra
      refScriptSizeWork
      "signed-tx"
      txBodyPublishRefScript
      [Some $ paymentKeyInfoPair wallet0]
  submitTx execConfig cEra signedTxPublishRefScript

  -- Wait until transaction is on chain and obtain transaction identifier
  txIdPublishRefScript <- retrieveTransactionId execConfig signedTxPublishRefScript
  txIxPublishRefScript <-
    H.evalMaybeM $
      watchEpochStateUpdate
        epochStateView
        (EpochInterval 2)
        (getTxIx sbe txIdPublishRefScript scriptPublishUTxOAmount)

  -- Submit a transaction to lock money in the reference script
  refScriptLock <- H.createDirectoryIfMissing $ work </> "ref-script-lock"

  let transferAmount = 20_000_000
      enoughAmountForFees = 2_000_000 -- Needs to be more than min ada
  txBodyLock <-
    mkSpendOutputsOnlyTx
      execConfig
      epochStateView
      sbe
      refScriptLock
      "tx-body"
      wallet0
      [(ScriptAddress plutusV3Script, transferAmount, Nothing)]
  signedTxLock <-
    signTx execConfig cEra refScriptLock "signed-tx" txBodyLock [Some $ paymentKeyInfoPair wallet0]
  submitTx execConfig cEra signedTxLock

  -- Wait until transaction is on chain and obtain transaction identifier
  txIdLock <- retrieveTransactionId execConfig signedTxLock
  txIxLock <-
    H.evalMaybeM $
      watchEpochStateUpdate epochStateView (EpochInterval 2) (getTxIx sbe txIdLock transferAmount)

  -- Create transaction that uses reference script
  refScriptUnlock <- H.createDirectoryIfMissing $ work </> "ref-script-unlock"
  let unsignedUnlockTx = File $ refScriptUnlock </> "unsigned-tx.tx"
  largestUTxO <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  void $
    execCli'
      execConfig
      [ eraName
      , "transaction", "build"
      , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
      , "--tx-in", txIdLock <> "#" <> show txIxLock
      , "--spending-reference-tx-in-inline-datum-present"
      , "--spending-tx-in-reference", txIdPublishRefScript <> "#" <> show txIxPublishRefScript
      , "--spending-plutus-script-v3"
      , "--spending-reference-tx-in-redeemer-value", "42"
      , "--tx-in-collateral", Text.unpack $ renderTxIn largestUTxO
      , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show (unCoin (transferAmount - enoughAmountForFees))
      , "--out-file", unFile unsignedUnlockTx
      ]

  signedUnlockTx <-
    signTx
      execConfig
      cEra
      refScriptUnlock
      "signed-tx"
      unsignedUnlockTx
      [Some $ paymentKeyInfoPair wallet1]

  submitTx execConfig cEra signedUnlockTx

  -- Calculate cost of the transaction
  let txCostOutput = File $ refScriptUnlock </> "unsigned-tx.tx"
  H.noteM_ $
    execCli'
      execConfig
      [ eraName
      , "transaction", "calculate-plutus-script-cost"
      , "--tx-file", unFile signedUnlockTx
      , "--out-file", unFile txCostOutput
      ]

  H.diffFileVsGoldenFile
    (unFile txCostOutput)
    "test/cardano-testnet-test/files/calculatePlutusScriptCost.json"

  -- Compare to stdout

  output <-
    H.noteM $
      execCli'
        execConfig
        [ eraName
        , "transaction", "calculate-plutus-script-cost"
        , "--tx-file", unFile signedUnlockTx
        ]

  H.diffVsGoldenFile output "test/cardano-testnet-test/files/calculatePlutusScriptCost.json"

-- @DISABLE_RETRIES=1 cabal run cardano-testnet-test -- -p "/Spec.hs.Spec.Ledger Events.Plutus.Cost Calc.Normal Script/"@
hprop_included_plutus_cost_calculation :: Property
hprop_included_plutus_cost_calculation = integrationRetryWorkspace 2 "included plutus script" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  H.note_ SYS.os
  conf@Conf{tempAbsPath} <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let
    sbe = ShelleyBasedEraConway
    era = toCardanoEra sbe
    cEra = AnyCardanoEra era
    eraName = eraToString era
    tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'
    options = def{cardanoNodeEra = AnyShelleyBasedEra sbe}

  TestnetRuntime
    { configurationFile
    , testnetMagic
    , testnetNodes
    , wallets = wallet0 : wallet1 : _
    } <-
    cardanoTestnetDefault options def conf

  poolNode1 <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket poolNode1
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  epochStateView <- getEpochStateView configurationFile (nodeSocketPath poolNode1)

  includedScriptLockWork <- H.createDirectoryIfMissing $ work </> "included-script-lock"
  plutusV3Script <-
    File <$> liftIO (makeAbsolute "test/cardano-testnet-test/files/plutus/v3/always-succeeds.plutus")

  let includedScriptLockAmount = 10_000_000
      enoughAmountForFees = 2_000_000 -- Needs to be more than min ada

  -- Submit a transaction to publish the reference script
  txBodyIncludedScriptLock <-
    mkSpendOutputsOnlyTx
      execConfig
      epochStateView
      sbe
      includedScriptLockWork
      "tx-body"
      wallet0
      [(ScriptAddress plutusV3Script, includedScriptLockAmount, Nothing)]
  signedTxIncludedScriptLock <-
    signTx
      execConfig
      cEra
      includedScriptLockWork
      "signed-tx"
      txBodyIncludedScriptLock
      [Some $ paymentKeyInfoPair wallet0]
  submitTx execConfig cEra signedTxIncludedScriptLock

  -- Wait until transaction is on chain and obtain transaction identifier
  txIdIncludedScriptLock <- retrieveTransactionId execConfig signedTxIncludedScriptLock
  txIxIncludedScriptLock <-
    H.evalMaybeM $
      watchEpochStateUpdate
        epochStateView
        (EpochInterval 2)
        (getTxIx sbe txIdIncludedScriptLock includedScriptLockAmount)

  -- Create transaction that uses reference script
  includedScriptUnlock <- H.createDirectoryIfMissing $ work </> "included-script-unlock"
  let unsignedIncludedScript = File $ includedScriptUnlock </> "unsigned-tx.tx"
  newLargestUTxO <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  void $
    execCli'
      execConfig
      [ eraName
      , "transaction", "build"
      , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
      , "--tx-in", txIdIncludedScriptLock <> "#" <> show txIxIncludedScriptLock
      , "--tx-in-script-file", unFile plutusV3Script
      , "--tx-in-redeemer-value", "42"
      , "--tx-in-collateral", Text.unpack $ renderTxIn newLargestUTxO
      , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show (unCoin (includedScriptLockAmount - enoughAmountForFees))
      , "--out-file", unFile unsignedIncludedScript
      ]

  signedIncludedScript <-
    signTx
      execConfig
      cEra
      includedScriptUnlock
      "signed-tx"
      unsignedIncludedScript
      [Some $ paymentKeyInfoPair wallet1]

  submitTx execConfig cEra signedIncludedScript

  -- Calculate cost of the transaction
  let includedScriptCostOutput = File $ includedScriptUnlock </> "unsigned-tx.tx"
  H.noteM_ $
    execCli'
      execConfig
      [ eraName
      , "transaction", "calculate-plutus-script-cost"
      , "--tx-file", unFile signedIncludedScript
      , "--out-file", unFile includedScriptCostOutput
      ]

  H.diffFileVsGoldenFile
    (unFile includedScriptCostOutput)
    "test/cardano-testnet-test/files/calculatePlutusScriptCost.json"
