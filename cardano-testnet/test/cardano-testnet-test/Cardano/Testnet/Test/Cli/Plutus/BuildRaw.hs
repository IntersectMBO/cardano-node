{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Cli.Plutus.BuildRaw (
    hprop_build_raw_ref_script_spend,
)
where

import           Cardano.Api hiding (Value)
import           Cardano.Api.Experimental (Some (Some))
import           Cardano.Api.Ledger (EpochInterval (..))

import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import           Data.Default.Class (Default (def))
import qualified Data.Text as Text
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Testnet.Components.Query (TestnetWaitPeriod (..), findLargestUtxoForPaymentKey,
                   getEpochStateDetails, getEpochStateView, getTxIx, retryUntilJustM)
import qualified Testnet.Defaults as Defaults
import           Testnet.Process.Cli.Transaction (TxOutAddress (..), mkSpendOutputsOnlyTx,
                   retrieveTransactionId, signTx, submitTx)
import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types (eraToString)
import           Testnet.Types

import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

{- | Test spending a reference script UTxO using @transaction build-raw@.
@DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Build Raw Ref Script/"'@
-}
hprop_build_raw_ref_script_spend :: Property
hprop_build_raw_ref_script_spend = integrationRetryWorkspace 2 "build-raw-ref-script" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
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
        creationOptions = def{creationEra = AnyShelleyBasedEra sbe}

    TestnetRuntime
        { configurationFile
        , testnetMagic
        , testnetNodes
        , wallets = wallet0 : wallet1 : _
        } <-
        createAndRunTestnet creationOptions def conf

    poolNode1 <- H.headM testnetNodes
    poolSprocket1 <- H.noteShow $ nodeSprocket poolNode1
    execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
    epochStateView <- getEpochStateView configurationFile (nodeSocketPath poolNode1)

    -- Write PlutusV3 always-succeeds script to file
    let plutusScriptFp = work </> "always-succeeds-script.plutusV3"
    H.writeFile plutusScriptFp $ Text.unpack Defaults.plutusV3Script
    let plutusV3Script = File plutusScriptFp

    -- Step 1: Publish reference script at wallet0's address (not the script address)
    refScriptPublishWork <- H.createDirectoryIfMissing $ work </> "ref-script-publish"
    let scriptPublishUTxOAmount = 10_000_000

    txinPublish <- findLargestUtxoForPaymentKey epochStateView sbe wallet0
    let txBodyPublishFp = File $ refScriptPublishWork </> "tx-body.txbody"
    void $
        execCli'
            execConfig
            [ eraName
            , "transaction"
            , "build"
            , "--change-address"
            , Text.unpack $ paymentKeyInfoAddr wallet0
            , "--tx-in"
            , prettyShow txinPublish
            , "--tx-out"
            , Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show (unCoin scriptPublishUTxOAmount)
            , "--tx-out-reference-script-file"
            , unFile plutusV3Script
            , "--out-file"
            , unFile txBodyPublishFp
            ]
    signedTxPublishRefScript <-
        signTx
            execConfig
            cEra
            refScriptPublishWork
            "signed-tx"
            txBodyPublishFp
            [Some $ paymentKeyInfoPair wallet0]
    submitTx execConfig cEra signedTxPublishRefScript

    txIdPublishRefScript <- retrieveTransactionId execConfig signedTxPublishRefScript
    txIxPublishRefScript <-
        retryUntilJustM epochStateView (WaitForEpochs $ EpochInterval 2) $
            getEpochStateDetails epochStateView >>= getTxIx sbe txIdPublishRefScript scriptPublishUTxOAmount

    -- Step 2: Lock funds at script address
    refScriptLock <- H.createDirectoryIfMissing $ work </> "ref-script-lock"
    let transferAmount = 20_000_000

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

    txIdLock <- retrieveTransactionId execConfig signedTxLock
    txIxLock <-
        retryUntilJustM epochStateView (WaitForEpochs $ EpochInterval 2) $
            getEpochStateDetails epochStateView >>= getTxIx sbe txIdLock transferAmount

    -- Step 3: Query protocol parameters
    void $
        execCli'
            execConfig
            [ eraName
            , "query"
            , "protocol-parameters"
            , "--out-file"
            , work </> "pparams.json"
            ]

    -- Step 4: Build raw transaction to unlock the script UTxO
    refScriptUnlock <- H.createDirectoryIfMissing $ work </> "ref-script-unlock"
    let unsignedUnlockTx = File $ refScriptUnlock </> "unsigned-tx.tx"
        fee = 500_000 :: Coin

    collateralUTxO <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

    void $
        execCli'
            execConfig
            [ eraName
            , "transaction"
            , "build-raw"
            , "--tx-in"
            , prettyShow (TxIn txIdLock txIxLock)
            , "--spending-reference-tx-in-inline-datum-present"
            , "--spending-tx-in-reference"
            , prettyShow (TxIn txIdPublishRefScript txIxPublishRefScript)
            , "--spending-plutus-script-v3"
            , "--spending-reference-tx-in-redeemer-value"
            , "42"
            , "--spending-reference-tx-in-execution-units"
            , "(200000000, 200000)"
            , "--tx-in-collateral"
            , prettyShow collateralUTxO
            , "--tx-out"
            , Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show (unCoin (transferAmount - fee))
            , "--fee"
            , show (unCoin fee)
            , "--protocol-params-file"
            , work </> "pparams.json"
            , "--out-file"
            , unFile unsignedUnlockTx
            ]

    -- Step 5: Sign and submit
    signedUnlockTx <-
        signTx
            execConfig
            cEra
            refScriptUnlock
            "signed-tx"
            unsignedUnlockTx
            [Some $ paymentKeyInfoPair wallet1]

    submitTx execConfig cEra signedUnlockTx

    -- Verify the transaction landed on chain
    txIdUnlock <- retrieveTransactionId execConfig signedUnlockTx
    void $
        retryUntilJustM epochStateView (WaitForEpochs $ EpochInterval 2) $
            getEpochStateDetails epochStateView >>= getTxIx sbe txIdUnlock (transferAmount - fee)
