{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Cli.Scripts.Simple.CostCalculation
  ( hprop_ref_simple_script_mint
  ) where

import           Cardano.Api hiding (Value)
import           Cardano.Api.Experimental (Some (Some))
import           Cardano.Api.Ledger (EpochInterval (..))
import           Cardano.Testnet

import           Prelude
import           Testnet.Types

import           Control.Monad (void)

import           Data.Default.Class (Default (def))
import qualified Data.Text as Text
import           System.Directory (makeAbsolute)
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Testnet.Components.Query (getEpochStateView, getTxIx,
                   watchEpochStateUpdate)
import qualified Testnet.Defaults  as Defaults
import           Testnet.Process.Cli.Transaction (TxOutAddress (..), mkSpendOutputsOnlyTx,
                   retrieveTransactionId, signTx, submitTx)
import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Process.RunIO (liftIOAnnotated)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types (eraToString)

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Simple Script.Simple Reference Script Mint/"'@
hprop_ref_simple_script_mint :: Property
hprop_ref_simple_script_mint = integrationRetryWorkspace 2 "ref-simple-script" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
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
    createAndRunTestnet options def conf

  poolNode1 <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket poolNode1
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  epochStateView <- getEpochStateView configurationFile (nodeSocketPath poolNode1)

  refScriptSizeWork <- H.createDirectoryIfMissing $ work </> "ref-script-publish"
  -- No longer using plutus script for reference script
  _plutusV3Script <-
    File <$> liftIOAnnotated (makeAbsolute "test/cardano-testnet-test/files/plutus/v3/always-succeeds.plutus")

  let utxoVKeyFile2 = verificationKeyFp $ paymentKeyInfoPair wallet1
      
  reqSignerHash <- filter (/= '\n') <$>
    execCli' execConfig
      [ eraName, "address", "key-hash"
      , "--payment-verification-key-file", utxoVKeyFile2
      ]
  simpleScriptFp <- H.note $ work </> "example-simple-script.json"
  H.writeFile simpleScriptFp $ Text.unpack $ Defaults.simpleScript $ Text.pack reqSignerHash
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
      [(ScriptAddress $ File simpleScriptFp, scriptPublishUTxOAmount, Just $ File simpleScriptFp)]
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
      [(ScriptAddress $ File simpleScriptFp, transferAmount, Nothing)]
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
  refScriptHash <- execCli' execConfig [ eraName, "transaction", "policyid", "--script-file", simpleScriptFp]
  H.note_ $ "Reference script hash: " <> refScriptHash

  void $ execCli' execConfig
      [ eraName, "query", "utxo"
      , "--whole-utxo"
      , "--cardano-mode"
      , "--out-file", work </> "utxo-1.json"
      ]
  H.cat $ work </> "utxo-1.json"
  -- Attempt to mint from a refernce script
  let assetName = "4D696C6C6172436F696E"

 
  simpleMintingPolicyId <- filter (/= '\n') <$>
     execCli' execConfig
       [ eraName, "transaction"
       , "policyid"
       , "--script-file", simpleScriptFp
       ]
  let mintValue = mconcat ["5 ", simpleMintingPolicyId, ".", assetName]
  void $
    execCli'
      execConfig
      [ eraName
      , "transaction", "build"
      , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
      , "--tx-in", prettyShow (TxIn txIdLock txIxLock)
      , "--mint", mintValue
      , "--simple-minting-script-tx-in-reference", prettyShow (TxIn txIdPublishRefScript txIxPublishRefScript)
      , "--policy-id", simpleMintingPolicyId
      , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show (unCoin (transferAmount - enoughAmountForFees))
      , "--out-file", unFile unsignedUnlockTx
      ]
  viewTx <- execCli' execConfig
      ["debug"
      , "transaction", "view"
      , "--tx-file", unFile unsignedUnlockTx
      ]
  H.note_ viewTx
  signedUnlockTx <-
    signTx
      execConfig
      cEra
      refScriptUnlock
      "signed-tx"
      unsignedUnlockTx
      [Some $ paymentKeyInfoPair wallet1]

  submitTx execConfig cEra signedUnlockTx
