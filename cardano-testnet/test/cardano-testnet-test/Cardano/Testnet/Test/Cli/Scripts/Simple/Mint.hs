
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


module Cardano.Testnet.Test.Cli.Scripts.Simple.Mint
  ( hprop_simple_script_mint
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.UTxO (difference, size)

import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import           Data.Default.Class
import qualified Data.Text as T
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import           Testnet.Components.Query
import           Testnet.Defaults
import           Testnet.Process.Cli.SPO
import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Types

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Simple Script.Simple Script Mint/"'@
hprop_simple_script_mint :: Property
hprop_simple_script_mint = integrationWorkspace "simple-script-mint" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let
    tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'
    ceo = ConwayEraOnwardsConway
    sbe = convert ceo
    era = toCardanoEra sbe
    anyEra = AnyCardanoEra era
    options = def { cardanoNodeEra = AnyShelleyBasedEra sbe }

  TestnetRuntime
    { configurationFile
    , testnetMagic
    , testnetNodes
    , wallets=wallet0:wallet1:_
    } <- createAndRunTestnet options def conf

  node <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket node
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  H.noteShow_ wallet0
  let utxoAddr = T.unpack $ paymentKeyInfoAddr wallet0
      utxoSKeyFile = signingKeyFp $ paymentKeyInfoPair wallet0
      utxoSKeyFile2 = signingKeyFp $ paymentKeyInfoPair wallet1
      utxoVKeyFile2 = verificationKeyFp $ paymentKeyInfoPair wallet1
      socketPath = nodeSocketPath node

  epochStateView <- getEpochStateView configurationFile socketPath
  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  plutusScript <- H.note $ work </> "always-succeeds-script.plutusV3"
  H.writeFile plutusScript $ T.unpack plutusV3SupplementalDatumScript

  let sendAdaToScriptAddressTxBody = work </> "send-ada-to-script-address-tx-body"

  plutusSpendingScriptAddr <-
    execCli' execConfig
      [ "latest", "address", "build"
      , "--payment-script-file", plutusScript
      ]

  let assetName = "4D696C6C6172436F696E"
  H.note_ $ "plutusSpendingScriptAddr: " <> plutusSpendingScriptAddr

  scriptdatumhash <- filter (/= '\n') <$>
    execCli' execConfig
      [ "latest", "transaction", "hash-script-data"
      , "--script-data-value", "0"
      ]

  supplementalDatumJsonFile
    <- H.note $ work </> "supplemental-datum.json"
  H.writeFile supplementalDatumJsonFile "{\"int\":1}"

  scriptStakeRegistrationCertificate
    <- H.note $ work </> "script-stake-registration-certificate"

  keyDeposit <- fromIntegral . L.unCoin <$> getKeyDeposit epochStateView ceo
  -- Create script stake registration certificate
  createScriptStakeRegistrationCertificate
    tempAbsPath
    anyEra
    plutusScript
    keyDeposit
    scriptStakeRegistrationCertificate

  -- 1. Put UTxO and datum at script address
  --    Register script stake address
  void $ execCli' execConfig
    [ anyEraToString anyEra, "transaction", "build"
    , "--change-address", T.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", T.unpack $ renderTxIn txin1
    , "--tx-out", plutusSpendingScriptAddr <> "+" <> show @Int 5_000_000
    , "--tx-out-datum-hash", scriptdatumhash
    , "--out-file", sendAdaToScriptAddressTxBody
    ]

  let sendAdaToScriptAddressTx = work </> "send-ada-to-script-address-tx"
  void $ execCli' execConfig
    [ "latest", "transaction", "sign"
    , "--tx-body-file", sendAdaToScriptAddressTxBody
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", sendAdaToScriptAddressTx
    ]

  void $ execCli' execConfig
    [ "latest", "transaction", "submit"
    , "--tx-file", sendAdaToScriptAddressTx
    ]

  -- 2. Successfully mint
  txinCollateral <- findLargestUtxoForPaymentKey epochStateView sbe wallet1
  plutusScriptTxIn <- fmap fst . retryUntilJustM epochStateView (WaitForBlocks 3) $
    findLargestUtxoWithAddress epochStateView sbe $ T.pack plutusSpendingScriptAddr

  let spendScriptUTxOTxBody = work </> "spend-script-utxo-tx-body"
      spendScriptUTxOTx = work </> "spend-script-utxo-tx"
 
      txoutWithSupplementalDatum = mconcat [utxoAddr, "+", show @Int 1_000_000]

  -- Mint with a simple script 
  reqSignerHash <- filter (/= '\n') <$>
    execCli' execConfig
      [ anyEraToString anyEra, "address", "key-hash"
      , "--payment-verification-key-file", utxoVKeyFile2
      ]
  simpleScriptFp <- H.note $ work </> "example-simple-script.json"
  H.writeFile simpleScriptFp $ T.unpack $ simpleScript $ T.pack reqSignerHash

  simpleMintingPolicyId <- filter (/= '\n') <$>
     execCli' execConfig
       [ anyEraToString anyEra, "transaction"
       , "policyid"
       , "--script-file", simpleScriptFp
       ]

  plutusScriptFp <- H.note $ work </> "example-plutus-script.json"
  H.writeFile plutusScriptFp $ T.unpack plutusV2Script

  plutusScriptTestPolId <- filter (/= '\n') <$>
     execCli' execConfig
       [ anyEraToString anyEra, "transaction"
       , "policyid"
       , "--script-file", plutusScriptFp
       ]

  H.note_ plutusScriptTestPolId
  
  let mintValue = mconcat ["5 ", simpleMintingPolicyId, ".", assetName]
      txout = mconcat [ utxoAddr, "+", show @Int 2_000_000
                       , "+", mintValue
                       ]
  void $ execCli' execConfig
    [ anyEraToString anyEra, "transaction", "build"
    , "--change-address", T.unpack $ paymentKeyInfoAddr wallet1
    , "--tx-in-collateral", T.unpack $ renderTxIn txinCollateral
    , "--tx-in", T.unpack $ renderTxIn plutusScriptTxIn
    , "--tx-in-script-file", plutusScript
    , "--tx-in-datum-value", "0"
    , "--tx-in-redeemer-value", "0"
    , "--mint", mintValue
    , "--mint-script-file", simpleScriptFp
    , "--certificate-file", scriptStakeRegistrationCertificate
    , "--certificate-script-file", plutusScript
    , "--certificate-redeemer-value", "0"
    , "--tx-out", txout
    , "--tx-out", txoutWithSupplementalDatum
    , "--tx-out-datum-embed-file", supplementalDatumJsonFile
    , "--out-file", spendScriptUTxOTxBody
    ]

  void $ execCli' execConfig
    [ "latest", "transaction", "sign"
    , "--tx-body-file", spendScriptUTxOTxBody
    , "--signing-key-file", utxoSKeyFile2
    , "--out-file", spendScriptUTxOTx
    ]
  utxoPre <- findAllUtxos epochStateView sbe
  void $ execCli' execConfig
    [ "latest", "transaction", "submit"
    , "--tx-file", spendScriptUTxOTx
    ]
  void $ waitForBlocks epochStateView 1 
  utxoPost <- findAllUtxos epochStateView sbe

  let diff = difference  utxoPost utxoPre

  size diff H.=== 3
