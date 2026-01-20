{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Cli.Plutus.MultiAssetReturnCollateral 
  ( hprop_collateral_with_tokens
  ) where 

import           Cardano.Api
import           Cardano.Testnet

import           Prelude
import qualified Data.Aeson as Aeson
import           Control.Monad (void)
import           Data.Default.Class
import qualified Data.Text as T
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import           Testnet.Components.Query
import           Testnet.Defaults
import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (integrationWorkspace, decodeEraUTxO)
import           Testnet.Types

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Collateral With Multiassets/"'@
hprop_collateral_with_tokens :: Property
hprop_collateral_with_tokens = integrationWorkspace "collateral-with-tokens" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
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
    , wallets=wallet0:wallet1:wallet2:_
    } <- createAndRunTestnet options def conf

  node <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket node
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  H.noteShow_ wallet0
  let utxoAddr = T.unpack $ paymentKeyInfoAddr wallet0
      utxoSKeyFile = signingKeyFp $ paymentKeyInfoPair wallet0
      socketPath = nodeSocketPath node

  epochStateView <- getEpochStateView configurationFile socketPath
  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0
  txinCollateral <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  void $ execCli' execConfig
    [ anyEraToString anyEra, "query", "utxo"
    , "--address", T.unpack $ paymentKeyInfoAddr wallet0
    , "--cardano-mode"
    , "--out-file", work </> "utxo-1.json"
    ]

  utxo1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
  H.noteShowM_ $ decodeEraUTxO sbe utxo1Json
    -- Create a simple always-succeeds Plutus V3 script
  plutusScript <- H.note $ work </> "always-succeeds-script.plutusV3"
  H.writeFile plutusScript $ T.unpack plutusV3Script

  
  -- Get the policy ID
  mintingPolicyId <- filter (/= '\n') <$>
    execCli' execConfig
      [ anyEraToString anyEra, "transaction"
      , "policyid"
      , "--script-file", plutusScript
      ]
      
  let assetName = "7161636f696e" -- "qacoin" in hex
  
  -- Create a Plutus script address
  plutusSpendingScriptAddr <-
    execCli' execConfig
      [ "latest", "address", "build"
      , "--payment-script-file", plutusScript
      ]


  -- STEP 1: Mint tokens and send to an address we control
  -- This address will later be used as a collateral UTxO
  let maCollateralAddress = T.unpack $ paymentKeyInfoAddr wallet2
      mintTokensTxBody = work </> "mint-tokens-tx-body"
      mintTokensTx = work </> "mint-tokens-tx"
      adaOnlyCollateralAddress = T.unpack $ paymentKeyInfoAddr wallet1
      mintValue = mconcat ["100 ", mintingPolicyId, ".", assetName]
      adaOnlyCollateralValue = mconcat [adaOnlyCollateralAddress, "+", show @Int 3_000_000]
      collateralToBeValue = mconcat [maCollateralAddress, "+", show @Int 5_000_000, "+", mintValue]
      fundPlutusScriptAddrVal = mconcat [plutusSpendingScriptAddr, "+", show @Int 2_000_000]

  void $ execCli' execConfig
    [ anyEraToString anyEra, "transaction", "build"
    , "--change-address", utxoAddr
    , "--tx-in", T.unpack $ renderTxIn txin1
    , "--tx-in-collateral", T.unpack $ renderTxIn txinCollateral
    , "--tx-out-return-collateral", adaOnlyCollateralValue
    , "--witness-override", show @Int 2
    , "--tx-out", collateralToBeValue 
    , "--tx-out", fundPlutusScriptAddrVal
    , "--tx-out-datum-hash-value", "0"
    , "--mint", mintValue
    , "--mint-script-file", plutusScript
    , "--mint-redeemer-value", "0"
    , "--out-file", mintTokensTxBody
    ]

  void $ execCli' execConfig
    [ "latest", "transaction", "sign"
    , "--tx-body-file", mintTokensTxBody
    , "--signing-key-file", utxoSKeyFile
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair wallet1
    , "--out-file", mintTokensTx
    ]
  let mintTxDebugFile = work </> "mint-tokens-tx-view.json"
  void $ execCli' execConfig ["debug", "transaction", "view", "--tx-file", mintTokensTx, "--out-file", mintTxDebugFile]

  H.note_ "Mint Tokens Tx"
  txMintJson :: Aeson.Value <- H.leftFailM . H.readJsonFile $ mintTxDebugFile
  H.noteShowPretty_ txMintJson

  void $ execCli' execConfig
    [ "latest", "transaction", "submit"
    , "--tx-file", mintTokensTx
    ]


  -- STEP 2: Attempt to spend from script with collateral containing tokens
  -- This will fail because collateral cannot contain non-ADA tokens
  
  -- Wait for transactions to be processed and find UTxOs
  _ <- waitForBlocks epochStateView 1
  
  -- Find the UTxO with tokens at wallet1 (for collateral)
  txinCollateralWithTokensM <- 
    findLargestMultiAssetUtxoWithAddress epochStateView sbe $ T.pack maCollateralAddress
  (txinCollateralWithTokens, collateralTxOut) <- H.evalMaybe txinCollateralWithTokensM
  H.note_ "Collateral TxOut"
  H.noteShow_  collateralTxOut
  -- Find the UTxO at the script address
  plutusScriptTxIn <- fmap fst . retryUntilJustM epochStateView (WaitForBlocks 10) $
    findLargestUtxoWithAddress epochStateView sbe $ T.pack plutusSpendingScriptAddr

  let spendScriptUTxOTxBody = work </> "spend-script-utxo-tx-body"
      spendScriptUTxOTx = work </> "spend-script-utxo-tx"

  void $ execCli' execConfig
    [ anyEraToString anyEra, "transaction", "build"
    , "--change-address", T.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", T.unpack $ renderTxIn plutusScriptTxIn
    , "--tx-in-script-file", plutusScript
    , "--tx-in-datum-value", "0"
    , "--tx-in-redeemer-value", "0"
    , "--witness-override", show @Int 2
    , "--tx-in-collateral", T.unpack $ renderTxIn txinCollateralWithTokens  -- This is the key issue - using collateral with tokens
    , "--out-file", spendScriptUTxOTxBody
    ]
  let prettyTxBodyFile = work </> "spend-script-utxo-tx-body-view.json"
  void $ execCli' execConfig ["debug", "transaction", "view", "--tx-body-file", spendScriptUTxOTxBody, "--out-file", prettyTxBodyFile] 
  
  txBodyPrettyJson :: Aeson.Value <- H.leftFailM . H.readJsonFile $ prettyTxBodyFile
  H.note_ "Tx body"
  H.noteShowPretty_ txBodyPrettyJson

  void $ execCli' execConfig
    [ "latest", "transaction", "sign"
    , "--tx-body-file", spendScriptUTxOTxBody
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair wallet1
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair wallet2
    , "--out-file", spendScriptUTxOTx
    ]
  
  let prettyTxFile = work </> "spend-script-utxo-tx-view.json"
  void $ execCli' execConfig ["debug", "transaction", "view", "--tx-file", spendScriptUTxOTx, "--out-file", prettyTxFile] 
  
  txPrettyJson :: Aeson.Value <- H.leftFailM . H.readJsonFile $ prettyTxFile
  H.noteShowPretty_ txPrettyJson

  void $ execCli' execConfig
    [ "latest", "transaction", "submit"
    , "--tx-file", spendScriptUTxOTx
    ]

  H.success