{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Redundant return" -}
{- HLINT ignore "Use head" -}
{- HLINT ignore "Use let" -}

module Cardano.Testnet.Test.Cli.Conway.Plutus
  ( hprop_plutus_v3
  ) where

import           Cardano.Api

import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Testnet.Components.Configuration
import           Testnet.Components.SPO
import           Testnet.Defaults
import qualified Testnet.Process.Run as H
import           Testnet.Process.Run
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Test all possible Plutus script purposes
-- Currently tested:
-- Spending YES
-- Minting YES
-- Rewarding NO
-- Certifying YES
-- Voting NO
-- Proposing NO
hprop_plutus_v3 :: Property
hprop_plutus_v3 = H.integrationWorkspace "all-plutus-script-purposes" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let
    tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'
    sbe = ShelleyBasedEraConway
    era = toCardanoEra sbe
    anyEra = AnyCardanoEra era
    options = cardanoDefaultTestnetOptions
                        { cardanoNodes = cardanoDefaultTestnetNodeOptions
                        , cardanoSlotLength = 0.1
                        , cardanoNodeEra = anyEra -- TODO: We should only support the latest era and the upcoming era
                        }
  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets
    } <- cardanoTestnetDefault options conf

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let utxoAddr = Text.unpack $ paymentKeyInfoAddr $ wallets !! 0
      utxoAddr2 = Text.unpack $ paymentKeyInfoAddr $ wallets !! 1
      utxoSKeyFile = paymentSKey . paymentKeyInfoPair $ wallets !! 0
      utxoSKeyFile2 = paymentSKey . paymentKeyInfoPair $ wallets !! 1

  void $ H.execCli' execConfig
    [ convertToEraString anyEra, "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--out-file", work </> "utxo-1.json"
    ]
  utxo1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
  UTxO utxo1 <- H.noteShowM $ decodeEraUTxO sbe utxo1Json

  let keys1 = Map.keys utxo1
  H.note_ $ "keys1: " <> show (length keys1)
  txin1 <- H.noteShow $ keys1 !! 0

  plutusMintingScript <- H.note $ work </> "always-succeeds-non-spending-script.plutusV3"
  H.writeFile plutusMintingScript $ Text.unpack plutusV3NonSpendingScript

  plutusSpendingScript <- H.note $ work </> "always-succeeds-spending-script.plutusV3"
  H.writeFile plutusSpendingScript $ Text.unpack plutusV3SpendingScript

  let sendAdaToScriptAddressTxBody = work </> "send-ada-to-script-address-tx-body"

  plutusSpendingScriptAddr <-
    H.execCli' execConfig
      [ "address", "build"
      , "--payment-script-file", plutusSpendingScript
      ]

  mintingPolicyId <- filter (/= '\n') <$>
    H.execCli' execConfig
      [ convertToEraString anyEra, "transaction"
      , "policyid"
      , "--script-file", plutusMintingScript
      ]
  let assetName = "4D696C6C6172436F696E"
  H.note_ $ "plutusSpendingScriptAddr: " <> plutusSpendingScriptAddr

  scriptdatumhash <- filter (/= '\n') <$>
    H.execCli' execConfig
      [ "transaction", "hash-script-data"
      , "--script-data-value", "0"
      ]

  scriptStakeRegistrationCertificate
    <- H.note $ work </> "script-stake-registration-certificate"

  -- Create script stake registration certificate
  createScriptStakeRegistrationCertificate
    tempAbsPath
    anyEra
    plutusSpendingScript
    0
    scriptStakeRegistrationCertificate

  -- 1. Put UTxO and datum at Plutus spending script address
  --    Register script stake address
  void $ execCli' execConfig
    [ convertToEraString anyEra, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 0
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", plutusSpendingScriptAddr <> "+" <> show @Int 5_000_000
    , "--tx-out-datum-hash", scriptdatumhash
    , "--out-file", sendAdaToScriptAddressTxBody
    ]

  let sendAdaToScriptAddressTx = work </> "send-ada-to-script-address-tx"
  void $ execCli' execConfig
    [ "transaction", "sign"
    , "--tx-body-file", sendAdaToScriptAddressTxBody
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", sendAdaToScriptAddressTx
    ]

  void $ execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", sendAdaToScriptAddressTx
    ]

  H.threadDelay 10_000_000
  -- 2. Successfully spend conway spending script
  void $ H.execCli' execConfig
    [ convertToEraString anyEra, "query", "utxo"
    , "--address", utxoAddr2
    , "--cardano-mode"
    , "--out-file", work </> "utxo-2.json"
    ]
  utxo2Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-2.json"
  UTxO utxo2 <- H.noteShowM $ decodeEraUTxO sbe utxo2Json

  let keys2 = Map.keys utxo2
  H.note_ $ "keys2: " <> show (length keys2)
  txinCollateral <- H.noteShow $ keys2 !! 0

  void $ H.execCli' execConfig
    [ convertToEraString anyEra, "query", "utxo"
    , "--address", plutusSpendingScriptAddr
    , "--cardano-mode"
    , "--out-file", work </> "plutus-script-utxo.json"
    ]
  utxoPlutusJson <- H.leftFailM . H.readJsonFile $ work </> "plutus-script-utxo.json"
  UTxO utxoPlutus <- H.noteShowM $ decodeEraUTxO sbe utxoPlutusJson

  let keys3 = Map.keys utxoPlutus
  H.note_ $ "keys3: " <> show (length keys3)

  plutusScriptTxIn <- H.noteShow $ keys3 !! 0
  let spendScriptUTxOTxBody = work </> "spend-script-utxo-tx-body"
      spendScriptUTxOTx = work </> "spend-script-utxo-tx"
      mintValue = mconcat ["5 ", mintingPolicyId, ".", assetName]
      txout = mconcat [ utxoAddr, "+", show @Int 2_000_000
                      , "+", mintValue
                      ]

  void $ execCli' execConfig
    [ convertToEraString anyEra, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 1
    , "--tx-in-collateral", Text.unpack $ renderTxIn txinCollateral
    , "--tx-in", Text.unpack $ renderTxIn plutusScriptTxIn
    , "--tx-in-script-file", plutusSpendingScript
    , "--tx-in-datum-value", "0"
    , "--tx-in-redeemer-value", "0"
    , "--mint", mintValue
    , "--mint-script-file", plutusMintingScript
    , "--mint-redeemer-value", "0"
    , "--certificate-file", scriptStakeRegistrationCertificate
    , "--certificate-script-file", plutusSpendingScript
    , "--certificate-redeemer-value", "0"
    , "--tx-out", txout
    , "--out-file", spendScriptUTxOTxBody
    ]

  void $ execCli' execConfig
    [ "transaction", "sign"
    , "--tx-body-file", spendScriptUTxOTxBody
    , "--signing-key-file", utxoSKeyFile2
    , "--out-file", spendScriptUTxOTx
    ]

  void $ execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", spendScriptUTxOTx
    ]
  H.success


