{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


module Cardano.Testnet.Test.Cli.Conway.Plutus
  ( hprop_plutus_v3
  ) where

import           Cardano.Api

import           Cardano.Testnet

import           Prelude

import           Control.Concurrent (threadDelay)
import           Control.Monad (void)
import qualified Data.Text as Text
import           System.FilePath ((</>))
import qualified System.Info as SYS

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

-- | Test all possible Plutus script purposes
-- Currently tested:
-- Spending YES
-- Minting YES
-- Rewarding NO
-- Certifying YES
-- Voting NO
-- Proposing NO
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/PlutusV3/"'@
hprop_plutus_v3 :: Property
hprop_plutus_v3 = integrationWorkspace "all-plutus-script-purposes" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
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
                { cardanoNodeEra = anyEra -- TODO: We should only support the latest era and the upcoming era
                }

  TestnetRuntime
    { configurationFile
    , testnetMagic
    , poolNodes
    , wallets=wallet0:wallet1:_
    } <- cardanoTestnetDefault options conf

  PoolNode{poolRuntime} <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket poolRuntime
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  H.noteShow_ wallet0
  let utxoAddr = Text.unpack $ paymentKeyInfoAddr wallet0
      utxoSKeyFile = signingKeyFp $ paymentKeyInfoPair wallet0
      utxoSKeyFile2 = signingKeyFp $ paymentKeyInfoPair wallet1
      socketPath = nodeSocketPath poolRuntime

  epochStateView <- getEpochStateView configurationFile socketPath
  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  plutusScript <- H.note $ work </> "always-succeeds-script.plutusV3"
  H.writeFile plutusScript $ Text.unpack plutusV3Script

  plutusV2ScriptFp <- H.note $ work </> "always-succeeds-script.plutusV2"
  H.writeFile plutusV2ScriptFp $ Text.unpack plutusV2Script

  let sendAdaToScriptAddressTxBody = work </> "send-ada-to-script-address-tx-body"

  plutusSpendingScriptAddr <-
    execCli' execConfig
      [ "address", "build"
      , "--payment-script-file", plutusScript
      ]

  _plutusV2ScriptAddr <-
    execCli' execConfig
      [ "address", "build"
      , "--payment-script-file", plutusV2ScriptFp
      ]

  mintingPolicyId <- filter (/= '\n') <$>
    execCli' execConfig
      [ anyEraToString anyEra, "transaction"
      , "policyid"
      , "--script-file", plutusScript
      ]
  let assetName = "4D696C6C6172436F696E"
  H.note_ $ "plutusSpendingScriptAddr: " <> plutusSpendingScriptAddr

  scriptdatumhash <- filter (/= '\n') <$>
    execCli' execConfig
      [ "transaction", "hash-script-data"
      , "--script-data-value", "0"
      ]

  scriptStakeRegistrationCertificate
    <- H.note $ work </> "script-stake-registration-certificate"

  -- Create script stake registration certificate
  createScriptStakeRegistrationCertificate
    tempAbsPath
    anyEra
    plutusScript
    0
    scriptStakeRegistrationCertificate

  scriptStakeRegistrationCertificateV2
    <- H.note $ work </> "script-stake-registration-certificateV2"

  createScriptStakeRegistrationCertificate
    tempAbsPath
    anyEra
    plutusV2ScriptFp
    0
    scriptStakeRegistrationCertificateV2

  scriptStakeDeRegistrationCertificateV2
    <- H.note $ work </> "script-stake-deregistration-certificateV2"

  -- Create script stake registration certificate
  createScriptStakeDeregistrationCertificate
    tempAbsPath
    anyEra
    plutusV2ScriptFp
    0
    scriptStakeDeRegistrationCertificateV2

  -- 1. Put UTxO and datum at Plutus spending script address
  --    Register script stake address
  void $ execCli' execConfig
    [ anyEraToString anyEra, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
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

  -- 2. Successfully spend conway spending script
  txinCollateral <- findLargestUtxoForPaymentKey epochStateView sbe wallet1
  _plutusScriptTxIn <- fmap fst . retryUntilJustM epochStateView (WaitForBlocks 3) $
    findLargestUtxoWithAddress epochStateView sbe $ Text.pack plutusSpendingScriptAddr

  let spendScriptUTxOTxBody = work </> "spend-script-utxo-tx-body"
      spendScriptUTxOTx = work </> "spend-script-utxo-tx"
      _mintValue = mconcat ["5 ", mintingPolicyId, ".", assetName]
      txout = mconcat [ utxoAddr, "+", show @Int 2_000_000
                    --  , "+", mintValue
                      ]
  txin3 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0
  ver <- execCli' execConfig ["--version"]
  H.note_ "Cli version"
  H.note_ ver
  void $ execCli' execConfig
    [ anyEraToString anyEra, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
    , "--tx-in-collateral", Text.unpack $ renderTxIn txinCollateral
    , "--tx-in", Text.unpack $ renderTxIn txin3
  --  , "--tx-in-script-file", plutusScript
  --  , "--tx-in-datum-value", "0"
  --  , "--tx-in-redeemer-value", "0"
  --  , "--mint", mintValue
  --  , "--mint-script-file", plutusScript
  --  , "--mint-redeemer-value", "0"
    -- , "--certificate-file", scriptStakeRegistrationCertificate
    -- , "--certificate-script-file", plutusScript
    -- , "--certificate-redeemer-value", "0"
    -- TODO: Left off here
    , "--certificate-file", scriptStakeRegistrationCertificateV2
    , "--certificate-script-file", plutusV2ScriptFp
    , "--certificate-redeemer-value", "0"
    , "--certificate-file", scriptStakeDeRegistrationCertificateV2
    , "--certificate-script-file", plutusV2ScriptFp
    , "--certificate-redeemer-value", "42"
    , "--tx-out", txout
    , "--out-file", spendScriptUTxOTxBody
    ]

  void $ execCli' execConfig
    [ "transaction", "sign"
    , "--tx-body-file", spendScriptUTxOTxBody
    , "--signing-key-file", utxoSKeyFile
    , "--signing-key-file", utxoSKeyFile2
    , "--out-file", spendScriptUTxOTx
    ]

  void $ execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", spendScriptUTxOTx
    ]

  liftIO $ threadDelay 10_000_000

--  scriptStakeDeRegistrationCertificate
--    <- H.note $ work </> "script-stake-deregistration-certificate"
--
--  -- Create script stake registration certificate
--  createScriptStakeDeregistrationCertificate
--    tempAbsPath
--    anyEra
--    plutusScript
--    0
--    scriptStakeDeRegistrationCertificate
--
--
--
--
--  deregScriptUTxOTxBody
--    <- H.note $ work </> "dereg-script-txbody"
--  sccriptUtxos <- findUtxosWithAddress epochStateView sbe $ Text.pack plutusSpendingScriptAddr
--
--  H.note_ $ show sccriptUtxos
--  let newtxout = mconcat [ utxoAddr, "+", show @Int 2_000_000]
--  txin2 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1
--
--  void $ execCli' execConfig
--    [ anyEraToString anyEra, "transaction", "build"
--    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
--    , "--tx-in", Text.unpack $ renderTxIn txin2
--    , "--tx-in-collateral", Text.unpack $ renderTxIn txinCollateral
--   -- , "--certificate-file", scriptStakeDeRegistrationCertificate
--   -- , "--certificate-script-file", plutusScript
--   -- , "--certificate-redeemer-value", "0"
--    , "--certificate-file", scriptStakeDeRegistrationCertificateV2
--    , "--certificate-script-file", plutusV2ScriptFp
--    , "--certificate-redeemer-value", "42"
--    , "--witness-override", "2"
--    , "--tx-out", newtxout
--    , "--out-file", deregScriptUTxOTxBody
--    ]
--
--  deregScriptUTxOTx
--    <- H.note $ work </> "dereg-script-tx"
--
--
--  void $ execCli' execConfig
--    [ "transaction", "sign"
--    , "--tx-body-file", deregScriptUTxOTxBody
--    , "--signing-key-file", utxoSKeyFile2
--    , "--signing-key-file", utxoSKeyFile
--    , "--out-file", deregScriptUTxOTx
--    ]
--
--  void $ execCli' execConfig
--    [ "transaction", "submit"
--    , "--tx-file", deregScriptUTxOTx
--    ]
--
  H.success

