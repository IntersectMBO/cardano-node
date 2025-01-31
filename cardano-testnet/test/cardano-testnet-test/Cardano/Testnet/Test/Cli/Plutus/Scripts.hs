{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


module Cardano.Testnet.Test.Cli.Plutus.Scripts
  ( hprop_plutus_purposes_v3
  , hprop_tx_two_script_certs_v2
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L

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
import           Testnet.Process.Run (execCli', execCliAny, mkExecConfig)
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
-- Execute me with:
-- @DISABLE_RETRIES=1 cabal run cardano-testnet-test -- -p "/Spec.hs.Spec.Ledger Events.Plutus.Scripts/"@
hprop_plutus_purposes_v3 :: Property
hprop_plutus_purposes_v3 = integrationWorkspace "all-plutus-script-purposes" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
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
    } <- cardanoTestnetDefault options def conf

  node <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket node
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  H.noteShow_ wallet0
  let utxoAddr = T.unpack $ paymentKeyInfoAddr wallet0
      utxoSKeyFile = signingKeyFp $ paymentKeyInfoPair wallet0
      utxoSKeyFile2 = signingKeyFp $ paymentKeyInfoPair wallet1
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

  -- 1. Put UTxO and datum at Plutus spending script address
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

  -- 2. Successfully spend conway spending script
  txinCollateral <- findLargestUtxoForPaymentKey epochStateView sbe wallet1
  plutusScriptTxIn <- fmap fst . retryUntilJustM epochStateView (WaitForBlocks 3) $
    findLargestUtxoWithAddress epochStateView sbe $ T.pack plutusSpendingScriptAddr

  let spendScriptUTxOTxBody = work </> "spend-script-utxo-tx-body"
      spendScriptUTxOTx = work </> "spend-script-utxo-tx"
      mintValue = mconcat ["5 ", mintingPolicyId, ".", assetName]
      txout = mconcat [ utxoAddr, "+", show @Int 2_000_000
                      , "+", mintValue
                      ]
      txoutWithSupplementalDatum = mconcat [utxoAddr, "+", show @Int 1_000_000]

  void $ execCli' execConfig
    [ anyEraToString anyEra, "transaction", "build"
    , "--change-address", T.unpack $ paymentKeyInfoAddr wallet1
    , "--tx-in-collateral", T.unpack $ renderTxIn txinCollateral
    , "--tx-in", T.unpack $ renderTxIn plutusScriptTxIn
    , "--tx-in-script-file", plutusScript
    , "--tx-in-datum-value", "0"
    , "--tx-in-redeemer-value", "0"
    , "--mint", mintValue
    , "--mint-script-file", plutusScript
    , "--mint-redeemer-value", "0"
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

  void $ execCli' execConfig
    [ "latest", "transaction", "submit"
    , "--tx-file", spendScriptUTxOTx
    ]

  H.success


-- |
-- Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/PlutusV2 Create transaction with two script certs/"'@
hprop_tx_two_script_certs_v2 :: Property
hprop_tx_two_script_certs_v2 = integrationWorkspace "tx-2-script-certs" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
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
    , wallets=wallet0:_
    } <- cardanoTestnetDefault options def conf

  node <- H.headM testnetNodes
  SpoNodeKeys{poolNodeKeysCold=KeyPair{verificationKey=spoKeyCold}} <- H.nothingFail $ poolKeys node
  poolSprocket1 <- H.noteShow $ nodeSprocket node
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  H.noteShow_ wallet0
  let utxoAddr = T.unpack $ paymentKeyInfoAddr wallet0
      utxoSKeyFile = signingKeyFp $ paymentKeyInfoPair wallet0
      socketPath = nodeSocketPath node

  epochStateView <- getEpochStateView configurationFile socketPath
  txin <- T.unpack . renderTxIn <$> findLargestUtxoForPaymentKey epochStateView sbe wallet0

  plutusScript <- H.note $ work </> "always-succeeds-script.plutusV2"
  H.writeFile plutusScript $ T.unpack plutusV2StakeScript

  scriptStakeRegistrationCertificate
    <- H.note $ work </> "script-stake-registration-certificate"

  keyDeposit <- fromIntegral . L.unCoin <$> getKeyDeposit epochStateView ceo

  -- Create script stake registration and certificates
  createScriptStakeRegistrationCertificate
    tempAbsPath
    anyEra
    plutusScript
    keyDeposit
    scriptStakeRegistrationCertificate

  scriptStakeDelegationCertificate
    <- H.note $ work </> "script-stake-delegation-certificate"

  createScriptStakeDelegationCertificate
    tempAbsPath
    anyEra
    plutusScript
    spoKeyCold
    scriptStakeDelegationCertificate

  let txbody = work </> "two-certs-tx-body"
      tx = work </> "two-certs-tx"
      txout = mconcat [ utxoAddr, "+", show @Int 2_000_000 ]

  let txBuildArgs =
        [ anyEraToString anyEra, "transaction", "build"
        , "--change-address", T.unpack $ paymentKeyInfoAddr wallet0
        , "--tx-in-collateral", txin
        , "--tx-in", txin
        , "--certificate-file", scriptStakeRegistrationCertificate
        , "--certificate-script-file", plutusScript
        , "--certificate-redeemer-value", "{\"int\":42}"
        , "--certificate-file", scriptStakeDelegationCertificate
        , "--certificate-script-file", plutusScript
        , "--certificate-redeemer-value", "{\"int\":42}"
        , "--tx-out", txout
        , "--witness-override", "1"
        ]

  (_,_, stderr1') <- execCliAny execConfig $
    txBuildArgs <> [ "--calculate-plutus-script-cost", "/dev/stderr" ]
  H.note_ stderr1'

  (_,_, stderr2') <- execCliAny execConfig $
    txBuildArgs <> [  "--out-file", txbody ]
  H.note_ stderr2'

  void $ execCli' execConfig
    [ "latest", "transaction", "sign"
    , "--tx-body-file", txbody
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", tx
    ]

  void $ execCli' execConfig
    [ "latest", "transaction", "submit"
    , "--tx-file", tx
    ]

  H.success

