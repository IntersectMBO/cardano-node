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
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Output (QueryTipLocalStateOutput (..))
import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text.Encoding as Text
import qualified Data.Time.Clock as DTC
import           GHC.Stack (callStack)
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

import           Testnet.Components.Configuration
import           Testnet.Components.SPO
import qualified Testnet.Process.Run as H
import           Testnet.Process.Run
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime


-- | Test all possible Plutus script purposes
-- Currently tested:
-- Spending YES
-- Minting YES
-- Rewarding NO
-- Certifying YES
-- Voting NO
-- Proposing NO

-- Script stake registration and delegation in a single tx works in Conway! (tried with spending and minting script) 
-- Try Babbage
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
  let utxoAddr = Text.unpack $ paymentKeyInfoAddr $ head wallets
      utxoAddr2 = Text.unpack $ paymentKeyInfoAddr $ wallets !! 1
      utxoSKeyFile = paymentSKey . paymentKeyInfoPair $ head wallets
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

  plutusSpendingScript <- H.note "/home/jordan/Repos/Work/intersect-mbo/cardano-node/cardano-testnet/test/cardano-testnet-test/files/plutus/v3/always-succeeds.plutus"
  plutusMintingScript <- H.note "/home/jordan/Repos/Work/intersect-mbo/cardano-node/cardano-testnet/test/cardano-testnet-test/files/plutus/v3/minting-script.plutus"
  datumFile <- H.note "/home/jordan/Repos/Work/intersect-mbo/cardano-node/cardano-testnet/test/cardano-testnet-test/files/plutus/v3/42.datum"
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
      , "--script-data-file", datumFile
      ]

  scriptStakeRegistrationCertificate
    <- H.note $ work </> "script-stake-registration-certificate"

  -- Create script stake registration certificate
  createScriptStakeRegistrationCertificate
    tempAbsPath
    anyEra
    plutusMintingScript
    0
    scriptStakeRegistrationCertificate
  -- Create script stake delegation certificate 

  void $ execCli' execConfig
    [ "query", "stake-pools"
    , "--out-file", work </> "stake-pools.json"
    ]

  currRegPools <- H.leftFailM $ H.readJsonFile $ work </> "stake-pools.json"
  poolIds <- H.noteShowM $ H.jsonErrorFail $ Aeson.fromJSON @(Set PoolId) currRegPools
  scriptStakeDelegationCertFp <- H.note $ work </> "script-stake-delegation.cert"
  void $ execCli
      [ convertToEraString anyEra
      , "stake-address", "stake-delegation-certificate"
      , "--stake-script-file", plutusMintingScript
      , "--stake-pool-id", Text.unpack $ serialiseToBech32 (Set.elemAt 0 poolIds)
      , "--out-file", scriptStakeDelegationCertFp
      ]


  -- 1. Put UTxO and datum at Plutus spending script address
  --    Register script stake address
  void $ execCli' execConfig
    [ convertToEraString anyEra, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ head wallets
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
  -- Try to delegate here as well
  void $ execCli' execConfig
    [ convertToEraString anyEra, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 1
    , "--tx-in-collateral", Text.unpack $ renderTxIn txinCollateral
    , "--tx-in", Text.unpack $ renderTxIn plutusScriptTxIn
    , "--tx-in-script-file", plutusSpendingScript
    , "--tx-in-datum-file", datumFile
    , "--tx-in-redeemer-file", datumFile -- We just reuse the datum file for the redeemer
    , "--mint", mintValue
    , "--mint-script-file", plutusMintingScript
    , "--mint-redeemer-file", datumFile -- We just reuse the datum file for the redeemer
    , "--certificate-file", scriptStakeRegistrationCertificate
    , "--certificate-script-file", plutusMintingScript
    , "--certificate-redeemer-file", datumFile
    , "--certificate-file", scriptStakeDelegationCertFp
    , "--certificate-script-file", plutusMintingScript
    , "--certificate-redeemer-file", datumFile
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


