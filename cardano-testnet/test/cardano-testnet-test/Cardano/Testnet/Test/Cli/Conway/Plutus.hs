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
  ( hprop_plutus
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Output (QueryTipLocalStateOutput (..))
import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
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
-- Spending NO
-- Minting NO
-- Rewarding NO
-- Certifying NO
-- Voting NO
-- Proposing NO
hprop_plutus :: Property
hprop_plutus = H.integrationWorkspace "all-plutus-script-purposes" $ \tempAbsBasePath' -> do
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

  plutusscriptinuse <- H.note "/home/jordan/Repos/Work/intersect-mbo/cardano-node/cardano-testnet/test/cardano-testnet-test/files/plutus/v3/always-succeeds.plutus"
  datumFile <- H.note "/home/jordan/Repos/Work/intersect-mbo/cardano-node/cardano-testnet/test/cardano-testnet-test/files/plutus/v3/42.datum"
  let sendAdaToScriptAddressTxBody = work </> "send-ada-to-script-address-tx-body"

  plutusScriptAddr <-
    H.execCli' execConfig
      [ "address", "build"
      , "--payment-script-file", plutusscriptinuse
      ]
  H.note_ $ "plutusScriptAddr: " <> plutusScriptAddr
  scriptdatumhash <- filter (/= '\n') <$>
    H.execCli' execConfig
      [ "transaction", "hash-script-data"
      , "--script-data-file", datumFile
      ]

  -- 1. Put UTxO and datum at Plutus spending script address
  void $ execCli' execConfig
    [ convertToEraString anyEra, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ head wallets
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", plutusScriptAddr <> "+" <> show @Int 5_000_000
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

  H.threadDelay 5_000_000
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
    , "--address", plutusScriptAddr
    , "--cardano-mode"
    , "--out-file", work </> "plutus-script-utxo.json"
    ]
  utxoPlutusJson <- H.leftFailM . H.readJsonFile $ work </> "plutus-script-utxo.json"
  UTxO utxoPlutus <- H.noteShowM $ decodeEraUTxO sbe utxoPlutusJson

  let keys3 = Map.keys utxoPlutus
  H.note_ $ "keys3: " <> show (length keys3)

  plutusScriptTxIn <- H.noteShow $ keys3 !! 0
  let spendScriptUTxOTxBody = work </> "spend-script-utxo-tx-body"
  void $ execCli' execConfig
    [ convertToEraString anyEra, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 1
    , "--tx-in-collateral", Text.unpack $ renderTxIn txinCollateral
    , "--tx-in", Text.unpack $ renderTxIn plutusScriptTxIn
    , "--tx-in-script-file", plutusscriptinuse
    , "--tx-in-datum-file", datumFile
    , "--tx-in-redeemer-file", datumFile -- We just reuse the datum file for the redeemer
    , "--tx-out", utxoAddr <> "+" <> show @Int 2_000_000
    , "--out-file", spendScriptUTxOTxBody
    ]

  H.success


