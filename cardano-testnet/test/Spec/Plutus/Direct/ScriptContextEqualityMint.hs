{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Plutus.Direct.ScriptContextEqualityMint
  ( hprop_plutus_script_context_mint_equality
  ) where

import           Prelude

import           Cardano.Api

import           Control.Monad
import           Data.Monoid (Last (..))
import           Data.String
import           Hedgehog (Property, (===))
import           System.Environment (getEnvironment)
import           System.FilePath ((</>))

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified Test.Base as H
import           Test.Process (execCreateScriptContext, execCreateScriptContext')
import qualified Test.Process as H
import           Testnet.Cardano (defaultTestnetOptions, testnet)
import qualified Testnet.Cardano as TC
import qualified Testnet.Conf as H

{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant return" -}
{- HLINT ignore "Use let" -}

hprop_plutus_script_context_mint_equality :: Property
hprop_plutus_script_context_mint_equality = H.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsBasePath' -> do
  projectBase <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  conf@H.Conf { H.tempBaseAbsPath, H.tempAbsPath } <- H.noteShowM $ H.mkConf tempAbsBasePath' Nothing

  TC.TestnetRuntime { bftSprockets, testnetMagic } <- testnet defaultTestnetOptions conf

  env <- H.evalIO getEnvironment

  execConfig <- H.noteShow H.ExecConfig
        { H.execConfigEnv = Last $ Just $
          [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName (head bftSprockets))
          ]
          -- The environment must be passed onto child process on Windows in order to
          -- successfully start that process.
          <> env
        , H.execConfigCwd = Last $ Just tempBaseAbsPath
        }

  -- First we note all the relevant files
  base <- H.note projectBase
  work <- H.note tempAbsPath

  -- We get our UTxOs from here
  utxoVKeyFile <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.vkey"
  utxoSKeyFile <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.skey"
  scriptDummyRedeemer <- H.note $ work </> "mint-script-context-dummy.redeemer"
  scriptContextRedeemer <- H.note $ work </> "mint-script-context.redeemer"
  requiredSignerSKey <- H.note $ tempAbsPath </> "addresses/user1.skey"
  plutusContextEqualityMintScript <- H.note $ base </> "scripts/plutus/scripts/minting-context-equivalance-test.plutus"

  policyId <- filter (/= '\n')
                <$> H.execCli
                      [ "transaction", "policyid"
                      , "--script-file", plutusContextEqualityMintScript
                      ]

  void . H.note $ "Policy ID: " <> policyId

  H.noteEachM_ . H.listDirectory $ base
  H.noteEachM_ . H.listDirectory $ base </> "scripts"
  H.noteEachM_ . H.listDirectory $ base </> "scripts/plutus"
  H.noteEachM_ . H.listDirectory $ base </> "scripts/plutus/scripts"

  utxoAddr <- H.execCli
    [ "address", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--payment-verification-key-file", utxoVKeyFile
    ]

  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-1.json"
    ]

  H.cat $ work </> "utxo-1.json"

  utxo1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
  UTxO utxo1 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxo1Json
  txin <- H.noteShow $ head $ Map.keys utxo1
  TxOut _ txoutVal _ <- H.nothingFailM . H.noteShow $ Map.lookup txin utxo1
  let Lovelace lovelaceAtTxin = txOutValueToLovelace txoutVal
  lovelaceAtTxinDiv3 <- H.noteShow $ lovelaceAtTxin `div` 3

  void $ H.execCli' execConfig
    [ "query", "protocol-parameters"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "pparams.json"
    ]

  let dummyaddress = "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4"


  -- STEP 1 - Create collateral
  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoAddr
    , "--tx-in", T.unpack $ renderTxIn txin
    , "--tx-out", utxoAddr <> "+" <> show @Integer lovelaceAtTxinDiv3
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "create-collateral-output.body"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "create-collateral-output.body"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "create-collateral-output.tx"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "create-collateral-output.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.threadDelay 5000000

  -- STEP 2
  -- We need to create a dummy tx in order to create the script context redeemer
  -- that we want to use when attempting to spend the spending script locked UTxO

  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-2.json"
    ]

  H.cat $ work </> "utxo-2.json"

  utxo2Json :: Aeson.Value <- H.leftFailM $ H.readJsonFile $ work </> "utxo-2.json"
  UTxO utxo2 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxo2Json
  txinFunding <- H.noteShow . head $ Map.keys utxo2
  txinCollateral <- H.noteShow $ Map.keys utxo2 !! 1

  void $ execCreateScriptContext ["--out-file", scriptDummyRedeemer]

  H.cat $ work </> scriptDummyRedeemer

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--cardano-mode"
    , "--script-invalid"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoAddr
    , "--invalid-before", "1"
    , "--invalid-hereafter", "3000"
    , "--required-signer", requiredSignerSKey
    , "--tx-in", T.unpack $ renderTxIn txinFunding
    , "--tx-in-collateral", T.unpack $ renderTxIn txinCollateral
    , "--mint-script-file", plutusContextEqualityMintScript
    , "--mint-redeemer-file", scriptDummyRedeemer
    , "--tx-out", dummyaddress <> "+" <> show @Integer 10000000 <> "+ 5 " <> (policyId <> ".MillarCoin")
    , "--mint", "5 " <> (policyId <> ".MillarCoin")
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "mint-dummy.body"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "mint-dummy.body"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "mint-dummy.tx"
    ]

  -- Generate the redeeemer we will use in the tx!
  void $ execCreateScriptContext' execConfig
           [ "--generate-tx" , work </> "mint-dummy.tx"
           , "--cardano-mode"
           , "--testnet-magic", show @Int testnetMagic
           , "--out-file", scriptContextRedeemer
           ]

  H.cat $ work </> scriptContextRedeemer

  H.threadDelay 5000000

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--cardano-mode"
    , "--script-valid"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoAddr
    , "--invalid-before", "1"
    , "--invalid-hereafter", "3000"
    , "--required-signer", requiredSignerSKey
    , "--tx-in", T.unpack $ renderTxIn txinFunding
    , "--tx-in-collateral", T.unpack $ renderTxIn txinCollateral
    , "--mint-script-file", plutusContextEqualityMintScript
    , "--mint-redeemer-file", scriptContextRedeemer
    , "--tx-out", dummyaddress <> "+" <> show @Integer 10000000 <> "+ 5 " <> (policyId <> ".MillarCoin")
    , "--mint", "5 " <> (policyId <> ".MillarCoin")
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "mint-final.body"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "mint-final.body"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--signing-key-file", requiredSignerSKey
    , "--out-file", work </> "mint-final.tx"
    ]


  void $ H.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "mint-final.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.threadDelay 5000000

  -- Query UTxO at dummyAddress.

  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", dummyaddress
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "dummyaddress.json"
    ]

  H.cat $ work </> "dummyaddress.json"

  dummyUtxoJson <- H.leftFailM . H.readJsonFile $ work </> "dummyaddress.json"
  UTxO dummyUtxo <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) dummyUtxoJson

  let allValues = mconcat . map (\(TxOut _ val _) -> txOutValueToValue val) $ Map.elems dummyUtxo
      millarAssetId = AssetId (fromString policyId) $ fromString "MillarCoin"

  -- There should be a multi asset value at the dummy address
  1 === length (valueToList $ filterValue (== millarAssetId) allValues)
