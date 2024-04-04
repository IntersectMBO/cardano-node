{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Cli.Babbage.Transaction
  ( hprop_transaction
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Ledger.Lens as A
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Core as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Lens.Micro
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Testnet.Components.SPO
import qualified Testnet.Process.Run as H
import           Testnet.Process.Run
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

hprop_transaction :: Property
hprop_transaction = H.integrationRetryWorkspace 0 "babbage-transaction" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let
    sbe = ShelleyBasedEraBabbage
    era = toCardanoEra sbe
    tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'
    options = cardanoDefaultTestnetOptions
      { cardanoNodeEra = AnyCardanoEra era -- TODO: We should only support the latest era and the upcoming era
      }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=wallet0:_
    } <- cardanoTestnetDefault options conf

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic


  txbodyFp <- H.note $ work </> "tx.body"
  txbodySignedFp <- H.note $ work </> "tx.body.signed"

  void $ execCli' execConfig
    [ "babbage", "query", "utxo"
    , "--address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--cardano-mode"
    , "--out-file", work </> "utxo-1.json"
    ]

  utxo1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
  UTxO utxo1 <- H.noteShowM $ decodeEraUTxO sbe utxo1Json
  txin1 <- H.noteShow =<< H.headM (Map.keys utxo1)

  void $ execCli' execConfig
    [ "babbage", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 5_000_001
    , "--out-file", txbodyFp
    ]
  cddlUnwitnessedTx <- H.readJsonFileOk txbodyFp
  apiTx <- H.evalEither $ deserialiseTxLedgerCddl sbe cddlUnwitnessedTx
  let txFee = L.unCoin $ extractTxFee apiTx

  -- This is the current calculated fee.
  -- It's a sanity check to see if anything has
  -- changed regarding fee calculation.
  228 H.=== txFee

  void $ execCli' execConfig
    [ "babbage", "transaction", "sign"
    , "--tx-body-file", txbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet0
    , "--out-file", txbodySignedFp
    ]

  void $ execCli' execConfig
    [ "babbage", "transaction", "submit"
    , "--tx-file", txbodySignedFp
    ]


  H.byDurationM 1 15 "Expected UTxO found" $ do
    void $ execCli' execConfig
      [ "babbage", "query", "utxo"
      , "--address", Text.unpack $ paymentKeyInfoAddr wallet0
      , "--cardano-mode"
      , "--out-file", work </> "utxo-2.json"
      ]

    utxo2Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-2.json"
    UTxO utxo2 <- H.noteShowM $ decodeEraUTxO sbe utxo2Json
    txouts2 <- H.noteShow $ L.unCoin . txOutValueLovelace . txOutValue . snd <$> Map.toList utxo2
    H.assert $ 5_000_001 `List.elem` txouts2

txOutValue :: TxOut ctx era -> TxOutValue era
txOutValue (TxOut _ v _ _) = v

txOutValueLovelace ::TxOutValue era -> L.Coin
txOutValueLovelace = \case
  TxOutValueShelleyBased sbe v -> v ^. A.adaAssetL sbe
  TxOutValueByron v -> v

extractTxFee :: Tx era -> L.Coin
extractTxFee (ShelleyTx sbe ledgerTx) =
  shelleyBasedEraConstraints sbe $ ledgerTx ^. (L.bodyTxL . L.feeTxBodyL)
