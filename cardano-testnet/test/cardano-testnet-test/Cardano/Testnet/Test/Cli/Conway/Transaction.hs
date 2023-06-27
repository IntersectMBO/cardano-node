{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Redundant return" -}
{- HLINT ignore "Use head" -}
{- HLINT ignore "Use let" -}

module Cardano.Testnet.Test.Cli.Conway.Transaction
  ( hprop_transaction
  ) where

import           Prelude

import           Control.Monad (void)
import qualified Data.Aeson as Aeson
import           Data.Function
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time.Clock as DTC
import           GHC.Stack (callStack)
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Cardano.Api
import           Cardano.CLI.Shelley.Output (QueryTipLocalStateOutput (..))
import           Cardano.Testnet

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Testnet.Process.Run as H
import           Testnet.Process.Run
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

hprop_transaction :: Property
hprop_transaction = H.integrationRetryWorkspace 2 "conway-transaction" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  conf@Conf { tempAbsPath } <- H.noteShowM $ mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let
    tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'
    testnetOptions = ConwayOnlyTestnetOptions $ conwayDefaultTestnetOptions
      { conwayNodeLoggingFormat = NodeLoggingFormatAsJson
      }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets
    } <- testnet testnetOptions conf

  poolNode1 <- H.headM poolNodes

  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1

  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1

  tipDeadline <- H.noteShowM $ DTC.addUTCTime 210 <$> H.noteShowIO DTC.getCurrentTime

  H.byDeadlineM 10 tipDeadline "Wait for two epochs" $ do
    void $ execCli' execConfig
      [ "query", "tip"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "current-tip.json"
      ]

    tipJson <- H.leftFailM . H.readJsonFile $ work </> "current-tip.json"
    tip <- H.noteShowM $ H.jsonErrorFail $ Aeson.fromJSON @QueryTipLocalStateOutput tipJson

    currEpoch <- case mEpoch tip of
      Nothing -> H.failMessage callStack "cardano-cli query tip returned Nothing for EpochNo"
      Just currEpoch -> return currEpoch

    H.note_ $ "Current Epoch: " <> show currEpoch
    H.assert $ currEpoch > 2

  wallet0 <- H.indexM 0 wallets
  wallet1 <- H.indexM 1 wallets

  utxoBeforeFile <- H.note $ work </> "utxo-before.json"
  utxoAfterFile <- H.note $ work </> "utxo-after.json"

  void $ execCli' execConfig
    [ "query", "utxo"
    , "--address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", utxoBeforeFile
    ]

  utxoBeforeJson <- H.leftFailM $ H.readJsonFile utxoBeforeFile

  UTxO utxoBeforeMap <- H.noteShowM $ H.jsonErrorFail $ Aeson.fromJSON @(UTxO ConwayEra) utxoBeforeJson

  txinsBefore <- H.noteShow $ Map.keys utxoBeforeMap

  txin <- H.noteShow =<< H.headM txinsBefore

  wallet0Addr   <- pure $ wallet0 & paymentKeyInfoAddr & Text.unpack
  wallet1Addr   <- pure $ wallet1 & paymentKeyInfoAddr & Text.unpack
  wallet0SKFile <- pure $ wallet0 & paymentKeyInfoPair  & paymentSKey

  void $ execCli' execConfig
    [ "transaction", "build"
    , "--conway-era"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", wallet0Addr
    , "--tx-in", Text.unpack $ renderTxIn txin
    , "--tx-out", wallet1Addr <> "+" <> show @Int 5000000
    , "--out-file", work </> "transfer.tx.raw"
    ]

  void $ execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "transfer.tx.raw"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", wallet0SKFile
    , "--out-file", work </> "transfer.tx.signed"
    ]

  H.noteM_ $ execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "transfer.tx.signed"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.threadDelay $ 2 * 1000000

  void $ execCli' execConfig
    [ "query", "utxo"
    , "--address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", utxoAfterFile
    ]

  utxoAfterJson <- H.leftFailM $ H.readJsonFile utxoAfterFile

  UTxO utxoAfterMap <- H.noteShowM $ H.jsonErrorFail $ Aeson.fromJSON @(UTxO ConwayEra) utxoAfterJson

  txinsAfter <- H.noteShow $ Map.keys utxoAfterMap

  -- This means the txin got spent, which serves as confirmation the transaction was processed
  H.assert $ txinsBefore /= txinsAfter
