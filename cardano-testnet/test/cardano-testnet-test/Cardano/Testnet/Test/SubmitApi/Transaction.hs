{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.SubmitApi.Transaction
  ( hprop_transaction
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Ledger.Lens as A

import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Lens as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import           Data.Default.Class
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Lens.Micro
import           Network.HTTP.Simple
import           System.FilePath ((</>))
import qualified System.Info as SYS
import           Text.Regex (mkRegex, subRegex)

import           Testnet.Components.Configuration
import           Testnet.Process.Run (execCli', mkExecConfig, procSubmitApi)
import           Testnet.Property.Util (decodeEraUTxO, integrationRetryWorkspace)
import           Testnet.SubmitApi
import           Testnet.Types

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Golden as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/transaction/"'@
hprop_transaction :: Property
hprop_transaction = integrationRetryWorkspace 2 "submit-api-transaction" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  H.note_ SYS.os
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      sbe = ShelleyBasedEraConway
      eraString = eraToString sbe
      tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'
      options = def
        { cardanoNodeEra = AnyShelleyBasedEra sbe -- TODO: We should only support the latest era and the upcoming era
        }

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  TestnetRuntime
    { configurationFile
    , testnetMagic
    , testnetNodes
    , wallets=wallet0:_
    } <- cardanoTestnetDefault options def conf

  poolNode1 <- H.headM testnetNodes

  poolSprocket1 <- H.noteShow $ nodeSprocket poolNode1

  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic

  void $ procSubmitApi
    [ "--config", unFile configurationFile
    , "--testnet-magic", show @Int testnetMagic
    , "--socket-path", "FILEPATH"
    ]

  txbodyFp <- H.note $ work </> "tx.body"
  txbodySignedFp <- H.note $ work </> "tx.body.signed"
  txbodySignedBinFp <- H.note $ work </> "tx.body.signed.bin"
  txFailedResponseFp <- H.note $ work </> "tx.failed.response"
  txFailedResponseYamlFp <- H.note $ work </> "tx.failed.response.json"
  txFailedResponseYamlGoldenFp <- H.note "test/cardano-testnet-test/files/golden/tx.failed.response.json.golden"

  void $ execCli' execConfig
    [ eraString, "query", "utxo"
    , "--address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--cardano-mode"
    , "--out-file", work </> "utxo-1.json"
    ]

  utxo1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
  UTxO utxo1 <- H.noteShowM $ decodeEraUTxO sbe utxo1Json
  txin1 <- H.noteShow =<< H.headM (Map.keys utxo1)

  void $ execCli' execConfig
    [ eraString, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 5_000_001
    , "--out-file", txbodyFp
    ]

  void $ execCli' execConfig
    [ eraString, "transaction", "sign"
    , "--tx-body-file", txbodyFp
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair wallet0
    , "--out-file", txbodySignedFp
    ]

  let submitApiConf = SubmitApiConf
        { tempAbsPath = unTmpAbsPath tempAbsPath
        , configPath = configurationFile
        , epochSlots = 2
        , sprocket = poolSprocket1
        , maybePort = Nothing
        , testnetMagic
        }

  withSubmitApi submitApiConf [] $ \uriBase -> do
    H.byDurationM 1 5 "Expected UTxO found" $ do
      txBodySigned <- H.leftFailM $ H.readJsonFile @Aeson.Value txbodySignedFp

      cborHex <- H.nothingFail $ txBodySigned ^? Aeson.key "cborHex" . Aeson._String

      let txBs = Base16.decodeLenient (Text.encodeUtf8 cborHex)

      H.evalIO $ BS.writeFile txbodySignedBinFp txBs

      let submitApiRequestEndpoint = "POST " <> uriBase <> "/api/submit/tx"

      request <- H.evalM $ parseRequest submitApiRequestEndpoint
        <&>
          ( setRequestBodyFile txbodySignedBinFp
          . setRequestHeader "Content-Type" ["application/cbor"]
          )

      response <- H.evalM $ httpLbs request

      getResponseStatusCode response === 202

    H.byDurationM 5 45 "Expected UTxO found" $ do
      void $ execCli' execConfig
        [ eraString, "query", "utxo"
        , "--address", Text.unpack $ paymentKeyInfoAddr wallet0
        , "--cardano-mode"
        , "--out-file", work </> "utxo-2.json"
        ]

      utxo2Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-2.json"
      UTxO utxo2 <- H.noteShowM $ decodeEraUTxO sbe utxo2Json
      txouts2 <- H.noteShow $ L.unCoin . txOutValueLovelace . txOutValue . snd <$> Map.toList utxo2

      H.assert $ 15_000_003_000_000 `List.elem` txouts2

    response <- H.byDurationM 1 5 "Expected UTxO found" $ do
      txBodySigned <- H.leftFailM $ H.readJsonFile @Aeson.Value txbodySignedFp

      cborHex <- H.nothingFail $ txBodySigned ^? Aeson.key "cborHex" . Aeson._String

      let txBs = Base16.decodeLenient (Text.encodeUtf8 cborHex)

      H.evalIO $ BS.writeFile txbodySignedBinFp txBs

      let submitApiRequestEndpoint = "POST " <> uriBase <> "/api/submit/tx"

      request <- H.evalM $ parseRequest submitApiRequestEndpoint
        <&>
          ( setRequestBodyFile txbodySignedBinFp
          . setRequestHeader "Content-Type" ["application/cbor"]
          )

      response <- H.evalM $ httpLbs request

      getResponseStatusCode response === 400

      pure response

    H.evalIO $ LBS.writeFile txFailedResponseFp $ redactHashLbs $ getResponseBody response

    v <- H.leftFailM $ H.evalIO $ Aeson.eitherDecodeFileStrict @Aeson.Value txFailedResponseFp

    H.evalIO $ LBS.writeFile txFailedResponseYamlFp $ Aeson.encodePretty v

    H.diffFileVsGoldenFile txFailedResponseYamlFp txFailedResponseYamlGoldenFp

redactHashLbs :: LBS.ByteString -> LBS.ByteString
redactHashLbs =
    LBS.fromStrict
  . Text.encodeUtf8
  . Text.pack
  . redactHashString
  . Text.unpack
  . Text.decodeUtf8
  . LBS.toStrict

redactHashString :: String -> String
redactHashString input =
  subRegex regex input "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  where
    regexPattern = "[0-9a-fA-F]{64}"
    regex = mkRegex regexPattern

txOutValue :: TxOut ctx era -> TxOutValue era
txOutValue (TxOut _ v _ _) = v

txOutValueLovelace ::TxOutValue era -> L.Coin
txOutValueLovelace = \case
  TxOutValueShelleyBased sbe v -> v ^. A.adaAssetL sbe
  TxOutValueByron v -> v
