{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Plutus.Direct.TxInLockingPlutus
  ( hprop_plutus
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson (FromJSON (..), Value, (.:))
import           Data.Bool (not)
import           Data.Eq
import           Data.Function
import           Data.Functor ((<&>))
import           Data.HashMap.Lazy (HashMap)
import           Data.Int
import           Data.List ((!!))
import           Data.Maybe
import           Data.Monoid (Last (..), (<>))
import           Data.Text (Text)
import           GHC.Num
import           GHC.Real
import           Hedgehog (Property, (===))
import           Prelude (head)
import           System.FilePath ((</>))
import           Text.Show (Show (..))

import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Text as T
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified Test.Base as H
import qualified Test.Process as H
import qualified Testnet.Cardano as H
import qualified Testnet.Conf as H

{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant return" -}
{- HLINT ignore "Use let" -}

data Utxo = Utxo
  { address :: Text
  , value :: HashMap Text Integer
  } deriving (Eq, Show)

instance FromJSON Utxo where
  parseJSON = J.withObject "Utxo" $ \v -> Utxo
    <$> v .: "address"
    <*> v .: "value"

hprop_plutus :: Property
hprop_plutus = H.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsBasePath' -> do
  projectBase <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  conf@H.Conf { H.tempBaseAbsPath, H.tempAbsPath } <- H.noteShowM $ H.mkConf tempAbsBasePath' Nothing

  H.TestnetRuntime { H.bftSprockets, H.testnetMagic } <- H.testnet H.defaultTestnetOptions conf

  env <- H.evalIO IO.getEnvironment

  execConfig <- H.noteShow H.ExecConfig
        { H.execConfigEnv = Last $ Just $
          [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName (head bftSprockets))
          ]
          -- The environment must be passed onto child process on Windows in order to
          -- successfully start that process.
          <> env
        , H.execConfigCwd = Last $ Just tempBaseAbsPath
        }

  base <- H.note projectBase
  work <- H.note tempAbsPath
  utxoVKeyFile <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.vkey"
  utxoSKeyFile <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.skey"

  plutusScriptFileInUse <- H.note $ base </> "scripts/plutus/scripts/always-succeeds-spending.plutus"

  H.noteEachM_ . H.listDirectory $ base
  H.noteEachM_ . H.listDirectory $ base </> "scripts"
  H.noteEachM_ . H.listDirectory $ base </> "scripts/plutus"
  H.noteEachM_ . H.listDirectory $ base </> "scripts/plutus/scripts"

  -- This datum hash is the hash of the untyped 42
  let scriptDatumHash = "9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b"

  datumFile <- H.note $ base </> "scripts/plutus/data/42.datum"
  redeemerFile <- H.note $ base </> "scripts/plutus/data/42.redeemer"

  -- Always succeeds Plutus script in use. Any datum and redeemer combination will succeed.
  -- Script at: $plutusscriptinuse

  -- Step 1: Create a tx output with a datum hash at the script address. In order for a tx output to be locked
  -- by a plutus script, it must have a datahash. We also need collateral tx inputs so we split the utxo
  -- in order to accommodate this.

  plutusScriptAddr <- H.execCli
    [ "address", "build"
    , "--payment-script-file", plutusScriptFileInUse
    , "--testnet-magic", show @Int testnetMagic
    ]

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
  utxo1 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(HashMap Text Utxo) utxo1Json
  txin <- H.noteShow $ head $ HM.keys utxo1
  lovelaceAtTxin <- H.nothingFailM . H.noteShow $ utxo1 & HM.lookup txin <&> value >>= HM.lookup "lovelace"
  lovelaceAtTxinDiv3 <- H.noteShow $ lovelaceAtTxin `div` 3

  void $ H.execCli' execConfig
    [ "query", "protocol-parameters"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "pparams.json"
    ]

  let dummyaddress = "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4"
      targetaddress = "addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh"

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", dummyaddress
    , "--tx-in", T.unpack txin
    , "--tx-out", plutusScriptAddr <> "+" <> show @Integer lovelaceAtTxinDiv3
    , "--tx-out-datum-hash", scriptDatumHash
    , "--tx-out", utxoAddr <> "+" <> show @Integer lovelaceAtTxinDiv3
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "create-datum-output.body"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "create-datum-output.body"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "create-datum-output.tx"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "create-datum-output.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.threadDelay 5000000

  -- With the tx output at the script address we can now attempt to spend it.

  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", plutusScriptAddr
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "plutusutxo.json"
    ]

  H.cat $ work </> "plutusutxo.json"

  plutusUtxoJson <- H.leftFailM . H.readJsonFile $ work </> "plutusutxo.json"
  plutusUtxo <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(HashMap Text Utxo) plutusUtxoJson
  plutusUtxoTxIn <- H.noteShow $ head $ HM.keys plutusUtxo

  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-2.json"
    ]

  H.cat $ work </> "utxo-2.json"

  utxo2Json :: Value <- H.leftFailM $ H.readJsonFile $ work </> "utxo-2.json"
  utxo2 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(HashMap Text Utxo) utxo2Json
  txinCollateral <- H.noteShow $ head $ HM.keys utxo2

  lovelaceAtplutusScriptAddr <- H.nothingFailM . H.noteShow $ plutusUtxo & HM.lookup plutusUtxoTxIn <&> value >>= HM.lookup "lovelace"

  spendable <- H.noteShow $ lovelaceAtplutusScriptAddr `div` 3

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", dummyaddress
    , "--tx-in", T.unpack plutusUtxoTxIn
    , "--tx-in-collateral", T.unpack txinCollateral
    , "--tx-out", targetaddress <> "+" <> show @Integer spendable
    , "--tx-in-script-file", plutusScriptFileInUse
    , "--tx-in-datum-file", datumFile
    , "--protocol-params-file", work </> "pparams.json"
    , "--tx-in-redeemer-file", redeemerFile
    , "--out-file", work </> "test-alonzo.body"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "test-alonzo.body"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "alonzo.tx"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "alonzo.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.threadDelay 5000000

  -- Querying UTxO at $targetaddress. If there is ADA at the address the Plutus script successfully executed!

  result <- T.pack <$> H.execCli' execConfig
    [ "query", "utxo"
    , "--address", targetaddress
    , "--testnet-magic", show @Int testnetMagic
    ]

  L.filter (not . T.null) (T.splitOn " " (T.lines result !! 2)) !! 2 === "100000000000"
