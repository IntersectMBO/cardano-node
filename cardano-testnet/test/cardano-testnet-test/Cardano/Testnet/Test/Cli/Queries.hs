{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Cli.Queries
  ( hprop_cli_queries
  ) where

import           Cardano.Api

import           Cardano.Testnet

import           Prelude

import           Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           GHC.IORef (newIORef)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           System.FilePath ((</>))

import qualified Testnet.Process.Cli as P
import qualified Testnet.Process.Run as H
import           Testnet.Property.Utils (queryUtxos)
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import           Cardano.CLI.Types.Output (QueryTipLocalStateOutput)
import           Control.Concurrent (threadDelay)
import           Control.Monad (void)
import qualified Control.Monad as H
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import           Data.List (sort)
import           Hedgehog
import qualified Hedgehog as H
import           Hedgehog.Extras (Integration)
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Golden as H
import           System.IO (hPutStrLn)
import qualified Testnet.Process.Cli as H
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Text.Encoding as TL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as LBS

-- | Test CLI queries
-- Execute me with:
-- @cabal test cardano-testnet-test --test-options '-p "/CliQueries/"'@
-- If you want to recreate golden files, run the comment with
-- RECREATE_GOLDEN_FILES=1 as its prefix
hprop_cli_queries :: Property
hprop_cli_queries = H.integrationWorkspace "cli-queries" $ \tempAbsBasePath' -> do
  conf@Conf { tempAbsPath=tempAbsPath@(TmpAbsolutePath work) }
    <- mkConf tempAbsBasePath'
  let _tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  utxoFileCounter <- liftIO $ newIORef 1

  let sbe = ShelleyBasedEraConway
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoSlotLength = 0.1
        , cardanoNodeEra = cEra
        }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets = _
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions conf

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic

  let _queryAnyUtxo :: Text.Text -> Integration TxIn
      _queryAnyUtxo address = withFrozenCallStack $ do
        utxos <- queryUtxos execConfig work utxoFileCounter sbe address
        H.noteShow =<< H.headM (Map.keys utxos)
      socketName' = IO.sprocketName poolSprocket1
      socketBase = IO.sprocketBase poolSprocket1 -- /tmp
      socketPath = socketBase </> socketName'

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> socketPath
  H.note_ $ "Foldblocks config file: " <> configurationFile

  -- stake-pools to stdout
  stakePoolsOut <- H.execCli' execConfig [ "conway", "query", "stake-pools" ]
  H.note_ stakePoolsOut

  stakePoolsOutJSON <- H.execCli' execConfig [ "conway", "query", "stake-pools", "--output-json" ]
  H.note_ stakePoolsOutJSON

  stakePoolsOutText <- H.execCli' execConfig [ "conway", "query", "stake-pools", "--output-text" ]
  H.note_ stakePoolsOutText

  -- stake-pools to a file
  let stakePoolsOutFile = work </> "stake-pools-out.json"
  H.noteM_ $ H.execCli' execConfig [ "conway", "query", "stake-pools"
                                   , "--out-file", stakePoolsOutFile]
  stakePoolsOutFileContent <- H.evalEitherM $ H.readJsonFile @Aeson.Value stakePoolsOutFile
  H.note_ $ Text.unpack $ TL.decodeUtf8 $ LBS.toStrict $ Aeson.encodePretty stakePoolsOutFileContent

  -- stake-pools to a file - JSON
  let stakePoolsOutFileJSON = work </> "stake-pools-out-new.json"
  H.noteM_ $ H.execCli' execConfig [ "conway", "query", "stake-pools"
                                   , "--output-json"
                                   , "--out-file", stakePoolsOutFileJSON]
  stakePoolsOutFileJSONContent <- H.evalEitherM $ H.readJsonFile @Aeson.Value stakePoolsOutFileJSON
  H.note_ $ Text.unpack $ TL.decodeUtf8 $ LBS.toStrict $ Aeson.encodePretty stakePoolsOutFileJSONContent

  -- stake-pools to a file - TEXT
  let stakePoolsOutFileText = work </> "stake-pools-out.json"
  H.noteM_ $ H.execCli' execConfig [ "conway", "query", "stake-pools"
                                   , "--output-text"
                                   , "--out-file", stakePoolsOutFileText]
  stakePoolsOutFileTextContent <- H.readFile stakePoolsOutFileText
  H.note_ stakePoolsOutFileTextContent

  H.assert False