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

module Test.Cli.Babbage.StakeSnapshot
  ( hprop_stakeSnapshot
  ) where

import           Prelude

import           Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time.Clock as DTC
import           GHC.Stack (callStack)
import qualified System.Directory as IO
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Cardano.CLI.Shelley.Output (QueryTipLocalStateOutput (..))
import           Cardano.Testnet

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified Testnet.Util.Base as H
import qualified Testnet.Util.Process as H
import           Testnet.Util.Process
import           Testnet.Util.Runtime

hprop_stakeSnapshot :: Property
hprop_stakeSnapshot = H.integrationRetryWorkspace 2 "babbage-stake-snapshot" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  base <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf@Conf { tempBaseAbsPath, tempAbsPath } <- H.noteShowM $
    mkConf (ProjectBase base) (YamlFilePath configurationTemplate) tempAbsBasePath' Nothing

  work <- H.note $ tempAbsPath </> "work"
  H.createDirectoryIfMissing work

  let
    testnetOptions = BabbageOnlyTestnetOptions $ babbageDefaultTestnetOptions
      { babbageNodeLoggingFormat = NodeLoggingFormatAsJson
      }

  TestnetRuntime
    { testnetMagic
    , poolNodes
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

  result <- execCli' execConfig
    [ "query", "stake-snapshot"
    , "--testnet-magic", show @Int testnetMagic
    , "--all-stake-pools"
    ]

  json <- H.leftFail $ Aeson.eitherDecode @Aeson.Value (LBS.fromStrict (Text.encodeUtf8 (Text.pack result)))

  -- There are three stake pools so check that "pools" has three entries
  case json of
    Aeson.Object kmJson -> do
      pools <- H.nothingFail $ KM.lookup "pools" kmJson
      case pools of
        Aeson.Object kmPools -> KM.size kmPools === 3
        _ -> H.failure
    _ -> H.failure
