{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Cli.StakeSnapshot
  ( hprop_stakeSnapshot
  ) where

import           Cardano.Api
import qualified Cardano.Api as Api

import           Cardano.Testnet

import           Prelude

import           Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import           Data.Default.Class
import qualified System.Info as SYS

import           Testnet.Process.Run (execCliStdoutToJson, mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

hprop_stakeSnapshot :: Property
hprop_stakeSnapshot = integrationRetryWorkspace 2 "stake-snapshot" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  H.note_ SYS.os
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , configurationFile
    } <- cardanoTestnetDefault def def conf

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic

  void $ waitUntilEpoch configurationFile
                        (Api.File $ IO.sprocketSystemName poolSprocket1) (EpochNo 3)

  json <- execCliStdoutToJson execConfig [ "query", "stake-snapshot", "--all-stake-pools" ]

  -- There are three stake pools so check that "pools" has three entries
  case json of
    Aeson.Object kmJson -> do
      pools <- H.nothingFail $ KM.lookup "pools" kmJson
      case pools of
        Aeson.Object kmPools -> KM.size kmPools === 3
        _ -> H.failure
    _ -> H.failure
