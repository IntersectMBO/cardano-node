{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cardano.Testnet.Test.Cli.Babbage.StakeSnapshot
  ( hprop_stakeSnapshot
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Output (QueryTipLocalStateOutput (..))
import           Cardano.Testnet

import           Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Time.Clock as DTC
import           GHC.Stack (callStack)
import qualified System.Info as SYS

import           Testnet.Process.Cli (execCliStdoutToJson)
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H

hprop_stakeSnapshot :: Property
hprop_stakeSnapshot = H.integrationRetryWorkspace 2 "babbage-stake-snapshot" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath

  let
    tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'
    era = BabbageEra
    options = cardanoDefaultTestnetOptions
                        { cardanoNodes = cardanoDefaultTestnetNodeOptions
                        , cardanoSlotLength = 0.1
                        , cardanoNodeEra = AnyCardanoEra era -- TODO: We should only support the latest era and the upcoming era
                        }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    } <- cardanoTestnetDefault options conf

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  tipDeadline <- H.noteShowM $ DTC.addUTCTime 210 <$> H.noteShowIO DTC.getCurrentTime

  H.byDeadlineM 10 tipDeadline "Wait for two epochs" $ do
    tip <- execCliStdoutToJson execConfig [ "query", "tip" ]

    currEpoch <- case mEpoch tip of
      Nothing -> H.failMessage callStack "cardano-cli query tip returned Nothing for EpochNo"
      Just currEpoch -> return currEpoch

    H.note_ $ "Current Epoch: " <> show currEpoch
    H.assert $ currEpoch > 2

  json <- execCliStdoutToJson execConfig [ "query", "stake-snapshot", "--all-stake-pools" ]

  -- There are three stake pools so check that "pools" has three entries
  case json of
    Aeson.Object kmJson -> do
      pools <- H.nothingFail $ KM.lookup "pools" kmJson
      case pools of
        Aeson.Object kmPools -> KM.size kmPools === 3
        _ -> H.failure
    _ -> H.failure
