{-# LANGUAGE BlockArguments #-}
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

module Spec.Cli.Babbage.LeadershipSchedule
  ( hprop_leadershipSchedule
  ) where

import           Cardano.CLI.Shelley.Output (QueryTipLocalStateOutput (..))
import           Control.Monad (void)
import           Data.List ((\\))
import           Data.Monoid (Last (..))
import           GHC.Stack (callStack)
import           Hedgehog (Property)
import           Prelude
import           System.Environment (getEnvironment)
import           System.FilePath ((</>))
import           Testnet.Babbage (TestnetOptions (..), TestnetRuntime (..))

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.List as L
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified System.Info as SYS
import qualified Test.Assert as H
import qualified Test.Base as H
import qualified Test.Process as H
import qualified Test.Runtime as TR
import           Test.Runtime (LeadershipSlot (..))
import qualified Testnet.Babbage as TC
import qualified Testnet.Conf as H

hprop_leadershipSchedule :: Property
hprop_leadershipSchedule = H.integration . H.runFinallies . H.workspace "alonzo" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  base <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf@H.Conf { H.tempBaseAbsPath, H.tempAbsPath } <- H.noteShowM $
    H.mkConf (H.ProjectBase base) (H.YamlFilePath configurationTemplate) tempAbsBasePath' Nothing

  work <- H.note $ tempAbsPath </> "work"
  H.createDirectoryIfMissing work

  testnetOptions <- pure TC.defaultTestnetOptions
    { nodeLoggingFormat = TR.NodeLoggingFormatAsJson
    }
  tr@TC.TestnetRuntime
    { testnetMagic
    , poolNodes
    -- , wallets
    -- , delegators
    } <- TC.testnet testnetOptions conf

  poolNode1 <- H.headM poolNodes

  env <- H.evalIO getEnvironment

  poolSprocket1 <- H.noteShow $ TR.poolNodeSprocket poolNode1

  execConfig <- H.noteShow H.ExecConfig
    { H.execConfigEnv = Last $ Just $
      [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName poolSprocket1)
      ]
      -- The environment must be passed onto child process on Windows in order to
      -- successfully start that process.
      <> env
    , H.execConfigCwd = Last $ Just tempBaseAbsPath
    }

  tipDeadline <- H.noteShowM $ DTC.addUTCTime 180 <$> H.noteShowIO DTC.getCurrentTime

  H.assertByDeadlineMCustom "stdout does not contain \"until genesis start time\"" tipDeadline $ do
    H.threadDelay 5000000
    void $ H.execCli' execConfig
      [ "query", "tip"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "current-tip.json"
      ]

    tipJson <- H.leftFailM . H.readJsonFile $ work </> "current-tip.json"
    tip <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @QueryTipLocalStateOutput tipJson

    currEpoch <- case mEpoch tip of
      Nothing -> H.failMessage callStack "cardano-cli query tip returned Nothing for EpochNo"
      Just currEpoch -> return currEpoch

    H.note_ $ "Current Epoch: " <> show currEpoch
    return (currEpoch > 2)

  stakePoolId <- filter ( /= '\n') <$> H.execCli
    [ "stake-pool", "id"
    , "--cold-verification-key-file", TR.poolNodeKeysColdVkey $ TR.poolNodeKeys poolNode1
    ]

  let poolVrfSkey = TR.poolNodeKeysVrfSkey $ TR.poolNodeKeys poolNode1

  id do
    scheduleFile <- H.noteTempFile tempAbsPath "schedule.log"

    leadershipScheduleDeadline <- H.noteShowM $ DTC.addUTCTime 180 <$> H.noteShowIO DTC.getCurrentTime

    H.byDeadlineM 5 leadershipScheduleDeadline $ do
      void $ H.execCli' execConfig
        [ "query", "leadership-schedule"
        , "--testnet-magic", show @Int testnetMagic
        , "--genesis", TC.shelleyGenesisFile tr
        , "--stake-pool-id", stakePoolId
        , "--vrf-signing-key-file", poolVrfSkey
        , "--out-file", scheduleFile
        , "--current"
        ]

    scheduleJson <- H.leftFailM $ H.readJsonFile scheduleFile

    expectedLeadershipSlotNumbers <- H.noteShowM $ fmap (fmap slotNumber) $ H.leftFail $ J.parseEither (J.parseJSON @[LeadershipSlot]) scheduleJson

    H.assert $ not (L.null expectedLeadershipSlotNumbers)

    leadershipDeadline <- H.noteShowM $ DTC.addUTCTime 90 <$> H.noteShowIO DTC.getCurrentTime

    H.assertByDeadlineMCustom "Retrieve actual slots" leadershipDeadline $ do
      leaderSlots <- H.getRelevantLeaderSlots (TR.poolNodeStdout poolNode1) (minimum expectedLeadershipSlotNumbers)
      maxSlotExpected <- H.noteShow $ maximum expectedLeadershipSlotNumbers
      if L.null leaderSlots
        then return False
        else do
          maxActualSlot <- H.noteShow $ maximum leaderSlots
          return $ maxActualSlot >= maxSlotExpected

    leaderSlots <- H.getRelevantLeaderSlots (TR.poolNodeStdout poolNode1) (minimum expectedLeadershipSlotNumbers)

    H.noteShow_ expectedLeadershipSlotNumbers
    H.noteShow_ leaderSlots

    -- As there are no BFT nodes, the next leadership schedule should match slots assigned exactly
    H.assert $ L.null (expectedLeadershipSlotNumbers \\ leaderSlots)

  id do
    scheduleFile <- H.noteTempFile tempAbsPath "schedule.log"

    leadershipScheduleDeadline <- H.noteShowM $ DTC.addUTCTime 180 <$> H.noteShowIO DTC.getCurrentTime

    H.byDeadlineM 5 leadershipScheduleDeadline $ do
      void $ H.execCli' execConfig
        [ "query", "leadership-schedule"
        , "--testnet-magic", show @Int testnetMagic
        , "--genesis", TC.shelleyGenesisFile tr
        , "--stake-pool-id", stakePoolId
        , "--vrf-signing-key-file", poolVrfSkey
        , "--out-file", scheduleFile
        , "--next"
        ]

    scheduleJson <- H.leftFailM $ H.readJsonFile scheduleFile

    expectedLeadershipSlotNumbers <- H.noteShowM $ fmap (fmap slotNumber) $ H.leftFail $ J.parseEither (J.parseJSON @[LeadershipSlot]) scheduleJson

    H.assert $ not (L.null expectedLeadershipSlotNumbers)

    leadershipDeadline <- H.noteShowM $ DTC.addUTCTime 90 <$> H.noteShowIO DTC.getCurrentTime

    H.assertByDeadlineMCustom "Retrieve actual slots" leadershipDeadline $ do
      leaderSlots <- H.getRelevantLeaderSlots (TR.poolNodeStdout poolNode1) (minimum expectedLeadershipSlotNumbers)
      maxSlotExpected <- H.noteShow $ maximum expectedLeadershipSlotNumbers
      if L.null leaderSlots
        then return False
        else do
          maxActualSlot <- H.noteShow $ maximum leaderSlots
          return $ maxActualSlot >= maxSlotExpected

    leaderSlots <- H.getRelevantLeaderSlots (TR.poolNodeStdout poolNode1) (minimum expectedLeadershipSlotNumbers)

    H.noteShow_ expectedLeadershipSlotNumbers
    H.noteShow_ leaderSlots

    -- As there are no BFT nodes, the next leadership schedule should match slots assigned exactly
    H.assert $ L.null (expectedLeadershipSlotNumbers \\ leaderSlots)
