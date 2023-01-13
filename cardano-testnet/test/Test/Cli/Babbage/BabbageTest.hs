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

module Test.Cli.Babbage.BabbageTest
  ( hprop_babbageTest
  ) where

import           Cardano.CLI.Shelley.Output (QueryTipLocalStateOutput (..))
import           Control.Monad (void)
import           Data.Monoid (Last (..))
import           GHC.Stack (callStack)
import           Hedgehog (Property)
import           Prelude
import           System.Environment (getEnvironment)
import           System.FilePath ((</>))

import qualified Data.Aeson as J
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified System.Info as SYS

import           Cardano.Testnet
import           Testnet.Util.Process
import           Testnet.Util.Runtime

-- TODO: Create a testnet and check the logs to see if indeed it is babbage
hprop_babbageTest :: Property
hprop_babbageTest = integration . H.runFinallies . H.workspace "babbage" $ \tempAbsBasePath' -> do
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
    -- , wallets
    -- , delegators
    } <- testnet testnetOptions conf

  poolNode1 <- H.headM poolNodes

  env <- H.evalIO getEnvironment

  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1

  execConfig <- H.noteShow H.ExecConfig
    { H.execConfigEnv = Last $ Just $
      [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName poolSprocket1)
      ]
      -- The environment must be passed onto child process on Windows in order to
      -- successfully start that process.
      <> env
    , H.execConfigCwd = Last $ Just tempBaseAbsPath
    }

  tipDeadline <- H.noteShowM $ DTC.addUTCTime 110 <$> H.noteShowIO DTC.getCurrentTime

  H.byDeadlineM 10 tipDeadline "Wait for two epochs" $ do
    void $ execCli' execConfig
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
    H.assert $ currEpoch > 2
    H.assert False
