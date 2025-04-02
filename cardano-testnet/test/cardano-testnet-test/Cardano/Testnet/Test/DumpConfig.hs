{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.DumpConfig
  ( hprop_dump_config
  ) where

import           Cardano.Testnet

import           Prelude

import           Data.Default.Class
import           System.Exit (ExitCode (..))
import           System.FilePath ((</>))
import qualified System.Process as IO

import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Start.Types (ConfigFilesBehaviour (..), GenesisOptions (..))

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Dumping config files/"'@
hprop_dump_config :: Property
hprop_dump_config = integrationWorkspace "dump-config-files" $ \tempAbsBasePath -> H.runWithDefaultWatchdog_ $ do

  H.workspace "config_files" $ \tmpDir -> do
    let shelleyOptions = def { genesisEpochLength = 200 }

    -- Generate config files in a temporary directory
    let generateTestnetOptions = def
          { cardanoConfigFilesBehaviour = OnlyGenerate
          , cardanoOutputDir = Just tmpDir
          }
    confGenerate <- mkConf tmpDir
    _ <- cardanoTestnetDefault generateTestnetOptions shelleyOptions confGenerate

    -- Run testnet with generated config
    let runTestnetOptions = def
          { cardanoConfigFilesBehaviour = GenerateAndRun -- This *is* the default value, but better make it explicit
          , cardanoNodes = UserProvidedNodeOptions $ tmpDir </> "configuration.yaml"
          }
    confRun <- mkConf tempAbsBasePath
    TestnetRuntime
      { testnetNodes = [singleNode]
      } <- cardanoTestnetDefault runTestnetOptions shelleyOptions confRun

    -- Let the node run for a minute, to let problems time to happen
    H.threadDelay 60_000 -- milliseconds
    -- If nothing happened, kill the node and exit with success
    exit <- H.evalIO $ do
      let handle = nodeProcessHandle singleNode
      IO.terminateProcess handle
      IO.getProcessExitCode handle
    H.diff exit (==) $ Just $ ExitFailure 143 -- gracefully exit when hit by SIGTERM
