{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.CommandLineExecutable
  ( hprop_cardano_testnet_executable
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           System.Directory
import           System.Exit (ExitCode (..))
import           System.FilePath
import           System.Process

import           Testnet.Property.Util (integrationWorkspace)

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

{- HLINT ignore "Use uncurry" -}

-- | Test the `cardano-testnet` executable
-- Execute me with:
-- @cabal test cardano-testnet-test --test-options '-p "/cardano-testnet-executable/"'@
hprop_cardano_testnet_executable :: Property
hprop_cardano_testnet_executable = integrationWorkspace "cardano-testnet-executable" $ \tempAbsBasePath -> H.runWithDefaultWatchdog_ $ do
  -- Install configuration and genesis files. We only install
  -- the Alonzo and Conway genesis files, to show that we support mixing user-provided
  -- files and automatically generated files
  let referenceInputsFileDir = "test/cardano-testnet-test/files/input/executable"
      allFiles = ["configuration.json", "alonzo-genesis.json", "conway-genesis.json"]
  liftIO $ forM_ allFiles $ \file -> copyFile (referenceInputsFileDir </> file) (tempAbsBasePath </> file)

  -- Alright, all files are in place, let's start the cluster:
  let cmd = ("cabal", [ "run", "cardano-testnet", "--", "cardano"
                      , "--node-config", tempAbsBasePath </> "configuration.json"
                      , "--output-dir", tempAbsBasePath
                      , "--testnet-magic", "42"])
      cmdString = unwords $ fst cmd : snd cmd
  (_, Just _hout, Just _herr, ph) <- liftIO $ createProcess (uncurry proc cmd) { std_out = CreatePipe, std_err = CreatePipe }
  pid <- liftIO $ getPid ph
  let pidString = maybe "unknown" show pid

  H.note_ $ "cardano-testnet started with pid " <> pidString
  H.note_ $ "cardano-testnet sandbox available at " <> show tempAbsBasePath <> " (see subdirectory \"logs\" for stdout and stderr)"

  -- Run in parallel:
  --
  -- 1. The cluster process. If the cluster fails to start or blocks are not produced,
  --    it will return at the latest in 45 seconds (as per the call to 'assertChainExtended' in Configuration.hs)
  -- 2. A watchdog that will kill the cluster process after 60 seconds. If this one returns first,
  --    it means the cluster started correctly.
  raceResult <- liftIO $ race (waitForProcess ph) (threadDelay 60_000_000 >> terminateProcess ph)

  -- TODO don't rely on knowing that there is 'assertChainExtended' check somewhere else.
  -- Instead, for example, check the chain's tip and observe it changes.

  case raceResult of
    Left exitCode -> do
      H.note_ "Cluster failed to start or blocks were not produced in time. This is unexpected."
      H.note_ $ case exitCode of
        ExitSuccess -> "Command succeeded: " <> cmdString
        ExitFailure code -> "Command failed with exit code " ++ show code ++ ": " <> cmdString
      H.assert False
    Right _ -> do
      H.note_ "Cluster started correctly and was killed after 60 seconds, as expected."
