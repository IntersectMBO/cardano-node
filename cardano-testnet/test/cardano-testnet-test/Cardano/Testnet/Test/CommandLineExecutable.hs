{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.CommandLineExecutable
  ( hprop_cardano_testnet_executable
  ) where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (decodeFileStrict', encodeFile, (.=), Value(..))
import           Data.Aeson.Types (Value (Object))
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Clock (addUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           System.Directory
import           System.Exit (ExitCode (..))
import           System.FilePath
import           System.IO (hClose, hGetContents)
import           System.Process

import           Testnet.Property.Util (integrationWorkspace)

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

{- HLINT ignore "Use uncurry" -}

-- | Function to update the @systemStart@ field of the Shelley genesis
-- and return the value set a seconds in Posix time.
updateShelleySystemStart :: Value -> IO (Value, Int)
updateShelleySystemStart (Object obj) = do
  currentTime <- getCurrentTime
  let futureTime = addUTCTime 15 currentTime
      formattedTime :: T.Text = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" futureTime
      timestamp = round $ utcTimeToPOSIXSeconds futureTime
  return (Object (Aeson.insert "systemStart" (String formattedTime) obj), {-formattedTime,-} timestamp)
updateShelleySystemStart _ = error "Expected a JSON object"

-- | Test the `cardano-testnet` executable
-- Execute me with:
-- @cabal test cardano-testnet-test --test-options '-p "/cardano-testnet-executable/"'@
hprop_cardano_testnet_executable :: Property
hprop_cardano_testnet_executable = integrationWorkspace "cardano-testnet-executable" $ \tempAbsBasePath -> H.runWithDefaultWatchdog_ $ do
  -- Install configuration and genesis files
  let referenceInputsFileDir = "test/cardano-testnet-test/files/input/executable"
      allFiles = ["configuration.json", "alonzo-genesis.json", "byron-genesis.json", "conway-genesis.json", "shelley-genesis.json"]
  liftIO $ forM_ allFiles $ \file -> copyFile (referenceInputsFileDir </> file) (tempAbsBasePath </> file)

  -- Amend the start time in the Shelley Genesis configuration file
  let shelleyGenesisFilePath = tempAbsBasePath </> "shelley-genesis.json"
  genesisValue <- liftIO $ fromJust <$> decodeFileStrict' shelleyGenesisFilePath
  (updatedShelleyGenesisValue, _startTime) <- liftIO $ updateShelleySystemStart genesisValue
  liftIO $ encodeFile shelleyGenesisFilePath updatedShelleyGenesisValue

  -- Alright, all files are in place, let's start the cluster:
  let cmd = ("cabal", [ "run", "cardano-testnet", "--", "cardano"
                      , "--node-config", tempAbsBasePath </> "configuration.json"
                      , "--testnet-magic", "44"])
      cmdString = unwords $ fst cmd : snd cmd
  (_, Just hout, Just herr, ph) <- liftIO $ createProcess (uncurry proc cmd) { std_out = CreatePipe, std_err = CreatePipe }
  exitCode <- liftIO $ waitForProcess ph
  stdOut <- liftIO $ hGetContents hout
  stdErr <- liftIO $ hGetContents herr
  case exitCode of
    ExitSuccess ->
      H.note_ $ "Command succeeded: " <> cmdString
    ExitFailure code -> do
      H.note_ $ "Command failed with exit code " ++ show code ++ ": " <> cmdString
      unless (null stdOut) $ H.note_ $ "stdout: " <> stdOut
      unless (null stdErr) $ H.note_ $ "stderr: " <> stdErr
      H.assert False
  liftIO $ do
    hClose hout
    hClose herr
