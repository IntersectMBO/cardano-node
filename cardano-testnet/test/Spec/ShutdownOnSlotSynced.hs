{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.ShutdownOnSlotSynced
  ( hprop_shutdownOnSlotSynced
  ) where

import           Control.Monad
import           Data.Bool ((&&))
import           Data.Either (Either (Right), isRight)
import           Data.Function
import           Data.Int
import           Data.List (find, isInfixOf, lines, reverse, words, (++))
import           Data.Maybe
import           Data.Ord
import           GHC.IO.Exception (ExitCode (ExitSuccess))
import           GHC.Num
import           GHC.Stack (callStack)
import           Hedgehog (Property, assert, (===))
import           Prelude (fromIntegral, round)
import           System.FilePath ((</>))
import           Text.Read (readMaybe)
import           Text.Show (Show (..))

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified Test.Base as H
import           Testnet.Cardano (TestnetNode (..), TestnetNodeOptions (TestnetNodeOptions),
                   TestnetOptions (..), TestnetRuntime (..), defaultTestnetNodeOptions,
                   defaultTestnetOptions, testnet)
import qualified Testnet.Cardano as TC
import qualified Testnet.Conf as H

hprop_shutdownOnSlotSynced :: Property
hprop_shutdownOnSlotSynced = H.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsBasePath' -> do
  -- Start a local test net
  base <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf <- H.noteShowM $ H.mkConf base configurationTemplate tempAbsBasePath' Nothing
  let maxSlot = 1500
      slotLen = 0.01
  let fastTestnetOptions = defaultTestnetOptions
        { epochLength = 300
        , slotLength = slotLen
        , bftNodeOptions =
          [ TestnetNodeOptions
              { TC.extraNodeCliArgs = ["--shutdown-on-slot-synced", show maxSlot]
              }
          , defaultTestnetNodeOptions
          , defaultTestnetNodeOptions
          ]
        }
  TC.TestnetRuntime { bftNodes = node:_ } <- testnet fastTestnetOptions conf

  -- Wait for the node to exit
  let timeout :: Int
      timeout = round (30 + (fromIntegral maxSlot * slotLen))
  mExitCodeRunning <- H.waitSecondsForProcess timeout (nodeProcessHandle node)

  -- Check results
  when (isRight mExitCodeRunning) $ do
    H.cat (nodeStdout node)
    H.cat (nodeStderr node)
  mExitCodeRunning === Right ExitSuccess
  log <- H.readFile (nodeStdout node)
  slotTip <- case find (isInfixOf "Closed db with immutable tip") (reverse (lines log)) of
    Nothing -> H.failMessage callStack "Could not find current tip in node's log."
    Just line -> case listToMaybe (reverse (words line)) of
      Nothing -> H.failMessage callStack "Impossible"
      Just lastWord -> case readMaybe @Integer lastWord of
        Nothing -> H.failMessage callStack ("Expected a node tip as the last word of the log line, but got: " ++ line)
        Just slotTip -> H.noteShow slotTip

  let epsilon = 50
  assert (maxSlot <= slotTip && slotTip <= maxSlot + epsilon)
  return ()
