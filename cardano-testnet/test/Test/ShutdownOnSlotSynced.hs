{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.ShutdownOnSlotSynced
  ( hprop_shutdownOnSlotSynced
  , testShutdownOnSlotSynced
  ) where

import           Prelude

import           Control.Monad
import           Data.Either (isRight)
import           Data.List (find, isInfixOf)
import           Data.Maybe
import           GHC.IO.Exception (ExitCode (ExitSuccess))
import           GHC.Stack (callStack)
import qualified System.Directory as IO
import           System.FilePath ((</>))
import           Text.Read (readMaybe)

import           Hedgehog (Property, assert, (===))
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified Testnet.Util.Base as H

import           Cardano.Testnet
import           Testnet.Util.Runtime (TestnetRuntime (..))

hprop_shutdownOnSlotSynced :: Property
hprop_shutdownOnSlotSynced = H.integrationRetryWorkspace 2 "shutdown-on-slot-synced" $ \tempAbsBasePath' -> do
  -- Start a local test net
  baseDir <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  configTemplate <- H.noteShow $ baseDir </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf <- H.noteShowM $
    mkConf (ProjectBase baseDir) (YamlFilePath configTemplate) tempAbsBasePath' Nothing
  let maxSlot = 1500
      slotLen = 0.01
  let fastTestnetOptions = CardanoOnlyTestnetOptions $ cardanoDefaultTestnetOptions
        { cardanoEpochLength = 300
        , cardanoSlotLength = slotLen
        , cardanoNodes =
          [ BftTestnetNodeOptions ["--shutdown-on-slot-synced", show maxSlot]
          , BftTestnetNodeOptions []
          , SpoTestnetNodeOptions
          ]
        }
  TestnetRuntime { bftNodes = node:_ } <- testnet fastTestnetOptions conf
  let timeout = round (40 + (fromIntegral maxSlot * slotLen))
  testShutdownOnSlotSynced maxSlot timeout node

---Test core with minimal needed arguments
testShutdownOnSlotSynced :: Integer -> Int -> NodeRuntime -> H.Integration ()
testShutdownOnSlotSynced maxSlot timeout node = do
  -- Wait for the node to exit
  mExitCodeRunning <- H.waitSecondsForProcess timeout (nodeProcessHandle node)

  -- Check results
  when (isRight mExitCodeRunning) $ do
    H.cat (nodeStdout node)
    H.cat (nodeStderr node)
  mExitCodeRunning === Right ExitSuccess
  logs <- H.readFile (nodeStdout node)
  slotTip <- case find (isInfixOf "Closed db with immutable tip") (reverse (lines logs)) of
    Nothing -> H.failMessage callStack "Could not find current tip in node's log."
    Just line -> case listToMaybe (reverse (words line)) of
      Nothing -> H.failMessage callStack "Impossible"
      Just lastWord -> case readMaybe @Integer lastWord of
        Nothing -> H.failMessage callStack ("Expected a node tip as the last word of the log line, but got: " ++ line)
        Just slotTip -> H.noteShow slotTip

  let epsilon = 50
  assert (maxSlot <= slotTip && slotTip <= maxSlot + epsilon)
  return ()
