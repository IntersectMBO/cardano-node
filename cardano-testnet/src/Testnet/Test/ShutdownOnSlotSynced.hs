{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Test.ShutdownOnSlotSynced
  ( testShutdownOnSlotSynced
  ) where

import           Prelude

import           Control.Monad
import           Data.Either (isRight)
import           Data.List (find, isInfixOf)
import           Data.Maybe
import           GHC.IO.Exception (ExitCode (ExitSuccess))
import           GHC.Stack (callStack)
import           Text.Read (readMaybe)

import           Hedgehog (assert, (===))
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H

import           Testnet.Util.Runtime (NodeRuntime (..))

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
