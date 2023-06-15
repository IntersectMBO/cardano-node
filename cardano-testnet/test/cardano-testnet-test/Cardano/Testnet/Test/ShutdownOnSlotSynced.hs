{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.ShutdownOnSlotSynced
  ( hprop_shutdownOnSlotSynced
  ) where

import           Prelude

import           Cardano.Testnet

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy.Char8 as LBS (pack)
import           Data.Either (isRight)
import           Data.Maybe
import           GHC.IO.Exception (ExitCode (ExitSuccess))
import           GHC.Stack (callStack)

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified Testnet.Property.Utils as H

import           Testnet.Runtime (TestnetRuntime (..))

hprop_shutdownOnSlotSynced :: Property
hprop_shutdownOnSlotSynced = H.integrationRetryWorkspace 2 "shutdown-on-slot-synced" $ \tempAbsBasePath' -> do
  -- Start a local test net
  -- TODO: Move yaml filepath specification into individual node options
  conf <- H.noteShowM $  mkConf tempAbsBasePath'

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

  -- Wait for the node to exit
  let timeout :: Int
      timeout = round (40 + (fromIntegral maxSlot * slotLen))
  mExitCodeRunning <- H.waitSecondsForProcess timeout (nodeProcessHandle node)

  -- Check results
  when (isRight mExitCodeRunning) $ do
    H.cat (nodeStdout node)
    H.cat (nodeStderr node)
  mExitCodeRunning === Right ExitSuccess

  logs <- H.readFile (nodeStdout node)
  slotTip <- case mapMaybe parseMsg $ reverse $ lines logs of
    [] -> H.failMessage callStack "Could not find close DB message."
    (Left err):_ -> H.failMessage callStack err
    (Right s):_ -> return s

  let epsilon = 50

  H.assert (maxSlot <= slotTip && slotTip <= maxSlot + epsilon)

  where
    parseMsg :: String -> Maybe (Either String Integer)
    parseMsg line = case decode $ LBS.pack line of
      Nothing -> Just $ Left $ "Expected JSON formated log message, but got: " ++ line
      Just obj -> Right <$> parseMaybe parseTipSlot obj

    parseTipSlot :: Object -> Parser Integer
    parseTipSlot obj = do
      body <- obj .: "data"
      tip <- body .: "tip"
      kind <- body .: "kind"
      if kind == ("TraceOpenEvent.ClosedDB" :: String)
        then tip .: "slot"
        else mzero
