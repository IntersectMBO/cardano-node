{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Node.Chairman
  ( tests
  ) where

import           Cardano.Prelude
import           Hedgehog (Property, discover)

import qualified Data.Time.Clock as DTC
import qualified Data.Time.Clock.POSIX as DTC
import qualified Hedgehog as H
import qualified System.IO as IO
import qualified Test.Common.Base as H
import qualified Test.Common.Process as H

prop_spawnOneNode :: Property
prop_spawnOneNode = H.propertyOnce . H.workspace "temp/chairman" $ \tempDir -> do
  base <- H.noteShowM H.getProjectBase
  baseConfig <- H.noteShow $ base <> "/configuration/chairman/defaults/simpleview"
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime 60 currentTime -- 60 seconds into the future

  -- Generate keys
  void $ H.execCli
    [ "genesis"
    , "--genesis-output-dir", tempDir <> "/genesis"
    , "--start-time", show @Int64 (floor (DTC.utcTimeToPOSIXSeconds startTime))
    , "--protocol-parameters-file", base <> "/scripts/protocol-params.json"
    , "--k", "2160"
    , "--protocol-magic", "459045235"
    , "--n-poor-addresses", "128"
    , "--n-delegate-addresses", "7"
    , "--total-balance", "8000000000000000"
    , "--avvm-entry-count", "128"
    , "--avvm-entry-balance", "10000000000000"
    , "--delegate-share", "0.9"
    , "--real-pbft"
    , "--secret-seed", "2718281828"
    ]

  -- Launch cluster of three nodes
  procResults <- forM [0..2] $ \i -> do
    si <- H.noteShow $ show @Int i
    dbDir <- H.noteShow $ tempDir <> "/db/node-" <> si
    socketDir <- H.noteShow $ tempDir <> "/socket"

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing socketDir

    H.copyFile (baseConfig <> "/topology-node-" <> si <> ".json") (tempDir <> "/topology-node-" <> si <> ".json")
    H.copyFile (baseConfig <> "/config-" <> si <> ".yaml") (tempDir <> "/config-" <> si <> ".yaml")

    (Just hIn, _mOut, _mErr, hProcess) <- H.createProcess =<< H.procNode
      [ "run"
      , "--database-path", dbDir
      , "--socket-path", socketDir <> "/node-" <> si <> "-socket"
      , "--port", "300" <> si <> ""
      , "--topology", tempDir <> "/topology-node-" <> si <> ".json"
      , "--config", tempDir <> "/config-" <> si <> ".yaml"
      , "--signing-key", tempDir <> "/genesis/delegate-keys.00" <> si <> ".key"
      , "--delegation-certificate", tempDir <> "/genesis/delegation-cert.00" <> si <> ".json"
      , "--shutdown-ipc", "0"
      ]

    return (hIn, hProcess)

  H.threadDelay 10000000

  -- Signal for cluster to shutdown and wait for shutdown to complete
  forM_ procResults $ \(hIn, _) -> liftIO $ IO.hClose hIn
  forM_ procResults $ \(_, hProcess) -> void $ H.waitForProcess hProcess

tests :: IO Bool
tests = H.checkParallel $$discover
