{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Node.Chairman
  ( tests
  ) where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Bool
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           GHC.Num
import           Hedgehog (Property, discover)
import           System.IO (IO)
import           Text.Show

import qualified Chairman.Base as H
import qualified Chairman.Network as IO
import qualified Chairman.Process as H
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified System.IO as IO
import qualified System.Process as IO

{- HLINT ignore "Redundant <&>" -}

prop_spawnOneNode :: Property
prop_spawnOneNode = H.propertyOnce . H.workspace "temp/chairman" $ \tempDir -> do
  let nodeCount = 3
  base <- H.noteShowM H.getProjectBase
  baseConfig <- H.noteShow $ base <> "/configuration/chairman/defaults/simpleview"
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime 60 currentTime -- 60 seconds into the future

  -- Generate keys
  void $ H.execCli
    [ "genesis"
    , "--genesis-output-dir", tempDir <> "/genesis"
    , "--start-time", H.showUTCTimeSeconds startTime
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
  forM_ [0..nodeCount - 1] $ \i -> do
    si <- H.noteShow $ show @Int i
    dbDir <- H.noteShow $ tempDir <> "/db/node-" <> si
    socketDir <- H.noteShow $ tempDir <> "/socket"
    nodeStdoutFile <- H.noteTempFile tempDir "cardano-node.stdout.log"
    nodeStderrFile <- H.noteTempFile tempDir "cardano-node.stderr.log"

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing socketDir

    H.copyFile (baseConfig <> "/topology-node-" <> si <> ".json") (tempDir <> "/topology-node-" <> si <> ".json")
    H.copyFile (baseConfig <> "/config-" <> si <> ".yaml") (tempDir <> "/config-" <> si <> ".yaml")

    hNodeStdout <- H.evalM . liftIO $ IO.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.evalM . liftIO $ IO.openFile nodeStderrFile IO.WriteMode

    (Just hIn, _mOut, _mErr, hProcess, _) <- H.createProcess =<<
      ( ( H.procNode
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
        ) <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          }
        )
      )

    return (hIn, hProcess)

  deadline <- H.noteShowIO $ DTC.addUTCTime 180 <$> DTC.getCurrentTime -- 60 seconds from now

  forM_ [0..nodeCount - 1] $ \i -> H.assertByDeadlineIO deadline $ IO.isPortOpen (3000 + i)

  H.threadDelay 10000000

tests :: IO Bool
tests = H.checkParallel $$discover
