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
import           Data.Ord
import           Data.Semigroup
import           GHC.Num
import           Hedgehog (Property, discover)
import           System.IO (IO)
import           Text.Show

import qualified Chairman.Base as H
import qualified Chairman.IO.Network as IO
import qualified Chairman.Network as H
import qualified Chairman.Process as H
import qualified Data.List as L
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified System.Info as IO
import qualified System.IO as IO
import qualified System.Process as IO

{- HLINT ignore "Redundant <&>" -}

prop_spawnOneNode :: Property
prop_spawnOneNode = H.propertyOnce . H.workspace "x" $ \tempDir -> do
  let nodeCount = 3
  base <- H.noteShowM H.getProjectBase
  baseConfig <- H.noteShow $ base <> "/configuration/chairman/defaults/simpleview"
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime 10 currentTime -- 10 seconds into the future
  socketDir <- H.noteShow "./socket"

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

  H.writeFile (tempDir <> "/genesis/GENHASH") . L.unlines . L.reverse . L.take 1 . L.reverse . L.lines =<< H.execCli
    [ "print-genesis-hash"
    , "--genesis-json"
    , tempDir <> "/genesis/genesis.json"
    ]

  let nodeIndexes = [0..nodeCount - 1]

  -- Launch cluster of three nodes
  forM_ nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    dbDir <- H.noteShow $ tempDir <> "/db/node-" <> si
    nodeStdoutFile <- H.noteTempFile tempDir $ "cardano-node-" <> si <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile tempDir $ "cardano-node-" <> si <> ".stderr.log"
    socketFile <- H.noteShow . IO.adjustSocketPath $ socketDir <> "/node-" <> si
    portString <- H.noteShow $ "300" <> si <> ""
    topologyFile <- H.noteShow $ tempDir <> "/topology-node-" <> si <> ".json"
    configFile <- H.noteShow $ tempDir <> "/config-" <> si <> ".yaml"
    signingKeyFile <- H.noteShow $ tempDir <> "/genesis/delegate-keys.00" <> si <> ".key"
    delegationCertificateFile <- H.noteShow $ tempDir <> "/genesis/delegation-cert.00" <> si <> ".json"

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing $ tempDir <> "/" <> socketDir

    H.copyFile (baseConfig <> "/topology-node-" <> si <> ".json") (tempDir <> "/topology-node-" <> si <> ".json")
    H.copyFile (baseConfig <> "/config-" <> si <> ".yaml") (tempDir <> "/config-" <> si <> ".yaml")

    hNodeStdout <- H.evalM . liftIO $ IO.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.evalM . liftIO $ IO.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length socketFile) (<=) IO.maxSocketNameLength

    (Just hIn, _mOut, _mErr, hProcess, _) <- H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--database-path", dbDir
        , "--socket-path", socketFile
        , "--port", portString
        , "--topology", topologyFile
        , "--config", configFile
        , "--signing-key", signingKeyFile
        , "--delegation-certificate", delegationCertificateFile
        , "--shutdown-ipc", "0"
        ]
        <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempDir
          }
        )
      )

    return (hIn, hProcess)

  deadline <- H.noteShowIO $ DTC.addUTCTime 60 <$> DTC.getCurrentTime -- 60 seconds from now

  forM_ [0..nodeCount - 1] $ \i -> H.assertByDeadlineIO deadline $ IO.isPortOpen (3000 + i)

  forM_ nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    socketFile <- if IO.os == "mingw32"
      then H.noteShow . IO.adjustSocketPath $ tempDir <> "/" <> socketDir <> "/node-" <> si <> "-moo"
      else H.noteShow . IO.adjustSocketPath $ tempDir <> "/" <> socketDir <> "/node-" <> si
    H.assertM $ H.doesSocketExist socketFile

tests :: IO Bool
tests = H.checkParallel $$discover
