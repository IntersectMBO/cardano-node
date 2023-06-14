{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Node.Shutdown
  ( hprop_shutdown
  ) where

import           Cardano.Api
import           Control.Monad
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor ((<&>))
import qualified Data.List as L
import           Data.Maybe
import qualified Data.Time.Clock as DTC
import           Hedgehog (Property, (===))
import           Prelude
import           System.FilePath ((</>))

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import           Hedgehog.Extras.Stock.Time (formatIso8601)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Exit as IO
import qualified System.IO as IO
import qualified System.Process as IO
import qualified Testnet.Property.Utils as H

import           Cardano.Testnet
import           Testnet.Defaults
import           Testnet.Process.Run (execCli_, procNode)
import           Testnet.Property.Utils
import           Testnet.Start.Byron
import           Testnet.Topology

{- HLINT ignore "Redundant <&>" -}

hprop_shutdown :: Property
hprop_shutdown = H.integrationRetryWorkspace 2 "shutdown" $ \tempAbsBasePath' -> do
  conf <- H.noteShowM $ mkConf tempAbsBasePath'
  let tempBaseAbsPath' = makeTmpBaseAbsPath $ tempAbsPath conf
      tempAbsPath' = unTmpAbsPath $ tempAbsPath conf
      logDir' = makeLogDir $ tempAbsPath conf
      socketDir' = makeSocketDir $ tempAbsPath conf
      testnetMagic' = 42

  -- TODO: We need to uniformly create these directories
  H.createDirectoryIfMissing_ logDir'
  H.createSubdirectoryIfMissing_ tempBaseAbsPath' socketDir'

  [port] <- H.noteShowIO $ IO.allocateRandomPorts 1

  sprocket <- H.noteShow $ IO.Sprocket tempBaseAbsPath' (socketDir' </> "node")

  H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

  nodeStdoutFile <- H.noteTempFile logDir' "node.stdout.log"
  nodeStderrFile <- H.noteTempFile logDir' "node.stderr.log"

  hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
  hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

  -- We need to create the relevant genesis files to start the node
  -- 1. Create Byron genesis

  H.lbsWriteFile (tempAbsPath' </> "byron.genesis.spec.json")
    . encode $ defaultByronProtocolParamsJsonValue

  startTime <- H.noteShowIO DTC.getCurrentTime
  createByronGenesis
    testnetMagic'
    startTime
    byronDefaultTestnetOptions
    (tempAbsPath' </> "byron.genesis.spec.json")
    (tempAbsPath' </> "byron")

  shelleyDir <- H.createDirectoryIfMissing $ tempAbsPath' </> "shelley"

  -- 2. Create Alonzo genesis
  alonzoBabbageTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath' </> shelleyDir </> "genesis.alonzo.spec.json"
  gen <- H.evalEither $ first displayError defaultAlonzoGenesis
  H.evalIO $ LBS.writeFile alonzoBabbageTestGenesisJsonTargetFile $ encode gen

  -- 2. Create Conway genesis
  conwayBabbageTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath' </> shelleyDir </> "genesis.conway.spec.json"
  H.evalIO $ LBS.writeFile conwayBabbageTestGenesisJsonTargetFile $ encode defaultConwayGenesis

  -- 4. Create Shelley genesis
  execCli_
    [ "genesis", "create"
    , "--testnet-magic", show @Int testnetMagic'
    , "--genesis-dir", shelleyDir
    , "--start-time", formatIso8601 startTime
    ]


  byronGenesisHash <- getByronGenesisHash $ tempAbsPath' </> "byron/genesis.json"
  shelleyGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> "shelley/genesis.json") "ShelleyGenesisHash"
  alonzoGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> "shelley/genesis.alonzo.json") "AlonzoGenesisHash"

  let finalYamlConfig :: LBS.ByteString
      finalYamlConfig = encode . Object
                                 $ mconcat [ byronGenesisHash
                                           , shelleyGenesisHash
                                           , alonzoGenesisHash
                                           , defaultYamlHardforkViaConfig (AnyCardanoEra BabbageEra)]

  H.evalIO $ LBS.writeFile (tempAbsPath' </> "configuration.yaml") finalYamlConfig

  H.evalIO $ LBS.writeFile (tempAbsPath' </> "mainnet-topology.json")
    $ encode defaultMainnetTopology

  -- TODO: Stopped here
  -- Run cardano-node with pipe as stdin.  Use 0 file descriptor as shutdown-ipc
  (mStdin, _mStdout, _mStderr, pHandle, _releaseKey) <- H.createProcess =<<
    ( procNode
      [ "run"
      , "--config", tempAbsPath' </> "configuration.yaml"
      , "--topology", tempAbsPath' </> "mainnet-topology.json"
      , "--database-path", tempAbsPath' </> "db"
      , "--socket-path", IO.sprocketArgumentName sprocket
      , "--host-addr", "127.0.0.1"
      , "--port", show @Int port
      , "--shutdown-ipc", "0"
      ] <&>
      ( \cp -> cp
        { IO.std_in = IO.CreatePipe
        , IO.std_out = IO.UseHandle hNodeStdout
        , IO.std_err = IO.UseHandle hNodeStderr
        , IO.cwd = Just tempBaseAbsPath'
        }
      )
    )

  H.threadDelay $ 10 * 1000000

  mExitCodeRunning <- H.evalIO $ IO.getProcessExitCode pHandle

  when (isJust mExitCodeRunning) $ do
    H.evalIO $ IO.hClose hNodeStdout
    H.evalIO $ IO.hClose hNodeStderr
    H.cat nodeStdoutFile
    H.cat nodeStderrFile

  mExitCodeRunning === Nothing

  forM_ mStdin $ \hStdin -> H.evalIO $ IO.hClose hStdin

  H.threadDelay $ 2 * 1000000

  mExitCode <- H.evalIO $ IO.getProcessExitCode pHandle

  mExitCode === Just IO.ExitSuccess

  return ()
