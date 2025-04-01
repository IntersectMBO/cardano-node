{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Node.Shutdown
  ( hprop_shutdown
  , hprop_shutdownOnSlotSynced
  , hprop_shutdownOnSigint
  ) where

import           Cardano.Api

import           Cardano.Testnet
import qualified Cardano.Testnet as Testnet

import           Prelude

import           Control.Applicative (Alternative ((<|>)))
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Default.Class
import           Data.Either (isRight)
import qualified Data.List as L
import           Data.Maybe
import qualified Data.Time.Clock as DTC
import           GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import           GHC.Stack (callStack)
import qualified GHC.Stack as GHC
import qualified System.Exit as IO
import           System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Process as IO
import           System.Process (interruptProcessGroupOf)

import           Testnet.Components.Configuration
import           Testnet.Defaults
import           Testnet.Process.Run (execCli_, initiateProcess, procNode)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Byron
import           Testnet.Start.Types

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import           Hedgehog.Extras.Stock.Time (formatIso8601)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

{- HLINT ignore "Redundant <&>" -}

-- Execute this test with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Shutdown/"'@
--
-- TODO: Use cardanoTestnet in hprop_shutdown
hprop_shutdown :: Property
hprop_shutdown = integrationRetryWorkspace 2 "shutdown" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf <- mkConf tempAbsBasePath'
  let tempBaseAbsPath' = makeTmpBaseAbsPath $ tempAbsPath conf
      tempAbsPath' = unTmpAbsPath $ tempAbsPath conf
      logDir' = makeLogDir $ tempAbsPath conf
      socketDir' = makeSocketDir $ tempAbsPath conf
      testnetMagic' = 42
      sbe = ShelleyBasedEraBabbage

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

  let byronGenesisOutputDir = tempAbsPath' </> "byron"

  startTime <- H.noteShowIO DTC.getCurrentTime
  createByronGenesis
    testnetMagic'
    startTime
    byronDefaultGenesisOptions
    (tempAbsPath' </> "byron.genesis.spec.json")
    byronGenesisOutputDir

  shelleyDir <- H.createDirectoryIfMissing $ tempAbsPath' </> "shelley"

  -- 2. Create Alonzo genesis
  alonzoBabbageTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath' </> shelleyDir </> "genesis.alonzo.spec.json"
  gen <- Testnet.getDefaultAlonzoGenesis sbe
  H.evalIO $ LBS.writeFile alonzoBabbageTestGenesisJsonTargetFile $ encode gen

  -- 2. Create Conway genesis
  conwayBabbageTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath' </> shelleyDir </> "genesis.conway.spec.json"
  H.evalIO $ LBS.writeFile conwayBabbageTestGenesisJsonTargetFile $ encode defaultConwayGenesis

  -- 4. Create Shelley genesis
  execCli_
    [ "latest", "genesis", "create"
    , "--testnet-magic", show @Int testnetMagic'
    , "--genesis-dir", shelleyDir
    , "--start-time", formatIso8601 startTime
    ]

  byronGenesisHash <- getByronGenesisHash $ byronGenesisOutputDir </> "genesis.json"
  -- Move the files to the paths expected by 'defaultYamlHardforkViaConfig' below
  H.renameFile (byronGenesisOutputDir </> "genesis.json") (tempAbsPath' </> defaultGenesisFilepath ByronEra)
  H.renameFile (tempAbsPath' </> "shelley/genesis.json")        (tempAbsPath' </> defaultGenesisFilepath ShelleyEra)
  H.renameFile (tempAbsPath' </> "shelley/genesis.alonzo.json") (tempAbsPath' </> defaultGenesisFilepath AlonzoEra)
  H.renameFile (tempAbsPath' </> "shelley/genesis.conway.json") (tempAbsPath' </> defaultGenesisFilepath ConwayEra)

  shelleyGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> defaultGenesisFilepath ShelleyEra) "ShelleyGenesisHash"
  alonzoGenesisHash  <- getShelleyGenesisHash (tempAbsPath' </> defaultGenesisFilepath AlonzoEra)  "AlonzoGenesisHash"

  let finalYamlConfig :: LBS.ByteString
      finalYamlConfig = encode . Object
                                 $ mconcat [ byronGenesisHash
                                           , shelleyGenesisHash
                                           , alonzoGenesisHash
                                           , defaultYamlHardforkViaConfig sbe]

  H.evalIO $ LBS.writeFile (tempAbsPath' </> "configuration.yaml") finalYamlConfig

  H.evalIO $ LBS.writeFile (tempAbsPath' </> "mainnet-topology.json")
    $ encode defaultMainnetTopology

  -- Run cardano-node with pipe as stdin.  Use 0 file descriptor as shutdown-ipc

  res <- procNode
           [ "run"
           , "--config", tempAbsPath' </> "configuration.yaml"
           , "--topology", tempAbsPath' </> "mainnet-topology.json"
           , "--database-path", tempAbsPath' </> "db"
           , "--socket-path", IO.sprocketArgumentName sprocket
           , "--host-addr", "127.0.0.1"
           , "--port", show @Int port
           , "--shutdown-ipc", "0"
           ]
  let process = res { IO.std_in = IO.CreatePipe
                    , IO.std_out = IO.UseHandle hNodeStdout
                    , IO.std_err = IO.UseHandle hNodeStderr
                    , IO.cwd = Just tempBaseAbsPath'
                    }

  eProcess <- runExceptT $ initiateProcess process
  case eProcess of
    Left e -> H.failMessage GHC.callStack $ mconcat ["Failed to initiate node process: ", show e]
    Right (mStdin, _mStdout, _mStderr, pHandle, _releaseKey) -> do
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


hprop_shutdownOnSlotSynced :: Property
hprop_shutdownOnSlotSynced = integrationRetryWorkspace 2 "shutdown-on-slot-synced" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  -- Start a local test net
  -- TODO: Move yaml filepath specification into individual node options
  conf <- mkConf tempAbsBasePath'

  let maxSlot = 150
      slotLen = 0.01
  let fastTestnetOptions = def
        { cardanoNodes =
          AutomaticNodeOptions
            [ SpoNodeOptions ["--shutdown-on-slot-synced", show maxSlot]
            ]
        }
      shelleyOptions = def
        { genesisEpochLength = 300
        , genesisSlotLength = slotLen
        }
  testnetRuntime <- cardanoTestnetDefault fastTestnetOptions shelleyOptions conf
  let allNodes = testnetNodes testnetRuntime
  H.note_ $ "All nodes: " <>  show (map nodeName allNodes)

  node <- H.headM allNodes
  H.note_ $ "Node name: " <> nodeName node

  -- Wait for the node to exit
  let timeout :: Int
      timeout = round (40 + (fromIntegral maxSlot * slotLen))

  H.note_ $ "Timeout: " <> show timeout

  mExitCodeRunning <- H.waitSecondsForProcess timeout (nodeProcessHandle node)

  -- Check results
  when (isRight mExitCodeRunning) $ do
    H.cat (nodeStdout node)
    H.cat (nodeStderr node)
  mExitCodeRunning === Right ExitSuccess

  logs <- H.readFile (nodeStdout node)
  slotTip <- case findLastSlot . reverse $ lines logs of
    Nothing -> H.failMessage callStack "Could not find close DB message."
    Just s -> return s

  let epsilon = 50
  H.assertWithinTolerance slotTip maxSlot epsilon

-- Execute this test with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/ShutdownOnSigint/"'@
hprop_shutdownOnSigint :: Property
hprop_shutdownOnSigint = integrationRetryWorkspace 2 "shutdown-on-sigint" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  -- Start a local test net
  -- TODO: Move yaml filepath specification into individual node options
  conf <- mkConf tempAbsBasePath'

  let fastTestnetOptions = def
      shelleyOptions = def { genesisEpochLength = 300 }
  testnetRuntime
    <- cardanoTestnetDefault fastTestnetOptions shelleyOptions conf
  TestnetNode{nodeProcessHandle, nodeStdout, nodeStderr} <- H.headM $ testnetNodes testnetRuntime

  -- send SIGINT
  H.evalIO $ interruptProcessGroupOf nodeProcessHandle

  -- Wait for the node to exit
  mExitCodeRunning <- H.waitSecondsForProcess 5 nodeProcessHandle

  -- Check results
  when (isRight mExitCodeRunning) $ do
    H.cat nodeStdout
    H.cat nodeStderr
  case mExitCodeRunning of
    Right (ExitFailure _) -> H.success
    other -> H.failMessage callStack $ "Unexpected exit status for the testnet process: " <> show other

  logs <- H.readFile nodeStdout
  case findLastSlot . reverse $ lines logs of
    Nothing -> H.failMessage callStack "Could not find close DB message."
    _ -> pure ()


findLastSlot :: [String] -> Maybe Int
findLastSlot = go (False, Nothing)
  where
    go (_, mSlot) [] = mSlot
    go (True, mSlot@(Just _)) _ = mSlot
    go r@(isDbClosed, mSlot) (line:ls) = do
      let mLineVal = decode $ LBS.pack line
      case mLineVal of
        -- ignore non-json lines
        Nothing -> go r ls
        Just obj -> do
          let isDbClosed' = isDbClosed || (parseMaybe parseDbClosed obj == Just True)
              mSlot' = mSlot <|> parseMaybe parseSlot obj
          go (isDbClosed', mSlot') ls

    parseDbClosed obj = do
      body <- obj .: "data"
      kind <- body .: "kind"
      pure $ kind == ("DBClosed" :: String)

    parseSlot obj = do
      body <- obj .: "data"
      body .: "slot" :: Parser Int

