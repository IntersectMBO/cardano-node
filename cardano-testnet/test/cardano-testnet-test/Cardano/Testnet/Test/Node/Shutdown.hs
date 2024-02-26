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

import           Prelude

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as LBS
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

import           Testnet.Defaults
import           Testnet.Process.Run
import qualified Testnet.Property.Utils as H
import           Testnet.Property.Utils
import           Testnet.Runtime
import           Testnet.Start.Byron

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import           Hedgehog.Extras.Stock.Time (formatIso8601)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H

{- HLINT ignore "Redundant <&>" -}
-- TODO: Use cardanoTestnet in hprop_shutdown
hprop_shutdown :: Property
hprop_shutdown = H.integrationRetryWorkspace 2 "shutdown" $ \tempAbsBasePath' -> do
  conf <- mkConf tempAbsBasePath'
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
    byronDefaultGenesisOptions
    (tempAbsPath' </> "byron.genesis.spec.json")
    (tempAbsPath' </> "byron")

  shelleyDir <- H.createDirectoryIfMissing $ tempAbsPath' </> "shelley"

  -- 2. Create Alonzo genesis
  alonzoBabbageTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath' </> shelleyDir </> "genesis.alonzo.spec.json"
  gen <- H.evalEither $ first (docToString . prettyError) defaultAlonzoGenesis
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
  H.renameFile (tempAbsPath' </> "shelley/genesis.json") (tempAbsPath' </> defaultShelleyGenesisFp)
  shelleyGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> defaultShelleyGenesisFp) "ShelleyGenesisHash"
  alonzoGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> "shelley/genesis.alonzo.json") "AlonzoGenesisHash"

  let finalYamlConfig :: LBS.ByteString
      finalYamlConfig = encode . Object
                                 $ mconcat [ byronGenesisHash
                                           , shelleyGenesisHash
                                           , alonzoGenesisHash
                                           , defaultYamlHardforkViaConfig (AnyCardanoEra BabbageEra)] -- TODO: This should not be hardcoded

  H.evalIO $ LBS.writeFile (tempAbsPath' </> "configuration.yaml") finalYamlConfig

  H.evalIO $ LBS.writeFile (tempAbsPath' </> "mainnet-topology.json")
    $ encode defaultMainnetTopology

  -- Run cardano-node with pipe as stdin.  Use 0 file descriptor as shutdown-ipc

  eRes <- liftIO . runExceptT $ procNode
                         [ "run"
                         , "--config", tempAbsPath' </> "configuration.yaml"
                         , "--topology", tempAbsPath' </> "mainnet-topology.json"
                         , "--database-path", tempAbsPath' </> "db"
                         , "--socket-path", IO.sprocketArgumentName sprocket
                         , "--host-addr", "127.0.0.1"
                         , "--port", show @Int port
                         , "--shutdown-ipc", "0"
                         ]
  res <- H.evalEither eRes
  let process = res { IO.std_in = IO.CreatePipe
                    , IO.std_out = IO.UseHandle hNodeStdout
                    , IO.std_err = IO.UseHandle hNodeStderr
                    , IO.cwd = Just tempBaseAbsPath'
                    }

  eProcess <- lift $ lift $ runExceptT $ initiateProcess process
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
hprop_shutdownOnSlotSynced = H.integrationRetryWorkspace 2 "shutdown-on-slot-synced" $ \tempAbsBasePath' -> do
  -- Start a local test net
  -- TODO: Move yaml filepath specification into individual node options
  conf <- mkConf tempAbsBasePath'

  let maxSlot = 150
      slotLen = 0.01
  let fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 300
        , cardanoSlotLength = slotLen
        , cardanoNodes =
          [ SpoTestnetNodeOptions Nothing ["--shutdown-on-slot-synced", show maxSlot]
          , SpoTestnetNodeOptions Nothing []
          , SpoTestnetNodeOptions Nothing []
          ]
        }
  testnetRuntime <- cardanoTestnetDefault fastTestnetOptions conf
  let allNodes' = poolNodes testnetRuntime
  H.note_ $ "All nodes: " <>  show (map (nodeName . poolRuntime) allNodes')

  node <- H.headM $ poolRuntime <$> allNodes'
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
  slotTip <- case mapMaybe parseMsg $ reverse $ lines logs of
    [] -> H.failMessage callStack "Could not find close DB message."
    (Left err):_ -> H.failMessage callStack err
    (Right s):_ -> return s

  let epsilon = 50

  H.assert (maxSlot <= slotTip && slotTip <= maxSlot + epsilon)

hprop_shutdownOnSigint :: Property
hprop_shutdownOnSigint = H.integrationRetryWorkspace 2 "shutdown-on-sigint" $ \tempAbsBasePath' -> do
  -- Start a local test net
  -- TODO: Move yaml filepath specification into individual node options
  conf <- mkConf tempAbsBasePath'

  let fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 300
        , cardanoSlotLength = 0.01
        }
  testnetRuntime
    <- cardanoTestnetDefault fastTestnetOptions conf
  node@NodeRuntime{nodeProcessHandle} <- H.headM $ poolRuntime <$> poolNodes testnetRuntime

  -- send SIGINT
  H.evalIO $ interruptProcessGroupOf nodeProcessHandle

  -- Wait for the node to exit
  mExitCodeRunning <- H.waitSecondsForProcess 5 nodeProcessHandle

  -- Check results
  when (isRight mExitCodeRunning) $ do
    H.cat (nodeStdout node)
    H.cat (nodeStderr node)
  case mExitCodeRunning of
    Right (ExitFailure _) -> H.success
    other -> H.failMessage callStack $ "Unexpected exit status for the testnet process: " <> show other

  logs <- H.readFile (nodeStdout node)
  case mapMaybe parseMsg $ reverse $ lines logs of
    [] -> H.failMessage callStack "Could not find close DB message."
    (Left err):_ -> H.failMessage callStack err
    (Right _):_ -> pure ()


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
