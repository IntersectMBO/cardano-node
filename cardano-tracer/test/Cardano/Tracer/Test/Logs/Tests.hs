{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- HLINT ignore "Redundant ==" -}

module Cardano.Tracer.Test.Logs.Tests
  ( tests
  ) where

import qualified Cardano.Logging.Types as Net
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Utils (isItLog)
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Run (doRunCardanoTracer)
import           Cardano.Tracer.Test.Forwarder
import           Cardano.Tracer.Test.TestSetup
import           Cardano.Tracer.Test.Utils
import           Cardano.Tracer.Utils (applyBrake, initDataPointRequestors, initProtocolsBrake)

import           Control.Concurrent.Async (withAsync, link)
import           Data.List.Extra (notNull)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Traversable (for)
import           Data.Word (Word64)
import           System.Directory
import           System.Directory.Extra
import           System.FilePath
import           System.Time.Extra

import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestSetup Identity -> TestTree
tests ts = localOption (QuickCheckTests 1) $ testGroup "Test.Logs"
  [ testProperty ".log"                    do propRunInLogsStructure       ts (propLogs      ts ForHuman   100 60)
  , testProperty ".log"                    do propRunInLogsStructure       ts (propLogs      ts ForHuman   4   1)
  , testProperty ".json"                   do propRunInLogsStructure       ts (propLogs      ts ForMachine 100 60)
  , testProperty "multi, initiator socket" do propRunInLogsStructureLocal2 ts (propMultiInit ts ForMachine)
  , testProperty "multi, responder socket" do propRunInLogsStructureLocal  ts (propMultiResp ts ForMachine)

  , testProperty "multi, initiator, port" do propRunInLogsStructurePort2   ts (propMultiInit ts ForMachine)

  , testProperty "multi, responder, port" do propRunInLogsStructurePort    ts (propMultiResp ts ForMachine)
  ]

propLogs :: TestSetup Identity -> LogFormat -> Word64 -> Word64 -> FilePath -> FilePath -> IO Property
propLogs ts@TestSetup{..} format logRotLimitBytes logRotMaxAgeMinutes rootDir localSock = do
  lock <- newLock
  stopProtocols <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  withAsync (doRunCardanoTracer (acceptConfig rootDir) (Just $ rootDir <> "/../state") stderrShowTracer stopProtocols dpRequestors) \async1 -> do
    link async1
    sleep 1.0
    withAsync (launchForwardersSimple ts Initiator (Net.LocalPipe localSock) 10000) \async2 -> do
      link async2
      sleep 8.0 -- Wait till some rotation is done.
      applyBrake stopProtocols
      sleep 0.5
      acquire lock

  release lock
  checkPropLogsResults rootDir format
 where
  acceptConfig root = TracerConfig
    { networkMagic   = unNetworkMagic $ unI tsNetworkMagic
    , network        = AcceptAt (Net.LocalPipe localSock)
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , hasRTView      = Nothing
    , logging        = LoggingParams root FileMode format :| []
    , rotation       = Just $ RotationParams
                         { rpFrequencySecs = 3
                         , rpLogLimitBytes = logRotLimitBytes
                         , rpMaxAgeMinutes = logRotMaxAgeMinutes
                         , rpKeepFilesNum  = 10
                         }
    , verbosity      = Just Minimum
    , metricsNoSuffix = Nothing
    , metricsHelp    = Nothing
    , hasForwarding  = Nothing
    , resourceFreq   = Nothing
    , ekgRequestFull = Nothing
    }

propMultiInit :: TestSetup Identity -> LogFormat -> FilePath -> HowToConnect -> HowToConnect -> IO Property
propMultiInit ts@TestSetup{..} format rootDir howToConnect1 howToConnect2 = do
  lock <- newLock
  stopProtocols <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  withAsync (doRunCardanoTracer initConfig (Just $ rootDir <> "/../state") stderrShowTracer stopProtocols dpRequestors) \async1 -> do
    link async1
    sleep 1.0
    withAsync (launchForwardersSimple ts Responder howToConnect1 10000) \async2 -> do
      link async2
      sleep 1.0
      withAsync (launchForwardersSimple ts Responder howToConnect2 10000) \async3 -> do
        link async3
        sleep 5.0 -- Wait till some work is done.
        applyBrake stopProtocols
        sleep 0.5
        acquire lock
  release lock
  checkMultiResults rootDir
 where
  initConfig :: TracerConfig
  initConfig = TracerConfig
    { networkMagic   = unNetworkMagic $ unI tsNetworkMagic
    , network        = ConnectTo $ howToConnect1 :| [howToConnect2]
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , hasRTView      = Nothing
    , logging        = LoggingParams rootDir FileMode format :| []
    , rotation       = Nothing
    , verbosity      = Just Minimum
    , metricsNoSuffix = Nothing
    , metricsHelp    = Nothing
    , hasForwarding  = Nothing
    , resourceFreq   = Nothing
    , ekgRequestFull = Nothing
    }

-- | Tests
propMultiResp :: TestSetup Identity -> LogFormat -> FilePath -> HowToConnect -> IO Property
propMultiResp ts@TestSetup{..} format rootDir howToConnect = do
  lock <- newLock
  stopProtocols <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  withAsync (doRunCardanoTracer respConfig (Just $ rootDir <> "/../state") stderrShowTracer stopProtocols dpRequestors) \async1 -> do
    link async1
    sleep 1.0
    -- withAsync (launchForwardersSimple ts Initiator howToConnect 10000) \async2 -> do
    withAsync (launchForwardersSimple ts Initiator howToConnect 10000) \async2 -> do
      link async2
      sleep 1.0
      -- withAsync (launchForwardersSimple ts Initiator howToConnect 10000) \async3 -> do
      withAsync (launchForwardersSimple ts Initiator howToConnect 10000) \async3 -> do
        link async3
        sleep 5.0 -- Wait till some work is done.
        applyBrake stopProtocols
        sleep 0.5
        acquire lock
  release lock
  checkMultiResults rootDir
 where
  respConfig :: TracerConfig
  respConfig = TracerConfig
    { networkMagic   = unNetworkMagic $ unI tsNetworkMagic
    , network        = AcceptAt howToConnect
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , hasRTView      = Nothing
    , logging        = LoggingParams rootDir FileMode format :| []
    , rotation       = Nothing
    , verbosity      = Just Minimum
    , metricsNoSuffix = Nothing
    , metricsHelp    = Nothing
    , hasForwarding  = Nothing
    , resourceFreq   = Nothing
    , ekgRequestFull = Nothing
    }

checkPropLogsResults :: FilePath -> LogFormat -> IO Property
checkPropLogsResults rootDir format =
  doesDirectoryExist rootDir >>= \case
    False -> false "root dir doesn't exist"
    True -> do
      -- ... and contains one node's subdir...
      listDirectories rootDir >>= \case
        [] -> false "root dir is empty"
        subDir:_ -> do
          -- ... with *.log-files inside...
          let pathToSubDir = rootDir </> subDir
          listFiles pathToSubDir >>= \case
            [] -> false "subdir is empty"
            logsAndSymLink -> do
              case filter (isItLog format) logsAndSymLink of
                []        -> false "subdir doesn't contain expected logs"
                [_oneLog] -> false "there is still 1 single log, no rotation"
                _logs     -> return $ property True


checkMultiResults :: FilePath -> IO Property
checkMultiResults rootDir =
  -- Check if the root directory exists...
  doesDirectoryExist rootDir >>= \case
    False ->
      false "root dir doesn't exist"
    True ->
      -- ... and contains two nodes' subdirs...
      listDirectories rootDir >>= \case
        [] ->
          false "root dir is empty"
        subdirs -> do
          areDirsFull <- for subdirs \sd -> notNull <$> listFiles (rootDir </> sd)
          return . property $ all (True ==) areDirsFull
