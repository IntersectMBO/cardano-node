{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- HLINT ignore "Redundant ==" -}

module Cardano.Tracer.Test.Logs.Tests
  ( tests
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Utils (isItLog)
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Run (doRunCardanoTracer)
import           Cardano.Tracer.Test.Forwarder
import           Cardano.Tracer.Test.TestSetup
import           Cardano.Tracer.Test.Utils
import           Cardano.Tracer.Utils (applyBrake, initDataPointRequestors, initProtocolsBrake)

import           Control.Concurrent.Async (withAsync)
import           Control.Monad (forM)
import           Data.List.Extra (notNull)
import qualified Data.List.NonEmpty as NE
import           Data.Word (Word64)
import           System.Directory
import           System.Directory.Extra
import           System.FilePath
import           System.Time.Extra

import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestSetup Identity -> TestTree
tests ts = localOption (QuickCheckTests 1) $ testGroup "Test.Logs"
  [ testProperty ".log"             do propRunInLogsStructure  ts (propLogs      ts ForHuman   100 60)
  , testProperty ".log"             do propRunInLogsStructure  ts (propLogs      ts ForHuman   4   1)
  , testProperty ".json"            do propRunInLogsStructure  ts (propLogs      ts ForMachine 100 60)
  , testProperty "multi, initiator" do propRunInLogsStructure2 ts (propMultiInit ts ForMachine)
  , testProperty "multi, responder" do propRunInLogsStructure  ts (propMultiResp ts ForMachine)
  ]

propLogs :: TestSetup Identity -> LogFormat -> Word64 -> Word64 -> FilePath -> FilePath -> IO Property
propLogs ts@TestSetup{..} format logRotLimitBytes logRotMaxAgeMinutes rootDir localSock = do
  stopProtocols <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  withAsync (doRunCardanoTracer config (Just $ rootDir <> "/../state") stderrShowTracer stopProtocols dpRequestors) \_ -> do
    sleep 1.0
    withAsync (launchForwardersSimple ts Initiator localSock 1000 10000) \_ -> do
      sleep 8.0 -- Wait till some rotation is done.
      applyBrake stopProtocols
      sleep 0.5

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
 where
  config = TracerConfig
    { networkMagic   = unNetworkMagic $ unI tsNetworkMagic
    , network        = AcceptAt (LocalSocket localSock)
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , hasRTView      = Nothing
    , logging        = NE.fromList [LoggingParams rootDir FileMode format]
    , rotation       = Just $ RotationParams
                         { rpFrequencySecs = 3
                         , rpLogLimitBytes = logRotLimitBytes
                         , rpMaxAgeMinutes = logRotMaxAgeMinutes
                         , rpKeepFilesNum  = 10
                         }
    , verbosity      = Just Minimum
    , metricsComp    = Nothing
    , metricsHelp    = Nothing
    , hasForwarding  = Nothing
    , resourceFreq   = Nothing
    , ekgRequestFull = Nothing
    }

propMultiInit :: TestSetup Identity -> LogFormat -> FilePath -> FilePath -> FilePath -> IO Property
propMultiInit ts@TestSetup{..} format rootDir localSock1 localSock2 = do
  stopProtocols <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  withAsync (doRunCardanoTracer (config rootDir localSock1 localSock2) (Just $ rootDir <> "/../state") stderrShowTracer stopProtocols dpRequestors) \_ -> do
    sleep 1.0
    withAsync (launchForwardersSimple ts Responder localSock1 1000 10000) \_ -> do
      sleep 1.0
      withAsync (launchForwardersSimple ts Responder localSock2 1000 10000) \_ -> do
        sleep 5.0 -- Wait till some work is done.
        applyBrake stopProtocols
        sleep 0.5
  checkMultiResults rootDir
 where
  config root p1 p2 = TracerConfig
    { networkMagic   = unNetworkMagic $ unI tsNetworkMagic
    , network        = ConnectTo $ NE.fromList [LocalSocket p1, LocalSocket p2]
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , hasRTView      = Nothing
    , logging        = NE.fromList [LoggingParams root FileMode format]
    , rotation       = Nothing
    , verbosity      = Just Minimum
    , metricsComp    = Nothing
    , metricsHelp    = Nothing
    , hasForwarding  = Nothing
    , resourceFreq   = Nothing
    , ekgRequestFull = Nothing
    }

propMultiResp :: TestSetup Identity -> LogFormat -> FilePath -> FilePath -> IO Property
propMultiResp ts@TestSetup{..} format rootDir localSock = do
  stopProtocols <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  withAsync (doRunCardanoTracer (config rootDir localSock) (Just $ rootDir <> "/../state") stderrShowTracer stopProtocols dpRequestors) \_ -> do
    sleep 1.0
    withAsync (launchForwardersSimple ts Initiator localSock 1000 10000) \_ -> do
      sleep 1.0
      withAsync (launchForwardersSimple ts Initiator localSock 1000 10000) \_ -> do
        sleep 5.0 -- Wait till some work is done.
        applyBrake stopProtocols
        sleep 0.5
  checkMultiResults rootDir
 where
  config root p = TracerConfig
    { networkMagic   = unNetworkMagic $ unI tsNetworkMagic
    , network        = AcceptAt $ LocalSocket p
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , hasRTView      = Nothing
    , logging        = NE.fromList [LoggingParams root FileMode format]
    , rotation       = Nothing
    , verbosity      = Just Minimum
    , metricsComp    = Nothing
    , metricsHelp    = Nothing
    , hasForwarding  = Nothing
    , resourceFreq   = Nothing
    , ekgRequestFull = Nothing
    }

checkMultiResults :: FilePath -> IO Property
checkMultiResults rootDir =
  -- Check if the root directory exists...
  doesDirectoryExist rootDir >>= \case
    False -> false "root dir doesn't exist"
    True ->
      -- ... and contains two nodes' subdirs...
      listDirectories rootDir >>= \case
        [] -> false "root dir is empty"
        subdirs -> do
          areDirsFull <- forM subdirs $ \sd -> notNull <$> listFiles (rootDir </> sd)
          return . property $ all (True ==) areDirsFull
