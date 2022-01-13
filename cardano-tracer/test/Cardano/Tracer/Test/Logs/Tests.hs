{-# LANGUAGE LambdaCase #-}

module Cardano.Tracer.Test.Logs.Tests
  ( tests
  ) where

import           Control.Concurrent.Async (withAsync)
import           Control.Monad (filterM)
import           Data.List.Extra (notNull)
import qualified Data.List.NonEmpty as NE
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.Directory
import           System.FilePath
import           System.Time.Extra

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Utils (isItLog, isItSymLink)
import           Cardano.Tracer.Run (doRunCardanoTracer)
import           Cardano.Tracer.Utils (applyBrake, initProtocolsBrake, initDataPointRequestors)

import           Cardano.Tracer.Test.Forwarder
import           Cardano.Tracer.Test.Utils

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.Logs"
  [ testProperty ".log"             $ propRunInLogsStructure  (propLogs ForHuman)
  , testProperty ".json"            $ propRunInLogsStructure  (propLogs ForMachine)
  , testProperty "multi, initiator" $ propRunInLogsStructure2 (propMultiInit ForMachine)
  , testProperty "multi, responder" $ propRunInLogsStructure  (propMultiResp ForMachine)
  ]

propLogs :: LogFormat -> FilePath -> FilePath -> IO Property
propLogs format rootDir localSock = do
  stopProtocols <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  withAsync (doRunCardanoTracer (config rootDir localSock) stopProtocols dpRequestors) . const $
    withAsync (launchForwardersSimple Responder localSock 1000 10000) . const $ do
      sleep 7.0 -- Wait till some rotation is done.
      applyBrake stopProtocols
      sleep 0.5

  doesDirectoryExist rootDir >>= \case
    True ->
      -- ... and contains one node's subdir...
      listDirectory rootDir >>= \case
        []  -> false "root dir is empty"
        [subDir] ->
          withCurrentDirectory rootDir $
            -- ... with *.log-files inside...
            listDirectory subDir >>= \case
              [] -> false "subdir is empty"
              logsAndSymLink ->
                withCurrentDirectory subDir $
                  case filter (isItLog format) logsAndSymLink of
                    [] -> false "subdir doesn't contain expected logs"
                    logsWeNeed ->
                      if length logsWeNeed > 1
                        then
                          -- ... and one symlink...
                          filterM (isItSymLink format) logsAndSymLink >>= \case
                            [] -> false "subdir doesn't contain a symlink"
                            [symLink] -> do
                              -- ... to the latest *.log-file.
                              maybeLatestLog <- getSymbolicLinkTarget symLink
                              -- The logs' names contain timestamps, so the
                              -- latest log is the maximum one.
                              let latestLog = maximum logsWeNeed
                              return $ latestLog === takeFileName maybeLatestLog
                            _ -> false "there is more than one symlink"
                        else false "there is still 1 single log, no rotation"
        _ -> false "root dir contains more than one subdir"
    False -> false "root dir doesn't exist"
 where
  config root p = TracerConfig
    { networkMagic   = 764824073
    , network        = ConnectTo $ NE.fromList [LocalSocket p]
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , logging        = NE.fromList [LoggingParams root FileMode format]
    , rotation       = Just $ RotationParams
                         { rpFrequencySecs = 5
                         , rpLogLimitBytes = 100
                         , rpMaxAgeHours   = 1
                         , rpKeepFilesNum  = 10
                         }
    , verbosity      = Just Minimum
    }

propMultiInit :: LogFormat -> FilePath -> FilePath -> FilePath -> IO Property
propMultiInit format rootDir localSock1 localSock2 = do
  stopProtocols <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  withAsync (doRunCardanoTracer (config rootDir localSock1 localSock2) stopProtocols dpRequestors) . const $
    withAsync (launchForwardersSimple Responder localSock1 1000 10000) . const $
      withAsync (launchForwardersSimple Responder localSock2 1000 10000) . const $ do
        sleep 3.0 -- Wait till some work is done.
        applyBrake stopProtocols
        sleep 0.5
  checkMultiResults rootDir
 where
  config root p1 p2 = TracerConfig
    { networkMagic   = 764824073
    , network        = ConnectTo $ NE.fromList [LocalSocket p1, LocalSocket p2]
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , logging        = NE.fromList [LoggingParams root FileMode format]
    , rotation       = Nothing
    , verbosity      = Just Minimum
    }

propMultiResp :: LogFormat -> FilePath -> FilePath -> IO Property
propMultiResp format rootDir localSock = do
  stopProtocols <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  withAsync (doRunCardanoTracer (config rootDir localSock) stopProtocols dpRequestors) . const $
    withAsync (launchForwardersSimple Initiator localSock 1000 10000) . const $
      withAsync (launchForwardersSimple Initiator localSock 1000 10000) . const $ do
        sleep 3.0 -- Wait till some work is done.
        applyBrake stopProtocols
        sleep 0.5

  checkMultiResults rootDir
 where
  config root p = TracerConfig
    { networkMagic   = 764824073
    , network        = AcceptAt $ LocalSocket p
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , logging        = NE.fromList [LoggingParams root FileMode format]
    , rotation       = Nothing
    , verbosity      = Just Minimum
    }

checkMultiResults :: FilePath -> IO Property
checkMultiResults rootDir =
  -- Check if the root directory exists...
  doesDirectoryExist rootDir >>= \case
    True ->
      -- ... and contains two nodes' subdirs...
      listDirectory rootDir >>= \case
        [] -> false "root dir is empty"
        [subDir1, subDir2] ->
          withCurrentDirectory rootDir $ do
            -- ... with *.log-files inside...
            subDir1list <- listDirectory subDir1
            subDir2list <- listDirectory subDir2
            return . property $ notNull subDir1list && notNull subDir2list
        _ -> false "root dir contains not 2 subdirs"
    False -> false "root dir doesn't exist"
