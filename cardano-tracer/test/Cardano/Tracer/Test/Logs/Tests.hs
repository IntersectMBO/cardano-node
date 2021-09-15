{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Test.Logs.Tests
  ( tests
  ) where

import           Control.Concurrent.Async (withAsync)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (modifyTVar', newTVarIO)
import           Control.Monad (filterM)
import qualified Data.List.NonEmpty as NE
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.Directory
import           System.FilePath
import           System.Time.Extra

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Log (isItLog, isItSymLink)
import           Cardano.Tracer.Run (runCardanoTracerWithConfigBrakes)

import           Cardano.Tracer.Test.Forwarder (launchForwardersSimple)
import           Cardano.Tracer.Test.Utils

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.Logs"
  [ testProperty ".log"  $ propRunInLogsStructure (propLogs ForHuman)
  , testProperty ".json" $ propRunInLogsStructure (propLogs ForMachine)
  ]

propLogs :: LogFormat -> FilePath -> FilePath -> IO Property
propLogs format rootDir localSock = do
  stopEKG <- newTVarIO False
  stopTF  <- newTVarIO False
  let brakes = NE.fromList [(stopEKG, stopTF)]
  withAsync (runCardanoTracerWithConfigBrakes (config rootDir localSock) brakes) $ \_ ->
    withAsync (launchForwardersSimple localSock 1000 10000) $ \_ -> do
      sleep 15.0 -- Wait till some rotation is done.
      atomically $ do
        modifyTVar' stopEKG . const $ True
        modifyTVar' stopTF  . const $ True
      sleep 1.0

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
    { network        = ConnectTo $ NE.fromList [LocalSocket p]
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , logging        = NE.fromList [LoggingParams root FileMode format]
    , rotation       = Just $ RotationParams
                         { rpLogLimitBytes = 100
                         , rpMaxAgeHours   = 1
                         , rpKeepFilesNum  = 10
                         }
    }
