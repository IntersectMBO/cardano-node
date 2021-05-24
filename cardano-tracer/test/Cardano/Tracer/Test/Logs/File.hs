{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Tracer.Test.Logs.File
  ( tests
  ) where

import           Control.Concurrent (forkIO, killThread, threadDelay)
import           Control.Monad (filterM)
import           Data.List (sort)
import           Data.Word (Word16)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.Directory

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Log (isItLog, isItSymLink)
import           Cardano.Tracer.Run (runCardanoTracerWithConfig)

import           Cardano.Tracer.Test.Forwarder (launchForwardersSimple)

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.Logs.File"
  [ testProperty ".log" $
      propFile AsText "/tmp/test-logs-text" "127.0.0.1" 3000
  , testProperty ".json" $
      propFile AsJSON "/tmp/test-logs-json" "127.0.0.1" 3010
  ]

propFile
  :: LogFormat
  -> FilePath
  -> String
  -> Word16
  -> Property
propFile format rootDir host port = ioProperty $ do
  -- Remove rootDir if needed.
  removePathForcibly rootDir
  -- Run cardano-tracer and demo-forwarder-mux.
  tracerThr <- forkIO $ runCardanoTracerWithConfig config
  threadDelay 500000
  forwarderThr <- forkIO $ launchForwardersSimple (host, port)
  -- Wait for some 'LogObject's...
  threadDelay 4000000
  -- Stop both sides.
  killThread forwarderThr
  killThread tracerThr
  threadDelay 100000
  -- Check that rootDir exists...
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
                      -- ... and one symlink...
                      filterM (isItSymLink format) logsAndSymLink >>= \case
                        [] -> false "subdir doesn't contain a symlink"
                        [symLink] -> do
                          -- ... to the latest *.log-file.
                          maybeLatestLog <- getSymbolicLinkTarget symLink
                          let latestLog = last $ sort logsWeNeed
                          return $ latestLog === maybeLatestLog
                        _ -> false "there is more than one symlink"
        _ -> false "root dir contains more than one subdir"
    False -> false "root dir doesn't exist"
 where
  config = TracerConfig
    { acceptAt       = RemoteSocket host (fromIntegral port)
    , loRequestNum   = 1
    , ekgRequestFreq = 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , logging        = [LoggingParams rootDir FileMode format]
    , rotation       = Nothing
    }

  false :: String -> IO Property
  false msg = return . counterexample msg $ property False
