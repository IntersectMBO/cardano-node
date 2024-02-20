{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Test.Queue.Tests
  ( tests
  ) where

import           Cardano.Tracer.Test.Forwarder
import           Cardano.Tracer.Test.TestSetup
import           Cardano.Tracer.Test.Utils

import           Control.Concurrent.Async (withAsyncBound)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           GHC.IO.Handle (hDuplicate, hDuplicateTo)
import           System.Directory (removeFile)
import           System.IO
import           System.Time.Extra (sleep)

import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestSetup Identity -> TestTree
tests ts = localOption (QuickCheckTests 1) $ testGroup "Test.Queue"
  [ testProperty "check queue" $ propRunInLogsStructure ts (propQueue ts)
  ]

propQueue :: TestSetup Identity -> FilePath -> FilePath -> IO Property
propQueue ts rootDir localSock = do
  -- Temporarily switch stdout to a temp file.
  (tmpPath, tmpHdl) <- openTempFile rootDir "cardano-tracer-tmp-stdout"
  putStrLn $ "Queue tmp file: " <> tmpPath
  -- Queue tmp file: ./tracer/extra-dir-50990537063901/cardano-tracer-tmp-stdout3063636-8
  stdDup <- hDuplicate stdout
  hDuplicateTo tmpHdl stdout
  hClose tmpHdl
  -- Run the forwarder only. It imitates the case when the acceptor is
  -- misconfigured and cannot be launched, so the connection cannot be established.
  -- In this case, the forwarder should collect trace items in its internal
  -- "flexible queue" and periodically flush them to stdout.
  withAsyncBound (launchForwardersSimple ts Responder localSock connSize disconnSize) . const $
    -- Wait till the queue will be redirected to stdout.
    sleep 7.0
  -- Return the normal stdout.
  hDuplicateTo stdDup stdout
  hClose stdDup
  -- Check what was flushed (if it was) to stdout.
  content <- TIO.readFile tmpPath
  removeFile tmpPath
  let flushedTraceObjectsNum = T.count "TraceObject" content
  return $ flushedTraceObjectsNum === fromIntegral disconnSize

connSize, disconnSize :: Word
connSize = 50
disconnSize = 100
