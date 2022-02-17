{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Test.Queue.Tests
  ( tests
  ) where

import           Control.Concurrent.Async (withAsyncBound)
import           GHC.IO.Handle (hDuplicate, hDuplicateTo)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.Directory (removeFile)
import           System.IO
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Test.Forwarder
import           Cardano.Tracer.Test.Utils

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.Queue"
  [ testProperty "check queue" $ propRunInLogsStructure propQueue
  ]

propQueue :: FilePath -> FilePath -> IO Property
propQueue rootDir localSock = do
  -- Temporarily switch stdout to a temp file.
  (tmpPath, tmpHdl) <- openTempFile rootDir "cardano-tracer-tmp-stdout"
  stdDup <- hDuplicate stdout
  hDuplicateTo tmpHdl stdout
  hClose tmpHdl
  -- Run the forwarder only. It imitates the case when the acceptor is
  -- misconfigured and cannot be launched, so the connection cannot be established.
  -- In this case, the forwarder should collect trace items in its internal
  -- "flexible queue" and periodically flush them to stdout.
  withAsyncBound (launchForwardersSimple Responder localSock connSize disconnSize) . const $
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
