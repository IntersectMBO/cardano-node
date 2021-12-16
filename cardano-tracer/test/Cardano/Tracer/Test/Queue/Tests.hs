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
propQueue rootDir localSock =
  -- withTempFile $ \tmpStdout ->
    -- Run the forwarder only. It imitates the case when the acceptor is
    -- misconfigured and cannot be launched, so the connection cannot be established.
    -- In this case, the forwarder should collect trace items in its internal
    -- "flexible queue" and periodically flush them to stdout.
    withAsyncBound (launchForwardersSimple Responder localSock connSize disconnSize) $ \_ -> do
      content <- getStdout rootDir
      let flushedTraceObjectsNum = T.count "TraceObject" content
      return $ flushedTraceObjectsNum === fromIntegral disconnSize

-- | Temporarily redirect stdout to file, get its content and redirect it back.
getStdout :: FilePath -> IO T.Text
getStdout dir = do
  (tmpPath, tmpHdl) <- openTempFile dir "cardano-tracer-tmp-stdout"
  stdDup <- hDuplicate stdout
  hDuplicateTo tmpHdl stdout
  hClose tmpHdl
  -- Wait till the queue will be redirected to stdout.
  sleep 6.5
  hDuplicateTo stdDup stdout
  hClose stdDup
  content <- TIO.readFile tmpPath
  removeFile tmpPath
  return content

connSize, disconnSize :: Word
connSize = 50
disconnSize = 100
