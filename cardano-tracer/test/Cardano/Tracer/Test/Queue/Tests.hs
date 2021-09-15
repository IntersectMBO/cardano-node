{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Test.Queue.Tests
  ( tests
  ) where

import           Control.Concurrent.Async (withAsyncBound)
import           GHC.IO.Handle (hDuplicateTo)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.IO
import           System.IO.Extra
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Test.Forwarder
import           Cardano.Tracer.Test.Utils

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.Queue"
  [ testProperty "check queue" $ propRunInLogsStructure propQueue
  ]

propQueue :: FilePath -> FilePath -> IO Property
propQueue _rootDir localSock =
  withTempFile $ \tmpStdout ->
    -- Run the forwarder only. It imitates the case when the acceptor is
    -- misconfigured and the connection cannot be established at all.
    -- In this case, the forwarder should collect trace items in its internal
    -- "flexible queue" and periodically flush them to stdout.
    withAsyncBound (launchForwardersSimple localSock connSize disconnSize) $ \_ -> do
      redirectStdoutToFile tmpStdout
      (True ===) <$> stdoutAnalyzer tmpStdout

redirectStdoutToFile :: FilePath -> IO ()
redirectStdoutToFile f = do
  fh <- openFile f ReadWriteMode
  hDuplicateTo fh stdout

stdoutAnalyzer :: FilePath -> IO Bool
stdoutAnalyzer f = do
  sleep 10.0
  -- Now the file 'f' should already contain flushed tracing items, so analyze them.
  content <- TIO.readFile f
  -- Check that all 'disconnSize' items are in the file.
  let traceObjectsNum = T.count "TraceObject" content
  return $ traceObjectsNum == fromIntegral disconnSize

connSize, disconnSize :: Word
connSize = 50
disconnSize = 100
