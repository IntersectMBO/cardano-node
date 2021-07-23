{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Trace.Forward.Demo.Tests
  ( tests
  ) where

import           Control.Concurrent.Async (withAsync)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO, writeTBQueue)
import           Data.Functor ((<&>))
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import           GHC.Conc
import           System.Directory (getTemporaryDirectory)
#if defined(mingw32_HOST_OS)
import           System.FilePath ((</>), dropDrive)
import qualified Data.Text as T
#else
import           System.FilePath ((</>))
#endif
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime (..))
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.Time.Extra (sleep)

import           Ouroboros.Network.IOManager (withIOManager)

import           Trace.Forward.Acceptor
import           Trace.Forward.Configuration
import           Trace.Forward.Forwarder
import           Trace.Forward.Protocol.Type

import           Test.Trace.Forward.Demo.Configs
import           Test.Trace.Forward.Protocol.Codec ()
import           Test.Trace.Forward.Protocol.TraceItem

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Trace.Forward.Demo"
  [ testProperty "LocalPipe" $ prop_RemoteSocket 200
  ]

prop_RemoteSocket :: Int -> Property
prop_RemoteSocket n = ioProperty . withIOManager $ \iomgr -> do
  ep <- LocalPipe <$> mkLocalPipePath

  forwarderQueue <- newTBQueueIO $ fromIntegral n
  acceptedItems :: IORef [TraceItem] <- newIORef []
  weAreDone <- newTVarIO False

  itemsToForward <- generateNTraceItems n

  withAsync (runTraceAcceptor
               iomgr
               (mkAcceptorConfig ep weAreDone)
               (traceItemsHandler acceptedItems)
               nodeInfoHandler) $ \_ -> do
    sleep 0.5
    withAsync (runTraceForwarder
                 iomgr
                 (mkForwarderConfig ep (return nodeInfo))
                 forwarderQueue) $ \_ -> do
      atomically $ mapM_ (writeTBQueue forwarderQueue) itemsToForward
      -- Just wait till the acceptor will ask and receive all 'TraceItem's from the forwarder.
      waitForFinish acceptedItems n weAreDone

  -- Take accepted items and compare results.
  acceptedItems' <- readIORef acceptedItems
  return $ itemsToForward === acceptedItems'

traceItemsHandler :: IORef [TraceItem] -> [TraceItem] -> IO ()
traceItemsHandler acceptedItems' items = do
  atomicModifyIORef' acceptedItems' $ \storedItems -> (storedItems ++ items, ())

nodeInfoHandler :: NodeInfo -> IO ()
nodeInfoHandler _ni = return ()

generateNTraceItems :: Int -> IO [TraceItem]
generateNTraceItems n = generate (infiniteListOf arbitrary) <&> take n

waitForFinish :: IORef [TraceItem] -> Int -> TVar Bool -> IO ()
waitForFinish acceptedItems' n weAreDone' = do
  items' <- readIORef acceptedItems'
  if length items' < n
    then sleep 0.001 >> waitForFinish acceptedItems' n weAreDone'
    else atomically $ writeTVar weAreDone' True

nodeInfo :: NodeInfo
nodeInfo = NodeInfo
  { niName            = "core-3"
  , niProtocol        = "Shelley"
  , niVersion         = "1.28.0"
  , niCommit          = "cffa06c"
  , niStartTime       = UTCTime (fromGregorian 2021 7 24) ((22 * 3600) + (15 * 60) +  1)
  , niSystemStartTime = UTCTime (fromGregorian 2017 9 24) (( 1 * 3600) + (44 * 60) + 51)
  }

mkLocalPipePath :: IO FilePath
mkLocalPipePath = do
  tmpDir <- getTemporaryDirectory
#if defined(mingw32_HOST_OS)
  return $ "\\\\.\\pipe\\" <> (T.unpack . T.replace "\\" "-" . T.pack) (dropDrive tmpDir)
                           <> "_" </> "trace-forward-test"
#else
  return $ tmpDir </> "trace-forward-test.sock"
#endif
