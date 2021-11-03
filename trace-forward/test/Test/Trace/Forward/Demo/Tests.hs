{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Trace.Forward.Demo.Tests
  ( tests
  ) where

import           Control.Concurrent.Async (withAsync)
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
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.Time.Extra (sleep)

import           Ouroboros.Network.IOManager (withIOManager)

import           Trace.Forward.Acceptor
import           Trace.Forward.Configuration
import           Trace.Forward.Forwarder
import           Trace.Forward.Utils

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

  acceptedItems :: IORef [TraceItem] <- newIORef []
  weAreDone <- newTVarIO False
  let forwarderConfig = mkForwarderConfig ep (fromIntegral n) (fromIntegral n)
  sink <- initForwardSink forwarderConfig

  itemsToForward <- generateNTraceItems n

  withAsync (runTraceAcceptor
               iomgr
               (mkAcceptorConfig ep weAreDone)
               (traceItemsHandler acceptedItems)) $ \_ -> do
    sleep 0.5
    withAsync (runTraceForwarder iomgr forwarderConfig sink) $ \_ -> do
      mapM_ (writeToSink sink) itemsToForward
      -- Just wait till the acceptor will ask and receive all 'TraceItem's from the forwarder.
      waitForFinish acceptedItems n weAreDone

  -- Take accepted items and compare results.
  acceptedItems' <- readIORef acceptedItems
  return $ itemsToForward === acceptedItems'

traceItemsHandler :: IORef [TraceItem] -> [TraceItem] -> IO ()
traceItemsHandler acceptedItems' items = do
  atomicModifyIORef' acceptedItems' $ \storedItems -> (storedItems ++ items, ())

generateNTraceItems :: Int -> IO [TraceItem]
generateNTraceItems n = generate (infiniteListOf arbitrary) <&> take n

waitForFinish :: IORef [TraceItem] -> Int -> TVar Bool -> IO ()
waitForFinish acceptedItems' n weAreDone' = do
  items' <- readIORef acceptedItems'
  if length items' < n
    then sleep 0.001 >> waitForFinish acceptedItems' n weAreDone'
    else atomically $ writeTVar weAreDone' True

mkLocalPipePath :: IO FilePath
mkLocalPipePath = do
  tmpDir <- getTemporaryDirectory
#if defined(mingw32_HOST_OS)
  return $ "\\\\.\\pipe\\" <> (T.unpack . T.replace "\\" "-" . T.pack) (dropDrive tmpDir)
                           <> "_" </> "trace-forward-test"
#else
  return $ tmpDir </> "trace-forward-test.sock"
#endif
