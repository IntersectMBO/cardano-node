{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Trace.Forward.Test.Demo
  ( tests
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO,
                                                 tryReadTBQueue, writeTBQueue)
import           Control.Monad (forM_, void)
import           Control.Monad.STM (STM, atomically)
import           Data.IORef (atomicModifyIORef', newIORef)
import           Data.Text (Text)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Cardano.BM.Data.LogItem (LogObject (..))

import           Trace.Forward.Acceptor (runTraceAcceptor)
import           Trace.Forward.Forwarder (runTraceForwarder)
import           Trace.Forward.Configuration (HowToConnect (..))
import           Trace.Forward.ReqResp (Request (..))

import           Trace.Forward.Test.MkConfig (mkAcceptorConfig, mkForwarderConfig)
import           Trace.Forward.Test.Types (Endpoint (..))

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.Demo"
  [ testProperty "LocalPipe IO" $ propDemoIO 10 Pipe
  , testProperty "Socket IO" $    propDemoIO 18 Socket
  ]

propDemoIO
  :: Int
  -> Endpoint
  -> NonEmptyList (LogObject Text)
  -> Property
propDemoIO maxLen Pipe logObjects = ioProperty $ do
  let pipePath =
#if defined(mingw32_HOST_OS)
        "\\\\.\\pipe\\trace_forward_test"
#else
        "/tmp/trace-forward-test.sock"
#endif
  propDemoIO' maxLen (LocalPipe pipePath) logObjects
propDemoIO maxLen Socket logObjects = ioProperty $
  propDemoIO' maxLen (RemoteSocket "127.0.0.1" 3010) logObjects

propDemoIO'
  :: Int
  -> HowToConnect
  -> NonEmptyList (LogObject Text)
  -> IO Property
propDemoIO' maxLen endpoint (NonEmpty logObjects') = do
  let logObjects = take maxLen logObjects' -- We don't want too big list here.
  forwarderQueue <- newTBQueueIO . fromIntegral . length $ logObjects
  acceptorQueue  <- newTBQueueIO . fromIntegral . length $ logObjects
  weAreDone <- newIORef False

  let acceptorConfig = mkAcceptorConfig endpoint weAreDone $ GetLogObjects 1
      forwarderConfig = mkForwarderConfig endpoint

  -- Run the forwarder.
  void . forkIO $ runTraceForwarder forwarderConfig forwarderQueue
  -- Fill forwarder's local queue.
  atomically $ forM_ logObjects (writeTBQueue forwarderQueue)

  threadDelay 1000000

  -- Run the acceptor.
  void . forkIO $ runTraceAcceptor acceptorConfig acceptorQueue
  waitTillAcceptorReceiveObjects
  atomicModifyIORef' weAreDone $ const (True, ())

  -- Take all 'LogObject's we received from the forwarder.
  acceptedObjects <- atomically $ toList acceptorQueue
  -- They must be equal.
  return $ logObjects === acceptedObjects

waitTillAcceptorReceiveObjects :: IO ()
waitTillAcceptorReceiveObjects = threadDelay 3000000

toList :: TBQueue (LogObject Text) -> STM [LogObject Text]
toList q =
  tryReadTBQueue q >>= \case
    Nothing -> return []
    Just lo -> do
      l <- toList q
      return $ lo : l

-- We need it for 'AcceptorConfiguration a' and 'ForwarderConfiguration a'
-- (in this example it is 'Text').
instance ShowProxy Text
