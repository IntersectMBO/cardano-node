{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, writeTBQueue)
import           Control.Monad (forever)
import           Control.Monad.STM (atomically)
import           Control.Tracer (contramap, stdoutTracer)
import           Data.Text (Text, pack)
import           System.Environment (getArgs)
import           System.Exit (die)

import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Cardano.BM.Data.LogItem (LogObject (..), LOContent (..), LOMeta (..),
                                          PrivacyAnnotation (..), mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))

import           Trace.Forward.Forwarder (runTraceForwarder)
import           Trace.Forward.Configuration (ForwarderConfiguration (..),
                                              HowToConnect (..), Port)
import           Trace.Forward.LogObject ()

main :: IO ()
main = do
  -- Prepare the forwarder's configuration.
  howToConnect <- getArgs >>= \case
    [path]       -> return $ LocalPipe path
    [host, port] -> return $ RemoteSocket (pack host) (read port :: Port)
    _            -> die "Usage: demo-forwarder (pathToLocalPipe | host port)"
  let config :: ForwarderConfiguration (LogObject Text)
      config =
        ForwarderConfiguration
          { forwarderTracer  = contramap show stdoutTracer
          , acceptorEndpoint = howToConnect
          , nodeBasicInfo    = return [("NodeName", "node-1")]
          , actionOnRequest  = print -- const (return ())
          }

  -- Create a queue for 'LogObject's: when the acceptor will ask for N 'LogObject's
  -- they will be taken from this queue.
  queue <- newTBQueueIO 1000

  -- This thread will write 'LogObject's to the queue.
  _ <- async $ loWriter queue

  -- Run the forwarder. It will establish the connection with the acceptor,
  -- then the acceptor will periodically ask for 'LogObject's, the forwarder
  -- will take them from the 'queue' and send them back.
  runTraceForwarder config queue

-- We need it for 'ForwarderConfiguration lo' (in this example it is 'LogObject Text').
instance ShowProxy (LogObject Text)

loWriter :: TBQueue (LogObject Text) -> IO ()
loWriter queue = forever $ do
  meta <- mkLOMeta Info Public
  atomically $ writeTBQueue queue (lo meta)
  threadDelay 500000
 where
  lo :: LOMeta -> LogObject Text
  lo meta = LogObject "demo.forwarder.LO.1" meta $ LogMessage "demo.forwarder.LogMessage.1"
