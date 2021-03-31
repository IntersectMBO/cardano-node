{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, writeTBQueue)
import           Control.Monad (forever)
import           Control.Monad.STM (atomically)
import           Control.Tracer (contramap, stdoutTracer)
import           Data.Fixed (Pico)
import           Data.Text (Text, pack)
import           Data.Time.Clock (secondsToNominalDiffTime)
import           System.Environment (getArgs)
import           System.Exit (die)

import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Cardano.BM.Data.LogItem (LogObject (..), LOContent (..), LOMeta (..),
                                          PrivacyAnnotation (..), mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))

import           Trace.Forward.Forwarder (runTraceForwarder)
import           Trace.Forward.Configuration (ForwarderConfiguration (..),
                                              HowToConnect (..), Port)

main :: IO ()
main = do
  -- Prepare the forwarder's configuration.
  (howToConnect, freq) <- getArgs >>= \case
    [path, freq]       -> return (LocalPipe path, freq)
    [host, port, freq] -> return (RemoteSocket (pack host) (read port :: Port), freq)
    _                  -> die "Usage: demo-forwarder (pathToLocalPipe | host port) freqInSecs"
  let config :: ForwarderConfiguration Text
      config =
        ForwarderConfiguration
          { forwarderTracer    = contramap show stdoutTracer
          , acceptorEndpoint   = howToConnect
          , reConnectFrequency = secondsToNominalDiffTime (read freq :: Pico)
          , actionOnRequest    = \_ -> return ()
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

-- We need it for 'ForwarderConfiguration a' (in this example it is 'Text').
instance ShowProxy Text

loWriter :: TBQueue (LogObject Text) -> IO ()
loWriter queue = forever $ do
  meta <- mkLOMeta Info Public
  atomically $ writeTBQueue queue (lo meta)
  threadDelay 500000
 where
  lo :: LOMeta -> LogObject Text
  lo meta = LogObject "demo.forwarder.LO.1" meta $ LogMessage "demo.forwarder.LogMessage.1"
