{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Tracer (contramap, stdoutTracer)
import           Data.Fixed (Pico)
import           Data.Text (pack)
import           Data.Time.Clock (secondsToNominalDiffTime)
import           System.Environment (getArgs)
import           System.Exit (die)
import qualified System.Metrics as EKG
import qualified System.Metrics.Counter as EKGC

import           System.Metrics.Forwarder (runEKGForwarder)
import           System.Metrics.Configuration (ForwarderConfiguration (..),
                                               HowToConnect (..), Port)

main :: IO ()
main = do
  -- Prepare the forwarder's configuration.
  (howToConnect, freq) <- getArgs >>= \case
    [path, freq]       -> return (LocalPipe path, freq)
    [host, port, freq] -> return (RemoteSocket (pack host) (read port :: Port), freq)
    _                  -> die "Usage: demo-forwarder (pathToLocalPipe | host port) freqInSecs"
  let config =
        ForwarderConfiguration
          { forwarderTracer    = contramap show stdoutTracer
          , acceptorEndpoint   = howToConnect
          , reConnectFrequency = secondsToNominalDiffTime (read freq :: Pico)
          , actionOnRequest    = \_ -> return ()
          , useDummyForwarder  = False
          }

  -- Create an empty EKG store and register predefined GC metrics in it.
  store <- EKG.newStore
  _ <- forkIO $ delayedRegisterMetrics store

  -- Run the forwarder. It will establish the connection with the acceptor,
  -- then the acceptor will periodically ask for the metrics, the forwarder
  -- will take them from the 'store' and send them back.
  runEKGForwarder config store

delayedRegisterMetrics :: EKG.Store -> IO ()
delayedRegisterMetrics store = do
  threadDelay 3000000
  cnt <- EKG.createCounter "counter.1" store
  threadDelay 3000000
  EKGC.inc cnt 
  threadDelay 3000000
  EKGC.inc cnt 
  threadDelay 3000000
  -- EKG.registerGcMetrics store
