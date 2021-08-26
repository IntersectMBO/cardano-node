{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Metrics.Monitoring
  ( runMonitoringServer
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Exception (SomeException, try)
import           Control.Monad (forever, unless)
import "contra-tracer" Control.Tracer (showTracing, stdoutTracer, traceWith)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import           System.Remote.Monitoring (forkServerWith)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Types (AcceptedMetrics)

runMonitoringServer
  :: Endpoint
  -> AcceptedMetrics
  -> IO ()
runMonitoringServer (Endpoint host port) acceptedMetrics = forever $ do
  try serveEKGPage >>= \case
    Left (e :: SomeException) ->
      logTrace $ "cardano-tracer, problem with EKG web server: " <> show e
    Right _ ->
      return ()
  threadDelay 1000000
 where
  serveEKGPage = do
    metrics <- readTVarIO acceptedMetrics
    unless (HM.null metrics) $ do
      -- TODO: temporary solution for testing
      -- (serve the metrics received from the first found node only).
      let (storeForFirstNode, _) = snd . head . HM.toList $ metrics
      _server <- forkServerWith storeForFirstNode (BSC.pack host) port
      waitForever

  waitForever = forever $ threadDelay 1000000000

  logTrace = traceWith $ showTracing stdoutTracer
