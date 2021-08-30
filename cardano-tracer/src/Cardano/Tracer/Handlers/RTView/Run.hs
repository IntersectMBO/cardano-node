{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.RTView.Run
  ( runRTView
  ) where

import           Control.Concurrent.Async (withAsync, wait)
import           Control.Monad.Extra (whenJust)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.WebServer (runWebServer)
import           Cardano.Tracer.Types

runRTView
  :: TracerConfig
  -> AcceptedMetrics
  -> AcceptedNodeInfo
  -> IO ()
runRTView config@TracerConfig{hasRTView} acceptedMetrics acceptedNodeInfo =
  whenJust hasRTView $ \rtViewEndpoint -> do
    withAsync (runWebServer rtViewEndpoint acceptedMetrics acceptedNodeInfo) wait
