{-# LANGUAGE NamedFieldPuns #-}

-- | This top-level module is used by 'cardano-tracer' app.
module Cardano.Tracer.Run
  ( runCardanoTracer
  -- | For testing purposes.
  , runCardanoTracerWithConfig
  , runCardanoTracerWithConfigBrakes
  ) where

import           Control.Concurrent.Async (withAsync, wait)
import           Control.Concurrent.STM.TVar (TVar)
import           Control.Monad (void)
import           Data.List.NonEmpty (NonEmpty)

import           Cardano.Tracer.Acceptors (runAcceptors, runAcceptorsWithBrakes)
import           Cardano.Tracer.CLI (TracerParams (..))
import           Cardano.Tracer.Configuration (TracerConfig, readTracerConfig)
import           Cardano.Tracer.Handlers.Logs.Rotator (runLogsRotator)
import           Cardano.Tracer.Handlers.Metrics.Run (runMetricsHandler)
import           Cardano.Tracer.Types

runCardanoTracer :: TracerParams -> IO ()
runCardanoTracer TracerParams{tracerConfig} =
  readTracerConfig tracerConfig >>= runCardanoTracerWithConfig

runCardanoTracerWithConfig
  :: TracerConfig
  -> IO ()
runCardanoTracerWithConfig config = do
  acceptedMetrics  <- initAcceptedMetrics
  acceptedNodeInfo <- initAcceptedNodeInfo
  run3ActionsInParallel
    (runLogsRotator config)
    (runMetricsHandler config acceptedMetrics acceptedNodeInfo)
    (runAcceptors config acceptedMetrics acceptedNodeInfo)

runCardanoTracerWithConfigBrakes
  :: TracerConfig
  -> NonEmpty (TVar Bool, TVar Bool)
  -> IO ()
runCardanoTracerWithConfigBrakes config protocolsBrakes = do
  acceptedMetrics  <- initAcceptedMetrics
  acceptedNodeInfo <- initAcceptedNodeInfo
  run3ActionsInParallel
    (runLogsRotator config)
    (runMetricsHandler config acceptedMetrics acceptedNodeInfo)
    (runAcceptorsWithBrakes config acceptedMetrics acceptedNodeInfo protocolsBrakes)

run3ActionsInParallel
  :: IO ()
  -> IO ()
  -> IO ()
  -> IO ()
run3ActionsInParallel action1 action2 action3 =
  withAsync action1 $ \a1 ->
    withAsync action2 $ \a2 ->
      withAsync action3 $ \a3 -> do
        void $ wait a1
        void $ wait a2
        void $ wait a3
