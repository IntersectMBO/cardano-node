{-# LANGUAGE RecordWildCards #-}

-- | This top-level module will be used by the 'cardano-tracer' app.
module Cardano.Tracer.Run
  ( runCardanoTracer
  ) where

import           Control.Concurrent.Async (concurrently_)

import           Cardano.Tracer.Acceptors (runAcceptors)
import           Cardano.Tracer.CLI (TracerParams (..))
import           Cardano.Tracer.Configuration (readTracerConfig)
import           Cardano.Tracer.Handlers (runHandlers)
import           Cardano.Tracer.Types (initAcceptedItems)

runCardanoTracer
  :: TracerParams
  -> IO ()
runCardanoTracer TracerParams{..} = do
  config <- readTracerConfig tracerConfig
  acceptedItems <- initAcceptedItems
  -- Run two main threads:
  -- 1. For all acceptors: they ask 'LogObject's and metrics from the node
  --    and collect them in 'acceptedItems'.
  -- 2. For all handlers: they take items from 'acceptedItems' and do something
  --    with them (write to log files and return by web-request via EKG API).
  concurrently_ (runAcceptors config acceptedItems)
                (runHandlers  config acceptedItems)
