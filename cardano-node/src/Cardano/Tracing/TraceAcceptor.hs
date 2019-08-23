module Cardano.Tracing.TraceAcceptor (
      handleTraceAcceptor
    ) where

import           Cardano.Prelude
import           Prelude (String)

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
import           Control.Tracer

{-------------------------------------------------------------------------------
  Main logic
-------------------------------------------------------------------------------}

handleTraceAcceptor :: Tracer IO String
                    -> IO ()
handleTraceAcceptor tracer = do

    traceWith tracer $ "**************************************"
    traceWith tracer $ "I am TraceAcceptor"
    traceWith tracer $ "**************************************"

    forever $ threadDelay 10000000 -- sleep 10s
