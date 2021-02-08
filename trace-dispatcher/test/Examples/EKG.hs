module Examples.EKG where

import           Cardano.Logging
import           Control.Concurrent
import           Control.Monad (liftM)
import           System.Remote.Monitoring (forkServer)

testEKG :: IO ()
testEKG = do
    server <- forkServer "localhost" 8000
    tracer <- ekgTracer' server
    loop (appendName "ekg1" tracer) 1
  where
    loop tracer count = do
      if count == 1000
        then pure ()
        else do
          traceWith (appendName "count" tracer) count
          threadDelay 100000
          loop tracer (count + 1)
