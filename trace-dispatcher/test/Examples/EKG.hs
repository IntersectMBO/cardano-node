module Examples.EKG where

import           Cardano.Logging
import           Control.Concurrent
import           Control.Monad (liftM)
import           Data.Text (pack)
import           System.Remote.Monitoring (forkServer)

instance LogFormatting Int where
  asMetrics i = [IntM Nothing (fromIntegral i)]

countDocumented :: Documented Int
countDocumented = Documented [(0,"count")]

testEKG :: IO ()
testEKG = do
    server <- forkServer "localhost" 8000
    tracer <- ekgTracer (Right server)
    configureTracers emptyTraceConfig countDocumented [tracer]
    loop (appendName "ekg1" tracer) 1
  where
    loop :: Trace IO Int -> Int -> IO ()
    loop tracer count = do
      if count == 1000
        then pure ()
        else do
          traceWith (appendName "count" tracer) count
          threadDelay 100000
          loop tracer (count + 1)
