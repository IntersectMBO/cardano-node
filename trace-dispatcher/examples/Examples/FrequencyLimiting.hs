module Examples.FrequencyLimiting (
  testLimiting
) where

import           Control.Concurrent

import           Cardano.Logging
import           Examples.TestObjects

repeated :: Trace IO (TraceForgeEvent LogBlock) -> Int -> Int -> IO ()
repeated _ 0 _ = pure ()
repeated t n d = do
  traceWith t (TraceStartLeadershipCheck (SlotNo (fromIntegral n)))
  threadDelay d
  repeated t (n-1) d

testLimiting :: IO ()
testLimiting = do
  t1 <- standardTracer
  tf1 <- humanFormatter True (Just "cardano") t1
  tf2 <- limitFrequency 5 "5 messages per second"
            (appendName "tracer1" (contramap Message tf1))
               (appendName "limiter1" (contramap Limit tf1))
  tf3 <- limitFrequency 15 "15 messages per second"
            (appendName "tracer2"  (contramap Message tf1))
               (appendName "limiter2" (contramap Limit tf1))
  let t = tf2 <> tf3
  configureTracers emptyTraceConfig traceForgeEventDocu [t]

  repeated t 1000 10000 -- 100 messages per second
  repeated t 20 1000000 -- 1  message per second
  repeated t 300 100000 -- 10  message per second
