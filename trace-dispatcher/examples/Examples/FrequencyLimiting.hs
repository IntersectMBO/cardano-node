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
  t <- standardTracer
  tf <- humanFormatter True (Just "cardano") t
  tflimit <- humanFormatter True (Just "limiter") t
  tf2 <- limitFrequency 5 "5 messages per second" tflimit tf
  tf3 <- limitFrequency 15 "15 messages per second" tflimit tf
  configureTracers emptyTraceConfig [tflimit]
  configureTracers emptyTraceConfig [tf2, tf3]
  let tr = tf2 <> tf3

  repeated tr 1000 10000 -- 100 messages per second
  repeated tr 20 1000000 -- 1  message per second
  repeated tr 300 100000 -- 10  message per second
