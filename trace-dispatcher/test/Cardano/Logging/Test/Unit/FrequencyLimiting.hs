module Cardano.Logging.Test.Unit.FrequencyLimiting (
    testLimiting
  , testLimitingResult
) where

import           Cardano.Logging
import           Cardano.Logging.Test.Tracer
import           Cardano.Logging.Test.Unit.TestObjects

import           Control.Concurrent
import           Data.IORef
import           Data.Text (Text)

repeated :: Trace IO (TraceForgeEvent LogBlock) -> Int -> Int -> IO ()
repeated _ 0 _ = pure ()
repeated t n d = do
  traceWith t (TraceStartLeadershipCheck (SlotNo (fromIntegral n)))
  threadDelay d
  repeated t (n-1) d

testLimiting :: IO [Text]
testLimiting = do
  testTracerRef <- newIORef []
  simpleTracer <- testTracer testTracerRef
  tf <- machineFormatter Nothing simpleTracer
  tflimit <- machineFormatter (Just "limiter") simpleTracer
  tf2 <- limitFrequency 5 "5 messages per second" tflimit tf
  tf3 <- limitFrequency 15 "15 messages per second" tflimit tf
  confState <- emptyConfigReflection
  configureTracers confState emptyTraceConfig [tflimit]
  configureTracers confState emptyTraceConfig [tf2, tf3]
  let tr = tf2 <> tf3

  repeated tr 1000 1000 -- 1000 messages per second
  repeated tr 20 100000 -- 10  message per second
  repeated tr 300 10000 -- 100  message per second

  msgs <- reverse <$> readIORef testTracerRef
  let res = map formattedMsgAsText msgs
  pure res

testLimitingResult :: [Text]
testLimitingResult = []
