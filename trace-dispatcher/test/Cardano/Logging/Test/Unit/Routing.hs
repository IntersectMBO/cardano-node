{-# LANGUAGE FlexibleContexts #-}

module Cardano.Logging.Test.Unit.Routing (
  testRouting
, testRoutingResult
) where

import           Cardano.Logging
import           Cardano.Logging.Test.Tracer
import           Cardano.Logging.Test.Unit.TestObjects

import           Data.IORef
import           Data.Text (Text)


routingTracer1 :: (Monad m)
  => Trace m (TraceForgeEvent LogBlock)
  -> Trace m (TraceForgeEvent LogBlock)
  -> Trace m (TraceForgeEvent LogBlock)
routingTracer1 t1 t2 = routingTrace routingf (t1 <> t2)
  where
    routingf TraceStartLeadershipCheck {} = pure t1
    routingf _                            = pure t2

routingTracer2 :: (Monad m)
  => Trace m (TraceForgeEvent LogBlock)
  -> Trace m (TraceForgeEvent LogBlock)
  -> Trace m (TraceForgeEvent LogBlock)
routingTracer2 t1 t2 = t1 <> t2

testRouting :: IO [Text]
testRouting = do
    testTracerRef <- newIORef []
    simpleTracer <- testTracer testTracerRef
    tf <- machineFormatter Nothing simpleTracer
    let t1 = appendPrefixName "tracer1" tf
    let t2 = appendPrefixName "tracer2" tf
    confState <- emptyConfigReflection
    configureTracers confState emptyTraceConfig [t1, t2]
    let r1 = routingTracer1 t1 t2
        r2 = routingTracer2 t1 t2
    traceWith r1 message1
    traceWith r1 message2
    traceWith r2 message3

    msgs <- reverse <$> readIORef testTracerRef
    let res = map formattedMsgAsText msgs
    pure res

testRoutingResult :: [Text]
testRoutingResult = [
    "{\"at\":\"2023-11-23T16:03:38.710828111Z\",\"ns\":\"tracer1\",\"data\":{\"kind\":\"TraceStartLeadershipCheck\",\"slot\":1001},\"sev\":\"Info\",\"thread\":\"1484\",\"host\":\"deusXmachina\"}"
    ,"{\"at\":\"2023-11-23T16:03:38.710830144Z\",\"ns\":\"tracer2\",\"data\":{\"kind\":\"TraceSlotIsImmutable\",\"slot\":3333,\"tip\":\"Origin\",\"tipBlockNo\":1},\"sev\":\"Info\",\"thread\":\"1484\",\"host\":\"deusXmachina\"}"
    ,"{\"at\":\"2023-11-23T16:03:38.710834117Z\",\"ns\":\"tracer1\",\"data\":{\"current slot\":4400,\"kind\":\"TraceBlockFromFuture\",\"tip\":300},\"sev\":\"Info\",\"thread\":\"1484\",\"host\":\"deusXmachina\"}"
    ,"{\"at\":\"2023-11-23T16:03:38.710835429Z\",\"ns\":\"tracer2\",\"data\":{\"current slot\":4400,\"kind\":\"TraceBlockFromFuture\",\"tip\":300},\"sev\":\"Info\",\"thread\":\"1484\",\"host\":\"deusXmachina\"}"
    ]
