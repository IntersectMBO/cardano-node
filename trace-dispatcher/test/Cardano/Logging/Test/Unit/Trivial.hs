{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Logging.Test.Unit.Trivial (
    test1
  , test1Res
  , test2
  , test2Res
) where


import           Cardano.Logging
import           Cardano.Logging.Test.Tracer
import           Cardano.Logging.Test.Unit.TestObjects

import           Data.IORef
import           Data.Text (Text)


-- | Make sure the function append name is only called once
--   for every path element
test1 :: IO [Text]
test1 = do
    testTracerRef <- newIORef []
    testTracer' <- testTracer testTracerRef
    simpleTracer <- machineFormatter Nothing testTracer'
    confState <- emptyConfigReflection
    configureTracers confState emptyTraceConfig [simpleTracer]
    let simpleTracer1  = filterTraceBySeverity
                           (Just (SeverityF (Just Warning)))
                           simpleTracer
    let simpleTracerC1 = appendInnerName "Outer1" simpleTracer1
    let simpleTracerC2 = appendInnerName "Inner1" simpleTracerC1
    let simpleTracerC3 = setSeverity Error
                           $ setPrivacy Confidential
                             $ appendInnerName "Inner2" simpleTracerC1
    traceWith (setSeverity Error simpleTracerC2) message1
    traceWith (setSeverity Warning simpleTracerC3) message2
    traceWith simpleTracerC2 message3
    traceWith (setSeverity Critical (appendInnerName "Inner3" simpleTracerC3)) message4
    msgs <- reverse <$> readIORef testTracerRef
    let res = map formattedMsgAsText msgs
--    print res
    pure res

test1Res :: [Text]
test1Res = [
     "{\"at\":\"2023-11-28T13:35:37.465134879Z\",\"ns\":\"Outer1.Inner1\",\"data\":{\"kind\":\"TraceStartLeadershipCheck\",\"slot\":1001},\"sev\":\"Error\",\"thread\":\"3\",\"host\":\"deusXmachina\"}"
    ,"{\"at\":\"2023-11-28T13:35:37.465138573Z\",\"ns\":\"Outer1.Inner2\",\"data\":{\"kind\":\"TraceSlotIsImmutable\",\"slot\":3333,\"tip\":\"Origin\",\"tipBlockNo\":1},\"sev\":\"Warning\",\"thread\":\"3\",\"host\":\"deusXmachina\"}"
    ,"{\"at\":\"2023-11-28T13:35:37.465139932Z\",\"ns\":\"Outer1.Inner1\",\"data\":{\"current slot\":4400,\"kind\":\"TraceBlockFromFuture\",\"tip\":300},\"sev\":\"Info\",\"thread\":\"3\",\"host\":\"deusXmachina\"}"
    ,"{\"at\":\"2023-11-28T13:35:37.465140622Z\",\"ns\":\"Outer1.Inner2.Inner3\",\"data\":{\"kind\":\"TraceStartLeadershipCheck\",\"slot\":2002},\"sev\":\"Critical\",\"thread\":\"3\",\"host\":\"deusXmachina\"}"
  ]

test2 :: IO [Text]
test2 = do
    stdoutTracerRef <- newIORef []
    stdoutTracer' <- testTracer stdoutTracerRef
    simpleTracer <- machineFormatter Nothing stdoutTracer'
    confState <- emptyConfigReflection
    configureTracers confState emptyTraceConfig [simpleTracer]
    let simpleTracer1  = filterTraceBySeverity
                              (Just (SeverityF (Just Warning)))
                              (withSeverity simpleTracer)
    let simpleTracerC1 = appendInnerName "Outer1" simpleTracer1
    let simpleTracerC2 = appendInnerName "Inner1" simpleTracerC1
    let simpleTracerC3 = setPrivacy Confidential $ appendInnerName "Inner2" simpleTracerC1
    traceWith simpleTracerC2 message1
    traceWith (setSeverity Critical simpleTracerC3) message2
    traceWith simpleTracerC2 message3
    traceWith (appendInnerName "Inner3" simpleTracerC3) message4
    traceWith (appendInnerName "cont1" $ appendInnerName "cont2" $ appendInnerName "cont3" simpleTracerC2) message1
    msgs <- reverse <$> readIORef stdoutTracerRef
    let res = map formattedMsgAsText msgs
    pure res

test2Res :: [Text]
test2Res = [
            "{\"at\":\"2023-11-23T14:07:26.112085435Z\",\"ns\":\"Outer1.Inner1\",\"data\":{\"kind\":\"TraceStartLeadershipCheck\",\"slot\":1001},\"sev\":\"Info\",\"thread\":\"460\",\"host\":\"deusXmachina\"}"
           ,"{\"at\":\"2023-11-23T14:07:26.112096216Z\",\"ns\":\"Outer1.Inner2\",\"data\":{\"kind\":\"TraceSlotIsImmutable\",\"slot\":3333,\"tip\":\"Origin\",\"tipBlockNo\":1},\"sev\":\"Critical\",\"thread\":\"460\",\"host\":\"deusXmachina\"}"
           ,"{\"at\":\"2023-11-23T14:07:26.112101607Z\",\"ns\":\"Outer1.Inner1\",\"data\":{\"current slot\":4400,\"kind\":\"TraceBlockFromFuture\",\"tip\":300},\"sev\":\"Info\",\"thread\":\"460\",\"host\":\"deusXmachina\"}"
           ,"{\"at\":\"2023-11-23T14:07:26.112107139Z\",\"ns\":\"Outer1.Inner2.Inner3\",\"data\":{\"kind\":\"TraceStartLeadershipCheck\",\"slot\":2002},\"sev\":\"Info\",\"thread\":\"460\",\"host\":\"deusXmachina\"}"
           ,"{\"at\":\"2023-11-23T14:07:26.112114044Z\",\"ns\":\"Outer1.Inner1.cont3.cont2.cont1\",\"data\":{\"kind\":\"TraceStartLeadershipCheck\",\"slot\":1001},\"sev\":\"Info\",\"thread\":\"460\",\"host\":\"deusXmachina\"}"
           ]
