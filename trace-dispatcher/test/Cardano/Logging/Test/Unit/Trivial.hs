{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Logging.Test.Unit.Trivial (
    test1
  , test2
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
    simpleTracer <- machineFormatter (Just "cardano") testTracer'
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
    msgs <- readIORef testTracerRef
    pure $ map formattedMsgAsText msgs

test2 :: IO ()
test2 = do
    stdoutTracerRef <- newIORef []
    stdoutTracer' <- testTracer stdoutTracerRef
    simpleTracer <- humanFormatter True (Just "cardano") stdoutTracer'
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
