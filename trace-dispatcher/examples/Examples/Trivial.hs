{-# LANGUAGE ScopedTypeVariables #-}
module Examples.Trivial (
    test1
  , test2
) where


import           Cardano.Logging
import           Examples.TestObjects


-- | Make sure the function append name is only called once
--   for every path element
test1 :: IO ()
test1 = do
    stdoutTracer' <- standardTracer
    simpleTracer <- machineFormatter (Just "cardano") stdoutTracer'
    confState <- emptyConfigReflection
    configureTracers confState emptyTraceConfig [simpleTracer]
    let simpleTracer1 = filterTraceBySeverity
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

test2 :: IO ()
test2 = do
    stdoutTracer' <- standardTracer
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
