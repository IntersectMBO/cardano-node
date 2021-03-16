{-# LANGUAGE FlexibleContexts #-}

module Examples.Routing (
  testRouting
) where

import           Cardano.Logging
import           Examples.TestObjects

routingTracer1 :: (Monad m)
  => Trace m (TraceForgeEvent LogBlock)
  -> Trace m (TraceForgeEvent LogBlock)
  -> Trace m (TraceForgeEvent LogBlock)
routingTracer1 t1 t2 = routingTrace routingf (t1 <> t2)
  where
    routingf TraceStartLeadershipCheck {} = t1
    routingf _                            = t2

routingTracer2 :: (Monad m)
  => Trace m (TraceForgeEvent LogBlock)
  -> Trace m (TraceForgeEvent LogBlock)
  -> Trace m (TraceForgeEvent LogBlock)
routingTracer2 t1 t2 = t1 <> t2

testRouting :: IO ()
testRouting = do
    t <- standardTracer "stdout"
    tf <- machineFormatter DRegular "cardano" t
    let t1 = appendName "tracer1" tf
    let t2 = appendName "tracer1" tf
    configureTracers emptyTraceConfig traceForgeEventDocu [t1, t2]
    traceWith (routingTracer1 t1 t2) message1
    traceWith (routingTracer2 t1 t2) message2
    traceWith (t1 <> t2) message3
