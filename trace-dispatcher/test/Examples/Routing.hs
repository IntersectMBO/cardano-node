{-# LANGUAGE FlexibleContexts #-}

module Examples.Routing (
  testRouting
) where

import           Control.Monad.IO.Class

import           Cardano.Logging
import           Examples.TestObjects

tracer1 :: (LogFormatting (TraceForgeEvent blk), MonadIO m) =>
  m (Trace m (TraceForgeEvent blk))
tracer1  = fmap (appendName "tracer1") (standardHumanTracer "t1" Nothing)

tracer2 :: (LogFormatting (TraceForgeEvent blk), MonadIO m) =>
  m (Trace m (TraceForgeEvent blk))
tracer2  = fmap (appendName "tracer2") (standardMachineTracer "t2" Nothing)

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
    t1 <- tracer1
    t2 <- tracer2
    configureTracers emptyTraceConfig traceForgeEventDocu [t1, t2]
    traceWith (routingTracer1 t1 t2) message1
    traceWith (routingTracer2 t1 t2) message2
    traceWith (t1 <> t2) message3
