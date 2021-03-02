{-# LANGUAGE FlexibleContexts #-}
module Examples.Routing where

import           Control.Monad (liftM)
import           Control.Monad.IO.Class
import           Katip
import           Katip.Scribes.Handle (ioLogEnv)

import           Cardano.Logging
import           Examples.TestObjects

tracer1 :: (LogFormatting (TraceForgeEvent blk), MonadIO m) =>
  m (Trace m (TraceForgeEvent blk))
tracer1  = fmap (appendName "tracer1") stdoutObjectKatipTracer

tracer2 :: (LogFormatting (TraceForgeEvent blk), MonadIO m) =>
  m (Trace m (TraceForgeEvent blk))
tracer2  = fmap (appendName "tracer2") stdoutJsonKatipTracer

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
    traceWith (routingTracer1 t1 t2) message1
    traceWith (routingTracer1 t1 t2) message2
    traceWith (t1 <> t2) message3
