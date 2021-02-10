module Examples.Routing where

import           Control.Monad (liftM)
import           Control.Monad.IO.Class
import           Katip
import           Katip.Scribes.Handle (ioLogEnv)

import           Cardano.Logging
import           Examples.TestObjects

tracer1 :: MonadIO m => m (Trace m LO)
tracer1  = liftM (appendName "tracer1") stdoutObjectKatipTracer

tracer2 :: MonadIO m => m (Trace m LO)
tracer2  = liftM (appendName "tracer2") stdoutJsonKatipTracer

routingTracer1 :: (Monad m)
  => Trace m LO
  -> Trace m LO
  -> Trace m LO
routingTracer1 t1 t2 = routingTrace routingf (t1 <> t2)
  where
    routingf LO1 {} = t1
    routingf LO2 {} = t2

routingTracer2 :: (Monad m)
  => Trace m LO
  -> Trace m LO
  -> Trace m LO
routingTracer2 t1 t2 = t1 <> t2

testRouting :: IO ()
testRouting = do
    t1 <- tracer1
    t2 <- tracer2
    traceWith (routingTracer1 t1 t2) logObject1
    traceWith (routingTracer1 t1 t2) logObject2
    traceWith (t1 <> t2) logObject3
