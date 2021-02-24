{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module Examples.FrequencyLimiting where

import           Control.Concurrent
import           Control.Monad (liftM)
import           Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           GHC.Generics

import           Cardano.Logging
import           Examples.TestObjects

data LOX = LOS LO | LOL LimitingMessage
 deriving (Logging, Generic, A.ToJSON)

tracer1 :: MonadIO m => m (Trace m LO)
tracer1  = do
  t1      <- fmap (appendName "tracer1") stdoutObjectKatipTracer
  limitFrequency 30 "one every 2 seconds" (cmap LOS t1) (cmap LOL t1)

tracer2 :: MonadIO m => m (Trace m LO)
tracer2  = do
  t2      <- fmap (appendName "tracer2") stdoutJsonKatipTracer
  limitFrequency 15 "one every four seconds" (cmap LOS t2) (cmap LOL t2)

repeated :: Trace IO LO -> Int -> Int -> IO ()
repeated _ 0 _ = pure ()
repeated t n d = do
  traceWith t (LO1 n)
  threadDelay d
  repeated t (n-1) d

testLimiting :: IO ()
testLimiting = do
  t1 <- tracer1
  t2 <- tracer2
  let t = t1 <> t2
  repeated t 1000 10000 --wait 100 per second
  repeated t 100 100000 --wait 10 per second
  repeated t 30  200000 -- wait 5 per second
