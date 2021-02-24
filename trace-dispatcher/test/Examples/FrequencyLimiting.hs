{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module Examples.FrequencyLimiting where

import           Control.Concurrent
import           Control.Monad (liftM)
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           GHC.Generics

import           Cardano.Logging
import           Examples.TestObjects

data LOX = LOS LO | LOL LimitingMessage
 deriving (Logging, Generic, A.ToJSON)

tracer1 :: (MonadIO m, MonadUnliftIO m) => m (Trace m LO)
tracer1  = do
  t1      <- fmap (appendName "tracer1") stdoutObjectKatipTracer
  limitFrequency 5 "5 messages per second" (cmap LOS t1) (cmap LOL t1)

tracer2 :: (MonadIO m, MonadUnliftIO m) => m (Trace m LO)
tracer2  = do
  t2      <- fmap (appendName "tracer2") stdoutJsonKatipTracer
  limitFrequency 15 "15 messages per second" (cmap LOS t2) (cmap LOL t2)

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
  repeated t 1000 10000 -- 100 messages per second
  repeated t 20 1000000 -- 1  message per second
  repeated t 300 100000 -- 10  message per second
