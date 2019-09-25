{-# LANGUAGE BangPatterns #-}
module Cardano.Tracing.TraceAcceptor
  ( runTraceAcceptor
  , handleTraceAcceptor
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Data.Text (pack)

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
import           Control.Tracer (Tracer, contramap, traceWith)

import qualified Cardano.BM.Trace as Trace
import qualified Cardano.BM.Tracing as Trace
import           Cardano.Config.Logging (LoggingLayer(..))


runTraceAcceptor :: LoggingLayer -> IO ()
runTraceAcceptor loggingLayer =
  handleTraceAcceptor tracer
  where
    tr :: Trace.Trace IO Text
    !tr = llAppendName loggingLayer "node" (llBasicTrace loggingLayer)
    trace' = Trace.appendName "acceptor" tr
    tracer = contramap pack $ Trace.toLogObject trace'

handleTraceAcceptor :: Tracer IO String -> IO ()
handleTraceAcceptor tracer = do

    traceWith tracer $ "**************************************"
    traceWith tracer $ "I am TraceAcceptor"
    traceWith tracer $ "**************************************"

    forever $ threadDelay 10000000 -- sleep 10s
