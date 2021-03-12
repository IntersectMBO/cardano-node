{-# LANGUAGE FlexibleContexts #-}
module Examples.Documentation (
  docTracer
) where

import           Control.Monad.IO.Class
import qualified Data.Text.IO as T

import           Cardano.Logging
import           Examples.TestObjects

tracer1 :: (LogFormatting (TraceForgeEvent blk), MonadIO m) =>
  m (Trace m (TraceForgeEvent blk))
tracer1  = stdoutHumanKatipTracer

tracer2 :: (LogFormatting (TraceForgeEvent blk), MonadIO m) =>
  m (Trace m (TraceForgeEvent blk))
tracer2  = stdoutJsonKatipTracer

docTracer :: IO ()
docTracer = do
  t1 <- fmap (withSeverityTraceForgeEvent
                . appendName "node"
                . appendName "cardano") tracer1
  t2 <-fmap (withSeverityTraceForgeEvent
                . appendName "node"
                . appendName "cardano") tracer2
  bl <- documentMarkdown traceForgeEventDocu [t1, t2]
  T.writeFile "/home/yupanqui/IOHK/Testdocu.md" (buildersToText bl)
