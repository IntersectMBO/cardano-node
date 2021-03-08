{-# LANGUAGE FlexibleContexts #-}
module Examples.Documentation where

import           Control.Monad (liftM)
import           Control.Monad.IO.Class
import           Data.IORef (readIORef)
import           Data.Map
import qualified Data.Text.IO as T
import           Katip
import           Katip.Scribes.Handle (ioLogEnv)

import           Cardano.Logging
import           Data.Text (Text)
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
