{-# LANGUAGE PackageImports #-}

module Cardano.Node.Tracing.Tracers.Resources
  ( startResourceTracer
  , namesForResources
  , severityResources
  ) where

import           "contra-tracer" Control.Tracer

import           Cardano.Logging (SeverityS (..))
import           Cardano.Logging.Resources
import           Cardano.Prelude hiding (trace)

startResourceTracer
  :: Tracer IO ResourceStats
  -> Int
  -> IO ()
startResourceTracer tr delayMilliseconds = do
    as <- async resourceThread
    link as
  where
    resourceThread :: IO ()
    resourceThread = forever $ do
      mbrs <- readResourceStats
      forM_ mbrs $ \rs -> traceWith tr rs
      threadDelay (delayMilliseconds * 1000)

--------------------------------------------------------------------------------
-- ResourceStats Tracer
--------------------------------------------------------------------------------

namesForResources :: ResourceStats -> [Text]
namesForResources _ = []

severityResources :: ResourceStats -> SeverityS
severityResources _ = Info
