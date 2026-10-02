module Cardano.Tracer.Handlers.Logs.Journal.NoSystemd
  ( writeTraceObjectsToJournal
  ) where

import           Cardano.Tracer.Configuration (LogFormat)
import           Cardano.Tracer.Types (NodeName)

import           Hermod.Tracing (TraceObject)


writeTraceObjectsToJournal :: LogFormat -> NodeName -> [TraceObject] -> IO ()
writeTraceObjectsToJournal _ _ _ = pure ()
