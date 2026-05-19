module Cardano.Tracer.Handlers.Logs.Journal.NoSystemd
  ( writeTraceObjectsToJournal
  ) where

import           Cardano.Logging (TraceObject)
import           Cardano.Tracer.Configuration (LogFormat)
import           Cardano.Tracer.Types (NodeName)


writeTraceObjectsToJournal :: LogFormat -> NodeName -> [TraceObject] -> IO ()
writeTraceObjectsToJournal _ _ _ = pure ()
