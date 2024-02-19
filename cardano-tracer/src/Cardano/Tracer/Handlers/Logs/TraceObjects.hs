{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.Logs.TraceObjects
  ( traceObjectsHandler
  ) where

import           Cardano.Logging (TraceObject)
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Logs.File
import           Cardano.Tracer.Handlers.Logs.Journal
import           Cardano.Tracer.Handlers.RTView.Run
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

import           Control.Concurrent.Async (forConcurrently_)
import           Control.Monad.Extra (whenJust)
import qualified Data.List.NonEmpty as NE

-- | This handler is called periodically by 'TraceObjectForward' protocol
--   from 'trace-forward' library.
traceObjectsHandler
  :: TracerEnv         -- ^ Tracer environment.
  -> NodeId            -- ^ An id of the node 'TraceObject's were received from.
  -> [TraceObject]     -- ^ The list of received 'TraceObject's (may be empty).
  -> IO ()
traceObjectsHandler _ _ [] = return ()
traceObjectsHandler tracerEnv nodeId traceObjects = do
  nodeName <- askNodeName tracerEnv nodeId
  forConcurrently_ (NE.nub logging) \LoggingParams{logMode, logRoot, logFormat} ->
    showProblemIfAny verbosity
      case logMode of
        FileMode    -> writeTraceObjectsToFile nodeName teCurrentLogLock logRoot logFormat traceObjects
        JournalMode -> writeTraceObjectsToJournal nodeName traceObjects
  whenJust hasRTView \_ ->
    saveTraceObjects teSavedTO nodeId traceObjects
  teReforwardTraceObjects traceObjects

 where
  TracerEnv
    { teConfig = TracerConfig{logging, verbosity, hasRTView}
    , teCurrentLogLock
    , teSavedTO
    , teReforwardTraceObjects
    }
    = tracerEnv
