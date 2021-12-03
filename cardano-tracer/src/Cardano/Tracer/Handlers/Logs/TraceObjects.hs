{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.Logs.TraceObjects
  ( traceObjectsHandler
  ) where

import           Control.Concurrent.Async (concurrently_, forConcurrently_)
import           Control.Concurrent.Extra (Lock)
import qualified Data.List.NonEmpty as NE

import           Cardano.Logging (TraceObject)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.File (writeTraceObjectsToFile)
import           Cardano.Tracer.Handlers.Logs.Journal (writeTraceObjectsToJournal)
import           Cardano.Tracer.Handlers.RTView.Run (SavedTraceObjects, saveTraceObjects)
import           Cardano.Tracer.Types (NodeId)
import           Cardano.Tracer.Utils (showProblemIfAny)

-- | This handler is called periodically by 'TraceObjectForward' protocol
--   from 'trace-forward' library.
traceObjectsHandler
  :: TracerConfig      -- ^ Tracer configuration.
  -> NodeId            -- ^ An id of the node 'TraceObject's were received from.
  -> Lock              -- ^ The lock we use for single-threaded access to the current log.
  -> SavedTraceObjects -- ^ Saved 'TraceObject's for RTView.
  -> [TraceObject]     -- ^ The list of received 'TraceObject's (may be empty).
  -> IO ()
traceObjectsHandler _ _ _ _ [] = return ()
traceObjectsHandler TracerConfig{hasRTView, logging, verbosity} nodeId currentLogLock savedTO traceObjects =
  case hasRTView of
    Nothing -> writeTraceObjects
    Just _  -> concurrently_ (saveTraceObjects savedTO nodeId traceObjects)
                             writeTraceObjects
 where
  writeTraceObjects =
    forConcurrently_ (NE.nub logging) $ \LoggingParams{logMode, logRoot, logFormat} ->
      showProblemIfAny verbosity $
        case logMode of
          FileMode    -> writeTraceObjectsToFile nodeId currentLogLock logRoot logFormat traceObjects
          JournalMode -> writeTraceObjectsToJournal nodeId traceObjects
