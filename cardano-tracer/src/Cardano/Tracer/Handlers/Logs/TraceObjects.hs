{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.Logs.TraceObjects
  ( traceObjectsHandler
  ) where

import           Control.Concurrent.Async (forConcurrently_)
import qualified Data.List.NonEmpty as NE

import           Cardano.Logging (TraceObject)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.File (writeTraceObjectsToFile)
import           Cardano.Tracer.Handlers.Logs.Journal (writeTraceObjectsToJournal)
import           Cardano.Tracer.Types (NodeId)
import           Cardano.Tracer.Utils (showProblemIfAny)

-- | This handler is called periodically by 'TraceObjectForward' protocol
--   from 'trace-forward' library.
traceObjectsHandler
  :: TracerConfig  -- ^ Tracer configuration.
  -> NodeId        -- ^ An id of the node 'TraceObject's were received from.
  -> [TraceObject] -- ^ The list of received 'TraceObject's (may be empty).
  -> IO ()
traceObjectsHandler _ _ [] = return ()
traceObjectsHandler TracerConfig{logging, verbosity} nodeId traceObjects =
  forConcurrently_ (NE.nub logging) $ \LoggingParams{logMode, logRoot, logFormat} ->
    case logMode of
      FileMode ->
        showProblemIfAny verbosity $
          writeTraceObjectsToFile nodeId logRoot logFormat traceObjects
      JournalMode ->
        showProblemIfAny verbosity $
          writeTraceObjectsToJournal nodeId traceObjects
