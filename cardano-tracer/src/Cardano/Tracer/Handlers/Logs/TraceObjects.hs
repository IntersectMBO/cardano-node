{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.Logs.TraceObjects
  ( traceObjectsHandler
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.Async (forConcurrently_)
import Control.Monad.Extra (whenJust)
import Data.Map qualified as Map
import Data.Map (Map)
import Data.List.NonEmpty qualified as NE
import System.IO (Handle)

import Cardano.Logging (TraceObject)

import Cardano.Tracer.Configuration
import Cardano.Tracer.Environment
import Cardano.Tracer.Handlers.Logs.File
import Cardano.Tracer.Handlers.Logs.Journal
import Cardano.Tracer.Handlers.RTView.Run
import Cardano.Tracer.Types
import Cardano.Tracer.Utils

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
  -- Options
  -- 1. newMVar @(Map LoginParams (Maybe Handle))
  -- 2. Map LoginParams (MVar Handle)
  --                     ^^^^^^^^^^^ newEmptyMVar
  handleMapMVar <- newMVar @(Map FilePath Handle) Map.empty

  forConcurrently_ logging \LoggingParams{logMode, logRoot, logFormat} ->
    showProblemIfAny verbosity
      case logMode of
        FileMode    -> writeTraceObjectsToFile handleMapMVar nodeName teCurrentLogLock logRoot logFormat traceObjects
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
