{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.Logs.TraceObjects
  ( traceObjectsHandler
  , deregisterNodeId
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
import qualified Data.Map as Map
import           System.IO (Handle, hClose)

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

  forConcurrently_ logging \loggingParams@LoggingParams{logMode} -> do
    showProblemIfAny verbosity do
      case logMode of
        FileMode ->
          writeTraceObjectsToFile teRegistry
             loggingParams nodeName teCurrentLogLock traceObjects
        JournalMode ->
          writeTraceObjectsToJournal nodeName traceObjects
  whenJust hasRTView \_ ->
    saveTraceObjects teSavedTO nodeId traceObjects
  teReforwardTraceObjects traceObjects

  where
    TracerEnv
      { teConfig = TracerConfig{logging, verbosity, hasRTView}
      , teCurrentLogLock
      , teSavedTO
      , teReforwardTraceObjects
      , teRegistry
      } = tracerEnv

deregisterNodeId :: TracerEnv -> NodeId -> IO ()
deregisterNodeId tracerEnv@TracerEnv{ teConfig = TracerConfig { logging }, teRegistry } nodeId = do
  nodeName <- askNodeName tracerEnv nodeId

  forConcurrently_ logging \loggingParams@LoggingParams{logMode} -> do

    case logMode of
      FileMode -> do
        modifyRegistry_ teRegistry \handles -> do
          case Map.updateLookupWithKey alwaysDelete (nodeName, loggingParams) handles of
            (Nothing, _newHandles) -> pure handles
            (Just (handle, _filePath), newHandles) -> do
              hClose handle
              pure newHandles
      JournalMode ->
        pure ()

  where

  alwaysDelete :: (NodeName, LoggingParams) -> (Handle, FilePath) -> Maybe (Handle, FilePath)
  alwaysDelete _key _value = Nothing
