{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.Logs.TraceObjects
  ( traceObjectsHandler
  ) where

import           Control.Concurrent.Async (forConcurrently_)
import           Control.Concurrent.STM.TVar (readTVarIO)
import           Data.Map.Strict ((!?))
import qualified Data.List.NonEmpty as NE

import           Cardano.Logging (TraceObject)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.File
import           Cardano.Tracer.Handlers.Logs.Journal
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

traceObjectsHandler
  :: TracerConfig
  -> NodeId
  -> AcceptedNodeInfo
  -> [TraceObject]
  -> IO ()
traceObjectsHandler _ _ _ [] = return ()
traceObjectsHandler TracerConfig{logging} nodeId acceptedNodeInfo traceObjects = do
  nodesInfo <- readTVarIO acceptedNodeInfo
  case nodesInfo !? nodeId of
    Nothing -> return ()
    Just ni -> do
      let NodeInfo{niName} = ni
      forConcurrently_ (NE.nub logging) $ \LoggingParams{logMode, logRoot, logFormat} ->
        case logMode of
          FileMode ->
            showProblemIfAny $ writeTraceObjectsToFile nodeId niName logRoot logFormat traceObjects
          JournalMode ->
            showProblemIfAny $ writeTraceObjectsToJournal nodeId niName traceObjects
