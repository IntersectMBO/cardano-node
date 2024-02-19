{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.State.TraceObjects
  ( LogsLiveViewCounters
  , Namespace
  , SavedTraceObjects
  , TraceObjectInfo
  , getLogsLiveViewCounter
  , getTraceObjects
  , incLogsLiveViewCounter
  , initLogsLiveViewCounters
  , initSavedTraceObjects
  , saveTraceObjects
  ) where

import           Cardano.Logging (SeverityS, TraceObject (..))
import           Cardano.Tracer.Types (NodeId)

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO)
import           Control.Monad (forM_, unless)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import           Data.Text (Text, intercalate)
import           Data.Time.Clock (UTCTime)

type Namespace       = Text
type TraceObjectInfo = (Text, SeverityS, UTCTime)

type SavedForNode      = TQueue (Namespace, TraceObjectInfo)
type SavedTraceObjects = TVar (Map NodeId SavedForNode)

initSavedTraceObjects :: IO SavedTraceObjects
initSavedTraceObjects = newTVarIO M.empty

saveTraceObjects
  :: SavedTraceObjects
  -> NodeId
  -> [TraceObject]
  -> IO ()
saveTraceObjects savedTraceObjects nodeId traceObjects =
  unless (null itemsToSave) $ atomically $ do
    savedTO' <- readTVar savedTraceObjects
    case M.lookup nodeId savedTO' of
      Nothing -> do
        -- There is no queue for this node yet, so create it, fill it and save it.
        newQ <- newTQueue
        pushItemsToQueue newQ
        modifyTVar' savedTraceObjects $ \savedTO ->
          case M.lookup nodeId savedTO of
            Nothing -> M.insert nodeId newQ savedTO
            Just _  -> savedTO
      Just qForThisNode ->
        -- There is a queue for this node already, so fill it.
        pushItemsToQueue qForThisNode
 where
  itemsToSave = mapMaybe getTOValue traceObjects

  getTOValue TraceObject{toNamespace, toHuman, toMachine, toSeverity, toTimestamp} =
    case (toNamespace, toHuman, toMachine) of
      ([], _,        _)        -> Nothing
      (ns, _, msg)   -> Just (mkName ns, (msg, toSeverity, toTimestamp))

  mkName = intercalate "."

  pushItemsToQueue = forM_ itemsToSave . writeTQueue

getTraceObjects
  :: SavedTraceObjects
  -> NodeId
  -> IO [(Namespace, TraceObjectInfo)]
getTraceObjects savedTraceObjects nodeId = atomically $ do
  qForThisNode <- M.lookup nodeId <$> readTVar savedTraceObjects
  maybe (return []) flushTQueue qForThisNode

-- | Counters for displayed logs item in "live view window".
type LogsLiveViewCounters = TVar (Map NodeId Int)

initLogsLiveViewCounters :: IO LogsLiveViewCounters
initLogsLiveViewCounters = newTVarIO M.empty

incLogsLiveViewCounter
  :: LogsLiveViewCounters
  -> NodeId
  -> IO ()
incLogsLiveViewCounter llvCounters nodeId = atomically $
  modifyTVar' llvCounters $ \currentCounters ->
    case M.lookup nodeId currentCounters of
      Nothing -> M.insert nodeId 1 currentCounters
      Just counterForNode -> M.adjust (const $! counterForNode + 1) nodeId currentCounters

getLogsLiveViewCounter
  :: LogsLiveViewCounters
  -> NodeId
  -> IO (Maybe Int)
getLogsLiveViewCounter llvCounters nodeId = M.lookup nodeId <$> readTVarIO llvCounters
