{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.State.TraceObjects
  ( Namespace
  , SavedTraceObjects
  , TraceObjectInfo
  , initSavedTraceObjects
  , saveTraceObjects
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO)
import           Control.Monad (unless)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import           Data.Text (Text, intercalate)
import           Data.Time.Clock (UTCTime)

import           Cardano.Logging (TraceObject (..), SeverityS)

import           Cardano.Tracer.Types (NodeId)

type Namespace       = Text
type TraceObjectInfo = (Text, SeverityS, UTCTime)

-- | We have to store 'TraceObject's received from the node,
--   to be able to update corresponding elements (on the web page)
--   using the values extracted from these 'TraceObject's.
type SavedForNode      = Map Namespace TraceObjectInfo
type SavedTraceObjects = TVar (Map NodeId SavedForNode)

initSavedTraceObjects :: IO SavedTraceObjects
initSavedTraceObjects = newTVarIO M.empty

saveTraceObjects :: SavedTraceObjects -> NodeId -> [TraceObject] -> IO ()
saveTraceObjects savedTraceObjects nodeId traceObjects =
  unless (null itemsToSave) $ atomically $ modifyTVar' savedTraceObjects $ \savedTO ->
    case M.lookup nodeId savedTO of
      Nothing ->
        M.insert nodeId (M.fromList itemsToSave) savedTO
      Just savedTOForThisNode ->
        M.adjust (const $ savedTOForThisNode `updateSavedBy` itemsToSave) nodeId savedTO
 where
  itemsToSave = mapMaybe getTOValue traceObjects

  getTOValue TraceObject{toNamespace, toHuman, toMachine, toSeverity, toTimestamp} =
    case (toNamespace, toHuman, toMachine) of
      ([], _,        _)        -> Nothing
      (ns, Just msg, Nothing)  -> Just (mkName ns, (msg, toSeverity, toTimestamp))
      (ns, Nothing,  Just msg) -> Just (mkName ns, (msg, toSeverity, toTimestamp))
      (ns, Just msg, Just _)   -> Just (mkName ns, (msg, toSeverity, toTimestamp))
      _                        -> Nothing

  mkName = intercalate "."

  -- Update saved 'TraceObject's by new ones: existing value will be replaced.
  updateSavedBy = go
   where
    go saved [] = saved
    go saved ((ns, toI):others) = M.insert ns toI saved `go` others
