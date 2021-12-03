{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.NodeInfo
  ( askNodeInfoIfNeeded
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import           Control.Monad (forM, forM_, unless)
import           Data.Aeson (FromJSON, decode')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.State.DataPoint
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

-- | TMP, the type should be taken from 'cardano-node'.
type NodeInfo = Int

-- | For all newly connected nodes we should:
--
--   1. ask for their 'NodeInfo',
--   2. set the values of corresponding elements on the nodes' panels,
--   3. save these values in 'displayedElements' to keep them for future.
--
--   But if node's info was already saved (and user just reloaded web-page),
--   there is no need to ask node's info again, just use saved values.
askNodeInfoIfNeeded
  :: DataPointAskers
  -> Set NodeId
  -> DisplayedElements
  -> UI ()
askNodeInfoIfNeeded dpAskers connectedNodes displayedElements =
  unless (S.null connectedNodes) $ do
    dElements <- liftIO $ readTVarIO displayedElements
    nodesInfoValues <-
      forM (S.toList connectedNodes) $ \nodeId -> liftIO $
        checkSavedNodeInfo nodeId >>= \case
          Nothing ->
            getDataPointFromNode dpAskers nodeId "name.of.node.info" >>= \case
              Nothing -> return Nothing -- The node didn't provide its info.
              Just (nodeInfo :: NodeInfo) -> return Nothing
          Just savedInfo ->
            return Nothing

    setElements nodesInfoValues
    -- saveElements
 where
  checkSavedNodeInfo _ = return Nothing

  setElements nodesInfoValues =
    forM_ nodesInfoValues $ \nodeInfoValues -> do
      return ()
