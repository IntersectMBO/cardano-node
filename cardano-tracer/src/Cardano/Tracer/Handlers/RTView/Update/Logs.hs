{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Logs
  ( updateLogsLiveViewItems
  , updateLogsLiveViewNodes
  ) where

import           Cardano.Logging (SeverityS (..), showT)
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.JS.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Nodes
import           Cardano.Tracer.Handlers.RTView.Utils
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_, void, when)
import           Control.Monad.Extra (whenJustM, whenM)
import qualified Data.Text as T
import           Data.Time.Format (defaultTimeLocale, formatTime)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

updateLogsLiveViewItems
  :: TracerEnv
  -> LogsLiveViewCounters
  -> UI ()
updateLogsLiveViewItems tracerEnv@TracerEnv{teSavedTO} llvCounters =
  whenM logsLiveViewIsOpened $ do
    window <- askWindow
    whenJustM (UI.getElementById window "node-logs-live-view-tbody") $ \el ->
      forConnectedUI_ tracerEnv $ \nodeId@(NodeId anId) -> do
        nodeName        <- liftIO $ askNodeName tracerEnv nodeId
        nodeColor       <- liftIO $ getSavedColorForNode tracerEnv nodeName
        tosFromThisNode <- liftIO $ getTraceObjects teSavedTO nodeId
        forM_ tosFromThisNode $ \trObInfo -> do
          -- We should add log items only for nodes which is "enabled" via checkbox.
          let checkId = T.unpack anId <> "__node-live-view-checkbox"
          whenJustM (UI.getElementById window checkId) $ \checkbox -> do
            whenM (get UI.checked checkbox) $ do
              doAddItemRow nodeId nodeName nodeColor llvCounters el trObInfo
              -- Since we have added a new item row, we have to check if there are
              -- too many items already. If so - we have to remove old item row,
              -- to prevent too big number of them (if the user opened the window
              -- for a long time).
              liftIO (getLogsLiveViewCounter llvCounters nodeId) >>= \case
                Nothing -> return ()
                Just currentNumber ->
                  when (currentNumber > maxNumberOfLogsLiveViewItems) $ do
                    -- Ok, we have to delete outdated item row.
                    let !outdatedItemNumber = currentNumber - maxNumberOfLogsLiveViewItems
                        outdatedItemId = nodeName <> "llv" <> showT outdatedItemNumber
                    findAndDo window outdatedItemId delete'
 where
  -- TODO: Probably it will be configured by the user.
  maxNumberOfLogsLiveViewItems = 20

logsLiveViewIsOpened :: UI Bool
logsLiveViewIsOpened = do
  window <- askWindow
  UI.getElementById window "logs-live-view-modal-window" >>= \case
    Nothing -> return False
    Just el -> (==) "opened" <$> get dataState el

doAddItemRow
  :: NodeId
  -> NodeName
  -> Maybe Color
  -> LogsLiveViewCounters
  -> Element
  -> (Namespace, TraceObjectInfo)
  -> UI ()
doAddItemRow nodeId@(NodeId anId) nodeName nodeColor
             llvCounters parentEl (ns, (msg, sev, ts)) = do
  liftIO $ incLogsLiveViewCounter llvCounters nodeId
  aRow <- mkItemRow
  void $ element parentEl #+ [aRow]
 where
  mkItemRow = do
    copyItemIcon <- image "has-tooltip-multiline has-tooltip-left rt-view-copy-icon" copySVG
                          # set dataTooltip "Click to copy this log item"
    on UI.click copyItemIcon . const $ copyTextToClipboard $
      "[" <> preparedTS ts <> "] [" <> show sev <> "] [" <> T.unpack ns <> "] [" <> T.unpack msg <> "]"

    let nodeNamePrepared = T.unpack $
          if T.length nodeName > 13
            then T.take 10 nodeName <> "..."
            else nodeName

    nodeNameLabel <-
      case nodeColor of
        Nothing -> UI.span #. "rt-view-logs-live-view-msg-node"
                           # set text nodeNamePrepared
        Just (Color code) -> UI.span #. "rt-view-logs-live-view-msg-node"
                                     # set style [("color", code)]
                                     # set text nodeNamePrepared

    let sevClass =
          case sev of
            Debug   -> "has-text-primary"
            Info    -> "has-text-link"
            Notice  -> "has-text-info"
            Warning -> "has-text-warning"
            _       -> "has-text-danger"
    severity <- UI.span #. ("rt-view-logs-live-view-msg-severity " <> sevClass) # set text (show sev)

    logItemRowId <-
      liftIO (getLogsLiveViewCounter llvCounters nodeId) >>= \case
        Nothing -> return $ T.unpack nodeName <> "llv0"
        Just currentNumber -> return $ T.unpack nodeName <> "llv" <> show currentNumber

    return $
      UI.p ## logItemRowId #. (T.unpack anId <> "-node-logs-live-view-row") #+
        [ UI.span #. "rt-view-logs-live-view-msg-timestamp" # set text (preparedTS ts)
        , element nodeNameLabel
        , element severity
        , UI.span #. "rt-view-logs-live-view-msg-namespace" # set text ("[" <> T.unpack ns <> "]")
        , UI.span #. "rt-view-logs-live-view-msg-body" # set text (T.unpack msg)
        -- , element copyItemIcon
        ]

  preparedTS = formatTime defaultTimeLocale "%D %T"

-- | The userck clicks to button that opens live logs view window - so we should update its content.
--   Particularly, update nodes' checkboxes.
updateLogsLiveViewNodes :: TracerEnv -> UI ()
updateLogsLiveViewNodes tracerEnv@TracerEnv{teConnectedNodes} = do
  deleteAllNodesCheckboxes
  addNodesCheckboxesForConnected
 where
  deleteAllNodesCheckboxes = do
    window <- askWindow
    findByClassAndDo window "rt-view-ncbl" delete'
    findByClassAndDo window "is-checkradio is-medium rt-view-ncb" delete'

  addNodesCheckboxesForConnected =
    liftIO (readTVarIO teConnectedNodes) >>= doAddLiveViewNodesForConnected tracerEnv
