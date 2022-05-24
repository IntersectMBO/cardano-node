{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Errors
  ( runErrorsUpdater
  , updateNodesErrors
  , searchErrorMessages
  , deleteAllErrorMessages
  , exitSearchMode
  , sortErrorsByTime
  , sortErrorsBySeverity
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM, forM_, forever, unless, void, when)
import           Control.Monad.Extra (whenJust, whenJustM)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.Time.Extra (sleep)
import           Text.Read (readMaybe)

import           Cardano.Logging (SeverityS (..))

import           Cardano.Tracer.Handlers.RTView.State.Errors
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.JS.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

runErrorsUpdater
  :: ConnectedNodes
  -> Errors
  -> SavedTraceObjects
  -> IO ()
runErrorsUpdater connectedNodes nodesErrors savedTO = forever $ do
  sleep 2.0
  connected <- readTVarIO connectedNodes
  savedTraceObjects <- readTVarIO savedTO
  forM_ connected $ \nodeId ->
    whenJust (M.lookup nodeId savedTraceObjects) $ \savedTOForNode ->
      forM_ (M.toList savedTOForNode) $ \(_, trObInfo@(_, severity, _)) ->
        when (itIsError severity) $
          addError nodesErrors nodeId trObInfo
 where
  itIsError sev =
    case sev of
      Error     -> True
      Critical  -> True
      Alert     -> True
      Emergency -> True
      _         -> False

-- | Update error messages in a corresponding modal window.
updateNodesErrors
  :: UI.Window
  -> ConnectedNodes
  -> Errors
  -> UI ()
updateNodesErrors window connectedNodes nodesErrors = do
  connected <- liftIO $ readTVarIO connectedNodes
  forM_ connected $ \nodeId@(NodeId anId) -> do
    errorsFromNode <- liftIO $ getErrors nodesErrors nodeId
    unless (null errorsFromNode) $ do
      -- Update errors number (as it is in the state).
      setTextValue (anId <> "__node-errors-num") (showT $ length errorsFromNode)
      -- Enable 'Details' button.
      findAndSet (set UI.enabled True) window (anId <> "__node-errors-details-button")
      -- Add errors if needed.
      whenJustM (UI.getElementById window (T.unpack anId <> "__node-errors-tbody")) $ \el ->
        whenJustM (readMaybe <$> get dataState el) $ \(numberOfDisplayedRows :: Int) -> do
          let onlyNewErrors = drop numberOfDisplayedRows errorsFromNode
          doAddErrorRows nodeId onlyNewErrors el numberOfDisplayedRows

doAddErrorRows
  :: NodeId
  -> [ErrorInfo]
  -> Element
  -> Int
  -> UI ()
doAddErrorRows nodeId errorsToAdd parentEl numberOfDisplayedRows = do
  errorRows <-
    forM errorsToAdd $ \(errorIx, (msg, sev, ts)) ->
      mkErrorRow errorIx nodeId msg sev ts
  -- Add them actually and remember their new number.
  let newNumberOfDisplayedRows = numberOfDisplayedRows + length errorsToAdd
  void $ element parentEl # set dataState (show newNumberOfDisplayedRows)
                          #+ errorRows
 where
  mkErrorRow _errorIx (NodeId anId) msg sev ts = do
    copyErrorIcon <- image "has-tooltip-multiline has-tooltip-left rt-view-copy-icon" copySVG
                           # set dataTooltip "Click to copy this error"
    on UI.click copyErrorIcon . const $
      copyTextToClipboard $ errorToCopy ts sev msg

    return $
      UI.tr #. (T.unpack anId <> "-node-error-row") #+
        [ UI.td #+
            [ UI.span # set text (preparedTS ts)
            ]
        , UI.td #+
            [ UI.span #. "tag is-medium is-danger" # set text (show sev)
            ]
        , UI.td #+
            [ UI.p #. "control" #+
                [ UI.input #. "input rt-view-error-msg-input"
                           # set UI.type_ "text"
                           # set (UI.attr "readonly") "readonly"
                           # set UI.value (T.unpack msg)
                ]
            ]
        , UI.td #+
            [ element copyErrorIcon
            ]
        ]

  preparedTS = formatTime defaultTimeLocale "%b %e, %Y %T"

  errorToCopy ts sev msg = "[" <> preparedTS ts <> "] [" <> show sev <> "] [" <> T.unpack msg <> "]"

searchErrorMessages
  :: UI.Window
  -> Element
  -> NodeId
  -> Errors
  -> UI.Timer
  -> UI ()
searchErrorMessages window searchInput nodeId@(NodeId anId) nodesErrors updateErrorsTimer = do
  textToSearch <- T.strip . T.pack <$> get value searchInput
  unless (T.null textToSearch) $ do
    -- Ok, there is non-empty text we want to search. It means that now we are
    -- in search/filter mode, and during this period the new messages shouldn't be added,
    -- so we stop update timer temporarily.
    UI.stop updateErrorsTimer
    liftIO (getErrorsFilteredByText textToSearch nodesErrors nodeId) >>= \case
      [] -> do
        -- There is nothing found. So we have to inform the user that
        -- there is no corresponding errors.
        findByClassAndDo window (anId <> "-node-error-row") delete'
        -- Reset number of currently displayed errors rows.
        whenJustM (UI.getElementById window (T.unpack anId <> "__node-errors-tbody")) $ \el ->
          void $ element el # set dataState "0"
      foundErrors -> do
        -- Delete displayed errors from window.
        findByClassAndDo window (anId <> "-node-error-row") delete'
        -- Do add found errors.
        whenJustM (UI.getElementById window (T.unpack anId <> "__node-errors-tbody")) $ \el ->
          doAddErrorRows nodeId foundErrors el 0

deleteAllErrorMessages
  :: UI.Window
  -> NodeId
  -> Errors
  -> UI ()
deleteAllErrorMessages window nodeId@(NodeId anId) nodesErrors = do
  -- Delete errors from window.
  findByClassAndDo window (anId <> "-node-error-row") delete'
  -- Delete errors from state.
  liftIO $ deleteAllErrors nodesErrors nodeId
  -- Reset number of errors and disable Detail button.
  setTextValue (anId <> "__node-errors-num") "0"
  findAndSet (set UI.enabled False) window (anId <> "__node-errors-details-button")
  -- Reset number of currently displayed errors rows.
  whenJustM (UI.getElementById window (T.unpack anId <> "__node-errors-tbody")) $ \el ->
    void $ element el # set dataState "0"

exitSearchMode
  :: UI.Window
  -> NodeId
  -> UI.Timer
  -> UI ()
exitSearchMode window (NodeId anId) updateErrorsTimer = do
  -- Delete errors (last search result) from window.
  findByClassAndDo window (anId <> "-node-error-row") delete'
  -- Reset number of currently displayed errors rows.
  whenJustM (UI.getElementById window (T.unpack anId <> "__node-errors-tbody")) $ \el ->
    void $ element el # set dataState "0"
  -- Start update errors timer again.
  UI.start updateErrorsTimer

sortErrorsByTime, sortErrorsBySeverity
  :: UI.Window
  -> NodeId
  -> Element
  -> Errors
  -> UI ()
sortErrorsByTime     = sortErrors timeAsc timeDesc
sortErrorsBySeverity = sortErrors severityAsc severityDesc

sortErrors
  :: (ErrorInfo -> ErrorInfo -> Ordering)
  -> (ErrorInfo -> ErrorInfo -> Ordering)
  -> UI.Window
  -> NodeId
  -> Element
  -> Errors
  -> UI ()
sortErrors orderingAsc orderingDesc window nodeId@(NodeId anId) sortIcon nodesErrors = do
  -- Delete errors from window.
  findByClassAndDo window (anId <> "-node-error-row") delete'
  get dataState sortIcon >>= \case
    "desc" -> doSortErrors orderingAsc  "asc"
    _      -> doSortErrors orderingDesc "desc"
 where
  doSortErrors ordering orderState = do
    sortedErrors <- liftIO $ getErrorsSortedBy ordering nodesErrors nodeId
    -- Do add sorted errors.
    whenJustM (UI.getElementById window (T.unpack anId <> "__node-errors-tbody")) $ \el ->
      doAddErrorRows nodeId sortedErrors el 0
    void $ element sortIcon # set dataState orderState
