{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Errors
  ( mkErrorsTable
  ) where

import           Control.Monad (when)
import           Control.Monad.Extra (unlessM)
import           Data.Maybe (fromMaybe)
import           Data.Text (unpack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.Errors
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Errors
import           Cardano.Tracer.Types

mkErrorsTable
  :: UI.Window
  -> NodeId
  -> Errors
  -> UI.Timer
  -> DisplayedElements
  -> UI Element
mkErrorsTable window nodeId@(NodeId anId) nodesErrors updateErrorsTimer displayedElements = do
  let id' = unpack anId
  closeIt <- UI.button #. "delete"
  deleteAll <- image "has-tooltip-multiline has-tooltip-left rt-view-delete-errors-icon" trashSVG
                     # set dataTooltip "Click to delete all errors. This action cannot be undone!"
  on UI.click deleteAll . const $
    deleteAllErrorMessages window nodeId nodesErrors

  searchMessagesInput <- UI.input #. "input rt-view-search-messages"
                                  # set UI.type_ "text"
                                  # set (UI.attr "placeholder") "Search messages"
  searchMessages <- UI.button #. "button is-info"
                              #+ [image "rt-view-search-errors-icon" searchSVG]

  -- If the user clicked the search button.
  on UI.click searchMessages . const $
    searchErrorMessages window searchMessagesInput nodeId nodesErrors updateErrorsTimer
  -- If the user hits Enter key.
  on UI.keyup searchMessagesInput $ \keyCode ->
    when (keyCode == 13) $
      searchErrorMessages window searchMessagesInput nodeId nodesErrors updateErrorsTimer
  -- If the user changed text in input...
  on UI.valueChange searchMessagesInput $ \inputText ->
    when (null inputText) $
      -- ... and this text is empty, it means that search/filter mode is off,
      -- so remove "search result" and start the timer for update errors again.
      unlessM (get UI.running updateErrorsTimer) $
        -- Ok, the timer is stopped, so we are in search/filter mode already, exit it.
        exitSearchMode window nodeId updateErrorsTimer

  sortByTimeIcon <- image "has-tooltip-multiline has-tooltip-right rt-view-sort-icon" sortSVG
                          # set dataTooltip "Click to sort errors by time"
                          # set dataState "asc"
  on UI.click sortByTimeIcon . const $
    sortErrorsByTime window nodeId sortByTimeIcon nodesErrors

  sortBySeverityIcon <- image "has-tooltip-multiline has-tooltip-right rt-view-sort-icon" sortSVG
                              # set dataTooltip "Click to sort errors by severity"
                              # set dataState "asc"
  on UI.click sortBySeverityIcon . const $
    sortErrorsBySeverity window nodeId sortBySeverityIcon nodesErrors

  exportToJSON <- image "has-tooltip-multiline has-tooltip-left rt-view-export-icon" exportSVG
                        # set dataTooltip "Click to export errors to JSON-file"
  on UI.click exportToJSON . const $ do
    nName <- liftIO $ getDisplayedValue displayedElements nodeId (anId <> "__node-name")
    exportErrorsToJSONFile nodesErrors nodeId $ fromMaybe anId nName

  errorsTable <-
    UI.div #. "modal" #+
      [ UI.div #. "modal-background" #+ []
      , UI.div #. "modal-card rt-view-errors-modal" #+
          [ UI.header #. "modal-card-head rt-view-errors-head" #+
              [ UI.p #. "modal-card-title rt-view-errors-title" #+
                  [ string "Errors from "
                  , UI.span ## (id' <> "__node-name-for-errors")
                            #. "has-text-weight-bold"
                            # set text id'
                  ]
              , element closeIt
              ]
          , UI.mkElement "section" #. "modal-card-body rt-view-errors-body" #+
              [ UI.div ## (id' <> "__errors-table-container") #. "table-container" #+
                  [ UI.table ## (id' <> "__errors-table") #. "table is-fullwidth rt-view-errors-table" #+
                      [ UI.mkElement "thead" #+
                          [ UI.tr #+
                              [ UI.th #. "rt-view-errors-timestamp" #+
                                  [ string "Timestamp"
                                  , element sortByTimeIcon
                                  ]
                              , UI.th #. "rt-view-errors-severity" #+
                                  [ string "Severity"
                                  , element sortBySeverityIcon
                                  ]
                              , UI.th #+
                                  [ string "Message"
                                  ]
                              , UI.th #+
                                  [ element deleteAll
                                  ]
                              ]
                          ]
                      , UI.mkElement "tbody" ## (id' <> "__node-errors-tbody")
                                             # set dataState "0"
                                             #+ []
                      ]
                  ]
              ]
          , UI.mkElement "footer" #. "modal-card-foot rt-view-errors-foot" #+
              [ UI.div #. "columns" #+
                  [ UI.div #. "column" #+
                      [ UI.div #. "field has-addons" #+
                          [ UI.p #. "control" #+
                              [ element searchMessagesInput
                              ]
                          , UI.p #. "control" #+
                              [ element searchMessages
                              ]
                          ]
                      ]
                  , UI.div #. "column has-text-right" #+
                      [ element exportToJSON
                      ]
                  ]
              ]
          ]
      ]
  on UI.click closeIt . const $ element errorsTable #. "modal"
  return errorsTable
