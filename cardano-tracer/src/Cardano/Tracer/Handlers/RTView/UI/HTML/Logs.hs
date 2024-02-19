{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Logs
  ( mkLogsLiveView
  ) where

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Logs
import           Cardano.Tracer.Handlers.RTView.UI.Utils

import           Control.Monad (void)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

mkLogsLiveView :: TracerEnv -> UI Element
mkLogsLiveView tracerEnv = do
  closeIt <- UI.button #. "delete"

  _searchMessagesInput <-
    UI.input #. "input rt-view-search-messages"
             # set UI.type_ "text"
             # set (UI.attr "placeholder") "Search log items"
  _searchMessages <-
    UI.button #. "button is-info"
              #+ [image "rt-view-search-logs-icon" searchSVG]

  _whenToSearch <-
    UI.div #. "dropdown is-hoverable" #+
      [ UI.div #. "dropdown-trigger" #+
          [ UI.button #. "button"
                      # set ariaHasPopup "true"
                      # set ariaControls "dropdown-menu4" #+
              [ UI.span # set text "Last 3 days"
              -- ICON?
              ]
          ]
      , UI.div ## "dropdown-menu4"
               #. "dropdown-menu"
               # set role "menu" #+
          [ UI.div #. "dropdown-content" #+
              [ UI.div #. "dropdown-item" #+
                  [ UI.p # set text "TEST ME"
                  ]
              ]
          ]
      ]

  _whereToSearch <-
    UI.div #. "dropdown is-hoverable" #+
      [ UI.div #. "dropdown-trigger" #+
          [ UI.button #. "button"
                      # set ariaHasPopup "true"
                      # set ariaControls "dropdown-menu4" #+
              [ UI.span # set text "In message"
              -- ICON?
              ]
          ]
      , UI.div ## "dropdown-menu4"
               #. "dropdown-menu"
               # set role "menu" #+
          [ UI.div #. "dropdown-content" #+
              [ UI.div #. "dropdown-item" #+
                  [ UI.p # set text "TEST ME"
                  ]
              ]
          ]
      ]

  fontSetter <-
    UI.input ## "logs-live-view-font-setter"
             # set UI.type_ "range"
             # set min_     "1"
             # set max_     "6"
             # set value    "5"
  on change fontSetter . const $ do
    window <- askWindow
    fontSizePct <-
      get value fontSetter >>= \case
        "1" -> return "50%"
        "2" -> return "60%"
        "3" -> return "70%"
        "4" -> return "80%"
        "5" -> return "90%"
        "6" -> return "100%"
        _   -> return "90%"
    findAndSet (set style [("font-size", fontSizePct)]) window "node-logs-live-view-tbody"
    saveLogsLiveViewFont tracerEnv fontSizePct

  logsLiveViewTable <-
    UI.div ## "logs-live-view-modal-window" #. "modal" # set dataState "closed" #+
      [ UI.div #. "modal-background" #+ []
      , UI.div #. "modal-card rt-view-logs-live-view-modal" #+
          [ UI.header #. "modal-card-head rt-view-logs-live-view-head" #+
              [ UI.p #. "modal-card-title rt-view-logs-live-view-title" #+
                  [ string "Log items from connected nodes"
                  ]
              , element closeIt
              ]
          , UI.mkElement "section" #. "modal-card-body rt-view-logs-live-view-body" #+
              [ UI.div #. "columns" #+
                  [ UI.div #. "column is-8" #+
                      [ UI.div #. "bd-notification" #+
                          [ UI.div ## "logs-live-view-nodes-checkboxes" #. "field" #+ []
                          ]
                      ]
                  , UI.div #. "column has-text-right" #+
                      [ string "A" #. "is-size-5 mr-2"
                      , element fontSetter
                      , string "A" #. "is-size-3 ml-2"
                      ]
                  ]
              , UI.div ## "logs-live-view-table-container" #. "table-container" #+
                  [ UI.div ## "node-logs-live-view-tbody"
                           #. "rt-view-logs-live-view-tbody"
                           # set dataState "0"
                           #+ []
                  ]
              ]
          {-
          , UI.mkElement "footer" #. "modal-card-foot rt-view-logs-live-view-foot" #+
              [ UI.div #. "columns" #+
                  [ UI.div #. "column" #+
                      [ UI.div #. "field has-addons" #+
                          [ UI.p #. "control" #+
                              [ element whenToSearch
                              ]
                          , UI.p #. "control" #+
                              [ element whereToSearch
                              ]
                          , UI.p #. "control" #+
                              [ element searchMessagesInput
                              ]
                          , UI.p #. "control" #+
                              [ element searchMessages
                              ]
                          ]
                      ]
                  --, UI.div #. "column has-text-right" #+
                  --    [ element exportToJSON
                  --    ]
                  ]
              ]
          -}
          ]
      ]
  on UI.click closeIt . const $ do
    void $ element logsLiveViewTable #. "modal"
    void $ element logsLiveViewTable # set dataState "closed"

  return logsLiveViewTable
 where
  change = void . domEvent "change"
