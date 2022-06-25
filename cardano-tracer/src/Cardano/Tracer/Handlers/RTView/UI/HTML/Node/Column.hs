{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Column
  ( addNodeColumn
  , deleteNodeColumn
  ) where

import           Control.Monad (forM, void)
import           Control.Monad.Extra (whenJustM)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Text (unpack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.FilePath ((</>))

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.Errors
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Node.EKG
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Errors
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Peers
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.JS.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

-- | For every connected node the new column should be added.
addNodeColumn
  :: UI.Window
  -> NonEmpty LoggingParams
  -> Errors
  -> UI.Timer
  -> DisplayedElements
  -> NodeId
  -> UI ()
addNodeColumn window loggingConfig nodesErrors updateErrorsTimer
              displayedElements nodeId@(NodeId anId) = do
  let id' = unpack anId
  ls <- logsSettings loggingConfig id'

  peersTable <- mkPeersTable id'
  peersDetailsButton <- UI.button ## (id' <> "__node-peers-details-button")
                                  #. "button is-info"
                                  # set UI.enabled False
                                  # set text "Details"
  on UI.click peersDetailsButton . const $ fadeInModal peersTable

  errorsTable <- mkErrorsTable window nodeId nodesErrors updateErrorsTimer displayedElements
  errorsDetailsButton <- UI.button ## (id' <> "__node-errors-details-button")
                                   #. "button is-danger"
                                   # set UI.enabled False
                                   # set text "Details"
  on UI.click errorsDetailsButton . const $ fadeInModal errorsTable

  ekgMetricsWindow <- mkEKGMetricsWindow id'
  ekgMetricsButton <- UI.button ## (id' <> "__node-ekg-metrics-button")
                                #. "button is-info"
                                # set text "Details"
  on UI.click ekgMetricsButton . const $ fadeInModal ekgMetricsWindow

  addNodeCellH "name"    [ image "rt-view-node-chart-label has-tooltip-multiline has-tooltip-left" rectangleSVG
                                 ## (id' <> "__node-chart-label")
                                 # set dataTooltip "Label using for this node on charts"
                         , UI.span ## (id' <> "__node-name")
                                   #. "has-text-weight-bold is-size-4 rt-view-node-name"
                                   # set text "Node"
                         , image "has-tooltip-multiline has-tooltip-bottom rt-view-what-icon" whatSVG
                                 # set dataTooltip "Node's name, taken from its configuration file"
                         ]
  addNodeCell "version"  [ UI.span ## (id' <> "__node-version")
                                   # set text "—"
                         ]
  addNodeCell "commit"   [ UI.anchor ## (id' <> "__node-commit")
                                     #. ("rt-view-href is-family-monospace has-text-weight-normal"
                                         <> " has-tooltip-multiline has-tooltip-right")
                                     # set UI.href "#"
                                     # set UI.target "_blank"
                                     # set dataTooltip "Browse cardano-node repository on this commit"
                                     # set text "—"
                         , image "rt-view-href-icon" externalLinkSVG
                         ]
  addNodeCell "protocol" [ UI.span ## (id' <> "__node-protocol")
                                   # set text "—"
                         ]
  addNodeCell "era" [ UI.span ## (id' <> "__node-era")
                              # set text "—"
                    ]
  addNodeCell "epoch" [ string "#"
                      , UI.span ## (id' <> "__node-epoch-num") # set text "—"
                      , image "has-tooltip-multiline has-tooltip-top rt-view-epoch-end" endSVG
                              # set dataTooltip "End date of this epoch"
                      , UI.span ## (id' <> "__node-epoch-end") # set text "—"
                      ]
  addNodeCell "sync" [ UI.span ## (id' <> "__node-sync-progress")
                               # set text "—"
                     ]
  addNodeCell "system-start-time" [ UI.span ## (id' <> "__node-system-start-time")
                                            # set text "—"
                                  ]
  addNodeCell "start-time"        [ UI.span ## (id' <> "__node-start-time")
                                            # set text "—"
                                  ]
  addNodeCell "uptime"   [ UI.span ## (id' <> "__node-uptime")
                                   # set text "—"
                         ]
  addNodeCell "logs"     [ UI.span ## (id' <> "__node-logs")
                                   #+ ls
                         ]
  addNodeCell "block-replay" [ UI.span ## (id' <> "__node-block-replay")
                                       # set html "0&nbsp;%"
                             ]
  addNodeCell "chunk-validation" [ UI.span ## (id' <> "__node-chunk-validation")
                                           # set text "—"
                                 ]
  addNodeCell "update-ledger-db" [ UI.span ## (id' <> "__node-update-ledger-db")
                                           # set html "0&nbsp;%"
                                 ]
  addNodeCell "peers" [ UI.div #. "buttons has-addons" #+
                          [ UI.button ## (id' <> "__node-peers-num")
                                      #. "button is-static"
                                      # set text "—"
                          , element peersDetailsButton
                          ]
                      , element peersTable
                      ]
  addNodeCell "errors" [ UI.div #. "buttons has-addons" #+
                           [ UI.button ## (id' <> "__node-errors-num")
                                       #. "button is-static"
                                       # set text "0"
                           , element errorsDetailsButton
                           ]
                       , element errorsTable
                       ]
  addNodeCell "leadership" [ UI.span ## (id' <> "__node-leadership")
                                     # set text "—"
                           ]
  addNodeCell "forged-blocks" [ UI.span ## (id' <> "__node-forged-blocks")
                                        # set text "—"
                              ]
  addNodeCell "cannot-forge" [ UI.span ## (id' <> "__node-cannot-forge")
                                       # set text "—"
                             ]
  addNodeCell "missed-slots" [ UI.span ## (id' <> "__node-missed-slots")
                                       # set text "—"
                             ]
  addNodeCell "current-kes-period" [ UI.span ## (id' <> "__node-current-kes-period")
                                             # set text "—"
                                   ]
  addNodeCell "op-cert-expiry-kes-period" [ UI.span ## (id' <> "__node-op-cert-expiry-kes-period")
                                                    # set text "—"
                                          ]
  addNodeCell "remaining-kes-periods" [ UI.span ## (id' <> "__node-remaining-kes-periods")
                                                # set text "—"
                                      ]
  addNodeCell "op-cert-start-kes-period" [ UI.span ## (id' <> "__node-op-cert-start-kes-period")
                                                   # set text "—"
                                         ]
  addNodeCell "days-until-op-cert-renew" [ UI.span ## (id' <> "__node-days-until-op-cert-renew")
                                                   # set text "—"
                                         ]
  addNodeCell "ekg-metrics" [ UI.div #. "buttons has-addons" #+
                                [ UI.button ## (id' <> "__node-ekg-metrics-num")
                                            #. "button is-static"
                                            # set text "—"
                                , element ekgMetricsButton
                                ]
                            , element ekgMetricsWindow
                            ]
 where
  addNodeCellH rowId cellContent =
    whenJustM (UI.getElementById window ("node-" <> rowId <> "-row")) $ \el ->
      void $ element el #+ [ UI.th #. (unpack anId <> "__column_cell")
                                   #+ cellContent
                           ]
  addNodeCell rowId cellContent =
    whenJustM (UI.getElementById window ("node-" <> rowId <> "-row")) $ \el ->
      void $ element el #+ [ UI.td #. (unpack anId <> "__column_cell")
                                   #+ cellContent
                           ]

-- | The new node is already connected, so we can display its logging settings.
logsSettings
  :: NonEmpty LoggingParams
  -> String
  -> UI [UI Element]
logsSettings loggingConfig anId =
  forM (NE.toList loggingConfig) $ \(LoggingParams root mode format) ->
    case mode of
      FileMode -> do
        let pathToSubdir = root </> anId

        copyPath <- UI.button #. "button is-info"
                               #+ [image "rt-view-copy-icon-on-button" copySVG]
        on UI.click copyPath . const $
          copyTextToClipboard pathToSubdir

        return $
          UI.p #+
            [ UI.div #. "field has-addons" #+
                [ UI.p #. "control" #+
                    [ UI.button #. "button is-static"
                                # set text (if format == ForHuman then "LOG" else "JSON")
                    ]
                , UI.p #. "control" #+
                    [ UI.input #. "input rt-view-logs-input"
                               # set UI.type_ "text"
                               # set (UI.attr "readonly") "readonly"
                               # set UI.value pathToSubdir
                    ]
                , UI.p #. "control" #+
                    [ element copyPath
                    ]
                ]
            ]
      JournalMode -> do
        copyId <- UI.button #. "button is-info"
                               #+ [image "rt-view-copy-icon" copySVG]
        on UI.click copyId . const $
          copyTextToClipboard anId

        return $
          UI.p #+
            [ UI.div #. "field has-addons" #+
                [ UI.p #. "control" #+
                    [ UI.button #. "button is-static"
                                # set text "JRNL"
                    ]
                , UI.p #. "control" #+
                    [ UI.input #. "input rt-view-logs-input"
                               # set UI.type_ "text"
                               # set (UI.attr "readonly") "readonly"
                               # set UI.value anId
                    ]
                , UI.p #. "control" #+
                    [ element copyId
                    ]
                ]
            ]

-- | The node was disconnected, so its column should be deleted.
deleteNodeColumn
  :: UI.Window
  -> NodeId
  -> UI ()
deleteNodeColumn window (NodeId anId) = do
  let className = anId <> "__column_cell"
  findByClassAndDo window className delete'
