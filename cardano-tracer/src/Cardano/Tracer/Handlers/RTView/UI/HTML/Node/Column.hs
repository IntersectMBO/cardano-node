{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Column
  ( addNodeColumn
  , deleteNodeColumn
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Node.EKG
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Peers
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.JS.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

import           Control.Monad (forM, void)
import           Control.Monad.Extra (whenJustM)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Text (unpack)
import           System.FilePath ((</>))

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

-- | For every connected node the new column should be added.
addNodeColumn
  :: TracerEnv
  -> NonEmpty LoggingParams
  -> NodeId
  -> UI ()
addNodeColumn tracerEnv loggingConfig nodeId@(NodeId anId) = do
  nodeName <- liftIO $ askNodeName tracerEnv nodeId

  let id' = unpack anId

  bi <- basicInfo id'
  st <- startTime id'
  ut <- nodeUptime id'
  ls <- logsSettings loggingConfig nodeName
  leadership <- nodeLeadership id'
  epoch <- nodeEpoch id'
  kes <- nodeKES id'
  opCert <- nodeOpCert id'

  peersTable <- mkPeersTable id'
  peersDetailsButton <- UI.button ## (id' <> "__node-peers-details-button")
                                  #. "button is-info"
                                  # set UI.enabled False
                                  # set text "Details"
  on UI.click peersDetailsButton . const $ fadeInModal peersTable

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
                         , image "rt-view-node-producer-label has-tooltip-multiline has-tooltip-right" forgeSVG
                                 ## (id' <> "__node-producer-label")
                                 # set dataTooltip "This node is a producer"
                                 # hideIt
                         ]
  addNodeCell "basic-info" bi
  addNodeCell "era" [ UI.span ## (id' <> "__node-era") #. "has-text-weight-semibold" # set text "—"
                    ]
  addNodeCell "epoch" epoch
  addNodeCell "block-replay" [ UI.span ## (id' <> "__node-block-replay")
                                       # set html "0&nbsp;%"
                             ]
  addNodeCell "sync" [ UI.span ## (id' <> "__node-sync-progress")
                               # set text "—"
                     ]
  addNodeCell "start-time" st
  addNodeCell "uptime" ut
  addNodeCell "logs" ls
  addNodeCell "peers" [ UI.div #. "buttons has-addons" #+
                          [ UI.button ## (id' <> "__node-peers-num")
                                      #. "button is-static"
                                      # set text "—"
                          , element peersDetailsButton
                          ]
                      , element peersTable
                      ]
  addNodeCell "leadership" leadership
  addNodeCell "kes" kes
  addNodeCell "op-cert" opCert
  addNodeCell "ekg-metrics" [ UI.div #. "buttons has-addons" #+
                                [ UI.button ## (id' <> "__node-ekg-metrics-num")
                                            #. "button is-static"
                                            # set text "—"
                                , element ekgMetricsButton
                                ]
                            , element ekgMetricsWindow
                            ]
 where
  addNodeCellH rowId cellContent = do
    window <- askWindow
    whenJustM (UI.getElementById window ("node-" <> rowId <> "-row")) $ \el ->
      void $ element el #+ [ UI.th #. (unpack anId <> "__column_cell")
                                   #+ cellContent
                           ]
  addNodeCell rowId cellContent = do
    window <- askWindow
    whenJustM (UI.getElementById window ("node-" <> rowId <> "-row")) $ \el ->
      void $ element el #+ [ UI.td #. (unpack anId <> "__column_cell")
                                   #+ cellContent
                           ]

basicInfo :: String -> UI [UI Element]
basicInfo id' = return
  [ UI.div #. "field is-grouped" #+
      [ UI.p #. "control" #+
          [ UI.div #. "tags has-addons" #+
              [ UI.span #. "tag is-info is-medium" #+ [image "rt-view-info-icon-on-button" versionSVG]
              , UI.span ## (id' <> "__node-version")
                        #. "tag is-medium has-tooltip-multiline has-tooltip-top"
                        # set dataTooltip "Node's version"
                        # set text "—"
              ]
          ]
      , UI.p #. "control" #+
          [ UI.div #. "tags has-addons" #+
              [ UI.span #. "tag is-link is-medium" #+ [image "rt-view-info-icon-on-button" commitSVG]
              , UI.span ## (id' <> "__node-commit")
                        #. "tag is-medium is-family-monospace has-tooltip-multiline has-tooltip-top"
                        # set dataTooltip "Node's commit hash"
                        # set text "—"
              ]
          ]
      , UI.p #. "control" #+
          [ UI.div #. "tags has-addons" #+
              [ UI.span #. "tag is-success is-medium" #+ [image "rt-view-info-icon-on-button" protocolSVG]
              , UI.span ## (id' <> "__node-protocol")
                        #. "tag is-medium has-tooltip-multiline has-tooltip-top"
                        # set dataTooltip "Node's protocol"
                        # set text "—"
              ]
          ]
      ]
  ]

nodeEpoch :: String -> UI [UI Element]
nodeEpoch id' = return
  [ UI.div #. "field is-grouped" #+
      [ UI.p #. "control" #+
          [ UI.div #. "tags has-addons" #+
              [ UI.span #. "tag is-info is-medium" #+ [image "rt-view-leader-icon-2-on-button" hashtagSVG]
              , UI.span ## (id' <> "__node-epoch-num")
                        #. "tag is-medium"
                        # set text "—"
              ]
          ]
      , UI.p #. "control" #+
          [ UI.div #. "tags has-addons has-tooltip-multiline has-tooltip-top"
                   # set dataTooltip "Current epoch ends..."
                   #+
              [ UI.span #. "tag is-info is-medium" #+ [image "rt-view-leader-icon-2-on-button" endSVG]
              , UI.span ## (id' <> "__node-epoch-end")
                        #. "tag is-medium"
                        # set text "—"
              ]
          ]
      , UI.p #. "control" #+
          [ UI.div #. "rt-view-progress-container has-tooltip-multiline has-tooltip-top"
                   # set dataTooltip "Current epoch progress"
                   #+
              [ UI.div #. "rt-view-progress-container-text" #+
                  [ UI.div ## (id' <> "__node-epoch-progress-label")
                           #. "rt-view-progress-container-text-label"
                           # set text "0%"
                  ]
              , UI.div ## (id' <> "__node-epoch-progress")
                       #. "rt-view-loading-bar"
              ]
          ]
      ]
  ]

nodeKES :: String -> UI [UI Element]
nodeKES id' = return
  [ UI.div #. "field is-grouped" #+
      [ UI.p #. "control" #+
          [ UI.div #. "tags has-addons has-tooltip-multiline has-tooltip-top"
                   # set dataTooltip "KES current"
                   #+
              [ UI.span #. "tag is-info is-medium" #+ [image "rt-view-leader-icon-2-on-button" cSVG]
              , UI.span ## (id' <> "__node-current-kes-period")
                        #. "tag is-medium"
                        # set text "—"
              ]
          ]
      , UI.p #. "control" #+
          [ UI.div #. "tags has-addons has-tooltip-multiline has-tooltip-top"
                   # set dataTooltip "KES expiry"
                   #+
              [ UI.span #. "tag is-info is-medium" #+ [image "rt-view-leader-icon-3-on-button" eSVG]
              , UI.span ## (id' <> "__node-op-cert-expiry-kes-period")
                        #. "tag is-medium"
                        # set text "—"
              ]
          ]
      , UI.p #. "control" #+
          [ UI.div #. "tags has-addons has-tooltip-multiline has-tooltip-top"
                   # set dataTooltip "KES remaining"
                   #+
              [ UI.span #. "tag is-info is-medium" #+ [image "rt-view-leader-icon-3-on-button" rSVG]
              , UI.span ## (id' <> "__node-remaining-kes-periods")
                        #. "tag is-medium"
                        # set text "—"
              ]
          ]
      ]
  ]

nodeOpCert :: String -> UI [UI Element]
nodeOpCert id' = return
  [ UI.div #. "field is-grouped" #+
      [ UI.p #. "control" #+
          [ UI.div #. "tags has-addons has-tooltip-multiline has-tooltip-top"
                   # set dataTooltip "Op Cert start KES"
                   #+
              [ UI.span #. "tag is-info is-medium" #+ [image "rt-view-leader-icon-2-on-button" start2SVG]
              , UI.span ## (id' <> "__node-op-cert-start-kes-period")
                        #. "tag is-medium"
                        # set text "—"
              ]
          ]
      , UI.p #. "control" #+
          [ UI.div #. "tags has-addons has-tooltip-multiline has-tooltip-top"
                   # set dataTooltip "Days until Op Cert renew"
                   #+
              [ UI.span #. "tag is-danger is-medium" #+ [image "rt-view-leader-icon-2-on-button" endSVG]
              , UI.span ## (id' <> "__node-days-until-op-cert-renew")
                        #. "tag is-medium"
                        # set text "—"
              ]
          ]
      ]
  ]

nodeLeadership :: String -> UI [UI Element]
nodeLeadership id' = return
  [ UI.div #. "field is-grouped" #+
      [ UI.p #. "control" #+
          [ UI.div #. "tags has-addons has-tooltip-multiline has-tooltip-top"
                   # set dataTooltip "How many times this node was a leader"
                   #+
              [ UI.span #. "tag is-info is-medium" #+ [image "rt-view-leader-icon-on-button" leaderSVG]
              , UI.span ## (id' <> "__node-leadership")
                        #. "tag is-medium"
                        # set text "—"
              ]
          ]
      , UI.p #. "control" #+
          [ UI.div #. "tags has-addons has-tooltip-multiline has-tooltip-top"
                   # set dataTooltip "How many blocks were forge by this node"
                   #+
              [ UI.span #. "tag is-info is-medium" #+ [image "rt-view-leader-icon-on-button" forgeSVG]
              , UI.span ## (id' <> "__node-forged-blocks")
                        #. "tag is-medium"
                        # set text "—"
              ]
          ]
      , UI.p #. "control" #+
          [ UI.div #. "tags has-addons has-tooltip-multiline has-tooltip-top"
                   # set dataTooltip "How many times this node could not forge"
                   #+
              [ UI.span #. "tag is-danger is-medium" #+ [image "rt-view-leader-icon-on-button" notForgeSVG]
              , UI.span ## (id' <> "__node-cannot-forge")
                        #. "tag is-medium"
                        # set text "—"
              ]
          ]
      , UI.p #. "control" #+
          [ UI.div #. "tags has-addons has-tooltip-multiline has-tooltip-top"
                   # set dataTooltip "How many slots were missed by this node"
                   #+
              [ UI.span #. "tag is-danger is-medium" #+ [image "rt-view-leader-icon-2-on-button" missedSVG]
              , UI.span ## (id' <> "__node-missed-slots")
                        #. "tag is-medium"
                        # set text "—"
              ]
          ]
      ]
  ]

startTime :: String -> UI [UI Element]
startTime id' = return
  [ UI.div #. "field is-grouped" #+
      [ UI.p #. "control" #+
          [ UI.div #. "tags has-addons has-tooltip-multiline has-tooltip-top"
                   # set dataTooltip "Blockchain's start time in UTC"
                   #+
              [ UI.span #. "tag is-primary is-medium" #+ [image "rt-view-leader-icon-2-on-button" bSVG]
              , UI.span ## (id' <> "__node-system-start-time")
                        #. "tag is-medium"
                        # set text "—"
              ]
          ]
      , UI.p #. "control" #+
          [ UI.div #. "tags has-addons has-tooltip-multiline has-tooltip-top"
                   # set dataTooltip "Node's start time in UTC"
                   #+
              [ UI.span #. "tag is-primary is-medium" #+ [image "rt-view-leader-icon-2-on-button" nSVG]
              , UI.span ## (id' <> "__node-start-time")
                        #. "tag is-medium"
                        # set text "—"
              ]
          ]
      ]
  ]

nodeUptime :: String -> UI [UI Element]
nodeUptime id' = return
  [ UI.div #. "field is-grouped" #+
      [ UI.p #. "control" #+
          [ UI.span ## (id' <> "__node-uptime-days")  #. "tag is-medium is-link" # set text "—"
          ]
      , UI.p #. "control" #+
          [ UI.span ## (id' <> "__node-uptime-hours") #. "tag is-medium is-link" # set text "—"
          ]
      , UI.p #. "control" #+
          [ UI.span ## (id' <> "__node-uptime-mins")  #. "tag is-medium is-link" # set text "—"
          ]
      , UI.p #. "control" #+
          [ UI.span ## (id' <> "__node-uptime-secs")  #. "tag is-medium is-link" # set text "—"
          ]
      ]
  ]

-- | The new node is already connected, so we can display its logging settings.
logsSettings
  :: NonEmpty LoggingParams
  -> NodeName
  -> UI [UI Element]
logsSettings loggingConfig nodeName =
  forM (NE.toList loggingConfig) $ \(LoggingParams root mode format) ->
    case mode of
      FileMode -> do
        let pathToSubdir = root </> unpack nodeName

        copyPath <- UI.button #. "button is-info"
                               #+ [image "rt-view-copy-icon-on-button" copySVG]
        on UI.click copyPath . const $
          copyTextToClipboard pathToSubdir

        return $
          UI.div #. "field has-addons" #+
            [ UI.p #. "control" #+
                [ UI.button #. "button is-static"
                            # set html (if format == ForHuman then "&nbsp;LOG&nbsp;" else "JSON")
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
      JournalMode -> do
        copyId <- UI.button #. "button is-info"
                               #+ [image "rt-view-copy-icon" copySVG]
        on UI.click copyId . const $
          copyTextToClipboard (unpack nodeName)

        return $
          UI.div #. "field has-addons" #+
            [ UI.p #. "control" #+
                [ UI.button #. "button is-static"
                            # set text "JRNL"
                ]
            , UI.p #. "control" #+
                [ UI.input #. "input rt-view-logs-input"
                           # set UI.type_ "text"
                           # set (UI.attr "readonly") "readonly"
                           # set UI.value (unpack nodeName)
                ]
            , UI.p #. "control" #+
                [ element copyId
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
