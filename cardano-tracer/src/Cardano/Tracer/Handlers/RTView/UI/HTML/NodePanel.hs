{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.NodePanel
  ( addNodePanel
  ) where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, liftIO)

import           Trace.Forward.Protocol.Type (NodeInfo (..))

import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

addNodePanel
  :: UI.Window
  -> NodeId
  -> NodeInfo
  -> UI ()
addNodePanel window nodeId nodeInfo = do
  _nodePanel <- mkNodePanel window nodeInfo nodeInfo
  return ()



{-
              UI.div #. "panel" #+
                [ UI.p #. "panel-heading" #+
                    [ UI.div #. "columns" #+
                        [ UI.div #. "column" #+
                            [ string "Node: core-1"
                            ]
                        , UI.div #. "column has-text-right" #+
                            [ image "rt-view-node-panel-down" downSVG
                            ]
                        ]
                    ]
                , UI.p #. "panel-tabs is-size-5" #+
                    [ UI.anchor #. "is-active" #+ [image "rt-view-node-tab-icon" overviewSVG,   string "Overview"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" kesSVG,        string "KES"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" peersSVG,      string "Peers"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" blockchainSVG, string "Blockchain"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" mempoolSVG,    string "Mempool"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" errorsSVG,     string "Errors"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" rtsGCSVG,      string "RTS GC"]
                    ]
                , UI.div #. "panel-block is-size-5 rt-view-node-panel-block" #+
                    [ UI.div #. "columns is-variable is-2-mobile is-3-desktop is-5-widescreen rt-view-node-panel-cols" #+
                        [ UI.div #. "column is-half has-text-right" #+
                            [ UI.p #. "mb-1" #+ [ string "Node protocol" ]
                            , UI.p #. "mb-1" #+ [ string "Node version" ]
                            , UI.p #. "mb-1" #+ [ string "Node commit" ]
                            , UI.p #. "mb-1" #+ [ string "Node platform" ]
                            , UI.p #. "mb-1" #+ [ string "Node start time" ]
                            , UI.p           #+ [ string "Node uptime" ]
                            ]
                        , UI.div #. "column is-half has-text-weight-semibold" #+
                            [ UI.p #. "mb-1" #+ [image "rt-view-overview-icon" protocolSVG, string "Shelley"]
                            , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" versionSVG,  string "1.0"]
                            , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" commitSVG,   string "abcdefg"]
                            , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" linuxSVG,    string "Linux"]
                            , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" calendarSVG, string "2021-01-01 01:01:01 UTC"]
                            , UI.p           #+ [image "rt-view-overview-icon" clockSVG,    string "00:11:05"]
                            ]
                        ]
                    ]
                ]
-}

