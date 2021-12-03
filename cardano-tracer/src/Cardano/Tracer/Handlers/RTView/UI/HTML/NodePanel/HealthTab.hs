{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.NodePanel.HealthTab
  ( mkHealthTab
  ) where

import           Control.Monad (forM_, void, when)
import           Data.List (partition)
import           Data.Text (Text, breakOn, pack, unpack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

mkHealthTab :: UI.Window -> Text -> Int -> UI Element
mkHealthTab window anId ix =
  UI.div ## (unpack anId <> "__tab-content-" <> show ix)
         #. "panel-block is-size-5 rt-view-node-panel-block" #+
    [ UI.div #. "columns is-variable is-2-mobile is-3-desktop is-5-widescreen rt-view-node-panel-cols" #+
        [ UI.div #. "column is-half has-text-right" #+
            [ UI.p #+ [ string "Protocol" ]     #. "mb-1"
            , UI.p #+ [ string "Version" ]      #. "mb-1"
            , UI.p #+ [ string "Commit" ]       #. "mb-1"
            , UI.p #+ [ string "Platform" ]
            , UI.p #+ []                        #. "rt-view-vspace-with-hr"
            , UI.p #+ [ string "Start time" ]   #. "mb-1 mt-3"
            , UI.p #+ [ string "Uptime" ]
            , UI.p #+ []                        #. "rt-view-vspace-with-hr"
            , UI.p #+ [ string "Connected to" ] #. "mb-1 mt-3"
            ]
        , UI.div #. "column is-half has-text-weight-semibold" #+
            [ UI.p #+ [image "rt-view-overview-icon" protocolSVG, string . unpack $ "Shelley"]
                   #. "mb-1"
            , UI.p #+ [image "rt-view-overview-icon" versionSVG,  string "1.27.0"]  #. "mb-1"
            , UI.p #. "mb-1" #+
                [ image "rt-view-overview-icon" commitSVG
                , UI.anchor #. "rt-view-href has-tooltip-multiline has-tooltip-right"
                            # set UI.href "#"
                            # set dataTooltip "Browse cardano-node repository on this commit"
                            # set UI.text "df98476"
                ]
            , UI.p #+ [image "rt-view-overview-icon" linuxSVG,    string "Linux"]
            , UI.p #. "rt-view-vspace-for-hr" #+ []
            , UI.p #. "mb-1 mt-3" #+ [image "rt-view-overview-icon" calendarSVG, string "2021-08-03 10:24:01 UTC"]
            , UI.p #+ [image "rt-view-overview-icon" clockSVG,    string "00:11:05"]
            , UI.p #. "rt-view-vspace-for-hr" #+ []
            , UI.p #. "mb-1 mt-3" #+
                [ image "rt-view-overview-icon" connectedSVG
                , UI.span #. "tag is-success is-light is-medium is-family-monospace" # set text (unpack aSocket)
                ]
            ]
        ]
    ]
 where
  aSocket = fst $ breakOn "@" anId
