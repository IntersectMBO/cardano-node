{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.NodePanel.PeersTab
  ( mkPeersTab
  ) where

import           Control.Monad (forM_, void, when)
import           Data.List (partition)
import           Data.Text (Text, breakOn, pack, unpack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

mkPeersTab :: UI.Window -> Text -> Int -> UI Element
mkPeersTab window anId ix =
  UI.div ## (unpack anId <> "__tab-content-" <> show ix)
         #. "panel-block is-size-5 rt-view-node-panel-block" # hideIt #+
    [ UI.div #. "table-container rt-view-peers-table-container" #+
        [ UI.table #. "table rt-view-peers-table" #+
            [ UI.mkElement "thead" #+
                [ UI.tr #+
                    [ UI.th #+ [string "Endpoint"]
                    , UI.th #+ [string "Slots number"]
                    , UI.th #. "rt-view-narrow-th" #+
                        [UI.mkElement "abbr" # set UI.title__ "Bytes in flight" #+ [string "Bts"]]
                    , UI.th #. "rt-view-narrow-th" #+
                        [UI.mkElement "abbr" # set UI.title__ "Requests in flight" #+ [string "Req"]]
                    , UI.th #. "rt-view-narrow-th" #+
                        [UI.mkElement "abbr" # set UI.title__ "Blocks in flight" #+ [string "Blk"]]
                    , UI.th #+ [string "Status"]
                    ]
                ]
            , UI.mkElement "tbody" #+
                [ UI.tr #+
                    [ UI.td #+ [UI.span #. "tag is-success is-light is-medium is-family-monospace"
                                        # set text "127.0.0.1:8100"]
                    , UI.td #+ [string "346"]
                    , UI.td #+ [string "1"]
                    , UI.td #+ [string "0"]
                    , UI.td #+ [string "0"]
                    , UI.td #+ [UI.span #. "tag is-success is-medium" # set text "Ready"]
                    ]
                , UI.tr #+
                    [ UI.td #+ [UI.span #. "tag is-success is-light is-medium is-family-monospace"
                                        # set text "127.0.0.1:8200"]
                    , UI.td #+ [string "346"]
                    , UI.td #+ [string "1"]
                    , UI.td #+ [string "0"]
                    , UI.td #+ [string "1"]
                    , UI.td #+ [UI.span #. "tag is-success is-medium" # set text "Ready"]
                    ]
                , UI.tr #+
                    [ UI.td #+ [UI.span #. "tag is-success is-light is-medium is-family-monospace"
                                        # set text "127.0.0.1:8300"]
                    , UI.td #+ [string "344"]
                    , UI.td #+ [string "1"]
                    , UI.td #+ [string "0"]
                    , UI.td #+ [string "0"]
                    , UI.td #+ [UI.span #. "tag is-dark is-medium" # set text "Busy"]
                    ]
                ]
            ]
        ]
    ]
