{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.NodePanel.Heading
  ( mkNodePanelHeading
  ) where

import           Control.Monad (forM_, void, when)
import           Data.List (partition)
import           Data.Text (Text, breakOn, pack, unpack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

mkNodePanelHeading :: UI.Window -> Text -> UI Element
mkNodePanelHeading window anId = do
  collapseExpand <- image "rt-view-node-panel-down" downSVG # set UI.value "expanded"
  on UI.click collapseExpand . const $
    get UI.value collapseExpand >>= \case
      "expanded" -> do
        findAndHide window nodePanelContentId
        void $ element collapseExpand # set UI.value "collapsed"
      _ -> do
        findAndShow window nodePanelContentId
        void $ element collapseExpand # set UI.value "expanded"

  UI.p #. "panel-heading rt-view-node-panel-head" #+
    [ UI.div #. "columns" #+
        [ UI.div #. "column rt-view-node-name-column" #+
            [ string "Node: " #. "rt-view-node-name"
            , string . unpack $ anId
            ]
        , UI.div #. "column has-text-right" #+
            [ element collapseExpand
            ]
        ]
    ]
 where
  nodePanelContentId = anId <> "__node-panel-content"
