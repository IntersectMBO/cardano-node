{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.NodePanel.Add
  ( addNodePanel
  ) where

import           Control.Monad (void)
import           Data.Text (Text, unpack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.HTML.NodePanel.HealthTab
import           Cardano.Tracer.Handlers.RTView.UI.HTML.NodePanel.PeersTab
import           Cardano.Tracer.Handlers.RTView.UI.HTML.NodePanel.Heading
import           Cardano.Tracer.Handlers.RTView.UI.HTML.NodePanel.Tabs
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

addNodePanel
  :: UI.Window
  -> NodeId
  -> UI ()
addNodePanel window (NodeId anId) =
  findAndDo window "nodes-panels" $ \nodePanelsRoot ->
    void $ element nodePanelsRoot #+ [mkNodePanel]
 where
  mkNodePanel =
    UI.div ## unpack anId #. "panel rt-view-node-panel" #+
      [ mkNodePanelHeading window anId
      , UI.div ## (unpack anId <> "__node-panel-content")
               #. "rt-view-node-panel-content" #+
          [ mkNodePanelTabs window anId
          -- Tabs content.
          , mkHealthTab  window anId 1
          --, mkNodeTab    window anId 2
          --, mkKESTab     window anId 3
          , mkPeersTab   window anId 4
          --, mkChainTab   window anId 5
          --, mkTXTab      window anId 6
          --, mkMetricsTab window anId 7
          --, mkBugsTab    window anId 8
          --, mkGCTab      window anId 9
          ]
      ]
