{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.NodePanel.Tabs
  ( mkNodePanelTabs
  ) where

import           Control.Monad (forM_, void, when)
import           Data.List (partition)
import           Data.Text (Text, breakOn, pack, unpack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

mkNodePanelTabs :: UI.Window -> Text -> UI Element
mkNodePanelTabs window anId = do
  let mkTab icon tag =
        UI.li #+
          [ UI.anchor #+
              [ image "rt-view-node-tab-icon" icon
              , string tag
              ]
          ]

  healthTab  <- mkTab healthSVG "Health" #. "is-active"
  nodeTab    <- mkTab overviewSVG "Node"
  kesTab     <- mkTab kesSVG "KES"
  peersTab   <- mkTab peersSVG "Peers"
  chainTab   <- mkTab blockchainSVG "Chain"
  txTab      <- UI.li #+ [UI.anchor #+ [string "₳" #. "rt-view-ada-node-icon", string "Tx"]]
  errorsTab  <- mkTab errorsSVG "Errors"
  metricsTab <- mkTab errorsSVG "Metrics"
  stakingTab <- mkTab errorsSVG "Staking"
  gcTab      <- mkTab rtsGCSVG "GC"

  let tabs =
        [ healthTab
        , nodeTab
        , kesTab
        , peersTab
        , chainTab
        , txTab
        , errorsTab
        , metricsTab
        , stakingTab
        , gcTab
        ] `zip` [1..]
      tabPrefixId = anId <> "__tab-content-"

  forM_ tabs $ \(tab, ix :: Int) -> do
    on UI.click tab . const $ do
      let (tabWeNeed, otherTabs) = partition (\(_, ix') -> ix' == ix) tabs
      forM_ otherTabs $ \(otherTab, ix') -> do
        findAndHide window (tabPrefixId <> pack (show ix'))
        void $ element otherTab #. ""
      forM_ tabWeNeed $ \(_, ix') -> do
        findAndShow window (tabPrefixId <> pack (show ix'))
        void $ element tab #. "is-active"

  UI.div #. "tabs is-centered rt-view-node-panel-tabs" #+
    [ UI.ul #+
        [ element healthTab
        , element nodeTab
        , element kesTab
        , element peersTab
        , element chainTab
        , element txTab
        , element errorsTab
        , element metricsTab
        , element stakingTab
        , element gcTab
        ]
    ]
