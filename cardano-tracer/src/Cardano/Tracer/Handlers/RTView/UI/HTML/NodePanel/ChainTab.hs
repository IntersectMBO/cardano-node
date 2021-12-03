{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.NodePanel.Add
  ( addNodePanel
  ) where

import           Control.Monad (forM_, void, when)
import           Data.List (partition)
import           Data.Text (Text, breakOn, pack, unpack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

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
      [ mkPanelHeading window anId
      , UI.div ## (unpack anId <> "__node-panel-content")
               #. "rt-view-node-panel-content" #+
          [ mkPanelTabs window anId
          , mkPanelTabsContent window anId
          ]
      ]

mkPanelHeading :: UI.Window -> Text -> UI Element
mkPanelHeading window anId = do
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

mkPanelTabs :: UI.Window -> Text -> UI Element
mkPanelTabs window anId = do
  let mkTab icon tag =
        UI.li #+
          [ UI.anchor #+
              [ image "rt-view-node-tab-icon" icon
              , string tag
              ]
          ]

  healthTab <- mkTab healthSVG "Health" #. "is-active"
  nodeTab   <- mkTab overviewSVG "Node"
  kesTab    <- mkTab kesSVG "KES"
  peersTab  <- mkTab peersSVG "Peers"
  chainTab  <- mkTab blockchainSVG "Chain"
  txTab     <- UI.li #+ [UI.anchor #+ [string "₳" #. "rt-view-ada-node-icon", string "Tx"]]
  bugsTab   <- mkTab errorsSVG "Bugs"
  gcTab     <- mkTab rtsGCSVG "GC"

  let tabs =
        [ healthTab
        , nodeTab
        , kesTab
        , peersTab
        , chainTab
        , txTab
        , bugsTab
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
        , element bugsTab
        , element gcTab
        ]
    ]

mkPanelTabsContent :: UI.Window -> Text -> UI Element
mkPanelTabsContent window anId =
  UI.div #+
    [ mkHealthTab window anId 1
    --, mkNodeTab
    --, mkKESTab
    , mkPeersTab  window anId 4
    --, mkChainTab
    --, mkTXTab
    --, mkBugsTab
    --, mkGCTab
    ]

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



  {-
              [ 
              , 
              , UI.div #. "panel-block is-size-5 rt-view-node-panel-block" #+
                  [ UI.div #. "table-container rt-view-peers-table-container" #+
                      [ UI.table #. "table rt-view-peers-table" #+
                          [ UI.mkElement "thead" #+
                              [ UI.tr #+
                                  [ UI.th #+ [string "Endpoint"]
                                  , UI.th #+ [string "Slots number"]
                                  , UI.th #. "rt-view-narrow-th" #+ [UI.mkElement "abbr" # set UI.title__ "Bytes in flight" #+ [string "Bts"]]
                                  , UI.th #. "rt-view-narrow-th" #+ [UI.mkElement "abbr" # set UI.title__ "Requests in flight" #+ [string "Req"]]
                                  , UI.th #. "rt-view-narrow-th" #+ [UI.mkElement "abbr" # set UI.title__ "Blocks in flight" #+ [string "Blk"]]
                                  , UI.th #+ [string "Status"]
                                  ]
                              ]
                          , UI.mkElement "tbody" #+
                              [ UI.tr #+
                                  [ UI.td #+ [UI.span #. "tag is-success is-light is-medium is-family-monospace" # set text "127.0.0.1:8100"]
                                  , UI.td #+ [string "346"]
                                  , UI.td #+ [string "1"]
                                  , UI.td #+ [string "0"]
                                  , UI.td #+ [string "0"]
                                  , UI.td #+ [UI.span #. "tag is-success is-medium" # set text "Ready"]
                                  ]
                              , UI.tr #+
                                  [ UI.td #+ [UI.span #. "tag is-success is-light is-medium is-family-monospace" # set text "127.0.0.1:8200"]
                                  , UI.td #+ [string "346"]
                                  , UI.td #+ [string "1"]
                                  , UI.td #+ [string "0"]
                                  , UI.td #+ [string "1"]
                                  , UI.td #+ [UI.span #. "tag is-success is-medium" # set text "Ready"]
                                  ]
                              , UI.tr #+
                                  [ UI.td #+ [UI.span #. "tag is-success is-light is-medium is-family-monospace" # set text "127.0.0.1:8300"]
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
              ]
         , UI.div #. "panel is-link rt-view-node-panel" #+
              [ UI.p #. "panel-heading" #+
                  [ UI.div #. "columns" #+
                      [ UI.div #. "column rt-view-node-name-column" #+
                          [ string "Name:" #. "rt-view-node-name"
                          , string "core-2"
                          , UI.span #. "tag is-warning is-medium ml-4" # set text "Producer"
                          ]
                      , UI.div #. "column has-text-right" #+
                          [ image "rt-view-node-panel-down" downSVG
                          ]
                      ]
                  ]
              , UI.div #. "tabs is-centered is-boxed is-size-5 mt-1 rt-view-tabs" #+
                  [ UI.ul #+
                      [ UI.li #. "is-active" #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" healthSVG,     string "Health"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" overviewSVG,   string "Node"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" kesSVG,        string "KES"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" peersSVG,      string "Peers"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" blockchainSVG, string "Chain"] ]
                      , UI.li                #+ [ UI.anchor #+ [string "₳" #. "rt-view-ada-node-icon",       string "Tx"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" errorsSVG,     string "Bugs"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" rtsGCSVG,      string "GC"] ]
                      ]
                  ]
              , UI.div #. "panel-block is-size-5 rt-view-node-panel-block" #+
                  [ UI.div #. "columns is-variable is-2-mobile is-3-desktop is-5-widescreen rt-view-node-panel-cols" #+
                      [ UI.div #. "column is-half has-text-right" #+
                          [ UI.p #. "mb-1" #+ [ string "Node version" ]
                          , UI.p #+ [ string "Node uptime" ]
                          , UI.p #. "rt-view-vspace-with-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [ string "Epoch", string "/" #. "rt-view-slash", string "Slot" ]
                          , UI.p #. "mb-1" #+ [ string "Minted blocks, number" ]
                          , UI.p #+ [ string "KES periods remaining" ]
                          , UI.p #. "rt-view-vspace-with-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [ string "Connected peers" ]
                          ]
                      , UI.div #. "column is-half has-text-weight-semibold" #+
                          [ UI.p #. "mb-1" #+ [image "rt-view-overview-icon" versionSVG, string "1.27.0"]
                          , UI.p #+ [image "rt-view-overview-icon" clockSVG, string "00:12:15"]
                          , UI.p #. "rt-view-vspace-for-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+
                              [ image "rt-view-overview-icon" epochSlotSVG
                              , string "125", string "/" #. "rt-view-slash", string "67"
                              ]
                          , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" hammerSVG, string "16"]
                          , UI.p #+ [image "rt-view-overview-icon" remainingSVG, string "230"]
                          , UI.p #. "rt-view-vspace-for-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [image "rt-view-overview-icon" peersNumSVG, string "3"]
                          ]
                      ]
                  ]
              ]
         , UI.div #. "panel is-link rt-view-node-panel" #+
              [ UI.p #. "panel-heading" #+
                  [ UI.div #. "columns" #+
                      [ UI.div #. "column" #+
                          [ string "Name:" #. "rt-view-node-name"
                          , string "relay-1"
                          , UI.span #. "tag is-light is-medium ml-4" # set text "Relay"
                          ]
                      , UI.div #. "column has-text-right" #+
                          [ image "rt-view-node-panel-down" downSVG
                          ]
                      ]
                  ]
              , UI.div #. "tabs is-centered is-boxed is-size-5 mt-1 rt-view-tabs" #+
                  [ UI.ul #+
                      [ UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" healthSVG,     string "Health"] ]
                      , UI.li #. "is-active" #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" overviewSVG,   string "Node"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" kesSVG,        string "KES"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" peersSVG,      string "Peers"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" blockchainSVG, string "Chain"] ]
                      , UI.li                #+ [ UI.anchor #+ [string "₳" #. "rt-view-ada-node-icon",       string "Tx"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" errorsSVG,     string "Bugs"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" rtsGCSVG,      string "GC"] ]
                      ]
                  ]
              , 
              ]
         , UI.div #. "panel is-link rt-view-node-panel" #+
              [ UI.p #. "panel-heading" #+
                  [ UI.div #. "columns" #+
                      [ UI.div #. "column" #+
                          [ string "Name:" #. "rt-view-node-name"
                          , string "relay-2"
                          , UI.span #. "tag is-light is-medium ml-4" # set text "Relay"
                          ]
                      , UI.div #. "column has-text-right" #+
                          [ image "rt-view-node-panel-down" downSVG
                          ]
                      ]
                  ]
              , UI.div #. "tabs is-centered is-boxed is-size-5 mt-1 rt-view-tabs" #+
                  [ UI.ul #+
                      [ UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" healthSVG,     string "Health"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" overviewSVG,   string "Node"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" kesSVG,        string "KES"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" peersSVG,      string "Peers"] ]
                      , UI.li #. "is-active" #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" blockchainSVG, string "Chain"] ]
                      , UI.li                #+ [ UI.anchor #+ [string "₳" #. "rt-view-ada-node-icon",       string "Tx"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" errorsSVG,     string "Bugs"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" rtsGCSVG,      string "GC"] ]
                      ]
                  ]
              , UI.div #. "panel-block is-size-5 rt-view-node-panel-block" #+
                  [ UI.div #. "columns is-variable is-2-mobile is-3-desktop is-5-widescreen rt-view-node-panel-cols" #+
                      [ UI.div #. "column is-half has-text-right" #+
                          [ UI.p #+ [ string "Blockchain start time" ]
                          , UI.p #. "rt-view-vspace-with-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [ string "Epoch", string "/" #. "rt-view-slash", string "Slot" ]
                          , UI.p #. "mb-1" #+ [ string "Blocks number" ]
                          , UI.p #+ [ string "Chain density" ]
                          , UI.p #. "rt-view-vspace-with-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [ string "Slot leader, number" ]
                          , UI.p #+ [ string "Minted blocks number" ]
                          , UI.p #. "rt-view-vspace-with-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [ string "Cannot mint, number" ]
                          , UI.p #+ [ string "Missed slots, number" ]
                          ]
                      , UI.div #. "column is-half has-text-weight-semibold" #+
                          [ UI.p           #+ [image "rt-view-overview-icon" calendarSVG, string "2021-08-03 10:24:01 UTC"]
                          , UI.p #. "rt-view-vspace-for-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+
                              [ image "rt-view-overview-icon" epochSlotSVG
                              , string "124", string "/" #. "rt-view-slash", string "5"
                              ]
                          , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" blockSVG, string "568"]
                          , UI.p #+ [image "rt-view-overview-icon" chainSVG, string "2.4%"]
                          , UI.p #. "rt-view-vspace-for-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [image "rt-view-overview-icon" leaderSVG, string "15"]
                          , UI.p #+ [image "rt-view-overview-icon" hammerSVG, string "15"]
                          , UI.p #. "rt-view-vspace-for-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [image "rt-view-overview-icon" sadSVG, string "—"]
                          , UI.p #+ [image "rt-view-overview-icon" sadSVG, string "2" #. "has-text-danger"]
                          ]
                      ]
                  ]
              ] 
         -}
