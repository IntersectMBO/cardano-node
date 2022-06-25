{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Body
  ( mkPageBody
  ) where

import           Control.Monad (unless, void, when)
import           Control.Monad.Extra (whenJustM, whenM)
import           Data.Text (Text)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.JQuery (Easing (..), fadeIn, fadeOut)
import           Text.Read (readMaybe)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.UI.HTML.About
import           Cardano.Tracer.Handlers.RTView.UI.HTML.NoNodes
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Notifications
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.JS.ChartJS
import qualified Cardano.Tracer.Handlers.RTView.UI.JS.Charts as Chart
import           Cardano.Tracer.Handlers.RTView.UI.JS.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Notifications
import           Cardano.Tracer.Handlers.RTView.UI.Theme
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

mkPageBody
  :: UI.Window
  -> Network
  -> ConnectedNodes
  -> ResourcesHistory
  -> BlockchainHistory
  -> TransactionsHistory
  -> DatasetsIndices
  -> DatasetsTimestamps
  -> EventsQueues
  -> UI Element
mkPageBody window networkConfig connected
           (ResHistory rHistory) (ChainHistory cHistory) (TXHistory tHistory)
           dsIxs dsTss eventsQueues = do
  txsProcessedNumTimer <- mkChartTimer connected tHistory dsIxs dsTss TxsProcessedNumData TxsProcessedNumChart
  mempoolBytesTimer    <- mkChartTimer connected tHistory dsIxs dsTss MempoolBytesData    MempoolBytesChart
  txsInMempoolTimer    <- mkChartTimer connected tHistory dsIxs dsTss TxsInMempoolData    TxsInMempoolChart

  txsProcessedNumChart <- mkChart window txsProcessedNumTimer TxsProcessedNumChart "Processed txs"
  mempoolBytesChart    <- mkChart window mempoolBytesTimer    MempoolBytesChart    "Mempool size"
  txsInMempoolChart    <- mkChart window txsInMempoolTimer    TxsInMempoolChart    "Txs in mempool"

  -- Resources charts.
  cpuTimer          <- mkChartTimer connected rHistory dsIxs dsTss CPUData          CPUChart
  memoryTimer       <- mkChartTimer connected rHistory dsIxs dsTss MemoryData       MemoryChart
  gcMajorNumTimer   <- mkChartTimer connected rHistory dsIxs dsTss GCMajorNumData   GCMajorNumChart
  gcMinorNumTimer   <- mkChartTimer connected rHistory dsIxs dsTss GCMinorNumData   GCMinorNumChart
  gcLiveMemoryTimer <- mkChartTimer connected rHistory dsIxs dsTss GCLiveMemoryData GCLiveMemoryChart
  cpuTimeGCTimer    <- mkChartTimer connected rHistory dsIxs dsTss CPUTimeGCData    CPUTimeGCChart
  cpuTimeAppTimer   <- mkChartTimer connected rHistory dsIxs dsTss CPUTimeAppData   CPUTimeAppChart
  threadsNumTimer   <- mkChartTimer connected rHistory dsIxs dsTss ThreadsNumData   ThreadsNumChart

  cpuChart          <- mkChart window cpuTimer          CPUChart          "CPU usage"
  memoryChart       <- mkChart window memoryTimer       MemoryChart       "Memory usage"
  gcMajorNumChart   <- mkChart window gcMajorNumTimer   GCMajorNumChart   "Number of major GCs"
  gcMinorNumChart   <- mkChart window gcMinorNumTimer   GCMinorNumChart   "Number of minor GCs"
  gcLiveMemoryChart <- mkChart window gcLiveMemoryTimer GCLiveMemoryChart "GC, live data in heap"
  cpuTimeGCChart    <- mkChart window cpuTimeGCTimer    CPUTimeGCChart    "CPU time used by GC"
  cpuTimeAppChart   <- mkChart window cpuTimeAppTimer   CPUTimeAppChart   "CPU time used by app"
  threadsNumChart   <- mkChart window threadsNumTimer   ThreadsNumChart   "Number of threads"

  -- Blockchain charts.
  chainDensityTimer <- mkChartTimer connected cHistory dsIxs dsTss ChainDensityData ChainDensityChart
  slotNumTimer      <- mkChartTimer connected cHistory dsIxs dsTss SlotNumData      SlotNumChart
  blockNumTimer     <- mkChartTimer connected cHistory dsIxs dsTss BlockNumData     BlockNumChart
  slotInEpochTimer  <- mkChartTimer connected cHistory dsIxs dsTss SlotInEpochData  SlotInEpochChart
  epochTimer        <- mkChartTimer connected cHistory dsIxs dsTss EpochData        EpochChart

  chainDensityChart <- mkChart window chainDensityTimer ChainDensityChart "Chain density"
  slotNumChart      <- mkChart window slotNumTimer      SlotNumChart      "Slot height"
  blockNumChart     <- mkChart window blockNumTimer     BlockNumChart     "Block height"
  slotInEpochChart  <- mkChart window slotInEpochTimer  SlotInEpochChart  "Slot in epoch"
  epochChart        <- mkChart window epochTimer        EpochChart        "Epoch"

  -- Leadership charts.
  cannotForgeTimer     <- mkChartTimer' connected cHistory dsIxs dsTss NodeCannotForgeData       NodeCannotForgeChart
  forgedSlotTimer      <- mkChartTimer' connected cHistory dsIxs dsTss ForgedSlotLastData        ForgedSlotLastChart
  nodeIsLeaderTimer    <- mkChartTimer' connected cHistory dsIxs dsTss NodeIsLeaderData          NodeIsLeaderChart
  nodeIsNotLeaderTimer <- mkChartTimer' connected cHistory dsIxs dsTss NodeIsNotLeaderData       NodeIsNotLeaderChart
  forgedInvalidTimer   <- mkChartTimer' connected cHistory dsIxs dsTss ForgedInvalidSlotLastData ForgedInvalidSlotLastChart
  adoptedTimer         <- mkChartTimer' connected cHistory dsIxs dsTss AdoptedSlotLastData       AdoptedSlotLastChart
  notAdoptedTimer      <- mkChartTimer' connected cHistory dsIxs dsTss NotAdoptedSlotLastData    NotAdoptedSlotLastChart
  aboutToLeadTimer     <- mkChartTimer' connected cHistory dsIxs dsTss AboutToLeadSlotLastData   AboutToLeadSlotLastChart
  couldNotForgeTimer   <- mkChartTimer' connected cHistory dsIxs dsTss CouldNotForgeSlotLastData CouldNotForgeSlotLastChart

  cannotForgeChart     <- mkChart window cannotForgeTimer     NodeCannotForgeChart       "Cannot forge"
  forgedSlotChart      <- mkChart window forgedSlotTimer      ForgedSlotLastChart        "Forged"
  nodeIsLeaderChart    <- mkChart window nodeIsLeaderTimer    NodeIsLeaderChart          "Is leader"
  nodeIsNotLeaderChart <- mkChart window nodeIsNotLeaderTimer NodeIsNotLeaderChart       "Is not leader"
  forgedInvalidChart   <- mkChart window forgedInvalidTimer   ForgedInvalidSlotLastChart "Forged invalid"
  adoptedChart         <- mkChart window adoptedTimer         AdoptedSlotLastChart       "Is adopted"
  notAdoptedChart      <- mkChart window notAdoptedTimer      NotAdoptedSlotLastChart    "Is not adopted"
  aboutToLeadChart     <- mkChart window aboutToLeadTimer     AboutToLeadSlotLastChart   "About to lead"
  couldNotForgeChart   <- mkChart window couldNotForgeTimer   CouldNotForgeSlotLastChart "Could not forge"

  -- Visibility of charts groups.
  showHideTxs        <- image "has-tooltip-multiline has-tooltip-top rt-view-show-hide-chart-group" showSVG
                              # set dataTooltip "Click to hide Transactions"
                              # set dataState shownState
  showHideChain      <- image "has-tooltip-multiline has-tooltip-top rt-view-show-hide-chart-group" showSVG
                              # set dataTooltip "Click to hide Blockchain"
                              # set dataState shownState
  showHideLeadership <- image "has-tooltip-multiline has-tooltip-top rt-view-show-hide-chart-group" showSVG
                              # set dataTooltip "Click to hide Leadership"
                              # set dataState shownState
  showHideResources  <- image "has-tooltip-multiline has-tooltip-top rt-view-show-hide-chart-group" showSVG
                              # set dataTooltip "Click to hide Resources"
                              # set dataState shownState

  on UI.click showHideTxs . const $
    changeVisibilityForCharts window showHideTxs "transactions-charts" "Transactions"
  on UI.click showHideChain . const $
    changeVisibilityForCharts window showHideChain "chain-charts" "Blockchain"
  on UI.click showHideLeadership . const $
    changeVisibilityForCharts window showHideLeadership "leadership-charts" "Leadership"
  on UI.click showHideResources . const $
    changeVisibilityForCharts window showHideResources "resources-charts" "Resources"

  -- Body.
  body <-
    UI.getBody window #+
      [ UI.div #. "wrapper" #+
          [ UI.div ## "preloader" #. "pageloader is-active" #+
              [ UI.span #. "title" # set text "Just a second..."
              ]
          , topNavigation window eventsQueues
          , mkNoNodesInfo networkConfig
          , UI.mkElement "section" #. "section" #+
              [ UI.div ## "main-table-container"
                       #. "table-container"
                       # hideIt #+
                  [ UI.table ## "main-table" #. "table rt-view-main-table" #+
                      [ UI.mkElement "thead" #+
                          [ UI.tr ## "node-name-row" #+
                              [ UI.th #. "rt-view-main-table-description"
                                      #+ [UI.span # set html "&nbsp;"]
                              ]
                          ]
                      , UI.mkElement "tbody" #+
                          [ UI.tr ## "node-version-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" versionSVG
                                         , string "Version"
                                         ]
                              ]
                          , UI.tr ## "node-commit-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" commitSVG
                                         , string "Commit"
                                         ]
                              ]
                          , UI.tr ## "node-protocol-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" protocolSVG
                                         , string "Protocol"
                                         ]
                              ]
                          , UI.tr ## "node-era-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" eraSVG
                                         , string "Era"
                                         ]
                              ]
                          , UI.tr ## "node-epoch-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" lengthSVG
                                         , string "Epoch"
                                         , image "has-tooltip-multiline has-tooltip-right rt-view-what-icon" whatSVG
                                                 # set dataTooltip ("Current epoch from this node."
                                                                    <> " It can be outdated because of node's out of sync!")
                                         ]
                              ]
                          , UI.tr ## "node-sync-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" refreshSVG
                                         , string "Sync"
                                         ]
                              ]
                          , UI.tr ## "node-system-start-time-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" systemStartSVG
                                         , string "Blockchain start"
                                         ]
                              ]
                          , UI.tr ## "node-start-time-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" startSVG
                                         , string "Node start"
                                         ]
                              ]
                          , UI.tr ## "node-uptime-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" uptimeSVG
                                         , string "Uptime"
                                         ]
                              ]
                          , UI.tr ## "node-logs-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" logsSVG
                                         , string "Logs"
                                         ]
                              ]
                          , UI.tr ## "node-block-replay-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" blocksSVG
                                         , string "Block replay"
                                         ]
                              ]
                          , UI.tr ## "node-chunk-validation-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" dbSVG
                                         , string "Chunk validation"
                                         ]
                              ]
                          , UI.tr ## "node-update-ledger-db-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" dbSVG
                                         , string "Ledger DB"
                                         ]
                              ]
                          , UI.tr ## "node-peers-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" peersSVG
                                         , string "Peers"
                                         ]
                              ]
                          , UI.tr ## "node-errors-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" errorsSVG
                                         , string "Errors"
                                         ]
                              ]
                          , UI.tr ## "node-leadership-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" leaderSVG
                                         , string "Leadership"
                                         , image "has-tooltip-multiline has-tooltip-right rt-view-what-icon" whatSVG
                                                 # set dataTooltip "How many times this node was leader"
                                         ]
                              ]
                          , UI.tr ## "node-forged-blocks-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" forgeSVG
                                         , string "Forged blocks"
                                         , image "has-tooltip-multiline has-tooltip-right rt-view-what-icon" whatSVG
                                                 # set dataTooltip "How many blocks did forge by this node"
                                         ]
                              ]
                          , UI.tr ## "node-cannot-forge-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" notForgeSVG
                                         , string "Cannot forge"
                                         , image "has-tooltip-multiline has-tooltip-right rt-view-what-icon" whatSVG
                                                 # set dataTooltip "How many times this node could not forge"
                                         ]
                              ]
                          , UI.tr ## "node-missed-slots-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" missedSVG
                                         , string "Missed slots"
                                         , image "has-tooltip-multiline has-tooltip-right rt-view-what-icon" whatSVG
                                                 # set dataTooltip "How many slots were missed by this node"
                                         ]
                              ]
                          , UI.tr ## "node-current-kes-period-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" certificateSVG
                                         , string "KES current"
                                         ]
                              ]
                          , UI.tr ## "node-op-cert-expiry-kes-period-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" certificateSVG
                                         , string "KES Expiry"
                                         ]
                              ]
                          , UI.tr ## "node-remaining-kes-periods-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" certificateSVG
                                         , string "Remainig KES"
                                         ]
                              ]
                          , UI.tr ## "node-op-cert-start-kes-period-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" certificateSVG
                                         , string "Op Cert Start KES"
                                         ]
                              ]
                          , UI.tr ## "node-days-until-op-cert-renew-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" endSVG
                                         , string "Days until Op Cert renew"
                                         ]
                              ]
                          , UI.tr ## "node-ekg-metrics-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" ekgMetricsSVG
                                         , string "EKG metrics"
                                         , image "has-tooltip-multiline has-tooltip-right rt-view-what-icon" whatSVG
                                                 # set dataTooltip ("All EKG metrics forwarded by this node, "
                                                                    <> "as they are (no preparing)")
                                         ]
                              ]
                          ]
                      ]
                  ]
              ]
          , UI.mkElement "section" #. "section" #+
              [ UI.div ## "main-charts-container"
                       #. "is-fluid rt-view-charts-container"
                       # hideIt #+
                  [ UI.p #. "mb-5" #+
                      [ UI.div #. "divider" #+
                          [ element showHideChain
                          , UI.span #. "rt-view-chart-group-title" # set text "Blockchain"
                          ]
                      ]
                  , UI.div ## "chain-charts" #. "columns" #+
                      [ UI.div #. "column" #+
                          [ element chainDensityChart
                          , element epochChart
                          , element blockNumChart
                          ]
                      , UI.div #. "column" #+
                          [ element slotInEpochChart
                          , element slotNumChart
                          ]
                      ]
                  -- Leadership charts.
                  , UI.p #. "mb-5" #+
                      [ UI.div #. "divider" #+
                          [ element showHideLeadership
                          , UI.span #. "rt-view-chart-group-title" # set text "Leadership"
                          ]
                      ]
                  , UI.div ## "leadership-charts" #. "columns" #+
                      [ UI.div #. "column" #+
                          [ element forgedSlotChart
                          , element nodeIsLeaderChart
                          , element forgedInvalidChart
                          , element adoptedChart
                          , element aboutToLeadChart
                          ]
                      , UI.div #. "column" #+
                          [ element cannotForgeChart
                          , element nodeIsNotLeaderChart
                          , element couldNotForgeChart
                          , element notAdoptedChart
                          ]
                      ]
                  -- Transactions charts.
                  , UI.p #. "mb-5" #+
                      [ UI.div #. "divider" #+
                          [ element showHideTxs
                          , UI.span #. "rt-view-chart-group-title" # set text "Transactions"
                          ]
                      ]
                  , UI.div ## "transactions-charts" #. "columns" #+
                      [ UI.div #. "column" #+
                          [ element mempoolBytesChart
                          , element txsProcessedNumChart
                          ]
                      , UI.div #. "column" #+
                          [ element txsInMempoolChart
                          ]
                      ]
                  -- Resources charts.
                  , UI.p #. "mb-5" #+
                      [ UI.div #. "divider" #+
                          [ element showHideResources
                          , UI.span #. "rt-view-chart-group-title" # set text "Resources"
                          ]
                      ]
                  , UI.div ## "resources-charts" #. "columns" #+
                      [ UI.div #. "column" #+
                          [ element cpuChart
                          , element gcMajorNumChart
                          , element gcLiveMemoryChart
                          , element cpuTimeGCChart
                          ]
                      , UI.div #. "column" #+
                          [ element memoryChart
                          , element gcMinorNumChart
                          , element threadsNumChart
                          , element cpuTimeAppChart
                          ]
                      ]
                  ]
              ]
          , footer
          ]
      -- JS
      , UI.mkElement "script" # set UI.html chartJS
      , UI.mkElement "script" # set UI.html chartJSLuxon
      , UI.mkElement "script" # set UI.html chartJSAdapter
      , UI.mkElement "script" # set UI.html chartJSPluginZoom
      ]

  closeModalsByEscapeButton

  Chart.prepareChartsJS

  Chart.newTimeChartJS TxsProcessedNumChart ""
  Chart.newTimeChartJS MempoolBytesChart    "MB"
  Chart.newTimeChartJS TxsInMempoolChart    ""

  Chart.newTimeChartJS CPUChart          "Percent"
  Chart.newTimeChartJS MemoryChart       "MB"
  Chart.newTimeChartJS GCMajorNumChart   ""
  Chart.newTimeChartJS GCMinorNumChart   ""
  Chart.newTimeChartJS GCLiveMemoryChart "MB"
  Chart.newTimeChartJS CPUTimeGCChart    "Percent"
  Chart.newTimeChartJS CPUTimeAppChart   "Percent"
  Chart.newTimeChartJS ThreadsNumChart   ""

  Chart.newTimeChartJS ChainDensityChart "Percent"
  Chart.newTimeChartJS SlotNumChart      ""
  Chart.newTimeChartJS BlockNumChart     ""
  Chart.newTimeChartJS SlotInEpochChart  ""
  Chart.newTimeChartJS EpochChart        ""

  Chart.newTimeChartJS NodeCannotForgeChart       "Slots"
  Chart.newTimeChartJS ForgedSlotLastChart        "Slots"
  Chart.newTimeChartJS NodeIsLeaderChart          "Slots"
  Chart.newTimeChartJS NodeIsNotLeaderChart       "Slots"
  Chart.newTimeChartJS ForgedInvalidSlotLastChart "Slots"
  Chart.newTimeChartJS AdoptedSlotLastChart       "Slots"
  Chart.newTimeChartJS NotAdoptedSlotLastChart    "Slots"
  Chart.newTimeChartJS AboutToLeadSlotLastChart   "Slots"
  Chart.newTimeChartJS CouldNotForgeSlotLastChart "Slots"

  -- Start all timer.

  UI.start txsProcessedNumTimer
  UI.start mempoolBytesTimer
  UI.start txsInMempoolTimer

  UI.start cpuTimer
  UI.start memoryTimer
  UI.start gcMajorNumTimer
  UI.start gcMinorNumTimer
  UI.start gcLiveMemoryTimer
  UI.start cpuTimeGCTimer
  UI.start cpuTimeAppTimer
  UI.start threadsNumTimer

  UI.start chainDensityTimer
  UI.start slotNumTimer
  UI.start blockNumTimer
  UI.start slotInEpochTimer
  UI.start epochTimer

  UI.start cannotForgeTimer
  UI.start forgedSlotTimer
  UI.start nodeIsLeaderTimer
  UI.start nodeIsNotLeaderTimer
  UI.start forgedInvalidTimer
  UI.start adoptedTimer
  UI.start notAdoptedTimer
  UI.start aboutToLeadTimer
  UI.start couldNotForgeTimer

  on UI.disconnect window . const $ do
    UI.stop txsProcessedNumTimer
    UI.stop mempoolBytesTimer
    UI.stop txsInMempoolTimer

    UI.stop cpuTimer
    UI.stop memoryTimer
    UI.stop gcMajorNumTimer
    UI.stop gcMinorNumTimer
    UI.stop gcLiveMemoryTimer
    UI.stop cpuTimeGCTimer
    UI.stop cpuTimeAppTimer
    UI.stop threadsNumTimer

    UI.stop chainDensityTimer
    UI.stop slotNumTimer
    UI.stop blockNumTimer
    UI.stop slotInEpochTimer
    UI.stop epochTimer

    UI.stop cannotForgeTimer
    UI.stop forgedSlotTimer
    UI.stop nodeIsLeaderTimer
    UI.stop nodeIsNotLeaderTimer
    UI.stop forgedInvalidTimer
    UI.stop adoptedTimer
    UI.stop notAdoptedTimer
    UI.stop aboutToLeadTimer
    UI.stop couldNotForgeTimer

  return body

topNavigation
  :: UI.Window
  -> EventsQueues
  -> UI Element
topNavigation window eventsQueues = do
  info <- mkAboutInfo
  infoIcon <- image "has-tooltip-multiline has-tooltip-bottom rt-view-info-icon mr-1" rtViewInfoSVG
                    ## "info-icon"
                    # set dataTooltip "RTView info"
  on UI.click infoIcon . const $ fadeInModal info

  notificationsEvents   <- mkNotificationsEvents eventsQueues
  notificationsSettings <- mkNotificationsSettings

  notificationsEventsItem <- UI.anchor #. "navbar-item" #+
                               [ image "rt-view-notify-menu-icon" eventsSVG
                               , string "Events"
                               ]
  notificationsSettingsItem <- UI.anchor #. "navbar-item" #+
                                 [ image "rt-view-notify-menu-icon" settingsSVG
                                 , string "Settings"
                                 ]
  on UI.click notificationsEventsItem . const $
    fadeInModal notificationsEvents
  on UI.click notificationsSettingsItem . const $ do
    restoreEmailSettings window
    fadeInModal notificationsSettings

  notificationsIcon <- image "rt-view-info-icon mr-2" rtViewNotifySVG
                             ## "notifications-icon"

  themeIcon <- image "has-tooltip-multiline has-tooltip-bottom rt-view-theme-icon" rtViewThemeToLightSVG
                     ## "theme-icon"
                     # set dataTooltip "Switch to light theme"
  on UI.click themeIcon . const $ switchTheme window

  UI.div ## "top-bar" #. "navbar rt-view-top-bar" #+
    [ element info
    , element notificationsEvents
    , element notificationsSettings
    , UI.div #. "navbar-brand" #+
        [ UI.div #. "navbar-item" #+
            [ image "rt-view-cardano-logo" cardanoLogoSVG ## "cardano-logo"
            , UI.span ## "name" #. "rt-view-name" # set text "Node Real-time View"
            ]
        ]
    , UI.div #. "navbar-menu" #+
        [ UI.div #. "navbar-start" #+ []
        , UI.div #. "navbar-end" #+
            [ UI.div #. "navbar-item" #+ [element themeIcon]
            , UI.div #. "navbar-item" #+ [element infoIcon]
            , UI.div #. "navbar-item has-dropdown is-hoverable" #+
                [ UI.anchor #. "navbar-link" #+ [element notificationsIcon]
                , UI.div #. "navbar-dropdown is-right" #+
                    [ element notificationsEventsItem
                    , element notificationsSettingsItem
                    ]
                ]
            ]
        ]
    ]

footer :: UI Element
footer =
  UI.mkElement "footer" #. "footer rt-view-footer" #+
    [ UI.div #. "columns" #+
        [ UI.div #. "column" #+
            [ string "© IOHK 2015—2022"
            ]
        , UI.div #. "column has-text-right" #+
            [ UI.anchor # set UI.href "https://github.com/input-output-hk/cardano-node/blob/master/cardano-tracer/README.md"
                        # set UI.target "_blank" #+
                [ image "has-tooltip-multiline has-tooltip-left rt-view-footer-github" githubSVG
                        # set dataTooltip "Browse our GitHub repository"
                ]
            , UI.anchor # set UI.href "https://github.com/input-output-hk/cardano-node/blob/master/cardano-tracer/docs/cardano-rtview.md"
                        # set UI.target "_blank" #+
                [ image "has-tooltip-multiline has-tooltip-left rt-view-footer-doc" docSVG
                        # set dataTooltip "Read RTView documentation"
                ]
            ]
        ]
    ]

mkChart
  :: UI.Window
  -> UI.Timer
  -> ChartId
  -> String
  -> UI Element
mkChart window chartUpdateTimer chartId chartName = do
  selectTimeRange <-
    UI.select ## (show chartId <> show TimeRangeSelect) #+
      -- Values are ranges in seconds.
      [ UI.option # set value "0"     # set text "All time"
      , UI.option # set value "300"   # set text "Last 5 minutes"
      , UI.option # set value "900"   # set text "Last 15 minutes"
      , UI.option # set value "1800"  # set text "Last 30 minutes"
      , UI.option # set value "3600"  # set text "Last 1 hour"
      , UI.option # set value "10800" # set text "Last 3 hours"
      , UI.option # set value "21600" # set text "Last 6 hours"
      ]
  selectUpdatePeriod <-
    UI.select ## (show chartId <> show UpdatePeriodSelect) #+
      -- Values are periods in seconds.
      [ UI.option # set value "0"    # set text "Off"
      , UI.option # set value "15"   # set text "15 seconds"
      , UI.option # set value "30"   # set text "30 seconds"
      , UI.option # set value "60"   # set text "1 minute"
      , UI.option # set value "300"  # set text "5 minutes"
      , UI.option # set value "900"  # set text "15 minutes"
      , UI.option # set value "1800" # set text "30 minutes"
      , UI.option # set value "3600" # set text "1 hour"
      ]

  on UI.selectionChange selectTimeRange . const $
    whenJustM (readMaybe <$> get value selectTimeRange) $ \(rangeInSec :: Int) -> do
      Chart.setTimeRange chartId rangeInSec
      when (rangeInSec == 0) $ Chart.resetZoomChartJS chartId
      saveChartsSettings window

  on UI.selectionChange selectUpdatePeriod . const $
    whenJustM (readMaybe <$> get value selectUpdatePeriod) $ \(periodInSec :: Int) -> do
      whenM (get UI.running chartUpdateTimer) $ UI.stop chartUpdateTimer
      unless (periodInSec == 0) $ do
        void $ return chartUpdateTimer # set UI.interval (periodInSec * 1000)
        UI.start chartUpdateTimer
      saveChartsSettings window

  UI.div #. "rt-view-chart-container" #+
    [ UI.div #. "columns" #+
        [ UI.div #. "column mt-1" #+
            [ UI.span #. "rt-view-chart-name" # set text chartName
            ]
        , UI.div #. "column has-text-right" #+
            [ UI.div #. "field is-grouped mt-3" #+
                [ image "has-tooltip-multiline has-tooltip-top rt-view-chart-icon" timeRangeSVG
                        # set dataTooltip "Select time range"
                , UI.div #. "select is-link is-small mr-4" #+ [element selectTimeRange]
                , image "has-tooltip-multiline has-tooltip-top rt-view-chart-icon" refreshSVG
                        # set dataTooltip "Select update period"
                , UI.div #. "select is-link is-small" #+ [element selectUpdatePeriod]
                ]
            ]
        ]
    , UI.canvas ## show chartId #. "rt-view-chart-area" #+ []
    ]

changeVisibilityForCharts
  :: UI.Window
  -> Element
  -> Text
  -> String
  -> UI ()
changeVisibilityForCharts window showHideIcon areaId areaName = do
  state <- get dataState showHideIcon
  let haveToHide = state == shownState
  if haveToHide
    then
      findAndDo window areaId $ \el ->
        fadeOut el 180 Swing $ runUI window $ do
          void $ element el # hideIt
          void $ element showHideIcon # set html        hideSVG
                                      # set dataState   hiddenState
                                      # set dataTooltip ("Click to show " <> areaName)
    else
      findAndDo window areaId $ \el -> do
        void $ element el # showFlex
        fadeIn el 180 Swing $ return ()
        void $ element showHideIcon # set html        showSVG
                                    # set dataState   shownState
                                    # set dataTooltip ("Click to hide " <> areaName)

mkChartTimer, mkChartTimer'
  :: ConnectedNodes
  -> History
  -> DatasetsIndices
  -> DatasetsTimestamps
  -> DataName
  -> ChartId
  -> UI UI.Timer
mkChartTimer  = doMakeChartTimer addPointsToChart
mkChartTimer' = doMakeChartTimer addAllPointsToChart

type PointsAdder =
     ConnectedNodes
  -> History
  -> DatasetsIndices
  -> DatasetsTimestamps
  -> DataName
  -> ChartId
  -> UI ()

doMakeChartTimer
  :: PointsAdder
  -> ConnectedNodes
  -> History
  -> DatasetsIndices
  -> DatasetsTimestamps
  -> DataName
  -> ChartId
  -> UI UI.Timer
doMakeChartTimer addPoints connectedNodes history datasetIndices
                 datasetTimestamps dataName chartId = do
  uiUpdateTimer <- UI.timer # set UI.interval defaultUpdatePeriodInMs
  on UI.tick uiUpdateTimer . const $
    addPoints connectedNodes history datasetIndices datasetTimestamps dataName chartId
  return uiUpdateTimer
 where
  defaultUpdatePeriodInMs = 15 * 1000
