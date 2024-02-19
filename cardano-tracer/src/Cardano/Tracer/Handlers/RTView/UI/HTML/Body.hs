{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Body
  ( mkPageBody
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.UI.HTML.About
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Logs
import           Cardano.Tracer.Handlers.RTView.UI.HTML.NoNodes
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Notifications
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.JS.ChartJS
import qualified Cardano.Tracer.Handlers.RTView.UI.JS.Charts as Chart
import           Cardano.Tracer.Handlers.RTView.UI.Logs
import           Cardano.Tracer.Handlers.RTView.UI.Notifications
import           Cardano.Tracer.Handlers.RTView.UI.Theme
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Logs

import           Control.Monad (unless, void, when)
import           Control.Monad.Extra (whenJustM, whenM)
import           Data.Text (Text)
import           Text.Read (readMaybe)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.JQuery (Easing (..), fadeIn, fadeOut)

mkPageBody
  :: TracerEnv
  -> Network
  -> DatasetsIndices
  -> UI Element
mkPageBody tracerEnv networkConfig dsIxs = do
  let ResHistory rHistory   = teResourcesHistory tracerEnv
      ChainHistory cHistory = teBlockchainHistory tracerEnv
      TXHistory tHistory    = teTxHistory tracerEnv

  txsProcessedNumTimer <- mkChartTimer tracerEnv tHistory dsIxs TxsProcessedNumData TxsProcessedNumChart
  mempoolBytesTimer    <- mkChartTimer tracerEnv tHistory dsIxs MempoolBytesData    MempoolBytesChart
  txsInMempoolTimer    <- mkChartTimer tracerEnv tHistory dsIxs TxsInMempoolData    TxsInMempoolChart

  txsProcessedNumChart <- mkChart txsProcessedNumTimer TxsProcessedNumData TxsProcessedNumChart "Processed txs"
  mempoolBytesChart    <- mkChart mempoolBytesTimer    MempoolBytesData    MempoolBytesChart    "Mempool size"
  txsInMempoolChart    <- mkChart txsInMempoolTimer    TxsInMempoolData    TxsInMempoolChart    "Txs in mempool"

  -- Resources charts.
  cpuTimer          <- mkChartTimer tracerEnv rHistory dsIxs CPUData          CPUChart
  memoryTimer       <- mkChartTimer tracerEnv rHistory dsIxs MemoryData       MemoryChart
  gcMajorNumTimer   <- mkChartTimer tracerEnv rHistory dsIxs GCMajorNumData   GCMajorNumChart
  gcMinorNumTimer   <- mkChartTimer tracerEnv rHistory dsIxs GCMinorNumData   GCMinorNumChart
  gcLiveMemoryTimer <- mkChartTimer tracerEnv rHistory dsIxs GCLiveMemoryData GCLiveMemoryChart
  cpuTimeGCTimer    <- mkChartTimer tracerEnv rHistory dsIxs CPUTimeGCData    CPUTimeGCChart
  cpuTimeAppTimer   <- mkChartTimer tracerEnv rHistory dsIxs CPUTimeAppData   CPUTimeAppChart
  threadsNumTimer   <- mkChartTimer tracerEnv rHistory dsIxs ThreadsNumData   ThreadsNumChart

  cpuChart          <- mkChart cpuTimer          CPUData          CPUChart          "CPU usage"
  memoryChart       <- mkChart memoryTimer       MemoryData       MemoryChart       "Memory usage"
  gcMajorNumChart   <- mkChart gcMajorNumTimer   GCMajorNumData   GCMajorNumChart   "Number of major GCs"
  gcMinorNumChart   <- mkChart gcMinorNumTimer   GCMinorNumData   GCMinorNumChart   "Number of minor GCs"
  gcLiveMemoryChart <- mkChart gcLiveMemoryTimer GCLiveMemoryData GCLiveMemoryChart "GC, live data in heap"
  cpuTimeGCChart    <- mkChart cpuTimeGCTimer    CPUTimeGCData    CPUTimeGCChart    "CPU time used by GC"
  cpuTimeAppChart   <- mkChart cpuTimeAppTimer   CPUTimeAppData   CPUTimeAppChart   "CPU time used by app"
  threadsNumChart   <- mkChart threadsNumTimer   ThreadsNumData   ThreadsNumChart   "Number of threads"

  -- Blockchain charts.
  chainDensityTimer <- mkChartTimer tracerEnv cHistory dsIxs ChainDensityData ChainDensityChart
  slotNumTimer      <- mkChartTimer tracerEnv cHistory dsIxs SlotNumData      SlotNumChart
  blockNumTimer     <- mkChartTimer tracerEnv cHistory dsIxs BlockNumData     BlockNumChart
  slotInEpochTimer  <- mkChartTimer tracerEnv cHistory dsIxs SlotInEpochData  SlotInEpochChart
  epochTimer        <- mkChartTimer tracerEnv cHistory dsIxs EpochData        EpochChart

  chainDensityChart <- mkChart chainDensityTimer ChainDensityData ChainDensityChart "Chain density"
  slotNumChart      <- mkChart slotNumTimer      SlotNumData      SlotNumChart      "Slot height"
  blockNumChart     <- mkChart blockNumTimer     BlockNumData     BlockNumChart     "Block height"
  slotInEpochChart  <- mkChart slotInEpochTimer  SlotInEpochData  SlotInEpochChart  "Slot in epoch"
  epochChart        <- mkChart epochTimer        EpochData        EpochChart        "Epoch"

  -- Leadership charts.
  cannotForgeTimer     <- mkChartTimer' tracerEnv cHistory dsIxs NodeCannotForgeData       NodeCannotForgeChart
  forgedSlotTimer      <- mkChartTimer' tracerEnv cHistory dsIxs ForgedSlotLastData        ForgedSlotLastChart
  nodeIsLeaderTimer    <- mkChartTimer' tracerEnv cHistory dsIxs NodeIsLeaderData          NodeIsLeaderChart
  nodeIsNotLeaderTimer <- mkChartTimer' tracerEnv cHistory dsIxs NodeIsNotLeaderData       NodeIsNotLeaderChart
  forgedInvalidTimer   <- mkChartTimer' tracerEnv cHistory dsIxs ForgedInvalidSlotLastData ForgedInvalidSlotLastChart
  adoptedTimer         <- mkChartTimer' tracerEnv cHistory dsIxs AdoptedSlotLastData       AdoptedSlotLastChart
  notAdoptedTimer      <- mkChartTimer' tracerEnv cHistory dsIxs NotAdoptedSlotLastData    NotAdoptedSlotLastChart
  aboutToLeadTimer     <- mkChartTimer' tracerEnv cHistory dsIxs AboutToLeadSlotLastData   AboutToLeadSlotLastChart
  couldNotForgeTimer   <- mkChartTimer' tracerEnv cHistory dsIxs CouldNotForgeSlotLastData CouldNotForgeSlotLastChart

  cannotForgeChart     <- mkChart cannotForgeTimer     NodeCannotForgeData       NodeCannotForgeChart       "Cannot forge"
  forgedSlotChart      <- mkChart forgedSlotTimer      ForgedSlotLastData        ForgedSlotLastChart        "Forged"
  nodeIsLeaderChart    <- mkChart nodeIsLeaderTimer    NodeIsLeaderData          NodeIsLeaderChart          "Is leader"
  nodeIsNotLeaderChart <- mkChart nodeIsNotLeaderTimer NodeIsNotLeaderData       NodeIsNotLeaderChart       "Is not leader"
  forgedInvalidChart   <- mkChart forgedInvalidTimer   ForgedInvalidSlotLastData ForgedInvalidSlotLastChart "Forged invalid"
  adoptedChart         <- mkChart adoptedTimer         AdoptedSlotLastData       AdoptedSlotLastChart       "Is adopted"
  notAdoptedChart      <- mkChart notAdoptedTimer      NotAdoptedSlotLastData    NotAdoptedSlotLastChart    "Is not adopted"
  aboutToLeadChart     <- mkChart aboutToLeadTimer     AboutToLeadSlotLastData   AboutToLeadSlotLastChart   "About to lead"
  couldNotForgeChart   <- mkChart couldNotForgeTimer   CouldNotForgeSlotLastData CouldNotForgeSlotLastChart "Could not forge"

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
    changeVisibilityForCharts showHideTxs "transactions-charts" "Transactions"
  on UI.click showHideChain . const $
    changeVisibilityForCharts showHideChain "chain-charts" "Blockchain"
  on UI.click showHideLeadership . const $
    changeVisibilityForCharts showHideLeadership "leadership-charts" "Leadership"
  on UI.click showHideResources . const $
    changeVisibilityForCharts showHideResources "resources-charts" "Resources"

  logsLiveView <- mkLogsLiveView tracerEnv
  logsLiveViewButton <- UI.button ## "logs-live-view-button"
                                  #. "button is-info is-medium"
                                  # set text "Logs view"
                                  # hideIt
  on UI.click logsLiveViewButton . const $ do
    fadeInModal logsLiveView
    void $ element logsLiveView # set dataState "opened"
    updateLogsLiveViewNodes tracerEnv
    restoreLogsLiveViewFont tracerEnv

  -- Body.
  window <- askWindow
  body <-
    UI.getBody window #+
      [ UI.div #. "wrapper" #+
          [ UI.div ## "preloader" #. "pageloader is-active" #+
              [ UI.span #. "title" # set text "Just a second..."
              ]
          , topNavigation tracerEnv
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
                          [ UI.tr ## "node-basic-info-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" infoSVG
                                         , string "Basic info"
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
                          , UI.tr ## "node-block-replay-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" blocksSVG
                                         , string "Block replay"
                                         ]
                              ]
                          , UI.tr ## "node-sync-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" refreshSVG
                                         , string "Sync"
                                         ]
                              ]
                          , UI.tr ## "node-start-time-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" startSVG
                                         , string "Start time"
                                         ]
                              ]
                          , UI.tr ## "node-uptime-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" uptimeSVG
                                         , string "Uptime"
                                         ]
                              ]
                          , UI.tr ## "node-logs-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" logsSVG
                                         , string "Logs paths"
                                         ]
                              ]
                          , UI.tr ## "node-peers-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" peersSVG
                                         , string "Peers"
                                         ]
                              ]
                          , UI.tr ## "node-leadership-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" firstSVG
                                         , string "Leadership"
                                         ]
                              ]
                          , UI.tr ## "node-kes-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" kesSVG
                                         , string "KES"
                                         ]
                              ]
                          , UI.tr ## "node-op-cert-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" certificateSVG
                                         , string "Op Cert"
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
              , UI.div #+
                  [ element logsLiveView
                  , element logsLiveViewButton
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

  -- closeModalsByEscapeButton

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
 where
  mkChart chartUpdateTimer dataName chartId chartName = do
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
        when (rangeInSec == 0) $ do
          -- Since the user changed '0' (which means "All time"),
          -- we have to load all the history for currently connected nodes,
          -- but for this 'dataName' only!
          restoreAllHistoryOnChart tracerEnv dataName chartId dsIxs
          Chart.resetZoomChartJS chartId
        saveChartsSettings tracerEnv

    on UI.selectionChange selectUpdatePeriod . const $
      whenJustM (readMaybe <$> get value selectUpdatePeriod) $ \(periodInSec :: Int) -> do
        whenM (get UI.running chartUpdateTimer) $ UI.stop chartUpdateTimer
        unless (periodInSec == 0) $ do
          void $ return chartUpdateTimer # set UI.interval (periodInSec * 1_000)
          UI.start chartUpdateTimer
        saveChartsSettings tracerEnv

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

topNavigation :: TracerEnv -> UI Element
topNavigation tracerEnv@TracerEnv{teEventsQueues} = do
  info <- mkAboutInfo
  infoIcon <- image "has-tooltip-multiline has-tooltip-bottom rt-view-info-icon mr-1" rtViewInfoSVG
                    ## "info-icon"
                    # set dataTooltip "RTView info"
  on UI.click infoIcon . const $ fadeInModal info

  notificationsEvents   <- mkNotificationsEvents tracerEnv teEventsQueues
  notificationsSettings <- mkNotificationsSettings tracerEnv

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
    restoreEmailSettings tracerEnv
    fadeInModal notificationsSettings

  notificationsIcon <- image "rt-view-info-icon mr-2" rtViewNotifySVG
                             ## "notifications-icon"

  themeIcon <- image "has-tooltip-multiline has-tooltip-bottom rt-view-theme-icon" rtViewThemeToLightSVG
                     ## "theme-icon"
                     # set dataTooltip "Switch to light theme"
  on UI.click themeIcon . const $ switchTheme tracerEnv

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
            [ UI.anchor # set UI.href "https://github.com/intersectmbo/cardano-node/blob/master/cardano-tracer/README.md"
                        # set UI.target "_blank" #+
                [ image "has-tooltip-multiline has-tooltip-left rt-view-footer-github" githubSVG
                        # set dataTooltip "Browse our GitHub repository"
                ]
            , UI.anchor # set UI.href "https://github.com/intersectmbo/cardano-node/blob/master/cardano-tracer/docs/cardano-rtview.md"
                        # set UI.target "_blank" #+
                [ image "has-tooltip-multiline has-tooltip-left rt-view-footer-doc" docSVG
                        # set dataTooltip "Read RTView documentation"
                ]
            ]
        ]
    ]

changeVisibilityForCharts
  :: Element
  -> Text
  -> String
  -> UI ()
changeVisibilityForCharts showHideIcon areaId areaName = do
  window <- askWindow
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
  :: TracerEnv
  -> History
  -> DatasetsIndices
  -> DataName
  -> ChartId
  -> UI UI.Timer
mkChartTimer  = doMakeChartTimer addPointsToChart
mkChartTimer' = doMakeChartTimer addAllPointsToChart

type PointsAdder =
     TracerEnv
  -> History
  -> DatasetsIndices
  -> DataName
  -> ChartId
  -> UI ()

doMakeChartTimer
  :: PointsAdder
  -> TracerEnv
  -> History
  -> DatasetsIndices
  -> DataName
  -> ChartId
  -> UI UI.Timer
doMakeChartTimer addPoints tracerEnv history datasetIndices dataName chartId = do
  uiUpdateTimer <- UI.timer # set UI.interval defaultUpdatePeriodInMs
  on UI.tick uiUpdateTimer . const $
    addPoints tracerEnv history datasetIndices dataName chartId
  return uiUpdateTimer
 where
  defaultUpdatePeriodInMs = 15_000
