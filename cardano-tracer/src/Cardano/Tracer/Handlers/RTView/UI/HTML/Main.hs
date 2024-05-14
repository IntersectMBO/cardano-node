{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Main
  ( mkMainPage
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.EraSettings
import           Cardano.Tracer.Handlers.RTView.State.Peers
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Bulma
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Own
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Body
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Notifications
import           Cardano.Tracer.Handlers.RTView.UI.Theme
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.EKG
import           Cardano.Tracer.Handlers.RTView.Update.KES
import           Cardano.Tracer.Handlers.RTView.Update.Logs
import           Cardano.Tracer.Handlers.RTView.Update.Nodes
import           Cardano.Tracer.Handlers.RTView.Update.NodeState
import           Cardano.Tracer.Handlers.RTView.Update.Peers
import           Cardano.Tracer.Handlers.RTView.Update.Reload
import           Cardano.Tracer.Handlers.RTView.Update.Utils

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (void)
import           Control.Monad.Extra (whenM)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (pack)
import           System.Time.Extra (sleep)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

mkMainPage
  :: TracerEnv
  -> TracerEnvRTView
  -> DisplayedElements
  -> ErasSettings
  -> PageReloadedFlag
  -> NonEmpty LoggingParams
  -> Network
  -> UI.Window
  -> UI ()
mkMainPage tracerEnv tracerEnvRTView displayedElements nodesEraSettings reloadFlag
           loggingConfig networkConfig window = do
  void $ return window # set UI.title pageTitle
  void $ UI.getHead window #+
    [ UI.link # set UI.rel "icon"
              # set UI.href ("data:image/svg+xml;base64," <> faviconSVGBase64)
    , UI.meta # set UI.name "viewport"
              # set UI.content "width=device-width, initial-scale=1"
    -- CSS
    , UI.mkElement "style" # set UI.html bulmaCSS
    , UI.mkElement "style" # set UI.html bulmaTooltipCSS
    , UI.mkElement "style" # set UI.html bulmaPageloaderCSS
    , UI.mkElement "style" # set UI.html bulmaSwitchCSS
    , UI.mkElement "style" # set UI.html bulmaDividerCSS
    , UI.mkElement "style" # set UI.html bulmaCheckboxCSS
    , UI.mkElement "style" # set UI.html ownCSS
    ]

  colors <- initColors
  datasetIndices <- initDatasetsIndices
  peers <- liftIO initPeers

  webPageIsOpened tracerEnvRTView

  pageBody <- mkPageBody tracerEnv tracerEnvRTView networkConfig datasetIndices

  -- Prepare and run the timer, which will hide the page preloader.
  preloaderTimer <- UI.timer # set UI.interval 10
  on_ UI.tick preloaderTimer do
    liftIO $ sleep 0.8
    findAndSet (set UI.class_ "pageloader") window "preloader"
    UI.stop preloaderTimer
  UI.start preloaderTimer

  restoreTheme tracerEnv
  restoreChartsSettings tracerEnv
  restoreEmailSettings tracerEnv
  restoreEventsSettings tracerEnv

  uiNoNodesProgressTimer <- UI.timer # set UI.interval 1000
  on_ UI.tick uiNoNodesProgressTimer do
    let elId = "no-nodes-progress"
    valueS <- findAndGetValue window elId
    let valueI = readInt (pack valueS) 0
    if valueI < 60
      then findAndSet (set UI.value $ show (valueI + 1)) window elId
      else do
        UI.stop uiNoNodesProgressTimer
        findAndSet hiddenOnly window elId

  whenM (liftIO $ readTVarIO reloadFlag) do
    liftIO $ cleanupDisplayedValues displayedElements

    updateUIAfterReload
      tracerEnv
      displayedElements
      loggingConfig
      colors
      datasetIndices
      uiNoNodesProgressTimer

    liftIO $ pageWasNotReload reloadFlag

  llvCounters <- liftIO initLogsLiveViewCounters

  uiLogsLiveViewTimer <- UI.timer # set UI.interval 1000
  on_ UI.tick uiLogsLiveViewTimer do
    updateLogsLiveViewItems tracerEnv tracerEnvRTView llvCounters

  -- Uptime is a real-time clock, so update it every second.
  uiUptimeTimer <- UI.timer # set UI.interval 1000
  on_ UI.tick uiUptimeTimer do
    updateNodesUptime tracerEnv displayedElements

  uiEKGTimer <- UI.timer # set UI.interval 1000
  on_ UI.tick uiEKGTimer do
    updateEKGMetrics tracerEnv

  uiNodesTimer <- UI.timer # set UI.interval 1000
  on_ UI.tick uiNodesTimer do
    updateNodesUI
      tracerEnv
      displayedElements
      nodesEraSettings
      loggingConfig
      colors
      datasetIndices
      uiNoNodesProgressTimer

  uiPeersTimer <- UI.timer # set UI.interval 4000
  on_ UI.tick uiPeersTimer do
    askNSetNodeState tracerEnv displayedElements
    updateNodesPeers tracerEnv peers
    updateKESInfo tracerEnv nodesEraSettings displayedElements

  UI.start uiLogsLiveViewTimer
  UI.start uiUptimeTimer
  UI.start uiNodesTimer
  UI.start uiPeersTimer
  UI.start uiEKGTimer
  UI.start uiNoNodesProgressTimer

  on_ UI.disconnect window do
    webPageIsClosed tracerEnvRTView
    UI.stop uiLogsLiveViewTimer
    UI.stop uiNodesTimer
    UI.stop uiUptimeTimer
    UI.stop uiPeersTimer
    UI.stop uiEKGTimer
    UI.stop uiNoNodesProgressTimer
    liftIO $ pageWasReload reloadFlag

  void $ UI.element pageBody
