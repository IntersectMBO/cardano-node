{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Main
  ( mkMainPage
  ) where

import           Control.Concurrent.Extra (Lock)
import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (void)
import           Control.Monad.Extra (whenM)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (pack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.EraSettings
import           Cardano.Tracer.Handlers.RTView.State.Errors
import           Cardano.Tracer.Handlers.RTView.State.Historical
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
import           Cardano.Tracer.Handlers.RTView.Update.Errors
import           Cardano.Tracer.Handlers.RTView.Update.KES
import           Cardano.Tracer.Handlers.RTView.Update.Nodes
import           Cardano.Tracer.Handlers.RTView.Update.NodeState
import           Cardano.Tracer.Handlers.RTView.Update.Peers
import           Cardano.Tracer.Handlers.RTView.Update.Reload
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

mkMainPage
  :: ConnectedNodes
  -> DisplayedElements
  -> AcceptedMetrics
  -> SavedTraceObjects
  -> ErasSettings
  -> DataPointRequestors
  -> Lock
  -> PageReloadedFlag
  -> NonEmpty LoggingParams
  -> Network
  -> ResourcesHistory
  -> BlockchainHistory
  -> TransactionsHistory
  -> Errors
  -> EventsQueues
  -> UI.Window
  -> UI ()
mkMainPage connectedNodes displayedElements acceptedMetrics savedTO
           nodesEraSettings dpRequestors currentDPLock reloadFlag loggingConfig networkConfig
           resourcesHistory chainHistory txHistory nodesErrors eventsQueues window = do
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
    , UI.mkElement "style" # set UI.html ownCSS
    ]

  colors <- initColors
  datasetIndices <- initDatasetsIndices
  datasetTimestamps <- initDatasetsTimestamps
  peers <- liftIO initPeers

  pageBody <-
    mkPageBody
      window
      networkConfig
      connectedNodes
      resourcesHistory
      chainHistory
      txHistory
      datasetIndices
      datasetTimestamps
      eventsQueues

  -- Prepare and run the timer, which will hide the page preloader.
  preloaderTimer <- UI.timer # set UI.interval 10
  on UI.tick preloaderTimer . const $ do
    liftIO $ sleep 0.8
    findAndSet (set UI.class_ "pageloader") window "preloader"
    UI.stop preloaderTimer
  UI.start preloaderTimer

  restoreTheme window
  restoreChartsSettings
  restoreEmailSettings window
  restoreEventsSettings window

  uiNoNodesProgressTimer <- UI.timer # set UI.interval 1000
  on UI.tick uiNoNodesProgressTimer . const $ do
    let elId = "no-nodes-progress"
    valueS <- findAndGetValue window elId
    let valueI = readInt (pack valueS) 0 
    if valueI < 60
      then findAndSet (set UI.value $ show (valueI + 1)) window elId
      else do
        UI.stop uiNoNodesProgressTimer
        findAndSet hiddenOnly window elId

  uiErrorsTimer <- UI.timer # set UI.interval 3000
  on UI.tick uiErrorsTimer . const $
    updateNodesErrors window connectedNodes nodesErrors

  whenM (liftIO $ readTVarIO reloadFlag) $ do
    liftIO $ cleanupDisplayedValues displayedElements
    updateUIAfterReload
      window
      connectedNodes
      displayedElements
      dpRequestors
      currentDPLock
      loggingConfig
      colors
      datasetIndices
      nodesErrors
      uiErrorsTimer
      uiNoNodesProgressTimer
    liftIO $ pageWasNotReload reloadFlag

  -- Uptime is a real-time clock, so update it every second.
  uiUptimeTimer <- UI.timer # set UI.interval 1000
  on UI.tick uiUptimeTimer . const $
    updateNodesUptime connectedNodes displayedElements

  uiEKGTimer <- UI.timer # set UI.interval 1000
  on UI.tick uiEKGTimer . const $
    updateEKGMetrics acceptedMetrics

  uiNodesTimer <- UI.timer # set UI.interval 1500
  on UI.tick uiNodesTimer . const $
    updateNodesUI
      window
      connectedNodes
      displayedElements
      acceptedMetrics
      savedTO
      nodesEraSettings
      dpRequestors
      currentDPLock
      loggingConfig
      colors
      datasetIndices
      nodesErrors
      uiErrorsTimer
      uiNoNodesProgressTimer

  uiPeersTimer <- UI.timer # set UI.interval 4000
  on UI.tick uiPeersTimer . const $ do
    askNSetNodeState connectedNodes dpRequestors currentDPLock displayedElements
    updateNodesPeers window connectedNodes dpRequestors currentDPLock peers
    updateKESInfo window acceptedMetrics nodesEraSettings displayedElements

  UI.start uiUptimeTimer
  UI.start uiNodesTimer
  UI.start uiPeersTimer
  UI.start uiErrorsTimer
  UI.start uiEKGTimer
  UI.start uiNoNodesProgressTimer

  on UI.disconnect window . const $ do
    UI.stop uiNodesTimer
    UI.stop uiUptimeTimer
    UI.stop uiPeersTimer
    UI.stop uiEKGTimer
    UI.stop uiErrorsTimer
    UI.stop uiNoNodesProgressTimer
    liftIO $ pageWasReload reloadFlag

  void $ UI.element pageBody
