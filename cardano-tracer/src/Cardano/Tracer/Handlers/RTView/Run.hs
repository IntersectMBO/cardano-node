{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.RTView.Run
  ( runRTView
  , module Cardano.Tracer.Handlers.RTView.State.TraceObjects
  ) where

import           Control.Concurrent.Async.Extra (sequenceConcurrently)
import           Control.Monad (void)
import           Control.Monad.Extra (whenJust)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Graphics.UI.Threepenny as UI
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.State.EraSettings
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.Errors
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.State.Last
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Main
import           Cardano.Tracer.Handlers.RTView.Update.EraSettings
import           Cardano.Tracer.Handlers.RTView.Update.Errors
import           Cardano.Tracer.Handlers.RTView.Update.Historical
import           Cardano.Tracer.Types

-- | RTView is a part of 'cardano-tracer' that provides an ability
--   to monitor Cardano nodes in a real-time. The core idea is simple:
--   RTView periodically receives some informations from the connected
--   node(s) and displays that information on a web-page.
--
--   The web-page is built using 'threepenny-gui' library. Please note
--   Gitub-version of this library is used, not Hackage-version!
--
--   TODO ...

runRTView
  :: TracerConfig
  -> ConnectedNodes
  -> AcceptedMetrics
  -> SavedTraceObjects
  -> DataPointRequestors
  -> IO ()
runRTView TracerConfig{logging, network, hasRTView}
          connectedNodes acceptedMetrics savedTO dpRequestors =
  whenJust hasRTView $ \(Endpoint host port) -> do
    -- Pause to prevent collision between "Listening"-notifications from servers.
    sleep 0.3
    -- Initialize displayed stuff outside of main page renderer,
    -- to be able to update corresponding elements after page reloading.
    displayedElements <- initDisplayedElements
    reloadFlag <- initPageReloadFlag
    -- We have to collect different information from the node and save it
    -- independently from RTView web-server. As a result, we'll be able to
    -- show charts with historical data (where X axis is the time) for the
    -- period when RTView web-page wasn't opened.
    resourcesHistory <- initResourcesHistory
    lastResources <- initLastResources
    chainHistory <- initBlockchainHistory
    txHistory <- initTransactionsHistory
    eraSettings <- initErasSettings
    errors <- initErrors

    void . sequenceConcurrently $
      [ UI.startGUI (config host port) $
          mkMainPage
            connectedNodes
            displayedElements
            acceptedMetrics
            savedTO
            eraSettings
            dpRequestors
            reloadFlag
            logging
            network
            resourcesHistory
            chainHistory
            txHistory
            errors
      , runHistoricalUpdater
          savedTO
          acceptedMetrics
          resourcesHistory
          lastResources
          chainHistory
          txHistory
      , runEraSettingsUpdater
          connectedNodes
          eraSettings
          savedTO
      , runErrorsUpdater
          connectedNodes
          errors
          savedTO
      ]
 where
  config h p = UI.defaultConfig
    { UI.jsPort = Just . fromIntegral $ p
    , UI.jsAddr = Just . encodeUtf8 . T.pack $ h
    , UI.jsLog  = const $ return () -- To hide 'threepenny-gui' internal messages.
    }
