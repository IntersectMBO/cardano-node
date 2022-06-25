{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Run
  ( runRTView
  , module Cardano.Tracer.Handlers.RTView.Notifications.Utils
  , module Cardano.Tracer.Handlers.RTView.State.TraceObjects
  ) where

import           Control.Concurrent.Async.Extra (sequenceConcurrently)
import           Control.Concurrent.Extra (Lock)
import           Control.Monad (void)
import           Control.Monad.Extra (whenJust)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Graphics.UI.Threepenny as UI
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.Notifications.Utils
import           Cardano.Tracer.Handlers.RTView.SSL.Certs
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.EraSettings
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

runRTView
  :: TracerConfig
  -> ConnectedNodes
  -> AcceptedMetrics
  -> SavedTraceObjects
  -> DataPointRequestors
  -> Lock
  -> EventsQueues
  -> IO ()
runRTView TracerConfig{logging, network, hasRTView}
          connectedNodes acceptedMetrics savedTO
          dpRequestors currentDPLock eventsQueues =
  whenJust hasRTView $ \(Endpoint host port) -> do
    -- Pause to prevent collision between "Listening"-notifications from servers.
    sleep 0.3
    -- Get paths to default SSL files for config.
    (certFile, keyFile) <- placeDefaultSSLFiles
    -- Initialize displayed stuff outside of main page renderer,
    -- to be able to update corresponding elements after page reloading.
    displayedElements <- initDisplayedElements
    reloadFlag <- initPageReloadFlag
    -- We have to collect different information from the node and save it
    -- independently from RTView web-server. As a result, we'll be able to
    -- show charts with historical data (where X axis is the time) for the
    -- period when RTView web-page wasn't opened.
    resourcesHistory <- initResourcesHistory
    lastResources    <- initLastResources
    chainHistory     <- initBlockchainHistory
    txHistory        <- initTransactionsHistory
    eraSettings      <- initErasSettings
    errors           <- initErrors

    void . sequenceConcurrently $
      [ UI.startGUI (config host port certFile keyFile) $
          mkMainPage
            connectedNodes
            displayedElements
            acceptedMetrics
            savedTO
            eraSettings
            dpRequestors
            currentDPLock
            reloadFlag
            logging
            network
            resourcesHistory
            chainHistory
            txHistory
            errors
            eventsQueues
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
          dpRequestors
          currentDPLock
      , runErrorsUpdater
          connectedNodes
          errors
          savedTO
          eventsQueues
      ]
 where
  -- RTView's web page is available via 'https://' url only.
  config h p cert key = UI.defaultConfig
    { UI.jsSSLBind = Just . encodeUtf8 . T.pack $ h
    , UI.jsSSLPort = Just . fromIntegral $ p
    , UI.jsSSLCert = Just cert
    , UI.jsSSLKey  = Just key
    , UI.jsLog     = const $ return () -- To hide 'threepenny-gui' internal messages.
    -- , UI.jsWindowReloadOnDisconnect = False
    }
