{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.RTView.Run
  ( runRTView
  , module Cardano.Tracer.Handlers.RTView.Notifications.Utils
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
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.Notifications.Utils
import           Cardano.Tracer.Handlers.RTView.SSL.Certs
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.EraSettings
import           Cardano.Tracer.Handlers.RTView.State.Last
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Main
import           Cardano.Tracer.Handlers.RTView.Update.EraSettings
import           Cardano.Tracer.Handlers.RTView.Update.Historical

-- | RTView is a part of 'cardano-tracer' that provides an ability
--   to monitor Cardano nodes in a real-time. The core idea is simple:
--   RTView periodically receives some informations from the connected
--   node(s) and displays that information on a web-page.
--
--   The web-page is built using 'threepenny-gui' library. Please note
--   Gitub-version of this library is used, not Hackage-version!

runRTView :: TracerEnv -> IO ()
runRTView tracerEnv =
  whenJust hasRTView $ \(Endpoint host port) -> do
    -- Pause to prevent collision between "Listening"-notifications from servers.
    sleep 0.3
    -- Get paths to default SSL files for config.
    (certFile, keyFile) <- placeDefaultSSLFiles tracerEnv
    -- Initialize displayed stuff outside of main page renderer,
    -- to be able to update corresponding elements after page reloading.
    displayedElements <- initDisplayedElements
    reloadFlag <- initPageReloadFlag
    -- We have to collect different information from the node and save it
    -- independently from RTView web-server. As a result, we'll be able to
    -- show charts with historical data (where X axis is the time) for the
    -- period when RTView web-page wasn't opened.
    lastResources <- initLastResources
    eraSettings   <- initErasSettings

    void . sequenceConcurrently $
      [ UI.startGUI (config host port certFile keyFile) $
          mkMainPage
            tracerEnv
            displayedElements
            eraSettings
            reloadFlag
            logging
            network
      , runHistoricalUpdater  tracerEnv lastResources
      , runHistoricalBackup   tracerEnv
      , runEraSettingsUpdater tracerEnv eraSettings
      ]
 where
  TracerConfig{network, logging, hasRTView} = teConfig tracerEnv

  -- RTView's web page is available via 'https://' url only.
  config h p cert key =
    UI.defaultConfig
      { UI.jsLog     = const $ return () -- To hide 'threepenny-gui' internal messages.
      , UI.jsWindowReloadOnDisconnect = False
      , UI.jsUseSSL =
          Just $ UI.ConfigSSL
            { UI.jsSSLBind = encodeUtf8 $ T.pack h
            , UI.jsSSLPort = fromIntegral p
            , UI.jsSSLCert = cert
            , UI.jsSSLKey  = key
            , UI.jsSSLChainCert = False
            }
      }
