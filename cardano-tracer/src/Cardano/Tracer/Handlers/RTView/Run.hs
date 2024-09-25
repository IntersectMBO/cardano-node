{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.RTView.Run
  ( runRTView
  , module Cardano.Tracer.Handlers.Notifications.Utils
  , module Cardano.Tracer.Handlers.State.TraceObjects
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Notifications.Utils
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.EraSettings
import           Cardano.Tracer.Handlers.RTView.State.Last
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Main
import           Cardano.Tracer.Handlers.RTView.Update.EraSettings
import           Cardano.Tracer.Handlers.RTView.Update.Historical
import           Cardano.Tracer.Handlers.State.TraceObjects
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Utils (sequenceConcurrently_)

import           Control.Monad.Extra (whenJust)
import           Data.ByteString.UTF8 (fromString)
import           Network.Wai.Handler.Warp (Port)
import           System.Time.Extra (sleep)

import qualified Graphics.UI.Threepenny as UI

-- | RTView is a part of 'cardano-tracer' that provides an ability
--   to monitor Cardano nodes in a real-time. The core idea is simple:
--   RTView periodically receives some informations from the connected
--   node(s) and displays that information on a web-page.
--
--   The web-page is built using 'threepenny-gui' library. Please note
--   Gitub-version of this library is used, not Hackage-version!

runRTView :: TracerEnv -> TracerEnvRTView -> IO ()
runRTView tracerEnv@TracerEnv{teTracer} tracerEnvRTView =
  whenJust hasRTView \(Endpoint host port) -> do
    traceWith teTracer TracerStartedRTView
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
    lastResources <- initLastResources
    eraSettings   <- initErasSettings

    sequenceConcurrently_
      [ UI.startGUI (config host port) $
          mkMainPage
            tracerEnv
            tracerEnvRTView
            displayedElements
            eraSettings
            reloadFlag
            logging
            network
      , runHistoricalUpdater  tracerEnv tracerEnvRTView lastResources
      , runHistoricalBackup   tracerEnv tracerEnvRTView
      , runEraSettingsUpdater tracerEnv eraSettings
      ]
 where
  TracerConfig{network, logging, hasRTView} = teConfig tracerEnv

  -- RTView's web page is available via 'https://' url only.
  config :: String -> Port -> UI.Config
  config host port =
    UI.defaultConfig
      { UI.jsAddr    = Just (fromString host)
      , UI.jsPort    = Just port
      , UI.jsLog     = const $ return () -- To hide 'threepenny-gui' internal messages.
      , UI.jsWindowReloadOnDisconnect = False
      , UI.jsUseSSL = Nothing
      }
