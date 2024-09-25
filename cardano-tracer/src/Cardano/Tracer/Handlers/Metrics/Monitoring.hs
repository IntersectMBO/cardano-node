{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Metrics.Monitoring
  ( runMonitoringServer
  ) where

import           Prelude hiding (head)
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Types

import qualified Data.Text as T
import           System.Time.Extra (sleep)

import qualified Cardano.Tracer.Handlers.Metrics.Utils as Utils
import           Cardano.Tracer.Handlers.Metrics.Utils (renderListOfConnectedNodes)
import           Data.ByteString.Builder (stringUtf8)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp (runSettings, defaultSettings)
import qualified System.Metrics as EKG
import           System.Remote.Monitoring.Wai

-- | 'ekg' package allows to run only one EKG server, to display only one web page
--   for particular EKG.Store. Since 'cardano-tracer' can be connected to any number
--   of nodes, we display their list on the first web page (the first 'Endpoint')
--   as a list of hrefs. After clicking on the particular href, the user will be
--   redirected to the monitoring web page (the second 'Endpoint') built by 'ekg' package.
--   This page will display the metrics received from that node.
--
 --   If the user returns to the first web page and clicks to another node's href,
--   the EKG server will be restarted and the monitoring page will display the metrics
--   received from that node.
runMonitoringServer
  :: TracerEnv
  -> Endpoint -- ^ (web page with list of connected nodes, EKG web page).
  -> IO Utils.RouteDictionary
  -> IO ()
runMonitoringServer TracerEnv{teTracer} endpoint computeRoutes_autoUpdate = do
  -- Pause to prevent collision between "Listening"-notifications from servers.
  sleep 0.2
  traceWith teTracer TracerStartedMonitoring
    { ttMonitoringEndpoint = endpoint
    , ttMonitoringType     = "list"
    }
  dummyStore <- EKG.newStore
  runSettings (setEndpoint endpoint defaultSettings) do
    renderEkg dummyStore computeRoutes_autoUpdate

renderEkg :: EKG.Store -> IO Utils.RouteDictionary -> Application
renderEkg dummyStore computeRoutes_autoUpdate request send = do
  routeDictionary :: Utils.RouteDictionary <-
    computeRoutes_autoUpdate

  let nodeNames :: [NodeName]
      nodeNames = Utils.nodeNames routeDictionary

  case pathInfo request of
    [] ->
      send $ responseLBS status200 [] (renderListOfConnectedNodes "EKG metrics" nodeNames)
    route:rest
      | Just (store :: EKG.Store, _ :: NodeName)
     <- lookup route (Utils.getRouteDictionary routeDictionary)
     -> monitor store request { pathInfo = rest } send
      -- all endings in ekg-wai's asset/ folder
      | any (`T.isSuffixOf` route) [".html", ".css", ".js", ".png"]
        -- we actually need an empty dummy store here, as we're sure monitor will internally invoke the staticApp to serve the assets
     -> monitor dummyStore request send
      | otherwise
     -> send $ responseBuilder status404 [] do
        "Not found: "
          <> stringUtf8 (show route)
          <> "\n" <> stringUtf8 (show nodeNames)
