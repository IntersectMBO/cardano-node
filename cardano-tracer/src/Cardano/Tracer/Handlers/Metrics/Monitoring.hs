{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Metrics.Monitoring
  ( runMonitoringServer
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Types

import           Prelude hiding (head)

import           Data.ByteString as ByteString (ByteString, isInfixOf)
import           Data.ByteString.Builder (stringUtf8)
import qualified Data.Text as T
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp (defaultSettings, runSettings)
import qualified System.Metrics as EKG
import           System.Remote.Monitoring.Wai
import           System.Time.Extra (sleep)

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
  -> IO RouteDictionary
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

renderEkg :: EKG.Store -> IO RouteDictionary -> Application
renderEkg dummyStore computeRoutes_autoUpdate request send = do
  routeDictionary :: RouteDictionary <-
    computeRoutes_autoUpdate

  let acceptHeader :: Maybe ByteString
      acceptHeader = lookup hAccept $ requestHeaders request

  let wantsJson :: Bool
      wantsJson = all @Maybe ("application/json" `ByteString.isInfixOf`) acceptHeader

  case pathInfo request of

    [] ->
      send $ uncurry (responseLBS status200) $ if wantsJson
        then (contentHdrJSON    , renderJson routeDictionary)
        else (contentHdrUtf8Html, renderListOfConnectedNodes "EKG metrics" routeDictionary)

    route:rest
      | Just (store :: EKG.Store, _ :: NodeName)
     <- lookup route (getRouteDictionary routeDictionary)
     -> monitor store request { pathInfo = rest } send
      -- all endings in ekg-wai's asset/ folder

      | any (`T.isSuffixOf` route) [".html", ".css", ".js", ".png"]
        -- we actually need an empty dummy store here, as we're sure monitor will internally invoke the staticApp to serve the assets
     -> monitor dummyStore request send

      | otherwise
     -> send $ responseBuilder status404 contentHdrUtf8Text do
        "Not found: "
          <> stringUtf8 (show route)
