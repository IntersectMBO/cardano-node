{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Metrics.Prometheus
  ( runPrometheusServer
  ) where

import           Cardano.Logging.Prometheus.Exposition (MetricName, renderExpositionFromSampleWith)
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.MetaTrace

import           Prelude hiding (head)

import qualified Data.ByteString as ByteString
import           Data.ByteString.Builder (stringUtf8)
import           Data.Functor ((<&>))
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Encoding as TL
import           Network.HTTP.Types
import           Network.Wai hiding (responseHeaders)
import           Network.Wai.Handler.Warp (defaultSettings, runSettings)
import           System.Metrics as EKG (Store, sampleAll)
import           System.Time.Extra (sleep)

-- | Runs a simple HTTP server that listens on @endpoint@.
--
--   At the root, it lists the connected nodes, either as HTML or JSON, depending
--   on the requests 'Accept: ' header.
--
--   Routing is dynamic, depending on the connected nodes. A valid URL is derived
--   from the nodeName configured for the connecting node. E.g. a node name
--   of `127.0.0.1:30004` will result in the route `/12700130004` which
--   renders that node's Prometheus / OpenMetrics text exposition:
--
-- # TYPE Mem_resident_int gauge
-- # HELP Mem_resident_int Kernel-reported RSS (resident set size)
-- Mem_resident_int 103792640
-- # TYPE rts_gc_max_bytes_used gauge
-- rts_gc_max_bytes_used 5811512
-- # TYPE rts_gc_gc_cpu_ms counter
-- rts_gc_gc_cpu_ms 50
-- # TYPE RTS_gcMajorNum_int gauge
-- # HELP RTS_gcMajorNum_int Major GCs
-- RTS_gcMajorNum_int 4
-- # TYPE rts_gc_num_bytes_usage_samples counter
-- rts_gc_num_bytes_usage_samples 4
-- # TYPE remainingKESPeriods_int gauge
-- remainingKESPeriods_int 62
-- # TYPE rts_gc_bytes_copied counter
-- rts_gc_bytes_copied 17114384
-- # TYPE nodeCannotForge_int gauge
-- # HELP nodeCannotForge_int How many times was this node unable to forge [a block]?
--
runPrometheusServer
  :: TracerEnv
  -> Endpoint
  -> IO RouteDictionary
  -> IO ()
runPrometheusServer tracerEnv endpoint computeRoutes_autoUpdate = do
  -- Pause to prevent collision between "Listening"-notifications from servers.
  sleep 0.1
  -- If everything is okay, the function 'simpleHttpServe' never returns.
  -- But if there is some problem, it never throws an exception, but just stops.
  -- So if it stopped - it will be re-started.
  traceWith teTracer TracerStartedPrometheus
    { ttPrometheusEndpoint = endpoint
    }
  runSettings (setEndpoint endpoint defaultSettings) do
    renderPrometheus computeRoutes_autoUpdate metricsComp teMetricsHelp where

  TracerEnv
    { teTracer
    , teConfig = TracerConfig { metricsComp }
    , teMetricsHelp
    } = tracerEnv

renderPrometheus
  :: IO RouteDictionary
  -> Maybe (Map Text Text)
  -> [(Text, Builder)]
  -> Application
renderPrometheus computeRoutes_autoUpdate metricsComp helpTextDict request send = do
  routeDictionary :: RouteDictionary <-
    computeRoutes_autoUpdate

  let acceptHeader :: Maybe ByteString.ByteString
      acceptHeader = lookup hAccept $ requestHeaders request

  let wantsJson, wantsOpenMetrics :: Bool
      wantsJson         = all @Maybe ("application/json"             `ByteString.isInfixOf`) acceptHeader
      wantsOpenMetrics  = all @Maybe ("application/openmetrics-text" `ByteString.isInfixOf`) acceptHeader

  case pathInfo request of

    [] ->
      send $ uncurry (responseLBS status200) $ if wantsJson
        then (contentHdrJSON    , renderJson routeDictionary)
        else (contentHdrUtf8Html, renderListOfConnectedNodes "Prometheus metrics" routeDictionary)

    route:_
      | Just (store :: EKG.Store, _) <- lookup route (getRouteDictionary routeDictionary)
     -> do metrics <- getMetricsFromNode metricsComp helpTextDict store
           send $ responseBuilder status200
            (if wantsOpenMetrics then contentHdrOpenMetrics else contentHdrUtf8Text)
            (TL.encodeUtf8Builder metrics)

      | otherwise
     -> send $ responseBuilder status404 contentHdrUtf8Text do
        "Not found: "
          <> stringUtf8 (show route)

getMetricsFromNode
  :: Maybe (Map MetricName MetricName)
  -> [(Text, Builder)]
  -> EKG.Store
  -> IO TL.Text
getMetricsFromNode metricsComp helpTextDict ekgStore =
  sampleAll ekgStore <&> renderExpositionFromSampleWith metricsComp helpTextDict
