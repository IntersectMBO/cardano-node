{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Metrics.Prometheus
  ( runPrometheusServer
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.MetaTrace

import           Prelude hiding (head)

import           Data.ByteString.Builder (stringUtf8)
import           Data.Functor ((<&>))
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import           Network.HTTP.Types
import           Network.Wai hiding (responseHeaders)
import           Network.Wai.Handler.Warp (runSettings, defaultSettings)
import           System.Metrics (Sample, Value (..), sampleAll)
import           System.Time.Extra (sleep)
import qualified Cardano.Tracer.Handlers.Metrics.Utils as Utils
import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy.Text
import qualified Data.Text.Lazy.Encoding as Lazy.Text
import qualified System.Metrics as EKG

-- | Runs simple HTTP server that listens host and port and returns
--   the list of currently connected nodes in such a format:
--
--   * relay-1
--   * relay-2
--   * core-1
--
--  where 'relay-1', 'relay-2' and 'core-1' are nodes' names.
--
--  Each of list items is a href. By clicking on it, the user will be
--  redirected to the page with the list of metrics received from that node,
--  in such a format:
--
--  rts_gc_par_tot_bytes_copied 0
--  rts_gc_num_gcs 17
--  rts_gc_max_bytes_slop 15888
--  rts_gc_bytes_copied 165952
--  ekg_server_timestamp_ms 1639569439623
--
runPrometheusServer
  :: TracerEnv
  -> Endpoint
  -> IO Utils.RouteDictionary
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
    renderPrometheus computeRoutes_autoUpdate metricsComp where

  TracerEnv
    { teTracer
    , teConfig = TracerConfig { metricsComp }
    } = tracerEnv

renderPrometheus :: IO Utils.RouteDictionary -> Maybe (Map Text Text) -> Application
renderPrometheus computeRoutes_autoUpdate metricsComp request send = do
  routeDictionary :: Utils.RouteDictionary <-
    computeRoutes_autoUpdate

  let header :: RequestHeaders
      header = requestHeaders request

  let wantsJson :: Bool
      wantsJson = all @Maybe ("application/json" `ByteString.isInfixOf`) (lookup hAccept header)

  let responseHeaders :: ResponseHeaders
      responseHeaders = [(hContentType, if wantsJson then "application/json" else "text/html")]

  case pathInfo request of

    [] ->
      send $ responseLBS status200 responseHeaders if wantsJson
        then Utils.renderJson routeDictionary
        else Utils.renderListOfConnectedNodes "Prometheus metrics" (Utils.nodeNames routeDictionary)

    route:_
      | Just (store :: EKG.Store, _) <- lookup route (Utils.getRouteDictionary routeDictionary)
     -> do metrics <- getMetricsFromNode metricsComp store
           send $ responseLBS status200 [(hContentType, "text/plain")] (Lazy.Text.encodeUtf8 (Lazy.Text.fromStrict metrics))

      -- all endings in ekg-wai's asset/ folder
      | otherwise
     -> send $ responseBuilder status404 [(hContentType, "text/plain")] do
        "Not found: "
          <> stringUtf8 (show route)

type MetricName  = Text
type MetricValue = Text
type MetricsList = [(MetricName, MetricValue)]

getMetricsFromNode
  :: Maybe (Map Text Text)
  -> EKG.Store
  -> IO Text
getMetricsFromNode metricsComp ekgStore =
  sampleAll ekgStore <&> renderListOfMetrics . getListOfMetrics
 where

  getListOfMetrics :: Sample -> MetricsList
  getListOfMetrics =
    metricsCompatibility
    . filter (not . T.null . fst)
    . map metricsWeNeed
    . HM.toList

  metricsWeNeed :: (Text, Value) -> (Text, Text)
  metricsWeNeed (mName, mValue) =
    case mValue of
      Counter c -> (mName, T.pack $ show c)
      Gauge g   -> (mName, T.pack $ show g)
      Label l   -> (mName, l)
      _         -> ("",    "") -- 'ekg-forward' doesn't support 'Distribution' yet.

  metricsCompatibility :: MetricsList -> MetricsList
  metricsCompatibility metricsList =
    case metricsComp of
      Nothing -> metricsList
      Just mmap -> foldl (\ accu p'@(mn,mv) -> case Map.lookup mn mmap of
                                             Nothing -> p' : accu
                                             Just rep -> p' : (rep,mv) : accu)
                         []
                         metricsList

  renderListOfMetrics :: MetricsList -> Text
  renderListOfMetrics [] = "No metrics were received from this node."
  renderListOfMetrics mList = T.intercalate "\n" $
    map (\(mName, mValue) -> prepareName mName <> " " <> mValue) mList

  prepareName :: Text -> Text
  prepareName =
      T.filter (`elem` (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']))
    . T.replace " " "_"
    . T.replace "-" "_"
    . T.replace "." "_"
