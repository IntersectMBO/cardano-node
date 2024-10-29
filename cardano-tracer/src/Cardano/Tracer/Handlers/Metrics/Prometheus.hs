{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Metrics.Prometheus
  ( runPrometheusServer
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.MetaTrace

import           Prelude hiding (head)

import qualified Data.ByteString as ByteString
import           Data.ByteString.Builder (stringUtf8)
import           Data.Char
import           Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HM
import           Data.List (find)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Encoding as TL
import           Network.HTTP.Types
import           Network.Wai hiding (responseHeaders)
import           Network.Wai.Handler.Warp (defaultSettings, runSettings)
import qualified System.Metrics as EKG
import           System.Metrics (Sample, Value (..), sampleAll)
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

type MetricName = Text

getMetricsFromNode
  :: Maybe (Map MetricName MetricName)
  -> [(Text, Builder)]
  -> EKG.Store
  -> IO TL.Text
getMetricsFromNode metricsComp helpTextDict ekgStore =
  sampleAll ekgStore <&> renderExpositionFromSample metricsComp helpTextDict

renderExpositionFromSample
  :: Maybe (Map MetricName MetricName)
  -> [(MetricName, Builder)]
  -> Sample
  -> TL.Text
renderExpositionFromSample renameMap helpTextDict =
  TB.toLazyText . (`mappend` buildEOF) . HM.foldlWithKey' buildMetric mempty
  where
    buildHelpText :: MetricName -> (Builder -> Builder)
    buildHelpText name = maybe
      (const mempty)
      (buildHelp . snd)
      (find ((`T.isInfixOf` name) . fst) helpTextDict)

    -- implements the metricsComp config option
    replaceName :: MetricName -> MetricName
    replaceName =
      case renameMap of
        Nothing   -> Prelude.id
        Just mmap -> \name -> M.findWithDefault name name mmap

    prepareName :: MetricName -> MetricName
    prepareName =
        T.filter (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c == '_')
      . T.replace " " "_"
      . T.replace "-" "_"
      . T.replace "." "_"

    -- the help annotation line
    buildHelp :: Builder -> Builder -> Builder
    buildHelp h n =
      TB.fromText "# HELP " `mappend` (n `mappend` (space `mappend` (h `mappend` newline)))

    buildMetric :: TB.Builder -> MetricName -> Value -> TB.Builder
    buildMetric acc mName mValue =
      acc `mappend` case mValue of
        Counter c -> annotate buildCounter `mappend` buildVal space  (TB.decimal c)
        Gauge g   -> annotate buildGauge   `mappend` buildVal space  (TB.decimal g)
        Label l
          | Just ('{', _) <- T.uncons l
                  -> annotate buildInfo    `mappend` buildVal mempty (TB.fromText l)
          | otherwise
                  -> helpAnnotation        `mappend` buildVal space  (TB.fromText l)
        _         ->                                 mempty
      where
        helpAnnotation = buildHelpText mName buildName

        -- annotates a metric in the order TYPE, UNIT, HELP
        -- TODO: UNIT annotation
        annotate annType =
          buildTypeAnn annType `mappend` helpAnnotation

        -- the name for exposition
        buildName = TB.fromText $ prepareName $ replaceName mName

        -- the type annotation line
        buildTypeAnn t =
          TB.fromText "# TYPE " `mappend` (buildName `mappend` (t `mappend` newline))

        -- the actual metric line, optional spacing after name, because of labels: 'metric_name{label_value="foo"} 1'
        buildVal spacing v =
          buildName `mappend` (spacing `mappend` (v `mappend` newline))

buildGauge, buildCounter, buildInfo, buildEOF, newline, space :: Builder
buildGauge    = TB.fromText " gauge"
buildCounter  = TB.fromText " counter"
buildInfo     = TB.fromText " info"
buildEOF      = TB.fromText "# EOF\n"
newline       = TB.singleton '\n'
space         = TB.singleton ' '
