import           Cardano.Logging (metricsFormatter)
import           Cardano.Logging.Configuration (configureTracers)
import           Cardano.Logging.Prometheus.TCPServer (spawnPrometheusSimple)
import           Cardano.Logging.Trace (traceWith)
import           Cardano.Logging.Tracer.EKG (ekgTracer)
import           Cardano.Logging.Types

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (cancel)
import           Control.Monad (unless)
import           Data.Aeson
import qualified Data.Map.Internal as Map
import           Data.Text (pack)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Network.HTTP.PrometheusTracker (scrapeOnce)
import           Network.HTTP.PrometheusTracker.Types (MetricsMap (MM), MetricsValue (MVDouble))
import           System.Exit (die)
import           System.Metrics (newStore)
import           System.Posix.Signals

newtype Measure = Measure Int

instance LogFormatting Measure where
  forMachine _dtal (Measure count) =
      mconcat
        [ "count" .= String (pack $ show count)
        ]
  asMetrics (Measure count) =
    [ DoubleM "measure" (fromIntegral count)]

instance MetaTrace Measure where
  namespaceFor (Measure _count) = Namespace [] ["Count"]
  severityFor (Namespace [] ["Count"]) _ = Just Info
  privacyFor  (Namespace [] ["Count"]) _ = Just Public
  documentFor (Namespace [] ["Count"])  = Just "A counter"
  metricsDocFor (Namespace [] ["Count"]) =
    [("count", "an integer")]
  allNamespaces = [Namespace [] ["Count"]]

{- Thread #1:
     - Run the prometheus simple server
     - Trace a metric
     - Spawn Thread #2
     - Wait

   Thread #2:
     - Scape the metrics
     - Ensure that we see the expected metric and its value in the list
-}
main :: IO ()
main = do
  store <- newStore
  let host = "localhost"
  let port = 9090
  Right metricsServerThread <- spawnPrometheusSimple store (True, Just host, port)
  pretracer <- ekgTracer emptyTraceConfig store
  let tracer = metricsFormatter pretracer :: Trace IO Measure
  confState <- emptyConfigReflection
  configureTracers confState emptyTraceConfig [tracer]
  traceWith tracer (Measure 42)
  _ <- installHandler sigTERM (Catch (cancel metricsServerThread)) Nothing
  manager <- newManager defaultManagerSettings
  _ <- threadDelay (3 * 1000000)
  MM metricsMap <- scrapeOnce manager ("http://" <> host <> ":" <> show port <> "/metrics")
  MVDouble value <- maybe (die "'measure' metric not found in the scape list") pure (Map.lookup "measure" metricsMap)
  unless (value == 42) $ die ("Unexpected value: " <> show value)
  putStrLn "Got correct metric value ✔"
  cancel metricsServerThread
