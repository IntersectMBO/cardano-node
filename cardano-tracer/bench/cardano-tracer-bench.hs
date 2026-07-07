{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Logging hiding (LocalSocket)
import qualified Cardano.Logging.Types as Net
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Logs.TraceObjects
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils


import           Control.Concurrent.Extra (newLock)
import           Control.DeepSeq
import qualified Data.List.NonEmpty as NE
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           System.Directory (getTemporaryDirectory, removePathForcibly)
import           System.FilePath ((</>))

import           Criterion.Main
import qualified Criterion.Types as Criterion

main :: IO ()
main = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "cardano-tracer-bench"
      c1 = mkConfig root ForHuman
      c2 = mkConfig root ForMachine

  to10   <- generate 10
  to100  <- generate 100
  to1000 <- generate 1000

  connectedNodes <- initConnectedNodes
  connectedNodesNames <- initConnectedNodesNames
  acceptedMetrics <- initAcceptedMetrics

  protocolsBrake <- initProtocolsBrake
  dpRequestors   <- initDataPointRequestors

  currentLogLock <- newLock
  currentDPLock  <- newLock

  tracer <- mkTraceBundle $ SeverityF $ Just Warning

  let tracerEnv :: TracerConfig -> HandleRegistry -> TracerEnv
      tracerEnv config handleRegistry = TracerEnv
        { teConfig                = config
        , teConnectedNodes        = connectedNodes
        , teConnectedNodesNames   = connectedNodesNames
        , teAcceptedMetrics       = acceptedMetrics
        , teCurrentLogLock        = currentLogLock
        , teCurrentDPLock         = currentDPLock
        , teDPRequestors          = dpRequestors
        , teProtocolsBrake        = protocolsBrake
        , teTracer                = tracer.assorted
        , teReforwardTraceObjects = \_-> pure ()
        , teRegistry              = handleRegistry
        , teStateDir              = Nothing
        , teMetricsHelp           = []
        , teTimeseriesHandle      = Nothing
        }

  removePathForcibly root

  let -- Handles cleanup between runs, closes handles before
      myBench :: TracerConfig -> [TraceObject] -> Benchmarkable
      myBench config traceObjects = let

        initialise :: IO TracerEnv
        initialise =
          tracerEnv config <$> newRegistry

        cleanup :: TracerEnv -> IO ()
        cleanup TracerEnv{teRegistry} =
          clearRegistry teRegistry

        benchmark :: TracerEnv -> IO ()
        benchmark trEnv = do
          beforeProgramStops do
            traceObjectsHandler trEnv nId traceObjects

        in
        perRunEnvWithCleanup @TracerEnv initialise cleanup benchmark

  now <- getCurrentTime

  let csvConfig :: Criterion.Config
      csvConfig = defaultConfig
        { Criterion.csvFile = Just $ "/tmp/cardano-tracer-bench_" ++ show now ++ ".csv"
        }

  defaultMainWith csvConfig { Criterion.verbosity = Criterion.Verbose }
    [ bgroup "cardano-tracer"
      [ -- 10 'TraceObject's per request.
        bench "Handle TraceObjects LOG,  10"   $ myBench c1 to10
      , bench "Handle TraceObjects JSON, 10"   $ myBench c2 to10
        -- 100 'TraceObject's per request.
      , bench "Handle TraceObjects LOG,  100"  $ myBench c1 to100
      , bench "Handle TraceObjects JSON, 100"  $ myBench c2 to100
        -- 1000 'TraceObject's per request.
      , bench "Handle TraceObjects LOG,  1000" $ myBench c1 to1000
      , bench "Handle TraceObjects JSON, 1000" $ myBench c2 to1000
      ]
    ]
 where
  nId :: NodeId
  nId = NodeId "run-user-1000-cardano-tracer-demo.sock@0"

  mkConfig :: FilePath -> LogFormat -> TracerConfig
  mkConfig root format = TracerConfig
    { networkMagic     = 764824073
    , network          = AcceptAt (Net.LocalPipe "")
    , loRequestNum     = Nothing
    , ekgRequestFreq   = Nothing
    , hasEKG           = Nothing
    , hasPrometheus    = Nothing
    , hasTimeseries    = Nothing
    , tlsCertificate   = Nothing
    , logging          = NE.fromList [LoggingParams root FileMode format]
    , rotation         = Nothing
    , verbosity        = Nothing
    , metricsNoSuffix  = Nothing
    , metricsHelp      = Nothing
    , hasForwarding    = Nothing
    , resourceFreq     = Nothing
    , ekgRequestFull   = Nothing
    , prometheusLabels = Nothing
    }

  generate :: Int -> IO [TraceObject]
  generate num = replicate num . mkTraceObject <$> getCurrentTime

  mkTraceObject :: UTCTime -> TraceObject
  mkTraceObject now = TraceObject
    { toHuman     = Just "Human Message About Some Important Information From The Cardano Node"
    , toMachine   = "{\"msg\": \"forMachine Important Message\"}"
    , toNamespace = ["name", "space", "for", "bench"]
    , toSeverity  = Info
    , toDetails   = DNormal
    , toTimestamp = now
    , toHostname  = "nixos"
    , toThreadId  = "1"
    }

-- Orphan instance: The `rnf' should reduce its argument to normal form.
instance NFData TracerEnv where
  rnf = rwhnf
