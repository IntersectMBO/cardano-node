{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Logging hiding (LocalSocket)
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Logs.TraceObjects
#if RTVIEW
import           Cardano.Tracer.Handlers.RTView.Run
import           Cardano.Tracer.Handlers.RTView.State.Historical
#endif
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

import           Control.Concurrent.Extra (newLock)
#if RTVIEW
import           Control.Concurrent.STM.TVar (newTVarIO)
#endif
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
#if RTVIEW
  savedTO         <- initSavedTraceObjects

  chainHistory     <- initBlockchainHistory
  resourcesHistory <- initResourcesHistory
  txHistory        <- initTransactionsHistory
#endif

  protocolsBrake <- initProtocolsBrake
  dpRequestors   <- initDataPointRequestors

  currentLogLock <- newLock
  currentDPLock  <- newLock
#if RTVIEW
  eventsQueues   <- initEventsQueues Nothing connectedNodesNames dpRequestors currentDPLock

  rtViewPageOpened <- newTVarIO False
#endif

  tracer <- mkTracerTracer $ SeverityF $ Just Warning

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
        , teTracer                = tracer
        , teReforwardTraceObjects = \_-> pure ()
        , teRegistry              = handleRegistry
        , teStateDir              = Nothing
        , teMetricsHelp           = []
        }

      tracerEnvRTView :: TracerEnvRTView
      tracerEnvRTView = TracerEnvRTView
#if RTVIEW
        { teSavedTO           = savedTO
        , teBlockchainHistory = chainHistory
        , teResourcesHistory  = resourcesHistory
        , teTxHistory         = txHistory
        , teEventsQueues      = eventsQueues
        , teRTViewPageOpened  = rtViewPageOpened
        }
#endif

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
            traceObjectsHandler trEnv tracerEnvRTView nId traceObjects

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
    { networkMagic   = 764824073
    , network        = AcceptAt (LocalSocket "")
    , loRequestNum   = Nothing
    , ekgRequestFreq = Nothing
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , hasRTView      = Nothing
    , logging        = NE.fromList [LoggingParams root FileMode format]
    , rotation       = Nothing
    , verbosity      = Nothing
    , metricsComp    = Nothing
    , metricsHelp    = Nothing
    , hasForwarding  = Nothing
    , resourceFreq   = Nothing
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
