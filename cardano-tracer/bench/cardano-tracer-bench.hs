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
import           Control.Concurrent.STM.TVar (newTVarIO)
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
  savedTO         <- initSavedTraceObjects

  chainHistory     <- initBlockchainHistory
  resourcesHistory <- initResourcesHistory
  txHistory        <- initTransactionsHistory

  protocolsBrake <- initProtocolsBrake
  dpRequestors   <- initDataPointRequestors

  currentLogLock <- newLock
  currentDPLock  <- newLock
  eventsQueues   <- initEventsQueues Nothing connectedNodesNames dpRequestors currentDPLock

  rtViewPageOpened <- newTVarIO False

  tr <- mkTracerTracer $ SeverityF $ Just Warning

  let te :: TracerConfig -> HandleRegistry -> TracerEnv
      te c r = TracerEnv
        { teConfig                = c
        , teConnectedNodes        = connectedNodes
        , teConnectedNodesNames   = connectedNodesNames
        , teAcceptedMetrics       = acceptedMetrics
        , teCurrentLogLock        = currentLogLock
        , teCurrentDPLock         = currentDPLock
        , teDPRequestors          = dpRequestors
        , teProtocolsBrake        = protocolsBrake
        , teTracer                = tr
        , teReforwardTraceObjects = \_-> pure ()
        , teRegistry              = r
        , teStateDir              = Nothing
        }

      tracerEnvRTView :: TracerEnvRTView
      tracerEnvRTView = TracerEnvRTView
        { teSavedTO           = savedTO
        , teBlockchainHistory = chainHistory
        , teResourcesHistory  = resourcesHistory
        , teTxHistory         = txHistory
        , teEventsQueues      = eventsQueues
        , teRTViewPageOpened  = rtViewPageOpened
        }

  removePathForcibly root

  let -- Handles cleanup between runs, closes handles before
      myBench :: TracerConfig -> [TraceObject] -> Benchmarkable
      myBench config traceObjects = let

        action :: IO TracerEnv
        action = te config <$> newRegistry

        cleanup :: TracerEnv -> IO ()
        cleanup TracerEnv{teRegistry} = clearRegistry teRegistry

        benchmark :: TracerEnv -> IO ()
        benchmark traceEnv = beforeProgramStops do
          traceObjectsHandler traceEnv nId traceObjects

        in
        perRunEnvWithCleanup @TracerEnv action cleanup benchmark

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
