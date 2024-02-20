{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Logging hiding (LocalSocket)
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Logs.TraceObjects
import           Cardano.Tracer.Handlers.RTView.Run
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

import           Control.Concurrent.Extra (newLock)
import           Control.Concurrent.STM.TVar (newTVarIO)
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

  let te c =
        TracerEnv
          { teConfig                = c
          , teConnectedNodes        = connectedNodes
          , teConnectedNodesNames   = connectedNodesNames
          , teAcceptedMetrics       = acceptedMetrics
          , teSavedTO               = savedTO
          , teBlockchainHistory     = chainHistory
          , teResourcesHistory      = resourcesHistory
          , teTxHistory             = txHistory
          , teCurrentLogLock        = currentLogLock
          , teCurrentDPLock         = currentDPLock
          , teEventsQueues          = eventsQueues
          , teDPRequestors          = dpRequestors
          , teProtocolsBrake        = protocolsBrake
          , teRTViewPageOpened      = rtViewPageOpened
          , teRTViewStateDir        = Nothing
          , teTracer                = tr
          , teReforwardTraceObjects = \_-> pure ()
          }
      te1 = te c1
      te2 = te c2

  removePathForcibly root

  defaultMainWith defaultConfig { Criterion.verbosity = Criterion.Verbose }
    [ bgroup "cardano-tracer"
      [  -- 10 'TraceObject's per request.
        bench "Handle TraceObjects LOG,  10"   $ whnfIO $ beforeProgramStops do traceObjectsHandler te1 nId to10
      , bench "Handle TraceObjects JSON, 10"   $ whnfIO $ beforeProgramStops do traceObjectsHandler te2 nId to10
        -- 100 'TraceObject's per request.
      , bench "Handle TraceObjects LOG,  100"  $ whnfIO $ beforeProgramStops do traceObjectsHandler te1 nId to100
      , bench "Handle TraceObjects JSON, 100"  $ whnfIO $ beforeProgramStops do traceObjectsHandler te2 nId to100
        -- 1000 'TraceObject's per request.
      , bench "Handle TraceObjects LOG,  1000" $ whnfIO $ beforeProgramStops do traceObjectsHandler te1 nId to1000
      , bench "Handle TraceObjects JSON, 1000" $ whnfIO $ beforeProgramStops do traceObjectsHandler te2 nId to1000
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
