{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.Extra (newLock)
import           Criterion.Main
import qualified Data.List.NonEmpty as NE
import           Data.Time.Clock (getCurrentTime)
import           System.FilePath ((</>))
import           System.Directory (getTemporaryDirectory, removePathForcibly)

import           Cardano.Logging hiding (LocalSocket)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.TraceObjects (traceObjectsHandler)
import           Cardano.Tracer.Types (NodeId (..))

main :: IO ()
main = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "cardano-tracer-bench"
      c1 = mkConfig root ForHuman
      c2 = mkConfig root ForMachine
  lock <- newLock

  to10   <- generate 10
  to100  <- generate 100
  to1000 <- generate 1000

  removePathForcibly root

  defaultMain
    [ bgroup "cardano-tracer"
      [ -- 10 'TraceObject's per request.
        bench "Handle TraceObjects LOG,  10"   $ whnfIO $ traceObjectsHandler c1 nId lock to10
      , bench "Handle TraceObjects JSON, 10"   $ whnfIO $ traceObjectsHandler c2 nId lock to10
        -- 100 'TraceObject's per request.
      , bench "Handle TraceObjects LOG,  100"  $ whnfIO $ traceObjectsHandler c1 nId lock to100
      , bench "Handle TraceObjects JSON, 100"  $ whnfIO $ traceObjectsHandler c2 nId lock to100
        -- 1000 'TraceObject's per request.
      , bench "Handle TraceObjects LOG,  1000" $ whnfIO $ traceObjectsHandler c1 nId lock to1000
      , bench "Handle TraceObjects JSON, 1000" $ whnfIO $ traceObjectsHandler c2 nId lock to1000
      ]
    ]
 where
  nId = NodeId "run-user-1000-cardano-tracer-demo.sock@0"

  mkConfig root format = TracerConfig
    { networkMagic   = 764824073
    , network        = AcceptAt (LocalSocket "")
    , loRequestNum   = Nothing
    , ekgRequestFreq = Nothing
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , logging        = NE.fromList [LoggingParams root FileMode format]
    , rotation       = Nothing
    , verbosity      = Nothing
    }

  generate num = replicate num . mkTraceObject <$> getCurrentTime

  mkTraceObject now = TraceObject
    { toHuman     = Just "Human Message About Some Important Information From The Cardano Node"
    , toMachine   = Just "{\"msg\": \"forMachine Important Message\"}"
    , toNamespace = ["name", "space", "for", "bench"]
    , toSeverity  = Info
    , toDetails   = DNormal
    , toTimestamp = now
    , toHostname  = "nixos"
    , toThreadId  = "1"
    }
