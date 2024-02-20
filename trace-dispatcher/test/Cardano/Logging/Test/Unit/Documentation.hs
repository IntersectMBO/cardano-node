{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Cardano.Logging.Test.Unit.Documentation (
    docTracers
) where

import           Cardano.Logging
import           Cardano.Logging.Test.Tracer
import           Cardano.Logging.Test.Unit.TestObjects

import           Data.IORef
import qualified Data.Map as Map
import qualified Data.Text as T

docTracers :: IO T.Text
docTracers = do
  testTracerRef <- newIORef []
  tt <- testTracer testTracerRef
  t1   <- mkCardanoTracer tt tt Nothing ["Node1"]
  t2   <- mkCardanoTracer tt tt Nothing ["Node2"]
  confState <- emptyConfigReflection
  configureTracers confState config1 [t1, t2]
  b1 <- documentTracer (t1 :: Trace IO (TraceForgeEvent LogBlock))
  b2 <- documentTracer (t2 :: Trace IO (TraceForgeEvent LogBlock))
  docuResultsToText (b1 <> b2) config1

config1 :: TraceConfig
config1 = TraceConfig {
      tcOptions = Map.fromList
          [ ([], [ConfSeverity (SeverityF Nothing), ConfBackend [Stdout MachineFormat]])
          , (["node2"], [ConfSeverity (SeverityF (Just Info)),  ConfBackend [Stdout MachineFormat]])
          , (["node1"], [ConfSeverity (SeverityF (Just Warning)),  ConfBackend [Stdout MachineFormat]])
          ]
    , tcForwarder = Just TraceOptionForwarder {
        tofConnQueueSize = 100
      , tofDisconnQueueSize = 1000
      , tofVerbosity = Minimum
      }
    , tcNodeName = Nothing
    , tcPeerFrequency = Nothing
    , tcResourceFrequency = Nothing
    }

