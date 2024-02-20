{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Test.Acceptor
  ( AcceptorsMode (..)
  , launchAcceptorsSimple
  ) where

import           Cardano.Tracer.Acceptors.Run
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.Run
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

import           Control.Concurrent.Async.Extra (sequenceConcurrently)
import           Control.Concurrent.Extra (newLock)
import           Control.Concurrent.STM.TVar (newTVarIO, readTVarIO)
import           Control.Monad (forM_, forever, void)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           System.Time.Extra (sleep)

import           Trace.Forward.Utils.DataPoint

data AcceptorsMode = Initiator | Responder

launchAcceptorsSimple
  :: AcceptorsMode
  -> FilePath
  -> String
  -> IO ()
launchAcceptorsSimple mode localSock dpName = do
  protocolsBrake <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  connectedNodes <- initConnectedNodes
  connectedNodesNames <- initConnectedNodesNames
  acceptedMetrics <- initAcceptedMetrics
  savedTO <- initSavedTraceObjects
  currentLogLock <- newLock
  currentDPLock <- newLock
  eventsQueues <- initEventsQueues Nothing connectedNodesNames dpRequestors currentDPLock

  chainHistory <- initBlockchainHistory
  resourcesHistory <- initResourcesHistory
  txHistory <- initTransactionsHistory

  rtViewPageOpened <- newTVarIO False

  tr <- mkTracerTracer $ SeverityF $ Just Warning

  let tracerEnv =
        TracerEnv
          { teConfig                = mkConfig
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
            -- NOTE: no reforwarding in this acceptor.
  void . sequenceConcurrently $
    [ runAcceptors tracerEnv
    , runDataPointsPrinter dpName dpRequestors
    ]
 where
  mkConfig = TracerConfig
    { networkMagic   = 764824073
    , network        = case mode of
                         Initiator -> ConnectTo $ NE.fromList [LocalSocket localSock]
                         Responder -> AcceptAt (LocalSocket localSock)
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , hasRTView      = Nothing
    , logging        = NE.fromList [LoggingParams "/tmp/demo-acceptor" FileMode ForHuman]
    , rotation       = Nothing
    , verbosity      = Just Minimum
    , metricsComp    = Nothing
    , hasForwarding  = Nothing
    , resourceFreq   = Nothing
    }

-- | To be able to ask any 'DataPoint' by the name without knowing the actual type,
--   we print it out as a raw 'ByteString'.
runDataPointsPrinter
  :: String
  -> DataPointRequestors
  -> IO ()
runDataPointsPrinter dpName dpRequestors = forever $ do
  sleep 1.0
  dpReqs <- M.toList <$> readTVarIO dpRequestors
  forM_ dpReqs $ \(_, dpReq) -> do
    dpValues <- askForDataPoints dpReq [T.pack dpName]
    forM_ dpValues $ \(dpName', dpValue) ->
      case dpValue of
        Nothing -> return ()
        Just rawDPValue -> do
          putStr $ "DataPoint, name: " <> T.unpack dpName' <> ", raw value: "
          LBS.putStr rawDPValue
          putStrLn ""
