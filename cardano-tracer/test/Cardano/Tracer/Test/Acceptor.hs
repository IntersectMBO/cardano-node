{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Test.Acceptor
  ( AcceptorsMode (..)
  , launchAcceptorsSimple
  ) where

import           Control.Concurrent.STM.TVar (newTVarIO, readTVarIO)
import           Control.Concurrent.Async.Extra (sequenceConcurrently)
import           Control.Concurrent.Extra (newLock)
import           Control.Monad (forever, forM_, unless, void)
import qualified Data.ByteString.Lazy as LBS
import           Data.Fixed (Pico)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           System.IO
import           System.Time.Extra (sleep,Seconds)

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Acceptors.Run
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.Run
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils
import           Trace.Forward.Utils.DataPoint
import           Trace.Forward.Protocol.DataPoint.Type

data AcceptorsMode = Initiator | Responder


launchAcceptorsSimple
  :: AcceptorsMode
  -> Pico
  -> [FilePath]
  -> [(Seconds,[DPName])] -- for each period, a groups of datapoints to query
  -> IO ()
launchAcceptorsSimple mode ekgFreq localSocks dpGroups = do
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

  network' <-
    case mode of
      Initiator -> return
                 $ ConnectTo $ NE.fromList $ map LocalSocket localSocks
      Responder -> do
                   unless (length localSocks == 1) $
                     fail "panic: internal error"
                   return $ AcceptAt (LocalSocket (head localSocks))
    
  let mkConfig =
        TracerConfig
          { networkMagic   = 764824073
          , network        = network'
          , loRequestNum   = Just 1
          , ekgRequestFreq = Just ekgFreq
          , hasEKG         = Nothing
          , hasPrometheus  = Nothing
          , hasRTView      = Nothing
          , logging        = NE.fromList
                               [LoggingParams "/tmp/demo-acceptor" FileMode ForHuman]
          , rotation       = Nothing
          , verbosity      = Just Minimum
          , metricsComp    = Nothing
          , hasForwarding  = Nothing
          }

      tracerEnv =
        TracerEnv
          { teConfig              = mkConfig
          , teConnectedNodes      = connectedNodes
          , teConnectedNodesNames = connectedNodesNames
          , teAcceptedMetrics     = acceptedMetrics
          , teSavedTO             = savedTO
          , teBlockchainHistory   = chainHistory
          , teResourcesHistory    = resourcesHistory
          , teTxHistory           = txHistory
          , teCurrentLogLock      = currentLogLock
          , teCurrentDPLock       = currentDPLock
          , teEventsQueues        = eventsQueues
          , teDPRequestors        = dpRequestors
          , teProtocolsBrake      = protocolsBrake
          , teRTViewPageOpened    = rtViewPageOpened
          , teRTViewStateDir      = Nothing
          , teTracer              = tr
          }

  void . sequenceConcurrently $
      runAcceptors tracerEnv
    : [ runDataPointsPrinter dpNames period dpRequestors (length localSocks)
      | (period,dpNames) <- dpGroups
      ]

----------------------------------------------------------------------   
-- Datapoint abstractions:

type DPName = String
              -- FIXME:
              --  - String because we used getArgs, but elsewhere:
              --    type DataPointName   = Text
              --  - is this going to be problematic??
              
----------------------------------------------------------------------  
-- handle datapoints

-- | To be able to ask any 'DataPoint' by the name without knowing the
--   actual type, we print it out as a raw 'ByteString'.
--
-- However, each ByteString *should*
--  1. be parseable as JSON
--  2. succeed for parseJSON (in FromJSON class) for the type
--     that was originally encoded into the datapoint.

runDataPointsPrinter
  :: [DPName]
  -> Seconds
  -> DataPointRequestors
  -> Int
  -> IO ()
runDataPointsPrinter dpNames wait dpRequestors _lenRequestors =
  do
  forever $ do
    dpReqs <- M.toList <$> readTVarIO dpRequestors
      -- length should expect to be either 0 or _lenRequestors!
    putStrLn $ ":DEBUG: dpReqs: " ++ show(length dpReqs)
    hFlush stdout 
    forM_ dpReqs $ \(nid, dpReq) -> do
      dpValues <- askForDataPoints dpReq (map T.pack dpNames)
      forM_ dpValues $ \(dpName, mValue) ->
        case mValue of
          Nothing -> do
            putStrLn $ ":WARN: no datapoint returned ("
                       <> T.unpack dpName <> ")"
            hFlush stdout
          Just value -> handleDPValue nid dpName value

    sleep wait
    -- FIXME: the "period" is not exactly 'wait'
    -- FIXME: The first time through:
    --   - this will sleep for 'wait' secs
    --   - as dpRequestors contains Map.empty in this case.

handleDPValue  :: NodeId -> DataPointName -> DataPointValue -> IO ()
handleDPValue nodeid dpName v = do
  putStr $ "DataPoint: name: " <> T.unpack dpName
         <> ", node: " <> show nodeid <> ", raw value: "
  LBS.putStr v
  putChar '\n'
  hFlush stdout
  
