{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Test.Acceptor
  ( AcceptorsMode (..)
  , launchAcceptorsSimple
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Concurrent.Async.Extra (sequenceConcurrently)
import           Control.Concurrent.Extra (newLock)
import           Control.Monad (forever, forM_, void)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Acceptors.Run (runAcceptors)
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Types (DataPointRequestors)
import           Cardano.Tracer.Utils (initAcceptedMetrics, initConnectedNodes,
                   initDataPointRequestors, initProtocolsBrake)
import           Trace.Forward.Utils.DataPoint (askForDataPoints)

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
  acceptedMetrics <- initAcceptedMetrics
  currentLogLock <- newLock
  void . sequenceConcurrently $
    [ runAcceptors mkConfig connectedNodes acceptedMetrics
                   dpRequestors protocolsBrake currentLogLock
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
    , logging        = NE.fromList [LoggingParams "/tmp/demo-acceptor" FileMode ForHuman]
    , rotation       = Nothing
    , verbosity      = Just Minimum
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
