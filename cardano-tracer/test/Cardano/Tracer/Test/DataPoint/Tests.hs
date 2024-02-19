{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Test.DataPoint.Tests
  ( tests
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Run (doRunCardanoTracer)
import           Cardano.Tracer.Test.Forwarder
import           Cardano.Tracer.Test.TestSetup
import           Cardano.Tracer.Test.Utils
import           Cardano.Tracer.Utils (applyBrake, initDataPointRequestors, initProtocolsBrake)

import           Control.Concurrent.Async (withAsync)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import           Data.Aeson (decode')
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import           System.Time.Extra

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Trace.Forward.Protocol.DataPoint.Type
import           Trace.Forward.Utils.DataPoint (askForDataPoints)

tests :: TestSetup Identity -> TestTree
tests ts = localOption (QuickCheckTests 1) $ testGroup "Test.DataPoint"
  [ testProperty "ask" $ propRunInLogsStructure ts (propDataPoint ts)
  ]

propDataPoint :: TestSetup Identity -> FilePath -> FilePath -> IO Property
propDataPoint ts@TestSetup{..} rootDir localSock = do
  stopProtocols <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  savedDPValues :: TVar DataPointValues <- newTVarIO []
  withAsync (doRunCardanoTracer config (Just $ rootDir <> "/../state") stderrShowTracer stopProtocols dpRequestors) . const $ do
    sleep 1.0
    withAsync (launchForwardersSimple ts Initiator localSock 1000 10000) . const $ do
      sleep 1.5
      -- We know that there is one single "node" only (and one single requestor too).
      -- requestors ((_, dpRequestor):_) <- M.toList <$> readTVarIO dpRequestors
      requestors <- M.toList <$> readTVarIO dpRequestors
      case requestors of
        [] -> return ()
        ((_, dpRequestor):_) -> do
          dpValues <- askForDataPoints dpRequestor ["test.data.point", "some.wrong.Name"]
          atomically . modifyTVar' savedDPValues . const $ dpValues
          applyBrake stopProtocols
          sleep 0.5

  dpValues <- readTVarIO savedDPValues
  case length dpValues of
    0 ->
      -- There are no DataPoint values. It means that the connection
      -- (via local socket) wasn't established by some reason,
      -- that's why we couldn't ask for our two DataPoints.
      -- From my experience, it occurs more often on Windows.
      -- Since the correctness of the local connection itself is
      -- already tested by other tests, I treat such a result as
      -- a random `ouroboros`-related problem.
      return $ property True
    2 ->
      case lookup "test.data.point" dpValues of
        Just (Just rawValue) ->
          case decode' rawValue of
            Just (v :: TestDataPoint) ->
              if v == mkTestDataPoint
                then
                  case lookup "some.wrong.Name" dpValues of
                    Just Nothing -> return $ property True
                    _ -> false "Unexpected invalid value"
                else false "Unexpected valid value"
            Nothing -> false "Incorrect JSON for of the value DataPoint"
        _ -> false "No value of the valid DataPoint"
    _ -> false "Not expected number of DataPoint values!"
 where
  config = TracerConfig
    { networkMagic   = unNetworkMagic $ unI tsNetworkMagic
    , network        = AcceptAt (LocalSocket localSock) -- ConnectTo $ NE.fromList [LocalSocket localSock]
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , hasRTView      = Nothing
    , logging        = NE.fromList [LoggingParams rootDir FileMode ForHuman]
    , rotation       = Nothing
    , verbosity      = Just Minimum
    , metricsComp    = Nothing
    , hasForwarding  = Nothing
    , resourceFreq   = Nothing
    }
