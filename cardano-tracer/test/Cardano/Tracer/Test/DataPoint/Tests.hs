{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Test.DataPoint.Tests
  ( tests
  ) where

import           Control.Concurrent.Async (withAsync)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import           Data.Aeson (decode')
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.Time.Extra

import           Trace.Forward.Protocol.DataPoint.Type
import           Trace.Forward.Utils.DataPoint (askForDataPoints)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Run (doRunCardanoTracer)
import           Cardano.Tracer.Utils (applyBrake, initProtocolsBrake, initDataPointRequestors)

import           Cardano.Tracer.Test.Forwarder
import           Cardano.Tracer.Test.Utils

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.DataPoint"
  [ testProperty "ask" $ propRunInLogsStructure propDataPoint
  ]

propDataPoint :: FilePath -> FilePath -> IO Property
propDataPoint rootDir localSock = do
  stopProtocols <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  savedDPValues :: TVar DataPointValues <- newTVarIO []
  withAsync (doRunCardanoTracer config stopProtocols dpRequestors) . const $
    withAsync (launchForwardersSimple Responder localSock 1000 10000) . const $ do
      sleep 1.0
      -- We know that there is one single "node" only (and one single requestor too).
      ((_, dpRequestor):_) <- M.toList <$> readTVarIO dpRequestors
      dpValues <- askForDataPoints dpRequestor ["test.data.point", "some.wrong.Name"]
      atomically . modifyTVar' savedDPValues . const $ dpValues
      applyBrake stopProtocols
      sleep 0.5

  dpValues <- readTVarIO savedDPValues
  if length dpValues == 2
    then
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
    else false "Not 2 values"
 where
  config = TracerConfig
    { networkMagic   = 764824073
    , network        = ConnectTo $ NE.fromList [LocalSocket localSock]
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , logging        = NE.fromList [LoggingParams rootDir FileMode ForHuman]
    , rotation       = Nothing
    , verbosity      = Just Minimum
    }
