{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Update.EKG
  ( updateEKGMetrics
  ) where

import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Monad (forM_, unless)
import Data.List (partition, sort)
import Data.Map.Strict qualified as M
import Data.Text (intercalate, isPrefixOf)
import Graphics.UI.Threepenny.Core (UI, liftIO)

import Cardano.Logging (showT)

import Cardano.Tracer.Utils (stmMapToList)
import Cardano.Tracer.Environment
import Cardano.Tracer.Handlers.Metrics.Utils
import Cardano.Tracer.Handlers.RTView.UI.Utils
import Cardano.Tracer.Types

import Control.Concurrent.STM
import StmContainers.Map qualified as STM.Map
import ListT qualified

updateEKGMetrics :: TracerEnv -> UI ()
updateEKGMetrics TracerEnv{teAcceptedMetrics} = do
  allMetrics <- liftIO do
    atomically do
      stmMapToList teAcceptedMetrics
  forM_ allMetrics \(NodeId anId, (ekgStore, _)) -> do
    metrics <- liftIO $ getListOfMetrics ekgStore
    unless (null metrics) do
      setTextValue (anId <> "__node-ekg-metrics-num") (showT $ length metrics)
      let sortedMetrics = sort metrics
          (rtsGCPredefinedMetrics, otherMetrics) = partition rtsGCPredefined sortedMetrics
          (mNames,    mValues)    = unzip otherMetrics
          (mNamesPre, mValuesPre) = unzip rtsGCPredefinedMetrics
          allNames  = intercalate br mNames  <> br <> br <> intercalate br mNamesPre
          allValues = intercalate br mValues <> br <> br <> intercalate br mValuesPre
      setTextValue (anId <> "__node-ekg-metrics-names")  allNames
      setTextValue (anId <> "__node-ekg-metrics-values") allValues
 where
  rtsGCPredefined (mName, _) = "rts.gc." `isPrefixOf` mName
  br = "<br>"
