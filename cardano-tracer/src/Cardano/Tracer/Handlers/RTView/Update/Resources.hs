{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Resources
  ( updateResourcesHistory
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import qualified Data.Map.Strict as M
import           Data.Time.Clock
import           Data.Text.Read
import           Data.Word (Word64)

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.State.Last
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

updateResourcesHistory
  :: NodeId
  -> ResourcesHistory
  -> LastResources
  -> MetricName
  -> MetricValue
  -> UTCTime
  -> IO ()
updateResourcesHistory nodeId (ResHistory rHistory) lastResources metricName metricValue now =
  case metricName of
    "stat.cputicks"    -> updateCPUUsage
    "mem.resident"     -> updateRSSMemory
    "rts.gcLiveBytes"  -> updateGCLiveMemory
    "rts.gcMajorNum"   -> updateGCMajorNum
    "rts.gcMinorNum"   -> updateGCMinorNum
    "rts.gcticks"      -> updateCPUTimeGC
    "rts.mutticks"     -> updateCPUTimeApp
    "rts.stat.threads" -> updateThreadsNum
    _ -> return ()
 where
  updateCPUUsage =
    case decimal metricValue of
      Left _ -> return ()
      Right (cpuTicks :: Int, _) -> do
        lastOnes <- readTVarIO lastResources
        case M.lookup nodeId lastOnes of
          Nothing ->
            -- There is no last resources for this node yet.
            addNullResources lastResources nodeId
          Just resourcesForNode -> do
            let tns        = utc2ns now
                tDiffInSec = max 0.1 $ fromIntegral (tns - cpuLastNS resourcesForNode) / 1000_000_000 :: Double
                ticksDiff  = cpuTicks - cpuLastTicks resourcesForNode
                !cpuV      = fromIntegral ticksDiff / fromIntegral (100 :: Int) / tDiffInSec
                newCPUPct  = if cpuV < 0 then 0.0 else cpuV * 100.0
            addHistoricalData rHistory nodeId now CPUData $ ValueD newCPUPct
            updateLastResources lastResources nodeId $ \current ->
              current { cpuLastTicks = cpuTicks
                      , cpuLastNS = tns
                      }

  updateRSSMemory =
    case decimal metricValue of
      Left _ -> return ()
      Right (bytes :: Word64, _) -> do
        let !memoryInMB = fromIntegral bytes / 1024 / 1024 :: Double
        addHistoricalData rHistory nodeId now MemoryData $ ValueD memoryInMB

  updateGCLiveMemory =
    case decimal metricValue of
      Left _ -> return ()
      Right (bytes :: Word64, _) -> do
        let !memoryInMB = fromIntegral bytes / 1024 / 1024 :: Double
        addHistoricalData rHistory nodeId now GCLiveMemoryData $ ValueD memoryInMB

  updateGCMajorNum =
    readValueI metricValue $ addHistoricalData rHistory nodeId now GCMajorNumData

  updateGCMinorNum =
    readValueI metricValue $ addHistoricalData rHistory nodeId now GCMinorNumData

  updateCPUTimeGC =
    case decimal metricValue of
      Left _ -> return ()
      Right (cpuTimeGCInCentiS :: Int, _) -> do
        -- This is a total CPU time used by the GC, as 1/100 second.
        let !cpuTimeGCInMs = cpuTimeGCInCentiS * 10
        addHistoricalData rHistory nodeId now CPUTimeGCData $ ValueI cpuTimeGCInMs

  updateCPUTimeApp =
    case decimal metricValue of
      Left _ -> return ()
      Right (cpuTimeAppInCentiS :: Int, _) -> do
        -- This is a total CPU time used by the the node itself, as 1/100 second.
        let !cpuTimeAppInMs = cpuTimeAppInCentiS * 10
        addHistoricalData rHistory nodeId now CPUTimeAppData $ ValueI cpuTimeAppInMs

  updateThreadsNum =
    readValueI metricValue $ addHistoricalData rHistory nodeId now ThreadsNumData
