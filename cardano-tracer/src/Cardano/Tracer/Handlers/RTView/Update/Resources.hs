{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Resources
  ( updateResourcesHistory
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import qualified Data.Map.Strict as M
import           Data.Text.Read (decimal)
import           Data.Time.Clock (UTCTime)
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
    "Resources.Stat.Cputicks"    -> updateCPUUsage
    "Resources.mem.Resident"     -> updateRSSMemory
    "Resources.RTS.GcLiveBytes"  -> updateGCLiveMemory
    "Resources.RTS.GcMajorNum"   -> updateGCMajorNum
    "Resources.RTS.GcMinorNum"   -> updateGCMinorNum
    "Resources.RTS.Gcticks"      -> updateCPUTimeGC
    "Resources.RTS.Mutticks"     -> updateCPUTimeApp
    "Resources.RTS.Stat.Threads" -> updateThreadsNum
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
        lastOnes <- readTVarIO lastResources
        case M.lookup nodeId lastOnes of
          Nothing ->
            -- There is no last resources for this node yet.
            addNullResources lastResources nodeId
          Just resourcesForNode -> do
            let tns        = utc2ns now
                tDiffInSec = max 0.1 $ fromIntegral (tns - cpuGCLastNS resourcesForNode) / 1000_000_000 :: Double
                ticksDiff  = cpuTimeGCInCentiS - cpuGCLastTicks resourcesForNode
                !cpuV      = fromIntegral ticksDiff / fromIntegral (100 :: Int) / tDiffInSec
                newCPUPct  = if cpuV < 0 then 0.0 else cpuV * 100.0
            addHistoricalData rHistory nodeId now CPUTimeGCData $ ValueD newCPUPct
            updateLastResources lastResources nodeId $ \current ->
              current { cpuGCLastTicks = cpuTimeGCInCentiS
                      , cpuGCLastNS    = tns
                      }

  updateCPUTimeApp =
    case decimal metricValue of
      Left _ -> return ()
      Right (cpuTimeAppInCentiS :: Int, _) -> do
        lastOnes <- readTVarIO lastResources
        case M.lookup nodeId lastOnes of
          Nothing ->
            -- There is no last resources for this node yet.
            addNullResources lastResources nodeId
          Just resourcesForNode -> do
            let tns        = utc2ns now
                tDiffInSec = max 0.1 $ fromIntegral (tns - cpuAppLastNS resourcesForNode) / 1000_000_000 :: Double
                ticksDiff  = cpuTimeAppInCentiS - cpuAppLastTicks resourcesForNode
                !cpuV      = fromIntegral ticksDiff / fromIntegral (100 :: Int) / tDiffInSec
                newCPUPct  = if cpuV < 0 then 0.0 else cpuV * 100.0
            addHistoricalData rHistory nodeId now CPUTimeAppData $ ValueD newCPUPct
            updateLastResources lastResources nodeId $ \current ->
              current { cpuAppLastTicks = cpuTimeAppInCentiS
                      , cpuAppLastNS    = tns
                      }

  updateThreadsNum =
    readValueI metricValue $ addHistoricalData rHistory nodeId now ThreadsNumData
