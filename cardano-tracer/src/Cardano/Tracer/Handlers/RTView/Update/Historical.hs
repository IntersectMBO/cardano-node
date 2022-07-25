{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Historical
  ( backupAllHistory
  , restoreHistoryFromBackup
  , restoreHistoryFromBackupAll
  , runHistoricalBackup
  , runHistoricalUpdater
  ) where

import           Control.Concurrent.Async (forConcurrently_)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (modifyTVar', readTVar, readTVarIO)
import           Control.Exception.Extra (ignore, try_)
import           Control.Monad (forM, forM_, forever, unless)
import           Control.Monad.Extra (ifM, whenJust)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as CSV
import           Data.List (find, isInfixOf, partition)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import qualified Data.Vector as V
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import           System.Directory.Extra (listFiles)
import           System.FilePath ((</>), takeBaseName)
import           System.Time.Extra (sleep)
import           Text.Read (readMaybe)

import           Cardano.Node.Startup (NodeInfo (..))

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.State.Last
import           Cardano.Tracer.Handlers.RTView.System
import           Cardano.Tracer.Handlers.RTView.Update.Chain
import           Cardano.Tracer.Handlers.RTView.Update.Leadership
import           Cardano.Tracer.Handlers.RTView.Update.Resources
import           Cardano.Tracer.Handlers.RTView.Update.Transactions
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

-- | A lot of information received from the node is useful as historical data.
--   It means that such an information should be displayed on time charts,
--   where X axis is a time in UTC. An example: resource metrics, chain information,
--   tx information, etc.
--
--   This information is extracted both from 'TraceObject's and 'EKG.Metrics' and then
--   it will be saved as chart coords '[(ts, v)]', where 'ts' is a timestamp
--   and 'v' is a value. Later, when the user will open RTView web-page, this
--   saved data will be used to render historical charts.
--
--   It allows to collect historical data even when RTView web-page is closed.
--
runHistoricalUpdater
  :: TracerEnv
  -> LastResources
  -> IO ()
runHistoricalUpdater tracerEnv lastResources = forever $ do
  sleep 1.0 -- TODO: should it be configured?
  now <- systemToUTCTime <$> getSystemTime
  allMetrics <- readTVarIO teAcceptedMetrics
  forM_ (M.toList allMetrics) $ \(nodeId, (ekgStore, _)) -> do
    metrics <- getListOfMetrics ekgStore
    forM_ metrics $ \(metricName, metricValue) -> do
      updateTransactionsHistory nodeId teTxHistory metricName metricValue now
      updateResourcesHistory nodeId teResourcesHistory lastResources metricName metricValue now
      updateBlockchainHistory nodeId teBlockchainHistory metricName metricValue now
      updateLeadershipHistory nodeId teBlockchainHistory metricName metricValue now
 where
  TracerEnv{teAcceptedMetrics, teTxHistory, teResourcesHistory, teBlockchainHistory} = tracerEnv

runHistoricalBackup :: TracerEnv -> IO ()
runHistoricalBackup tracerEnv = forever $ do
  sleep 300.0 -- TODO: 5 minutes, should it be changed?
  backupAllHistory tracerEnv

backupAllHistory :: TracerEnv -> IO ()
backupAllHistory tracerEnv@TracerEnv{teConnectedNodes} = do
  connected <- S.toList <$> readTVarIO teConnectedNodes
  nodesIdsWithNames <- getNodesIdsWithNames tracerEnv connected
  backupDir <- getPathToBackupDir
  (cHistory, rHistory, tHistory) <- atomically $ (,,)
    <$> readTVar chainHistory
    <*> readTVar resourcesHistory
    <*> readTVar txHistory
  -- We can safely work with files for different nodes concurrently.
  forConcurrently_ nodesIdsWithNames $ \(nodeId, nodeName) -> do
    backupHistory backupDir cHistory nodeId nodeName
    backupHistory backupDir rHistory nodeId nodeName
    backupHistory backupDir tHistory nodeId nodeName
  -- Now we can remove historical points from histories,
  -- to prevent big memory consumption.
  cleanupHistoryPoints chainHistory
  cleanupHistoryPoints resourcesHistory
  cleanupHistoryPoints txHistory
 where
  TracerEnv{teBlockchainHistory, teResourcesHistory, teTxHistory} = tracerEnv
  ChainHistory chainHistory   = teBlockchainHistory
  ResHistory resourcesHistory = teResourcesHistory
  TXHistory txHistory         = teTxHistory

  backupHistory backupDir history nodeId nodeName =
    whenJust (M.lookup nodeId history) $ \historyData -> ignore $ do
      let nodeSubdir = backupDir </> T.unpack nodeName
      createDirectoryIfMissing True nodeSubdir
      forM_ (M.toList historyData) $ \(historyDataName, historyPoints) -> do
        let historyDataFile = nodeSubdir </> show historyDataName
        ifM (doesFileExist historyDataFile)
          (BSL.appendFile historyDataFile $ pointsToBS historyPoints)
          (BSL.writeFile  historyDataFile $ pointsToBS historyPoints)

  pointsToBS = CSV.encode . S.toAscList

  -- Remove sets of historical points only, because they are already backed up.
  cleanupHistoryPoints history = atomically $
    modifyTVar' history $ M.map (M.map (const S.empty))

data HistoryMark = LatestHistory | AllHistory

restoreHistoryFromBackup
  :: TracerEnv
  -> Set NodeId
  -> IO ()
restoreHistoryFromBackup = restoreHistoryFromBackup' LatestHistory Nothing

restoreHistoryFromBackupAll
  :: TracerEnv
  -> DataName
  -> IO ()
restoreHistoryFromBackupAll tracerEnv@TracerEnv{teConnectedNodes} dataName =
  readTVarIO teConnectedNodes >>= restoreHistoryFromBackup' AllHistory (Just dataName) tracerEnv

restoreHistoryFromBackup'
  :: HistoryMark
  -> Maybe DataName
  -> TracerEnv
  -> Set NodeId
  -> IO ()
restoreHistoryFromBackup' historyMark aDataName tracerEnv connected = ignore $ do
  nodesIdsWithNames <- getNodesIdsWithNames tracerEnv (S.toList connected)
  backupDir <- getPathToBackupDir
  forM_ nodesIdsWithNames $ \(nodeId, nodeName) -> do
    let nodeSubdir = backupDir </> T.unpack nodeName
    doesDirectoryExist nodeSubdir >>= \case
      False -> return () -- There is no backup for this node.
      True -> do
        backupFiles <- listFiles nodeSubdir
        -- Check if we need a history for all historical data or for particular one only.
        namesWithPoints <-
          case aDataName of
            Nothing ->
              forM backupFiles $
                extractNamesWithHistoricalPoints nodeSubdir
            Just oneDataName ->
              case find (\bFile -> show oneDataName `isInfixOf` bFile) backupFiles of
                Nothing -> return []
                Just backupFile -> do
                  nameWithPoints <- extractNamesWithHistoricalPoints nodeSubdir backupFile
                  return [nameWithPoints]

        fillHistory nodeId chainHistory     chainData namesWithPoints
        fillHistory nodeId resourcesHistory resData   namesWithPoints
        fillHistory nodeId txHistory        txData    namesWithPoints
 where
  TracerEnv{teBlockchainHistory, teResourcesHistory, teTxHistory} = tracerEnv
  ChainHistory chainHistory   = teBlockchainHistory
  ResHistory resourcesHistory = teResourcesHistory
  TXHistory txHistory         = teTxHistory

  extractNamesWithHistoricalPoints nodeSubdir bFile = do
    let pureFile = takeBaseName bFile
    case readMaybe pureFile of
      Nothing -> return noPoints
      Just (dataName :: DataName) -> do
        -- Ok, this file contains historical points for 'dataName', extract them...
        let backupFile = nodeSubdir </> pureFile
        try_ (BSL.readFile backupFile) >>= \case
          Left _ -> return noPoints
          Right rawPoints ->
            case CSV.decode CSV.NoHeader rawPoints of
              Left _ -> return noPoints -- Maybe file was broken...
              Right (pointsV :: V.Vector HistoricalPoint) -> do
                let points = V.toList pointsV
                if null points
                  then return noPoints
                  else
                    -- Now we extracted all the points from this backup file.
                    -- Check if we need all of them or the latest ones only.
                    case historyMark of
                      LatestHistory -> do
                        now <- systemToUTCTime <$> getSystemTime
                        -- Ok, take the points for the last 6 hours, and only these
                        -- points will be rendered on JS-charts.
                        let sixHoursInS = 21600
                            !firstTSWeNeed = utc2s now - sixHoursInS
                            (earlyPoints, pointsWeNeed) =
                              partition (\(ts, _) -> ts < firstTSWeNeed) points
                        unless (null pointsWeNeed) $
                          -- Now we re-write backup file with all the points
                          -- except points we need now. These "last" points
                          -- will be stored in this file again (with the new points)
                          -- during the first backup.
                          BSL.writeFile backupFile $ CSV.encode earlyPoints
                        return (Just dataName, S.fromList pointsWeNeed)
                      AllHistory ->
                        -- Ok, take all the history.
                        return (Just dataName, S.fromList points)

  noPoints = (Nothing, S.empty)

  fillHistory _ _ _ [] = return ()
  fillHistory nodeId history dataNames dataNamesWithPoints = do
    let backupData = mkMapOfData dataNamesWithPoints M.empty
    atomically . modifyTVar' history $ \currentHistory ->
      case M.lookup nodeId currentHistory of
        Nothing -> M.insert nodeId backupData currentHistory
        Just currentData -> M.adjust (const $ M.unionWith S.union currentData backupData)
                                     nodeId currentHistory
   where
    mkMapOfData [] aMap = aMap
    mkMapOfData ((mDataName, points):others) aMap =
      case mDataName of
        Nothing -> mkMapOfData others aMap
        Just dataName ->
          if dataName `elem` dataNames
            then mkMapOfData others $ M.insert dataName points aMap
            else mkMapOfData others aMap

  chainData =
    [ ChainDensityData
    , SlotNumData
    , BlockNumData
    , SlotInEpochData
    , EpochData
    , NodeCannotForgeData
    , ForgedSlotLastData
    , NodeIsLeaderData
    , NodeIsNotLeaderData
    , ForgedInvalidSlotLastData
    , AdoptedSlotLastData
    , NotAdoptedSlotLastData
    , AboutToLeadSlotLastData
    , CouldNotForgeSlotLastData
    ]

  resData =
    [ CPUData
    , MemoryData
    , GCMajorNumData
    , GCMinorNumData
    , GCLiveMemoryData
    , CPUTimeGCData
    , CPUTimeAppData
    , ThreadsNumData
    ]

  txData =
    [ TxsProcessedNumData
    , MempoolBytesData
    , TxsInMempoolData
    ]

getNodesIdsWithNames
  :: TracerEnv
  -> [NodeId]
  -> IO [(NodeId, T.Text)]
getNodesIdsWithNames _ [] = return []
getNodesIdsWithNames TracerEnv{teDPRequestors, teCurrentDPLock} connected =
  forM connected $ \nodeId@(NodeId anId) ->
    askDataPoint teDPRequestors teCurrentDPLock nodeId "NodeInfo" >>= \case
      Nothing -> return (nodeId, anId)
      Just ni -> return (nodeId, niName ni)
