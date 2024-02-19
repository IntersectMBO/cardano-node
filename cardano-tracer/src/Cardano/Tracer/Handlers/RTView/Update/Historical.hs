{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Historical
  ( backupAllHistory
  , backupSpecificHistory
  , getAllHistoryFromBackup
  , getLastHistoryFromBackups
  , getLastHistoryFromBackupsAll
  , runHistoricalBackup
  , runHistoricalUpdater
  ) where

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
import           Cardano.Tracer.Handlers.RTView.Utils
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

import           Control.Concurrent.Async (forConcurrently_)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (modifyTVar', readTVar, readTVarIO)
import           Control.Exception.Extra (ignore, try_)
import           Control.Monad (forM, forM_, forever)
import           Control.Monad.Extra (ifM, whenJust)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as CSV
import           Data.List (find, isInfixOf)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import qualified Data.Vector as V
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import           System.Directory.Extra (listFiles)
import           System.FilePath (takeBaseName, (</>))
import           System.Time.Extra (sleep)
import           Text.Read (readMaybe)

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
  forAcceptedMetrics_ tracerEnv $ \(nodeId, (ekgStore, _)) ->
    forMM_ (getListOfMetrics ekgStore) $ \(metricName, metricValue) -> do
      updateTransactionsHistory nodeId teTxHistory metricName metricValue now
      updateResourcesHistory nodeId teResourcesHistory lastResources metricName metricValue now
      updateBlockchainHistory nodeId teBlockchainHistory metricName metricValue now
      updateLeadershipHistory nodeId teBlockchainHistory metricName metricValue now
 where
  TracerEnv{teTxHistory, teResourcesHistory, teBlockchainHistory} = tracerEnv

-- | If RTView's web page is opened, historical backup is performing by UI-code,
--   in this case we should skip backup.
runHistoricalBackup :: TracerEnv -> IO ()
runHistoricalBackup tracerEnv@TracerEnv{teRTViewPageOpened} = forever $ do
  sleep 300.0 -- TODO: 5 minutes, should it be changed?
  ifM (readTVarIO teRTViewPageOpened)
    (return ()) -- Skip, UI-code is performing backup.
    (backupAllHistory tracerEnv)

backupAllHistory :: TracerEnv -> IO ()
backupAllHistory tracerEnv@TracerEnv{teConnectedNodes} = do
  connected <- S.toList <$> readTVarIO teConnectedNodes
  nodesIdsWithNames <- getNodesIdsWithNames tracerEnv connected
  backupDir <- getPathToBackupDir tracerEnv
  (cHistory, rHistory, tHistory) <- atomically $ (,,)
    <$> readTVar chainHistory
    <*> readTVar resourcesHistory
    <*> readTVar txHistory
  -- We can safely work with files for different nodes concurrently.
  forConcurrently_ nodesIdsWithNames $ \(nodeId, nodeName) -> do
    backupHistory backupDir cHistory nodeId nodeName Nothing
    backupHistory backupDir rHistory nodeId nodeName Nothing
    backupHistory backupDir tHistory nodeId nodeName Nothing
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

  -- Remove sets of historical points only, because they are already backed up.
  cleanupHistoryPoints history = atomically $
    modifyTVar' history $ M.map (M.map (const S.empty))

-- | Backup specific history after these points were pushed to corresponding JS-chart.
--   After cleanup we can safely remove them from the memory.
backupSpecificHistory
  :: TracerEnv
  -> History
  -> [NodeId]
  -> DataName
  -> IO ()
backupSpecificHistory tracerEnv history connected dataName = do
  backupDir <- getPathToBackupDir tracerEnv
  hist <- readTVarIO history
  forMM_ (getNodesIdsWithNames tracerEnv connected) $ \(nodeId, nodeName) -> do
    backupHistory backupDir hist nodeId nodeName $ Just dataName
    cleanupSpecificHistoryPoints nodeId
 where
  cleanupSpecificHistoryPoints nodeId = atomically $
    -- Removes only the points for 'nodeId' and 'dataName'.
    modifyTVar' history $ \currentHistory ->
      case M.lookup nodeId currentHistory of
        Nothing -> currentHistory
        Just dataForNode ->
          case M.lookup dataName dataForNode of
            Nothing -> currentHistory
            Just _histPoints ->
              let newDataForNode = M.adjust (const S.empty) dataName dataForNode
              in M.adjust (const newDataForNode) nodeId currentHistory

backupHistory
  :: FilePath
  -> Map NodeId HistoricalData
  -> NodeId
  -> T.Text
  -> Maybe DataName
  -> IO ()
backupHistory backupDir history nodeId nodeName mDataName =
  whenJust (M.lookup nodeId history) $ \historyData -> ignore $ do
    let nodeSubdir = backupDir </> T.unpack nodeName
    createDirectoryIfMissing True nodeSubdir
    case mDataName of
      Nothing ->
        forM_ (M.toList historyData) $ doBackup nodeSubdir
      Just dataName ->
        case M.lookup dataName historyData of
          Nothing -> return ()
          Just historyPoints -> doBackup nodeSubdir (dataName, historyPoints)
 where
  doBackup nodeSubdir (dataName, historyPoints) = do
    let historyDataFile = nodeSubdir </> show dataName
    ifM (doesFileExist historyDataFile)
      (BSL.appendFile historyDataFile $ pointsToBS historyPoints)
      (BSL.writeFile  historyDataFile $ pointsToBS historyPoints)

  pointsToBS = CSV.encode . S.toAscList

getAllHistoryFromBackup
  :: TracerEnv
  -> DataName
  -> IO [(NodeId, [HistoricalPoint])]
getAllHistoryFromBackup tracerEnv@TracerEnv{teConnectedNodes} dataName = do
  connected <- S.toList <$> readTVarIO teConnectedNodes
  nodesIdsWithNames <- getNodesIdsWithNames tracerEnv connected
  backupDir <- getPathToBackupDir tracerEnv
  forM nodesIdsWithNames $ \(nodeId, nodeName) -> do
    let nodeSubdir = backupDir </> T.unpack nodeName
    doesDirectoryExist nodeSubdir >>= \case
      False -> return (nodeId, []) -- There is no backup for this node.
      True -> do
        backupFiles <- listFiles nodeSubdir
        case find (\bFile -> show dataName `isInfixOf` bFile) backupFiles of
          Nothing -> return (nodeId, [])
          Just backupFile -> do
            points <- extractHistoricalPoints nodeSubdir backupFile
            return (nodeId, points)
 where
  extractHistoricalPoints nodeSubdir bFile = do
    let backupFile = nodeSubdir </> takeBaseName bFile
    try_ (BSL.readFile backupFile) >>= \case
      Left _ -> return []
      Right rawPoints ->
        case CSV.decode CSV.NoHeader rawPoints of
          Left _ -> return [] -- Maybe file was broken...
          Right (pointsV :: V.Vector HistoricalPoint) -> return $! V.toList pointsV

getLastHistoryFromBackupsAll
  :: TracerEnv
  -> IO [(NodeId, [(DataName, [HistoricalPoint])])]
getLastHistoryFromBackupsAll tracerEnv@TracerEnv{teConnectedNodes} =
  getLastHistoryFromBackups' tracerEnv . S.toList =<< readTVarIO teConnectedNodes

getLastHistoryFromBackups
  :: TracerEnv
  -> Set NodeId
  -> IO [(NodeId, [(DataName, [HistoricalPoint])])]
getLastHistoryFromBackups tracerEnv =
  getLastHistoryFromBackups' tracerEnv . S.toList

getLastHistoryFromBackups'
  :: TracerEnv
  -> [NodeId]
  -> IO [(NodeId, [(DataName, [HistoricalPoint])])]
getLastHistoryFromBackups' tracerEnv nodeIds = do
  backupDir <- getPathToBackupDir tracerEnv
  forMM (getNodesIdsWithNames tracerEnv nodeIds) $ \(nodeId, nodeName) -> do
    let nodeSubdir = backupDir </> T.unpack nodeName
    doesDirectoryExist nodeSubdir >>= \case
      False -> return (nodeId, []) -- There is no backup for this node.
      True -> do
        namesWithPoints <-
          forMM (listFiles nodeSubdir) $
            extractNamesWithHistoricalPoints nodeSubdir
        return (nodeId, catMaybes namesWithPoints)
 where
  extractNamesWithHistoricalPoints nodeSubdir bFile = do
    let pureFile = takeBaseName bFile
    case readMaybe pureFile of
      Nothing -> return Nothing
      Just (dataName :: DataName) -> do
        -- Ok, this file contains historical points for 'dataName', extract them...
        let backupFile = nodeSubdir </> pureFile
        try_ (BSL.readFile backupFile) >>= \case
          Left _ -> return Nothing
          Right rawPoints ->
            case CSV.decode CSV.NoHeader rawPoints of
              Left _ -> return Nothing -- Maybe file was broken...
              Right (pointsV :: V.Vector HistoricalPoint) -> do
                now <- systemToUTCTime <$> getSystemTime
                -- Ok, take the points for the last 6 hours from now.
                let sixHoursInS = 21600
                    !firstTSWeNeed = utc2s now - sixHoursInS
                    pointsWeNeed = filter (\(ts, _) -> ts >= firstTSWeNeed) $ V.toList pointsV
                return $ Just (dataName, pointsWeNeed)

getNodesIdsWithNames
  :: TracerEnv
  -> [NodeId]
  -> IO [(NodeId, NodeName)]
getNodesIdsWithNames _ [] = return []
getNodesIdsWithNames tracerEnv connected =
  forM connected $ \nodeId -> do
    nodeName <- askNodeName tracerEnv nodeId
    return (nodeId, nodeName)
