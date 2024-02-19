{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The module 'Cardano.Tracer.Handlers.RTView.UI.JS.Charts' contains the tools
--   for rendering/updating charts using Chart.JS library, via JS FFI.
--
--   This module contains common tools for charts' state. We need it to be able
--   to re-render their values after web-page reloading.

module Cardano.Tracer.Handlers.RTView.UI.Charts
  ( initColors
  , initDatasetsIndices
  , addNodeDatasetsToCharts
  , addPointsToChart
  , addAllPointsToChart
  , getSavedColorForNode
  , restoreChartsSettings
  , saveChartsSettings
  , changeChartsToLightTheme
  , changeChartsToDarkTheme
  , replacePointsByAvgPoints
  , restoreAllHistoryOnChart
  , restoreLastHistoryOnAllCharts
  , restoreLastHistoryOnCharts
  ) where

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.System
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Own
import qualified Cardano.Tracer.Handlers.RTView.UI.JS.Charts as Chart
import qualified Cardano.Tracer.Handlers.RTView.UI.JS.Utils as JS
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Historical
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO, tryReadTBQueue, writeTBQueue)
import           Control.Concurrent.STM.TVar (modifyTVar', newTVarIO, readTVarIO)
import           Control.Exception.Extra (ignore, try_)
import           Control.Monad (forM, forM_, unless, when)
import           Control.Monad.Extra (whenJustM)
import           Data.Aeson (decodeFileStrict', encodeFile)
import           Data.List (find, isInfixOf)
import           Data.List.Extra (chunksOf)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import qualified Data.Set as S
import           Data.Text (pack, unpack)
import           System.Directory.Extra (listFiles)
import           System.FilePath (takeBaseName, (</>))
import           Text.Read (readMaybe)

import           Graphics.UI.Threepenny.Core

chartsIds :: [ChartId]
chartsIds = [minBound .. maxBound]

initColors :: UI Colors
initColors = liftIO $ do
  q <- newTBQueueIO . fromIntegral $ length colors
  mapM_ (atomically . writeTBQueue q . Color) colors
  return q
 where
  -- | There are unique colors for each chart line corresponding to each connected node.
  --   To make chart lines visually distinct, the colors in this list are contrast enough.
  --   It is assumed that the number of colors in this list is enough for real SPO cluster.
  colors =
    [ "#ff0000", "#66ff33", "#0066ff", "#ff00ff", "#cc0066", "#00ccff", "#ff9933", "#cc9900"
    , "#33cc33", "#0099ff", "#9a751b", "#7c0657", "#1a6f17", "#118989", "#c44153", "#b31c2c"
    ]

initDatasetsIndices :: UI DatasetsIndices
initDatasetsIndices = liftIO . newTVarIO $ M.empty

getDatasetIx
  :: DatasetsIndices
  -> NodeName
  -> UI (Maybe Index)
getDatasetIx indices nodeName = liftIO $ M.lookup nodeName <$> readTVarIO indices

addNodeDatasetsToCharts
  :: TracerEnv
  -> Colors
  -> DatasetsIndices
  -> NodeId
  -> UI ()
addNodeDatasetsToCharts tracerEnv colors datasetIndices nodeId@(NodeId anId) = do
  nodeName <- liftIO $ askNodeName tracerEnv nodeId
  mIx <- getDatasetIx datasetIndices nodeName
  -- We have to check if the node with 'nodeName' was connected previously.
  -- If so - we have to take its color again, from the file.
  -- If not - we have to take the new color for it and save it for the future.
  colorForNode@(Color code) <-
    liftIO (getSavedColorForNode tracerEnv nodeName) >>= \case
      Just savedColor -> return savedColor
      Nothing -> do
        newColor <- getNewColor
        liftIO $ saveColorForNode tracerEnv nodeName newColor
        return newColor
  forM_ chartsIds $ \chartId ->
    case mIx of
      Nothing -> do
        -- This is the new node, so add dataset for it with the new index and save it.
        newIx <- Chart.getDatasetsLengthChartJS chartId
        Chart.addDatasetChartJS chartId nodeName colorForNode
        saveDatasetIx nodeName $ Index newIx
      Just _ix ->
        -- This node was already connected, so use the same dataset for
        -- the new historical data received from it.
        return ()
  -- Change color label for node name as well.
  window <- askWindow
  findAndSet (set style [("color", code)]) window (anId <> "__node-chart-label")
 where
  saveDatasetIx nodeName ix = liftIO . atomically $
    modifyTVar' datasetIndices $ \currentIndices ->
      case M.lookup nodeName currentIndices of
        Nothing -> M.insert nodeName ix currentIndices
        Just _  -> M.adjust (const ix) nodeName currentIndices

  getNewColor =
    (liftIO . atomically $ tryReadTBQueue colors) >>= \case
      Just color -> return color
      Nothing    -> return defaultColor

  defaultColor = Color "#cccc00"

-- Each chart updates independently from others. Because of this, the user
-- can specify "auto-update period" for each chart. Some of data (by its nature)
-- shoudn't be updated too frequently.
--
-- All points will be added to all datasets (corresponding to the number of connected nodes)
-- using one single FFI-call, for better performance.
--
-- 'addAllPointsToChart' doesn not do average calculation, it pushes all the points as they are.
addPointsToChart, addAllPointsToChart
  :: TracerEnv
  -> History
  -> DatasetsIndices
  -> DataName
  -> ChartId
  -> UI ()
addPointsToChart    = doAddPointsToChart replacePointsByAvgPoints
addAllPointsToChart = doAddPointsToChart id

doAddPointsToChart
  :: ([HistoricalPoint] -> [HistoricalPoint])
  -> TracerEnv
  -> History
  -> DatasetsIndices
  -> DataName
  -> ChartId
  -> UI ()
doAddPointsToChart replaceByAvg tracerEnv hist datasetIndices dataName chartId = do
  connected <- liftIO $ S.toList <$> readTVarIO (teConnectedNodes tracerEnv)
  dataForPush <-
    forM connected $ \nodeId ->
      liftIO (getHistoricalData hist nodeId dataName) >>= \case
        []     -> return Nothing
        points -> do
          nodeName <- liftIO $ askNodeName tracerEnv nodeId
          getDatasetIx datasetIndices nodeName >>= \case
            Nothing -> return Nothing
            Just ix -> return $ Just (ix, replaceByAvg points)
  let datasetIxsWithPoints = catMaybes dataForPush
  Chart.addAllPointsChartJS chartId datasetIxsWithPoints
  -- Now all the points from 'hist' is already pushed to JS-chart.
  -- It means that we can back these points up and removed them from the 'hist'.
  liftIO $ backupSpecificHistory tracerEnv hist connected dataName

replacePointsByAvgPoints :: [HistoricalPoint] -> [HistoricalPoint]
replacePointsByAvgPoints [] = []
replacePointsByAvgPoints points =
  map calculateAvgPoint $ chunksOf numberOfPointsToAverage points
 where
  calculateAvgPoint :: [HistoricalPoint] -> HistoricalPoint
  calculateAvgPoint pointsForAvg =
    let !valuesSum = sum [v | (_, v) <- pointsForAvg]
        avgValue =
          case valuesSum of
            ValueI i -> ValueD $ fromIntegral i / fromIntegral (length pointsForAvg)
            ValueD d -> ValueD $              d / fromIntegral (length pointsForAvg)
        (latestTS, _) = last pointsForAvg
    in (latestTS, avgValue)
  -- TODO: calculate it, remove hardcoded value!
  -- P1 - Period of calling 'addAllPointsToChart'
  -- P2 - Period of asking of EKG.Metrics
  -- Number of new points received since last call of 'addAllPointsToChart'
  -- is P1/P2
  -- Maximum number of points to calculate avg = 15 s.
  numberOfPointsToAverage = 15

restoreChartsSettings :: TracerEnv -> UI ()
restoreChartsSettings tracerEnv = readSavedChartsSettings tracerEnv >>= setCharts
 where
  setCharts settings =
    forM_ settings $ \(chartId, ChartSettings tr up) -> do
      JS.selectOption (show chartId <> show TimeRangeSelect)    tr
      JS.selectOption (show chartId <> show UpdatePeriodSelect) up
      Chart.setTimeRange chartId tr
      when (tr == 0) $ Chart.resetZoomChartJS chartId

saveChartsSettings :: TracerEnv -> UI ()
saveChartsSettings tracerEnv = do
  settings <-
    forM chartsIds $ \chartId -> do
      selectedTR <- getOptionValue $ show chartId <> show TimeRangeSelect
      selectedUP <- getOptionValue $ show chartId <> show UpdatePeriodSelect
      return (chartId, ChartSettings selectedTR selectedUP)
  liftIO . ignore $ do
    pathToChartsConfig <- getPathToChartsConfig tracerEnv
    encodeFile pathToChartsConfig settings
 where
  getOptionValue selectId = do
    window <- askWindow
    v <- findAndGetValue window (pack selectId)
    case readMaybe v of
      Just (valueInS :: Int) -> return valueInS
      Nothing -> return 0

readSavedChartsSettings :: TracerEnv -> UI ChartsSettings
readSavedChartsSettings tracerEnv = liftIO $
  try_ (decodeFileStrict' =<< getPathToChartsConfig tracerEnv) >>= \case
    Right (Just (settings :: ChartsSettings)) -> return settings
    _ -> return defaultSettings
 where
  defaultSettings =
    [ (chartId, ChartSettings defaultTimeRangeInS defaultUpdatePeriodInS)
    | chartId <- chartsIds
    ]
  defaultTimeRangeInS = 21600 -- Last 6 hours
  defaultUpdatePeriodInS = 15

changeChartsToLightTheme :: UI ()
changeChartsToLightTheme =
  forM_ chartsIds $ \chartId ->
    Chart.changeColorsChartJS chartId (Color chartTextDark) (Color chartGridDark)

changeChartsToDarkTheme :: UI ()
changeChartsToDarkTheme =
  forM_ chartsIds $ \chartId ->
    Chart.changeColorsChartJS chartId (Color chartTextLight) (Color chartGridLight)

restoreAllHistoryOnChart
  :: TracerEnv
  -> DataName
  -> ChartId
  -> DatasetsIndices
  -> UI ()
restoreAllHistoryOnChart tracerEnv dataName chartId dsIxs = do
  pointsFromBackup <- liftIO $ getAllHistoryFromBackup tracerEnv dataName
  forM_ pointsFromBackup $ \(nodeId, points) -> do
    nodeName <- liftIO $ askNodeName tracerEnv nodeId
    whenJustM (getDatasetIx dsIxs nodeName) $ \ix -> do
      Chart.clearPointsChartJS chartId [ix]
      Chart.addAllPointsChartJS chartId [(ix, replacePointsByAvgPoints points)]

restoreLastHistoryOnAllCharts
  :: TracerEnv
  -> DatasetsIndices
  -> UI ()
restoreLastHistoryOnAllCharts tracerEnv =
  restoreLastHistoryOnCharts' tracerEnv (getLastHistoryFromBackupsAll tracerEnv)

restoreLastHistoryOnCharts
  :: TracerEnv
  -> DatasetsIndices
  -> S.Set NodeId
  -> UI ()
restoreLastHistoryOnCharts tracerEnv dsIxs nodeIds =
  restoreLastHistoryOnCharts' tracerEnv (getLastHistoryFromBackups tracerEnv nodeIds) dsIxs

restoreLastHistoryOnCharts'
  :: TracerEnv
  -> IO [(NodeId, [(DataName, [HistoricalPoint])])]
  -> DatasetsIndices
  -> UI ()
restoreLastHistoryOnCharts' tracerEnv historyGetter dsIxs =
  forMM_ (liftIO historyGetter) $ \(nodeId, dataNamesWithPoints) -> do
    nodeName <- liftIO $ askNodeName tracerEnv nodeId
    whenJustM (getDatasetIx dsIxs nodeName) $ \ix ->
      forM_ dataNamesWithPoints $ \(dataName, points) ->
        unless (null points) $ do
          let chartId = dataNameToChartId dataName
          Chart.clearPointsChartJS chartId [ix]
          Chart.addAllPointsChartJS chartId [(ix, replacePointsByAvgPoints points)]

dataNameToChartId :: DataName -> ChartId
dataNameToChartId dataName =
  case dataName of
    CPUData                   -> CPUChart
    MemoryData                -> MemoryChart
    GCMajorNumData            -> GCMajorNumChart
    GCMinorNumData            -> GCMinorNumChart
    GCLiveMemoryData          -> GCLiveMemoryChart
    CPUTimeGCData             -> CPUTimeGCChart
    CPUTimeAppData            -> CPUTimeAppChart
    ThreadsNumData            -> ThreadsNumChart
    -- Chain
    ChainDensityData          -> ChainDensityChart
    SlotNumData               -> SlotNumChart
    BlockNumData              -> BlockNumChart
    SlotInEpochData           -> SlotInEpochChart
    EpochData                 -> EpochChart
    NodeCannotForgeData       -> NodeCannotForgeChart
    ForgedSlotLastData        -> ForgedSlotLastChart
    NodeIsLeaderData          -> NodeIsLeaderChart
    NodeIsNotLeaderData       -> NodeIsNotLeaderChart
    ForgedInvalidSlotLastData -> ForgedInvalidSlotLastChart
    AdoptedSlotLastData       -> AdoptedSlotLastChart
    NotAdoptedSlotLastData    -> NotAdoptedSlotLastChart
    AboutToLeadSlotLastData   -> AboutToLeadSlotLastChart
    CouldNotForgeSlotLastData -> CouldNotForgeSlotLastChart
    -- TX
    TxsProcessedNumData       -> TxsProcessedNumChart
    MempoolBytesData          -> MempoolBytesChart
    TxsInMempoolData          -> TxsInMempoolChart

getSavedColorForNode
  :: TracerEnv
  -> NodeName
  -> IO (Maybe Color)
getSavedColorForNode tracerEnv nodeName = do
  colorsDir <- getPathToChartColorsDir tracerEnv
  colorFiles <- map (\cf -> colorsDir </> takeBaseName cf) <$> listFiles colorsDir
  case find (\cf -> unpack nodeName `isInfixOf` cf) colorFiles of
    Nothing -> return Nothing
    Just colorFile ->
      try_ (readFile colorFile) >>= \case
        Left _ -> return Nothing
        Right code -> return . Just $ Color code

saveColorForNode
  :: TracerEnv
  -> NodeName
  -> Color
  -> IO ()
saveColorForNode tracerEnv nodeName (Color code) = do
  colorsDir <- getPathToChartColorsDir tracerEnv
  ignore $ writeFile (colorsDir </> unpack nodeName) code
