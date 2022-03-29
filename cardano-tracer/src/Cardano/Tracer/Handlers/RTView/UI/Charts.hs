{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.Charts
  ( initColors
  , initDatasetsIndices
  , initDatasetsTimestamps
  , getDatasetIx
  , addNodeDatasetsToCharts
  , addPointsToChart
  , addAllPointsToChart
  , getLatestDisplayedTS
  , saveLatestDisplayedTS
  , restoreChartsSettings
  , saveChartsSettings
  , changeChartsToLightTheme
  , changeChartsToDarkTheme
  ) where

-- | The module 'Cardano.Tracer.Handlers.RTView.UI.JS.Charts' contains the tools
--   for rendering/updating charts using Chart.JS library, via JS FFI.
--
--   This module contains common tools for charts' state. We need it to be able
--   to re-render their values after web-page reloading.

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM.TVar
import           Control.Exception.Extra (ignore, try_)
import           Control.Monad (forM, forM_, when)
import           Data.Aeson
import           Data.List.Extra (chunksOf)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import           Data.Text (pack)
import           Graphics.UI.Threepenny.Core
import           Text.Read (readMaybe)

import           Cardano.Tracer.Types
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.System
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Own
import qualified Cardano.Tracer.Handlers.RTView.UI.JS.Charts as Chart
import qualified Cardano.Tracer.Handlers.RTView.UI.JS.Utils as JS
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.UI.Utils

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
  --   It is assumed that the number of colors in this list is enough.
  colors =
    [ "#ff0000", "#66ff33", "#0066ff", "#ff00ff", "#cc0066", "#00ccff", "#ff9933", "#cc9900"
    , "#33cc33", "#0099ff"
    ]

getNewColor :: Colors -> UI Color
getNewColor q =
  (liftIO . atomically $ tryReadTBQueue q) >>= \case
    Just color -> return color
    Nothing    -> return defaultColor
 where
  defaultColor = Color "#cccc00"

initDatasetsIndices :: UI DatasetsIndices
initDatasetsIndices = liftIO . newTVarIO $ M.empty

saveDatasetIx
  :: DatasetsIndices
  -> NodeId
  -> Index
  -> UI ()
saveDatasetIx indices nodeId ix = liftIO . atomically $
  modifyTVar' indices $ \currentIndices ->
    case M.lookup nodeId currentIndices of
      Nothing -> M.insert nodeId ix currentIndices
      Just _  -> M.adjust (const ix) nodeId currentIndices

getDatasetIx
  :: DatasetsIndices
  -> NodeId
  -> UI (Maybe Index)
getDatasetIx indices nodeId = liftIO $
  M.lookup nodeId <$> readTVarIO indices

addNodeDatasetsToCharts
  :: Window
  -> NodeId
  -> Colors
  -> DatasetsIndices
  -> DisplayedElements
  -> UI ()
addNodeDatasetsToCharts window nodeId@(NodeId anId) colors datasetIndices displayedElements = do
  colorForNode@(Color code) <- getNewColor colors
  forM_ chartsIds $ \chartId -> do
    newIx <- Chart.getDatasetsLengthChartJS chartId
    nodeName <- liftIO $ getDisplayedValue displayedElements nodeId (anId <> "__node-name")
    Chart.addDatasetChartJS chartId (fromMaybe anId nodeName) colorForNode
    saveDatasetIx datasetIndices nodeId (Index newIx)
  -- Change color label for node name as well.
  findAndSet (set style [("color", code)]) window (anId <> "__node-chart-label")

initDatasetsTimestamps :: UI DatasetsTimestamps
initDatasetsTimestamps = liftIO . newTVarIO $ M.empty

saveLatestDisplayedTS
  :: DatasetsTimestamps
  -> NodeId
  -> DataName
  -> POSIXTime
  -> UI ()
saveLatestDisplayedTS tss nodeId dataName ts = liftIO . atomically $
  modifyTVar' tss $ \currentTimestamps ->
    case M.lookup nodeId currentTimestamps of
      Nothing ->
        -- There is no latest timestamps for charts for this node yet.
        let newTSForNode = M.singleton dataName ts
        in M.insert nodeId newTSForNode currentTimestamps
      Just tssForNode ->
        let newTSForNode =
              case M.lookup dataName tssForNode of
                Nothing ->
                  -- There is no latest timestamps for this dataName yet.
                  M.insert dataName ts tssForNode
                Just _ ->
                  M.adjust (const ts) dataName tssForNode
        in M.adjust (const newTSForNode) nodeId currentTimestamps

getLatestDisplayedTS
  :: DatasetsTimestamps
  -> NodeId
  -> DataName
  -> UI (Maybe POSIXTime)
getLatestDisplayedTS tss nodeId dataName = liftIO $ do
  tss' <- readTVarIO tss
  case M.lookup nodeId tss' of
    Nothing         -> return Nothing
    Just tssForNode -> return $ M.lookup dataName tssForNode

-- Each chart updates independently from others. Because of this, the user
-- can specify "auto-update period" for each chart. Some of data (by its nature)
-- shoudn't be updated too frequently.
--
-- All points will be added to all datasets (corresponding to the number of connected nodes)
-- using one single FFI-call, for better performance.
--
-- 'addAllPointsToChart' doesn not do average calculation, it pushes all the points as they are.
addPointsToChart, addAllPointsToChart
  :: ConnectedNodes
  -> History
  -> DatasetsIndices
  -> DatasetsTimestamps
  -> DataName
  -> ChartId
  -> UI ()
addPointsToChart    = doAddPointsToChart replacePointsByAvgPoints
addAllPointsToChart = doAddPointsToChart id

doAddPointsToChart
  :: ([HistoricalPoint] -> [HistoricalPoint])
  -> ConnectedNodes
  -> History
  -> DatasetsIndices
  -> DatasetsTimestamps
  -> DataName
  -> ChartId
  -> UI ()
doAddPointsToChart replaceByAvg connectedNodes hist datasetIndices datasetTimestamps dataName chartId = do
  connected <- liftIO $ S.toList <$> readTVarIO connectedNodes
  dataForPush <-
    forM connected $ \nodeId ->
      liftIO (getHistoricalData hist nodeId dataName) >>= \case
        []     -> return Nothing
        points -> do
          let (latestTS, _) = last points
          getLatestDisplayedTS datasetTimestamps nodeId dataName >>= \case
            Nothing ->
              -- There is no saved latestTS for this node and chart yet,
              -- so display all the history and remember the latestTS.
              getDatasetIx datasetIndices nodeId >>= \case
                Nothing -> return Nothing
                Just ix ->
                  return . Just $ ( (nodeId, latestTS)
                                  , (ix, replaceByAvg points)
                                  )
            Just storedTS ->
              -- Some of the history for this node and chart is already displayed,
              -- so cut displayed points first. The only points we should add now
              -- are the points with 'ts' that is bigger than 'storedTS'.
              getDatasetIx datasetIndices nodeId >>= \case
                Nothing -> return Nothing
                Just ix ->
                  return . Just $ ( (nodeId, latestTS)
                                  , (ix, replaceByAvg $! cutOldPoints storedTS points)
                                  )
  let (nodeIdsWithLatestTss, datasetIxsWithPoints) = unzip $ catMaybes dataForPush
  Chart.addAllPointsChartJS chartId datasetIxsWithPoints
  forM_ nodeIdsWithLatestTss $ \(nodeId, latestTS) ->
    saveLatestDisplayedTS datasetTimestamps nodeId dataName latestTS

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

cutOldPoints
  :: POSIXTime
  -> [HistoricalPoint]
  -> [HistoricalPoint]
cutOldPoints _ [] = []
cutOldPoints oldTS (point@(ts, _):newerPoints) =
  if ts > oldTS
    then
      -- This point is newer than 'oldTS', take it and all the following
      -- as well, because they are definitely newer (points are sorted by ts).
      point : newerPoints
    else
      -- This point are older than 'oldTS', it means that it already was displayed.
      cutOldPoints oldTS newerPoints

restoreChartsSettings :: UI ()
restoreChartsSettings = readSavedChartsSettings >>= setCharts
 where
  setCharts settings =
    forM_ settings $ \(chartId, ChartSettings tr up) -> do
      JS.selectOption (show chartId <> show TimeRangeSelect)    tr
      JS.selectOption (show chartId <> show UpdatePeriodSelect) up
      Chart.setTimeRange chartId tr
      when (tr == 0) $ Chart.resetZoomChartJS chartId

saveChartsSettings :: Window -> UI ()
saveChartsSettings window = do
  settings <-
    forM chartsIds $ \chartId -> do
      selectedTR <- getOptionValue $ show chartId <> show TimeRangeSelect
      selectedUP <- getOptionValue $ show chartId <> show UpdatePeriodSelect
      return (chartId, ChartSettings selectedTR selectedUP)
  liftIO . ignore $ do
    pathToChartsConfig <- getPathToChartsConfig
    encodeFile pathToChartsConfig settings
 where
   getOptionValue selectId = do
    v <- findAndGetValue window (pack selectId)
    case readMaybe v of
      Just (valueInS :: Int) -> return valueInS
      Nothing -> return 0

readSavedChartsSettings :: UI ChartsSettings
readSavedChartsSettings = liftIO $
  try_ (decodeFileStrict' =<< getPathToChartsConfig) >>= \case
    Right (Just (settings :: ChartsSettings)) -> return settings
    _ -> return defaultSettings
 where
  defaultSettings =
    [ (chartId, ChartSettings defaultTimeRangeInS defaultUpdatePeriodInS)
    | chartId <- chartsIds
    ]
  defaultTimeRangeInS = 0 -- All time
  defaultUpdatePeriodInS = 15

changeChartsToLightTheme :: UI ()
changeChartsToLightTheme =
  forM_ chartsIds $ \chartId ->
    Chart.changeColorsChartJS chartId (Color chartTextDark) (Color chartGridDark)

changeChartsToDarkTheme :: UI ()
changeChartsToDarkTheme =
  forM_ chartsIds $ \chartId ->
    Chart.changeColorsChartJS chartId (Color chartTextLight) (Color chartGridLight)
