{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.JS.Charts
  ( prepareChartsJS
  , addDatasetChartJS
  , addAllPointsChartJS
  , clearPointsChartJS
  , getDatasetsLengthChartJS
  , newTimeChartJS
  , resetZoomChartJS
  , changeColorsChartJS
  , setTimeRange
  ) where

import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.Update.Utils

import           Data.List (intercalate)
import           Data.String.QQ
import           Data.Text (Text)
import           Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import           Data.Word (Word16)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

prepareChartsJS :: UI ()
prepareChartsJS =
  UI.runFunction $ UI.ffi "window.charts = new Map();"

newTimeChartJS
  :: ChartId
  -> String
  -> UI ()
newTimeChartJS chartId yValuesLabel =
  UI.runFunction $ UI.ffi newTimeChartJS' (show chartId) yValuesLabel

newTimeChartJS' :: String
newTimeChartJS' = [s|
var ctx = document.getElementById(%1).getContext('2d');
var chart = new Chart(ctx, {
  type: 'line',
  data: {
    datasets: []
  },
  options: {
    parsing: false,
    animation: false,
    normalized: true,
    showLine: true,
    spanGaps: true,
    interaction: {
      intersect: false,
      mode: 'index',
      axis: 'xy'
    },
    elements: {
      point: {
        radius: 0
      },
      line: {
        borderWidth: 2
      }
    },
    responsive: true,
    plugins: {
      zoom: {
        zoom: {
          drag: {
            enabled: true
          },
          mode: 'x'
        }
      }
    },
    transitions: {
      zoom: {
        animation: {
          duration: 0
        }
      }
    },
    scales: {
      x: {
        type: 'time',
        title: {
          display: true,
          text: 'Time in UTC'
        },
        time: {
          displayFormats: {
            second: 'HH:mm:ss',
            minute: 'HH:mm',
            hour:   'hh a',
            day:    'D'
          },
          unit: 'minute'
        },
        adapters: {
          date: {
            zone: 'UTC'
          }
        },
        ticks: {
          maxRotation: 0
        }
      },
      y: {
        title: {
          display: true,
          text: %2
        }
      }
    }
  }
});
window.charts.set(%1, chart);
|]

addDatasetChartJS
  :: ChartId
  -> Text
  -> Color
  -> UI ()
addDatasetChartJS chartId nodeName (Color color) =
  UI.runFunction $ UI.ffi addDatasetChartJS' (show chartId) nodeName color

addDatasetChartJS' :: String
addDatasetChartJS' = [s|
var chart = window.charts.get(%1);
chart.data.datasets.push({
  label: %2,
  backgroundColor: %3,
  borderColor: %3,
  data: [],
  fill: false
});
chart.update();
|]

getDatasetsLengthChartJS :: ChartId -> UI Word16
getDatasetsLengthChartJS chartId = do
  (l :: Int) <- UI.callFunction $ UI.ffi "window.charts.get(%1).data.datasets.length;" (show chartId)
  return $ fromIntegral l

clearPointsChartJS
  :: ChartId
  -> [Index]
  -> UI ()
clearPointsChartJS _ [] = return ()
clearPointsChartJS chartId ixs =
  UI.runFunction $ UI.ffi clearAllDatasets
 where
  clearAllDatasets =
       "var chart = window.charts.get('" <> show chartId <> "');"
    <> concatMap clearDataset ixs
    <> "chart.update();"

  clearDataset (Index ix) = "chart.data.datasets[" <> show ix <> "].data = [];"

addAllPointsChartJS
  :: ChartId
  -> [(Index, [HistoricalPoint])]
  -> UI ()
addAllPointsChartJS _ [] = return ()
addAllPointsChartJS chartId datasetIxsWithPoints =
  UI.runFunction $ UI.ffi pushToAllDatasets
 where
  pushToAllDatasets =
       "var chart = window.charts.get('" <> show chartId <> "');"
    <> concatMap pushToDataset datasetIxsWithPoints
    <> "chart.update();"

  pushToDataset (_, []) = ""
  pushToDataset (Index ix, points) =
    "chart.data.datasets[" <> show ix <> "].data.push(" <> prepareArrayToPush points <> ");"

  prepareArrayToPush [] = ""
  prepareArrayToPush points = intercalate ", " $ map mkArray points
   where
    mkArray (ts, valueH) =
      let !tsInMs = ts * 1000 -- ChartJS uses milliseconds since epoch as internal format.
      in "{x: " <> show tsInMs <> ", y: " <> show valueH <> "}"

resetZoomChartJS :: ChartId -> UI ()
resetZoomChartJS chartId =
  UI.runFunction $ UI.ffi "window.charts.get(%1).resetZoom();" (show chartId)

changeColorsChartJS
  :: ChartId
  -> Color
  -> Color
  -> UI ()
changeColorsChartJS chartId (Color textColor) (Color gridColor) =
  UI.runFunction $ UI.ffi changeColorsChartJS' (show chartId) textColor gridColor

changeColorsChartJS' :: String
changeColorsChartJS' = [s|
var chart = window.charts.get(%1);
chart.options.scales.x.ticks.color = %2;
chart.options.scales.x.title.color = %2;
chart.options.scales.x.grid.color = %3;
chart.options.scales.y.ticks.color = %2;
chart.options.scales.y.title.color = %2;
chart.options.scales.y.grid.color = %3;
chart.options.plugins.title.color = %2;
chart.options.plugins.legend.title.color = %2;
chart.update();
|]

setTimeRange
  :: ChartId
  -> Int
  -> UI ()
setTimeRange chartId rangeInSec = do
  now <- liftIO $ systemToUTCTime <$> getSystemTime
  let !rangeInMs = rangeInSec * 1000
      !maxInMs   = utc2s now * 1000
      !minInMs   = maxInMs - fromIntegral rangeInMs
  -- Set time units depends on selected range.
  let timeUnit = if | rangeInSec == 0                        -> "day"
                    | rangeInSec > 0   && rangeInSec <= 300  -> "second"
                    | rangeInSec > 300 && rangeInSec <= 1800 -> "minute"
                    | otherwise                              -> "hour"
  if rangeInSec == 0
    then
      -- Show all time, no zoom, so just set time units.
      UI.runFunction $ UI.ffi setUnitsChartJS (show chartId) timeUnit
    else
      UI.runFunction $ UI.ffi zoomScaleAndUnitsChartJS (show chartId)
                                                       (fromIntegral minInMs :: Int)
                                                       (fromIntegral maxInMs :: Int)
                                                       timeUnit

setUnitsChartJS :: String
setUnitsChartJS = [s|
var chart = window.charts.get(%1);
chart.options.scales.x.time.unit = %2;
chart.update();
|]

zoomScaleAndUnitsChartJS :: String
zoomScaleAndUnitsChartJS = [s|
var chart = window.charts.get(%1);
chart.zoomScale('x', {min: %2, max: %3});
chart.options.scales.x.time.unit = %4;
chart.update();
|]
