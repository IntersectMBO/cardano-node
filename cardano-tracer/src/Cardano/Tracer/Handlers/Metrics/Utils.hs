module Cardano.Tracer.Handlers.Metrics.Utils
  ( MetricName
  , MetricValue
  , MetricsList
  , getListOfMetrics
  ) where

import qualified Data.HashMap.Strict as HM
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Metrics (Store, Value (..), sampleAll)

type MetricName  = Text
type MetricValue = Text
type MetricsList = [(MetricName, MetricValue)]

getListOfMetrics :: Store -> IO MetricsList
getListOfMetrics = fmap (mapMaybe metricsWeNeed . HM.toList) . sampleAll
 where
  metricsWeNeed (mName, mValue) =
    case mValue of
      Counter c -> Just (mName, T.pack $ show c)
      Gauge g   -> Just (mName, T.pack $ show g)
      Label l   -> Just (mName, l)
      _         -> Nothing -- 'ekg-forward' doesn't support 'Distribution' yet.
