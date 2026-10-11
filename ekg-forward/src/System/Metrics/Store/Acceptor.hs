module System.Metrics.Store.Acceptor
  ( storeMetrics
  , MetricsLocalStore (..)
  , emptyMetricsLocalStore
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVarIO)
import           Control.Monad (forM_, when)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified System.Metrics as EKG
import qualified System.Metrics.Counter as C
import qualified System.Metrics.Gauge as G
import qualified System.Metrics.Label as L

import           System.Metrics.ReqResp (MetricName, MetricValue (..), Response (..))

storeMetrics
  :: Response
  -> EKG.Store
  -> TVar MetricsLocalStore
  -> IO ()
storeMetrics (ResponseMetrics []) _ _ = return ()
storeMetrics (ResponseMetrics newMetrics) ekgStore metricsStore = do
  storedMetrics <- readTVarIO metricsStore
  forM_ newMetrics $ \(mName, mValue) ->
    case mValue of
      CounterValue c -> addOrUpdate mName storedMetrics c checkCounter addCounter updateCounter
      GaugeValue g   -> addOrUpdate mName storedMetrics g checkGauge   addGauge   updateGauge
      LabelValue l   -> addOrUpdate mName storedMetrics l checkLabel   addLabel   updateLabel
 where
  addOrUpdate mName storedMetrics v checkIt addIt updateIt =
    if checkIt mName storedMetrics
      then updateIt v mName storedMetrics
      else addIt    v mName ekgStore metricsStore

  -- We have to check if this metric is already registered, because if we'll try
  -- to add the metric with existing name, 'ekg-core' will throw an exception.
  checkCounter mName = HM.member mName . ekgCounters
  checkGauge mName   = HM.member mName . ekgGauges
  checkLabel mName   = HM.member mName . ekgLabels

addCounter :: Int64 -> MetricName -> EKG.Store -> TVar MetricsLocalStore -> IO ()
addCounter c mName ekgStore metricsStore = do
  newCounter <- EKG.createCounter mName ekgStore
  C.add newCounter c
  atomically $ modifyTVar' metricsStore $ \currentStore ->
    currentStore { ekgCounters = HM.insert mName newCounter $ ekgCounters currentStore }

addGauge :: Int64 -> MetricName -> EKG.Store -> TVar MetricsLocalStore -> IO ()
addGauge g mName ekgStore metricsStore = do
  newGauge <- EKG.createGauge mName ekgStore
  G.set newGauge g
  atomically $ modifyTVar' metricsStore $ \currentStore ->
    currentStore { ekgGauges = HM.insert mName newGauge $ ekgGauges currentStore }

addLabel :: Text -> MetricName -> EKG.Store -> TVar MetricsLocalStore -> IO ()
addLabel l mName ekgStore metricsStore = do
  newLabel <- EKG.createLabel mName ekgStore
  L.set newLabel l
  atomically $ modifyTVar' metricsStore $ \currentStore ->
    currentStore { ekgLabels = HM.insert mName newLabel $ ekgLabels currentStore }

updateCounter :: Int64 -> MetricName -> MetricsLocalStore -> IO ()
updateCounter c mName storedMetrics = do
  let counter = ekgCounters storedMetrics ! mName
  currentValue <- C.read counter
  let difference = c - currentValue
  when (difference > 0) $
    C.add counter difference

updateGauge :: Int64 -> MetricName -> MetricsLocalStore -> IO ()
updateGauge g mName storedMetrics = do
  let gauge = ekgGauges storedMetrics ! mName
  currentValue <- G.read gauge
  when (currentValue /= g) $
    G.set gauge g

updateLabel :: Text -> MetricName -> MetricsLocalStore -> IO ()
updateLabel l mName storedMetrics = do
  let label = ekgLabels storedMetrics ! mName
  currentValue <- L.read label
  when (currentValue /= l) $
    L.set label l

-- Local storage for metrics. We need it be able to update metrics in EKG.Store
-- by new values we receive from the forwarder.

data MetricsLocalStore = MetricsLocalStore
  { ekgCounters :: !(HashMap MetricName C.Counter)
  , ekgGauges   :: !(HashMap MetricName G.Gauge)
  , ekgLabels   :: !(HashMap MetricName L.Label)
  }

emptyMetricsLocalStore :: MetricsLocalStore
emptyMetricsLocalStore =
  MetricsLocalStore
    { ekgCounters = HM.empty
    , ekgGauges   = HM.empty
    , ekgLabels   = HM.empty
    }
