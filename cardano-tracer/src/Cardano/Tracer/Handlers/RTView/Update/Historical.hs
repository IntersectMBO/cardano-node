module Cardano.Tracer.Handlers.RTView.Update.Historical
  ( runHistoricalUpdater
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forever, forM_)
import qualified Data.Map.Strict as M
import           Data.Time.Clock.System
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.State.Last
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.Update.Chain
import           Cardano.Tracer.Handlers.RTView.Update.Leadership
import           Cardano.Tracer.Handlers.RTView.Update.Resources
import           Cardano.Tracer.Handlers.RTView.Update.Transactions
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
  :: SavedTraceObjects
  -> AcceptedMetrics
  -> ResourcesHistory
  -> LastResources
  -> BlockchainHistory
  -> TransactionsHistory
  -> IO ()
runHistoricalUpdater _savedTO acceptedMetrics resourcesHistory
                     lastResources chainHistory txHistory = forever $ do
  sleep 1.0 -- TODO: should it be configured?

  now <- systemToUTCTime <$> getSystemTime
  allMetrics <- readTVarIO acceptedMetrics
  forM_ (M.toList allMetrics) $ \(nodeId, (ekgStore, _)) -> do
    metrics <- getListOfMetrics ekgStore
    forM_ metrics $ \(metricName, metricValue) -> do
      updateTransactionsHistory nodeId txHistory metricName metricValue now
      updateResourcesHistory nodeId resourcesHistory lastResources metricName metricValue now
      updateBlockchainHistory nodeId chainHistory metricName metricValue now
      updateLeadershipHistory nodeId chainHistory metricName metricValue now
