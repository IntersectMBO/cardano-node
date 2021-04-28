module Cardano.Tracer.Handlers.Metrics.Monitoring
  ( runMonitoringServer
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Types (AcceptedItems)

runMonitoringServer
  :: Endpoint
  -> AcceptedItems
  -> IO ()
runMonitoringServer _ep _acceptedItems = return ()
