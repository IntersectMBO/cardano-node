{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Workload.Latency (
  latencyWorkload
) where

--------------------------------------------------------------------------------

import           Prelude
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Types as Types

--------------------------------------------------------------------------------

latencyWorkload :: Types.Workload
latencyWorkload = Types.Workload {
    Types.workloadName = "latency"
  , Types.parameters = mempty
  , Types.entrypoint = "latency"
  , Types.phase = Types.Load
  , Types.placement = Types.Producers
  , Types.wait_pools = False
}
