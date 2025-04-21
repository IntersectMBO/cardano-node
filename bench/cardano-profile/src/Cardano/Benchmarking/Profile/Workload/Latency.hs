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
  , Types.entrypoints = Types.Entrypoints {
      Types.pre_generator = Nothing
    , Types.producers = "latency"
    }
  , Types.wait_pools = False
}
