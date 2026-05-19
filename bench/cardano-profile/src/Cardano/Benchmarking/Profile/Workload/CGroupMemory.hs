{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Workload.CGroupMemory (
  cgroupMemoryWorkload
) where

--------------------------------------------------------------------------------

import           Prelude
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Types as Types

--------------------------------------------------------------------------------

cgroupMemoryWorkload :: Types.Workload
cgroupMemoryWorkload = Types.Workload {
    Types.workloadName = "cgroup_memory"
  , Types.parameters = mempty
  , Types.entrypoints = Types.Entrypoints {
      Types.pre_generator = Nothing
    , Types.producers = "cgroup_memory"
    }
  , Types.before_nodes = True
  , Types.wait_pools = True
}
