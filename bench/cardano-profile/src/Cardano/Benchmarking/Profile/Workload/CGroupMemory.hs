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
  , Types.entrypoint = "cgroup_memory"
  , Types.phase = Types.BeforeNodes
  , Types.placement = Types.Producers
  , Types.wait_pools = True
}
