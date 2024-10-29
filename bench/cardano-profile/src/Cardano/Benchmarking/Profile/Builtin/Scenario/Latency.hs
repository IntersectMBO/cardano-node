{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- Profiles using "latency" as scenario.
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Builtin.Scenario.Latency (
  profilesNoEraLatency
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Builtin.Cloud as C
import qualified Cardano.Benchmarking.Profile.Builtin.Scenario.Base as B
import qualified Cardano.Benchmarking.Profile.Extra.Scaling as S
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V

--------------------------------------------------------------------------------

profilesNoEraLatency :: [Types.Profile]
profilesNoEraLatency =
  let latency =
          P.empty & B.base
        . P.latency
        . C.composeFiftytwo
        -- TODO: Use `genesisVariant300` like the others and to "Scenario.Base".
        . V.genesisVariantPreVoltaire
        . V.timescaleCompressed
         -- TODO: "tracer-only" and "idle" have `P.delegators 6`.
         --       Remove and use `V.datasetEmpty` in module "Scenario.Base".
        . P.delegators 0
        . P.analysisStandard
  in [
    latency & P.name "latency-nomadperf"
            . P.desc "AWS perf class cluster, stop when all latency services stop"
            . P.traceForwardingOn . P.newTracing . P.p2pOn . C.nomadPerf
  , latency & P.name "latency-nomadperfssd"
            . P.desc "AWS perf-ssd class cluster, stop when all latency services stop"
            . P.traceForwardingOn . P.newTracing . P.p2pOn . S.nomadSsd
  ]
