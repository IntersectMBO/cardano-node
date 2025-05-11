{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- Profiles using "tracer-only" as scenario.
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Builtin.Scenario.TracerOnly (
  profilesNoEraTracerOnly
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Builtin.Scenario.Base as B
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V

--------------------------------------------------------------------------------

profilesNoEraTracerOnly :: [Types.Profile]
profilesNoEraTracerOnly =
  let tracerOnly =
          P.empty & B.base
        . P.tracerOnly
        . P.uniCircle . V.hosts 6 . P.loopback
        . V.genesisVariant300
        . V.timescaleCompressed
         -- TODO: "tracer-only" and "idle" have `P.delegators 6`.
         --       Remove and use `V.datasetEmpty` in module "Scenario.Base".
        . P.delegators 6
        . P.analysisStandard . P.analysisUnitary
  in [
    tracerOnly & P.name "tracer-only"
               . P.desc "Idle scenario:  start only the tracer & detach from tty;  no termination"
               . P.traceForwardingOn . P.newTracing . P.p2pOff
  ]
