{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- Profiles using "idle" as scenario.
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Builtin.Scenario.Idle (
  profilesNoEraIdle
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
-- Package: aeson.
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Builtin.Scenario.Base as B
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V

--------------------------------------------------------------------------------

profilesNoEraIdle :: [Types.Profile]
profilesNoEraIdle =
  let idle =
          P.empty & B.base
        . P.idle
        . P.uniCircle . V.hosts 6 . P.loopback
        . V.genesisVariant300
         -- TODO: "tracer-only" and "idle" have `P.delegators 6`.
         --       Remove and use `V.datasetEmpty` in module "Scenario.Base".
        . P.delegators 6
        . P.analysisUnitary
      updateQuorum = P.shelley (KeyMap.insert "updateQuorum" (Aeson.Number 1))
  in [
    idle & P.name "devops" . V.timescaleDevops     . P.extraFutureOffset 10 . updateQuorum . P.traceForwardingOn . P.newTracing . P.p2pOff . P.analysisOff
  , idle & P.name "idle"   . V.timescaleCompressed . P.extraFutureOffset  0                . P.traceForwardingOn . P.newTracing . P.p2pOff . P.analysisStandard
         . P.desc "Idle scenario:  start nodes & detach from tty;  no cluster termination"
  ]
