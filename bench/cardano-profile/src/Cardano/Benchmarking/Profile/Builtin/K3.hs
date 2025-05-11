{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Builtin.K3 (
  profilesNoEraK3
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V

--------------------------------------------------------------------------------

profilesNoEraK3 :: [Types.Profile]
profilesNoEraK3 =
  ------------------------------------------------------------------------------
  -- k3: 3 nodes and no "--shutdown-on-slot-synced" and no "--shutdown-on-block-synced"
  ------------------------------------------------------------------------------
  let k3 =
          P.empty
        & P.fixedLoaded
        . P.uniCircle . V.hosts 3 . P.loopback
        . V.genesisVariant300
        . P.epochLength 600 . P.activeSlotsCoeff 0.05 . P.parameterK 3
        . V.datasetCurrent -- 10000000 UTxO (10000kU), 1300000 delegators (1300kD)
        . V.fundsDefault
        . P.shutdownOnOff . P.generatorEpochs 3 . P.initCooldown 5
        . P.p2pOff
        . P.traceForwardingOn . P.newTracing
        . P.analysisStandard . P.analysisUnitary
  in [
    k3 & P.name "k3-3ep-5kTx-10000kU-1300kD-64kbs-fixed-loaded"        . V.valueBase . P.slotDuration 0.2 . P.tps 12
  , k3 & P.name "k3-3ep-9kTx-10000kU-1300kD-64kbs-5tps-fixed-loaded"   . V.valueBase . P.slotDuration 1   . P.tps  5
  , k3 & P.name "k3-3ep-18kTx-10000kU-1300kD-64kbs-10tps-fixed-loaded" . V.valueBase . P.slotDuration 1   . P.tps 10
  ]
