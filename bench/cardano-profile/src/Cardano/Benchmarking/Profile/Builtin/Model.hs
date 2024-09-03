{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- Model timescale, "secp-ecdsa" and "value".
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Builtin.Model (
  profilesNoEraModel
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V

--------------------------------------------------------------------------------

modelFor7Epochs :: Types.Profile -> Types.Profile
modelFor7Epochs =
    V.timescaleModel
  . P.generatorEpochs 7 . P.initCooldown 45
  . P.shutdownOnSlot 56000

--------------------------------------------------------------------------------

profilesNoEraModel :: [Types.Profile]
profilesNoEraModel =
  ------------------------------------------------------------------------------
  -- model: 4 nodes, FixedLoaded and "--shutdown-on-slot-synced 56000"
  ------------------------------------------------------------------------------
  let model =
          P.empty
        & P.fixedLoaded
        . P.uniCircle . V.hosts 4 . P.loopback
        . modelFor7Epochs
        . V.fundsDouble
        . P.traceForwardingOn . P.newTracing
        . P.analysisStandard . P.analysisEpoch3Plus
        . V.clusterDefault -- TODO: "cluster" should be "null" here.
        . P.desc "Status-quo dataset, 7 epochs"
      secp  = V.plutusDoubleSaturation . V.plutusTypeECDSA
      value = V.valueBase . P.tps 9 -- "value" with the Plutus `txFee`.
  in [
  -- TODO: after dropping jq profiles, bump all to genesisVariantVoltaire
    model & P.name "model-secp-ecdsa-double" . secp  . V.genesisVariant300          . P.doubleBudget . P.v8Preview . V.datasetCurrent
  , model & P.name "model-secp-ecdsa-half"   . secp  . V.genesisVariant300          . P.stepHalf     . P.v8Preview . V.datasetCurrent
  , model & P.name "model-secp-ecdsa-plain"  . secp  . V.genesisVariantPreVoltaire                                 . V.datasetCurrent
  , model & P.name "model-value"             . value . V.genesisVariantPreVoltaire                                 . V.datasetCurrent . P.analysisSizeFull
  , model & P.name "model-value-test"        . value . V.genesisVariantPreVoltaire                                 . V.datasetSmall   . P.analysisSizeFull
  ]
