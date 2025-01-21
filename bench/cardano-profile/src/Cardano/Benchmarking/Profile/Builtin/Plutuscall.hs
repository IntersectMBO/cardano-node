{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- "loop", "secp-ecdsa" and "secp-schnorr"
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Builtin.Plutuscall (
  profilesNoEraPlutuscall
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V

--------------------------------------------------------------------------------

compressedFor15Epochs :: Types.Profile -> Types.Profile
compressedFor15Epochs =
    V.timescaleCompressed
  . P.generatorEpochs 15 . P.initCooldown 5
  . P.shutdownOnSlot 9000

--------------------------------------------------------------------------------

profilesNoEraPlutuscall :: [Types.Profile]
profilesNoEraPlutuscall =
  ------------------------------------------------------------------------------
  -- plutuscall: 6 nodes, FixedLoaded and "--shutdown-on-slot-synced 9000"
  ------------------------------------------------------------------------------
  let plutusCall =
          P.empty
        & P.fixedLoaded
        . P.uniCircle . V.hosts 6 . P.loopback
        . compressedFor15Epochs
        . V.datasetSmall
        . V.fundsDefault
        . P.p2pOff
        . P.traceForwardingOn . P.newTracing
        . P.analysisStandard
        . V.clusterDefault -- TODO: "cluster" should be "null" here.
        . P.desc "Small dataset, honest 15 epochs duration"

      loop            = plutusCall & V.plutusTypeLoop    . V.plutusDoubleSaturation     . P.analysisSizeModerate . P.analysisEpoch3Plus
      ecdsa           = plutusCall & V.plutusTypeECDSA   . V.plutusDoubleSaturation     . P.analysisSizeModerate . P.analysisEpoch3Plus
      schnorr         = plutusCall & V.plutusTypeSchnorr . V.plutusDoubleSaturation     . P.analysisSizeModerate . P.analysisEpoch3Plus

      loopVolt        = plutusCall & V.plutusTypeLoop    . V.plutusDoublePlusSaturation . P.analysisSizeSmall
      blstVolt        = plutusCall & V.plutusTypeBLST    . V.plutusDoublePlusSaturation . P.analysisSizeModerate2
      ripemdVolt      = plutusCall & V.plutusTypeRIPEMD  . V.plutusDoublePlusSaturation . P.analysisSizeSmall
  in [
  -- TODO: after dropping jq profiles, bump all to genesisVariantVoltaire
    loop        & P.name "plutuscall-loop-plain"          . V.genesisVariantPreVoltaire
  , loop        & P.name "plutuscall-loop-half"           . V.genesisVariant300  . P.stepHalf     . P.v8Preview
  , loop        & P.name "plutuscall-loop-double"         . V.genesisVariant300  . P.doubleBudget . P.v8Preview
  , ecdsa       & P.name "plutuscall-secp-ecdsa-plain"    . V.genesisVariantPreVoltaire
  , ecdsa       & P.name "plutuscall-secp-ecdsa-half"     . V.genesisVariant300  . P.stepHalf     . P.v8Preview
  , ecdsa       & P.name "plutuscall-secp-ecdsa-double"   . V.genesisVariant300  . P.doubleBudget . P.v8Preview
  , schnorr     & P.name "plutuscall-secp-schnorr-plain"  . V.genesisVariantPreVoltaire
  , schnorr     & P.name "plutuscall-secp-schnorr-half"   . V.genesisVariant300  . P.stepHalf     . P.v8Preview
  , schnorr     & P.name "plutuscall-secp-schnorr-double" . V.genesisVariant300  . P.doubleBudget . P.v8Preview

  , loopVolt    & P.name "plutuscall-volt-loop"           . V.genesisVariantVoltaire              . P.v10Preview
  , blstVolt    & P.name "plutuscall-volt-blst"           . V.genesisVariantVoltaire              . P.v10Preview
  , ripemdVolt  & P.name "plutuscall-volt-ripemd"         . V.genesisVariantVoltaire              . P.v10Preview
  ]
