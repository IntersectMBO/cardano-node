{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- "loop", "secp-ecdsa" and "secp-schnorr"
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Builtin.Plutuscall (
  profilesPlutuscall
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Playground as Pl (calibrateLoopBlockMemx2)
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

profilesPlutuscall :: [Types.Profile]
profilesPlutuscall =
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
        . P.traceForwardingOn
        . P.analysisStandard
        . P.desc "Small dataset, honest 15 epochs duration"

      loop            = plutusCall & V.plutusTypeLoop             . V.plutusDoubleSaturation     . P.analysisSizeModerate  . P.analysisEpoch3Plus
      ecdsa           = plutusCall & V.plutusTypeECDSA            . V.plutusDoubleSaturation     . P.analysisSizeModerate  . P.analysisEpoch3Plus
      schnorr         = plutusCall & V.plutusTypeSchnorr          . V.plutusDoubleSaturation     . P.analysisSizeModerate  . P.analysisEpoch3Plus
      schnorrV3       = plutusCall & V.plutusTypeSchnorrV3        . V.plutusDoubleSaturation     . P.analysisSizeSmall     . P.analysisEpoch3Plus
      mscalmul        = plutusCall & V.plutusTypeMultScalarMultG1 . V.plutusDoubleSaturation     . P.analysisSizeSmall     . P.analysisEpoch3Plus
      ripemdVolt4tx   = plutusCall & V.plutusTypeRIPEMD_4tx       . V.plutusDoubleSaturation     . P.analysisSizeSmall     . P.analysisEpoch3Plus

      loopVolt        = plutusCall & V.plutusTypeLoop       . V.plutusDoublePlusSaturation . P.analysisSizeSmall        . P.analysisEpoch3Plus
      blstVolt        = plutusCall & V.plutusTypeBLST       . V.plutusDoublePlusSaturation . P.analysisSizeModerate2    . P.analysisEpoch3Plus
      ripemdVolt      = plutusCall & V.plutusTypeRIPEMD     . V.plutusDoublePlusSaturation . P.analysisSizeSmall        . P.analysisEpoch3Plus

      postPlomin      = V.genesisVariantVoltaire
  in [
    loop        & P.name "plutuscall-loop"                 . postPlomin
  , loop        & P.name "plutuscall-loop-memx2"           . postPlomin . P.budgetBlockMemoryDouble . P.overlay Pl.calibrateLoopBlockMemx2
  , ecdsa       & P.name "plutuscall-secp-ecdsa"           . postPlomin
  , ecdsa       & P.name "plutuscall-secp-ecdsa-stepx2"    . postPlomin . P.budgetBlockStepsDouble
  , schnorr     & P.name "plutuscall-secp-schnorr"         . postPlomin
  , schnorr     & P.name "plutuscall-secp-schnorr-stepx2"  . postPlomin . P.budgetBlockStepsDouble

  , loopVolt    & P.name "plutuscall-volt-loop"            . postPlomin
  , blstVolt    & P.name "plutuscall-volt-blst"            . postPlomin
  , ripemdVolt  & P.name "plutuscall-volt-ripemd"          . postPlomin

  -- PlutusV3 benchmarks, targeting PV11
  , ripemdVolt4tx  & P.name "plutuscall-voltv11-ripemd"    . postPlomin . P.v11Preview
  , schnorrV3      & P.name "plutuscall-voltv11-schnorrv3" . postPlomin . P.v11Preview
  , mscalmul       & P.name "plutuscall-voltv11-mscalmul"  . postPlomin . P.v11Preview
  ]
