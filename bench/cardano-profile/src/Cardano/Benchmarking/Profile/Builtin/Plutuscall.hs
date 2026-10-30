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
      expmod          = plutusCall & V.plutusTypeExpMod           . V.plutusDoubleSaturation     . P.analysisSizeSmall     . P.analysisEpoch3Plus

      loopVolt        = plutusCall & V.plutusTypeLoop       . V.plutusDoublePlusSaturation . P.analysisSizeSmall        . P.analysisEpoch3Plus
      blstVolt        = plutusCall & V.plutusTypeBLST       . V.plutusDoublePlusSaturation . P.analysisSizeModerate2    . P.analysisEpoch3Plus
      ripemdVolt      = plutusCall & V.plutusTypeRIPEMD     . V.plutusDoublePlusSaturation . P.analysisSizeSmall        . P.analysisEpoch3Plus

      -- LoopV3 alone, then as 200 variants of 15 KB, each input locked at one,
      -- its script in the witnesses or by reference. The variants' padding is
      -- never run, so the three differ in script bytes, not execution. All pay
      -- the fee of the dearest, the witness discharge. No size filter, as their
      -- blocks differ in size by the scripts.
      loopV3          = plutusCall & P.txFeeOverwrite 2000000 . V.plutusTypeLoopV3 . V.plutusDoublePlusSaturation . P.analysisEpoch3Plus
      -- The same loop as a Plutus V4 script, for Dijkstra.
      loopV4          = plutusCall & P.txFeeOverwrite 2000000 . V.plutusTypeLoopV4 . V.plutusDoublePlusSaturation . P.analysisEpoch3Plus
      loopV3Script15k src =
                        plutusCall & P.plutusVariants src 200 (Just 15000) . P.txFeeOverwrite 2000000
                                   . V.plutusTypeLoopV3 . V.plutusDoublePlusSaturation . P.analysisEpoch3Plus

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

  , loopV3                                  & P.name "plutuscall-volt-loopv3"              . postPlomin
  , loopV3Script15k Types.ScriptInWitness   & P.name "plutuscall-volt-loopv3-script15k"    . postPlomin
  , loopV3Script15k Types.ScriptByReference & P.name "plutuscall-volt-loopv3-refscript15k" . postPlomin

  -- LoopV3 and LoopV4 on the v12 preview (Dijkstra, PV12): run with ERA=dijkstra.
  , loopV3         & P.name "plutuscall-voltv12-loopv3"    . postPlomin . P.v12Preview . P.v11Preview
  , loopV4         & P.name "plutuscall-voltv12-loopv4"    . postPlomin . P.v12Preview . P.v11Preview

  -- PlutusV3 benchmarks, targeting PV11
  , ripemdVolt4tx  & P.name "plutuscall-voltv11-ripemd"    . postPlomin . P.v11Preview
  , schnorrV3      & P.name "plutuscall-voltv11-schnorrv3" . postPlomin . P.v11Preview
  , mscalmul       & P.name "plutuscall-voltv11-mscalmul"  . postPlomin . P.v11Preview
  , expmod         & P.name "plutuscall-voltv11-expmod"    . postPlomin . P.v11Preview
  ]
