{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NumericUnderscores #-}
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

-- | 40 minutes, the last 30 of them (from slot 600) measured.
compressedFor4Epochs :: Types.Profile -> Types.Profile
compressedFor4Epochs =
    V.timescaleCompressed
  . P.generatorEpochs 4 . P.initCooldown 5
  . P.shutdownOnSlot 2400

--------------------------------------------------------------------------------

profilesPlutuscall :: [Types.Profile]
profilesPlutuscall =
  ------------------------------------------------------------------------------
  -- plutuscall: 6 nodes, FixedLoaded and "--shutdown-on-slot-synced 9000"
  ------------------------------------------------------------------------------
  let plutusCallFor duration =
          P.empty
        & P.fixedLoaded
        . P.uniCircle . V.hosts 6 . P.loopback
        . duration
        . V.datasetSmall
        . V.fundsDefault
        . P.traceForwardingOn
        . P.analysisStandard
      plutusCall = plutusCallFor compressedFor15Epochs & P.desc "Small dataset, honest 15 epochs duration"

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
      loopV3          = plutusCall & P.txFeeOverwrite 2_000_000 . V.plutusTypeLoopV3 . V.plutusDoublePlusSaturation . P.analysisEpoch3Plus
      -- The same loop as a Plutus V4 script, for Dijkstra.
      loopV4          = plutusCall & P.txFeeOverwrite 2_000_000 . V.plutusTypeLoopV4 . V.plutusDoublePlusSaturation . P.analysisEpoch3Plus
      loopV3Script15k src =
                        plutusCall & P.plutusVariants src 200 (Just 15_000) . P.txFeeOverwrite 2_000_000
                                   . V.plutusTypeLoopV3 . V.plutusDoublePlusSaturation . P.analysisEpoch3Plus

      -- The worst case of reference scripts on mainnet parameters: blocks full
      -- of them, at the limit of 1 MiB per block, which make the nodes decode
      -- the most script per lovelace. Every transaction spends one input,
      -- locked at a variant of LoopV3 of its own, of about 16 KB (the largest
      -- that a transaction publishing it can carry), within the first tier of
      -- the reference script fee (25,600 bytes per transaction). The loop
      -- runs no iterations, so execution costs next to nothing, and 65 such
      -- transactions fill a block's 1 MiB. 128 variants keep the transactions
      -- of a block from sharing one, which the ledger would decode once. 4 tps
      -- keeps the blocks full, about 66 transactions every 20 seconds.
      -- Mempool.AttemptAdd, silenced by the workbench, marks the start of the
      -- validation of each transaction the mempool adds.
      refScript1MiB   = plutusCallFor compressedFor4Epochs
                      & P.plutusVariants Types.ScriptByReference 128 (Just 15_900)
                      . P.plutusType "FixedLoop" . P.plutusScript "LoopV3" . P.redeemerInt 1_000_000
                      . V.plutusBase . P.txFee 600_000 . P.tps 4
                      . P.analysisEpoch2Plus . P.enableTracers ["Mempool.AttemptAdd"]
                      . P.desc "Small dataset, 4 epochs, blocks full of reference scripts"

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
  , refScript1MiB                           & P.name "plutuscall-volt-refscript-1mib"       . postPlomin

  -- LoopV3 and LoopV4 on the v12 preview (Dijkstra, PV12): run with ERA=dijkstra.
  , loopV3         & P.name "plutuscall-voltv12-loopv3"    . postPlomin . P.v12Preview . P.v11Preview
  , loopV4         & P.name "plutuscall-voltv12-loopv4"    . postPlomin . P.v12Preview . P.v11Preview

  -- PlutusV3 benchmarks, targeting PV11
  , ripemdVolt4tx  & P.name "plutuscall-voltv11-ripemd"    . postPlomin . P.v11Preview
  , schnorrV3      & P.name "plutuscall-voltv11-schnorrv3" . postPlomin . P.v11Preview
  , mscalmul       & P.name "plutuscall-voltv11-mscalmul"  . postPlomin . P.v11Preview
  , expmod         & P.name "plutuscall-voltv11-expmod"    . postPlomin . P.v11Preview
  ]
