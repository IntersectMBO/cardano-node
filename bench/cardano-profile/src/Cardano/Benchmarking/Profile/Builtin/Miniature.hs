{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Builtin.Miniature (
  base
, benchDuration
, profilesNoEraMiniature
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V

--------------------------------------------------------------------------------

base :: Types.Profile -> Types.Profile
base =
    P.fixedLoaded
  . V.datasetMiniature
  . V.fundsDefault
  . P.initCooldown 5
  . P.analysisStandard

benchDuration :: Types.Profile -> Types.Profile
benchDuration =
    V.timescaleCompressed
  . P.shutdownOnBlock 15
  . P.generatorEpochs 3

duration30 :: Types.Profile -> Types.Profile
duration30 =
    V.timescaleCompressed
  . P.shutdownOnSlot 1800
  . P.generatorEpochs 3

duration60 :: Types.Profile -> Types.Profile
duration60 =
    V.timescaleCompressed
  . P.shutdownOnSlot 3600
  . P.generatorEpochs 6

duration240 :: Types.Profile -> Types.Profile
duration240 =
    V.timescaleCompressed
  . P.shutdownOnSlot 14400
  . P.generatorEpochs 24

--------------------------------------------------------------------------------

profilesNoEraMiniature :: [Types.Profile]
profilesNoEraMiniature =
  ------------------------------------------------------------------------------
  -- ci-bench: 2|10 nodes, FixedLoaded and "--shutdown-on-block-synced 15"
  ------------------------------------------------------------------------------
  let ciBench =
          P.empty & base
        . P.desc "Miniature dataset, CI-friendly duration, bench scale"
        . P.uniCircle . P.loopback
        . benchDuration
      -- Helpers by size:
      ciBench02  = ciBench & V.hosts  2
      ciBench10  = ciBench & V.hosts 10
      -- Helpers by workload:
      ciBench02Value    = ciBench02 & V.genesisVariant300
      ciBench02Plutus   = ciBench02 & V.genesisVariantPreVoltaire
      ciBench02PlutusV3 = ciBench02 & V.genesisVariantVoltaire
      ciBench10Value    = ciBench10 & V.genesisVariant300
      ciBench10Plutus   = ciBench10 & V.genesisVariant300
      loop     = V.plutusSaturation           . V.plutusTypeLoop     . P.analysisSizeSmall
      loop2024 = V.plutusSaturation           . V.plutusTypeLoop2024 . P.analysisSizeSmall
      ecdsa    = V.plutusDoublePlusSaturation . V.plutusTypeECDSA    . P.analysisSizeModerate
      schnorr  = V.plutusDoublePlusSaturation . V.plutusTypeSchnorr  . P.analysisSizeModerate
      blst     = V.plutusDoublePlusSaturation . V.plutusTypeBLST     . P.analysisSizeModerate2
      ripemd   = V.plutusDoublePlusSaturation . V.plutusTypeRIPEMD   . P.analysisSizeSmall
  in [
  -- 2 nodes, local
    ciBench02Value    & P.name "ci-bench"                        . V.valueLocal . P.dreps  0 . P.traceForwardingOn  . P.newTracing . P.p2pOff
  , ciBench02Value    & P.name "ci-bench-lmdb"                   . V.valueLocal . P.dreps  0 . P.traceForwardingOn  . P.newTracing . P.p2pOn  . P.lmdb . P.ssdDirectory "/tmp"
  , ciBench02Value    & P.name "ci-bench-rtview"                 . V.valueLocal . P.dreps  0 . P.traceForwardingOn  . P.newTracing . P.p2pOff . P.tracerRtview
  , ciBench02Value    & P.name "ci-bench-p2p"                    . V.valueLocal . P.dreps  0 . P.traceForwardingOn  . P.newTracing . P.p2pOn 
  , ciBench02Value    & P.name "ci-bench-notracer"               . V.valueLocal . P.dreps  0 . P.traceForwardingOff . P.newTracing . P.p2pOff
  , ciBench02Value    & P.name "ci-bench-drep"                   . V.valueLocal . P.dreps 10 . P.traceForwardingOn  . P.newTracing . P.p2pOff
  , ciBench02Plutus   & P.name "ci-bench-plutus"                 . loop         . P.dreps  0 . P.traceForwardingOn  . P.newTracing . P.p2pOff
  , ciBench02Plutus   & P.name "ci-bench-plutus24"               . loop2024     . P.dreps  0 . P.traceForwardingOn  . P.newTracing . P.p2pOff
  , ciBench02Plutus   & P.name "ci-bench-plutus-secp-ecdsa"      . ecdsa        . P.dreps  0 . P.traceForwardingOn  . P.newTracing . P.p2pOff
  , ciBench02Plutus   & P.name "ci-bench-plutus-secp-schnorr"    . schnorr      . P.dreps  0 . P.traceForwardingOn  . P.newTracing . P.p2pOff
  , ciBench02PlutusV3 & P.name "ci-bench-plutusv3-blst"          . blst         . P.dreps  0 . P.traceForwardingOn  . P.newTracing . P.p2pOff
  , ciBench02PlutusV3 & P.name "ci-bench-plutusv3-ripemd"        . ripemd       . P.dreps  0 . P.traceForwardingOn  . P.newTracing . P.p2pOff . P.v10Preview
  , ciBench02PlutusV3 & P.name "ci-bench-plutusv3-ripemd-step2x" . ripemd       . P.dreps  0 . P.traceForwardingOn  . P.newTracing . P.p2pOff . P.v10Preview . P.budgetBlockStepsDouble
  -- 10 nodes, local
  , ciBench10Value    & P.name "10"                              . V.valueLocal . P.dreps  0 . P.traceForwardingOn  . P.newTracing . P.p2pOff
  , ciBench10Value    & P.name "10-p2p"                          . V.valueLocal . P.dreps  0 . P.traceForwardingOn  . P.newTracing . P.p2pOn
  , ciBench10Value    & P.name "10-notracer"                     . V.valueLocal . P.dreps  0 . P.traceForwardingOff . P.newTracing . P.p2pOff
  , ciBench10Plutus   & P.name "10-plutus"                       . loop         . P.dreps  0 . P.traceForwardingOn  . P.newTracing . P.p2pOff
  ]
  ++
  ---------------------------------------------------------------------------------------
  -- 6 nodes in dense topology, reduced blocksize, miniature dataset and varying runtimes
  ---------------------------------------------------------------------------------------
  let dense =
          P.empty & base
        . P.desc "Miniature dataset, dense topology on local cluster, 64k blocks"
        . P.torusDense . V.hosts 6 . P.loopback
        -- Function `genesisVariantPreVoltaire` last to match `jq` order.
        . P.blocksize64k . P.v9Preview . V.genesisVariantPreVoltaire
        . P.dreps 0
        . P.p2pOn
        . P.analysisSizeFull . P.analysisUnitary
  in [
    dense & P.name "6-dense"            . V.valueCloud . duration30  . P.traceForwardingOn . P.newTracing
  , dense & P.name "6-dense-rtsprof"    . V.valueCloud . duration30  . P.traceForwardingOn . P.newTracing . P.rtsHeapProf . P.rtsEventlogged
  , dense & P.name "6-dense-1h"         . V.valueCloud . duration60  . P.traceForwardingOn . P.newTracing
  , dense & P.name "6-dense-1h-rtsprof" . V.valueCloud . duration60  . P.traceForwardingOn . P.newTracing . P.rtsHeapProf . P.rtsEventlogged
  , dense & P.name "6-dense-4h"         . V.valueCloud . duration240 . P.traceForwardingOn . P.newTracing
  , dense & P.name "6-dense-4h-rtsprof" . V.valueCloud . duration240 . P.traceForwardingOn . P.newTracing . P.rtsHeapProf . P.rtsEventlogged
  ]
