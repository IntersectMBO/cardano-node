{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- cloud: (52 + 1) nodes, FixedLoaded "value" and "plutus" workloads.
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Builtin.Cloud (
  base
, composeFiftytwo
, valueDuration, plutusDuration
, profilesNoEraCloud
, nomadPerf -- Cluster needed to define the "latency-*" profile.
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Builtin.Empty as E
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V

--------------------------------------------------------------------------------

baseInternal :: Types.Profile -> Types.Profile
baseInternal =
    P.fixedLoaded
  . composeFiftytwo
  -- All cloud profiles use trace forwarding.
  . P.traceForwardingOn
  . P.initCooldown 45
  . P.analysisStandard

-- Use `base` / `baseVoltaire` and pick a duration.

base :: Types.Profile -> Types.Profile
base =
    baseInternal
  . V.genesisVariantPreVoltaire

baseVoltaire :: Types.Profile -> Types.Profile
baseVoltaire =
    baseInternal
  . V.genesisVariantVoltaire

--------------------------------------------------------------------------------

composeFiftytwo :: Types.Profile -> Types.Profile
composeFiftytwo = P.torusDense . V.hosts 52 . P.withExplorerNode

--------------------------------------------------------------------------------

-- Value / full blocks, 7 epochs.
valueDuration :: Types.Profile -> Types.Profile
valueDuration =
    V.timescaleModel
    -- Eight epochs.
  . P.shutdownOnSlot 64000 . P.generatorEpochs 8
  . P.analysisSizeFull . P.analysisEpoch3Plus

-- Plutus / small blocks, 9 epochs.
plutusDuration :: Types.Profile -> Types.Profile
plutusDuration =
    V.timescaleModel
    -- Nine epochs.
  . P.shutdownOnSlot 72000 . P.generatorEpochs 9
  . P.analysisEpoch3Plus

--------------------------------------------------------------------------------

-- Replaces "nomad_perf_plutus_base".
plutusLoopBase :: Types.Profile -> Types.Profile
plutusLoopBase =
    P.tps 0.85
  . P.analysisSizeSmall

-- Replaces "nomad_perf_plutussecp_base".
plutusSecpBase :: Types.Profile -> Types.Profile
plutusSecpBase =
    P.tps 2
  . P.analysisSizeModerate

-- Replaces "nomad_perf_plutusv3blst_base".
plutusBlstBase :: Types.Profile -> Types.Profile
plutusBlstBase =
    P.tps 2
  . P.analysisSizeModerate2

--------------------------------------------------------------------------------

profilesNoEraCloud :: [Types.Profile]
profilesNoEraCloud =
  ----------------------
  -- Release benchmarks.
  ----------------------
  let value      = P.empty & base         . V.valueCloud . V.datasetOct2021 . V.fundsDouble . valueDuration  . nomadPerf
                 . P.desc "AWS c5-2xlarge cluster dataset, 7 epochs"
      plutus     = P.empty & base         . V.plutusBase . V.datasetOct2021 . V.fundsDouble . plutusDuration . nomadPerf
                 . P.desc "AWS c5-2xlarge cluster dataset, 9 epochs"
      valueVolt  = P.empty & baseVoltaire . V.valueCloud . V.datasetOct2021 . V.fundsDouble . valueDuration  . nomadPerf
                 . P.desc "AWS c5-2xlarge cluster dataset, 7 epochs"
      plutusVolt = P.empty & baseVoltaire . V.plutusBase . V.datasetOct2021 . V.fundsDouble . plutusDuration . nomadPerf
                 . P.desc "AWS c5-2xlarge cluster dataset, 9 epochs"
      -- Loop.
      loop     = plutus     & plutusLoopBase . V.plutusTypeLoop
      loop2024 = plutus     & plutusLoopBase . V.plutusTypeLoop2024
      loopVolt = plutusVolt & plutusLoopBase . V.plutusTypeLoop
      -- Secp.
      ecdsa    = plutus     & plutusSecpBase . V.plutusTypeECDSA
      schnorr  = plutus     & plutusSecpBase . V.plutusTypeSchnorr
      blst     = plutusVolt & plutusBlstBase . V.plutusTypeBLST
  in [
  -- Value (pre-Voltaire profiles)
    value      & P.name "value-nomadperf"                                 . P.dreps      0 . P.newTracing . P.p2pOn
  , value      & P.name "value-nomadperf-nop2p"                           . P.dreps      0 . P.newTracing . P.p2pOff
  , value      & P.name "value-drep1k-nomadperf"                          . P.dreps   1000 . P.newTracing . P.p2pOn
  , value      & P.name "value-drep2k-nomadperf"                          . P.dreps   2000 . P.newTracing . P.p2pOn
  , value      & P.name "value-drep10k-nomadperf"                         . P.dreps  10000 . P.newTracing . P.p2pOn
  , value      & P.name "value-drep100k-nomadperf"                        . P.dreps 100000 . P.newTracing . P.p2pOn
  , value      & P.name "value-oldtracing-nomadperf"                      . P.dreps      0 . P.oldTracing . P.p2pOn
  , value      & P.name "value-oldtracing-nomadperf-nop2p"                . P.dreps      0 . P.oldTracing . P.p2pOff
  -- Value (post-Voltaire profiles)
  , valueVolt  & P.name "value-volt-nomadperf"                            . P.dreps  10000 . P.newTracing . P.p2pOn
  -- Plutus (pre-Voltaire profiles)
  , loop       & P.name "plutus-nomadperf"                                . P.dreps      0 . P.newTracing . P.p2pOn
  , loop       & P.name "plutus-nomadperf-nop2p"                          . P.dreps      0 . P.newTracing . P.p2pOff
  , loop       & P.name "plutus-drep1k-nomadperf"                         . P.dreps   1000 . P.newTracing . P.p2pOn
  , loop       & P.name "plutus-drep2k-nomadperf"                         . P.dreps   2000 . P.newTracing . P.p2pOn
  , loop       & P.name "plutus-drep10k-nomadperf"                        . P.dreps  10000 . P.newTracing . P.p2pOn
  , loop       & P.name "plutus-drep100k-nomadperf"                       . P.dreps 100000 . P.newTracing . P.p2pOn
  , loop2024   & P.name "plutus24-nomadperf"                              . P.dreps      0 . P.newTracing . P.p2pOn
  , ecdsa      & P.name "plutus-secp-ecdsa-nomadperf"                     . P.dreps      0 . P.newTracing . P.p2pOn
  , schnorr    & P.name "plutus-secp-schnorr-nomadperf"                   . P.dreps      0 . P.newTracing . P.p2pOn
  , blst       & P.name "plutusv3-blst-nomadperf"                         . P.dreps      0 . P.newTracing . P.p2pOn
  , blst       & P.name "plutusv3-blst-double-nomadperf" . P.doubleBudget . P.dreps      0 . P.newTracing . P.p2pOn
  , blst       & P.name "plutusv3-blst-half-nomadperf"   . P.stepHalf     . P.dreps      0 . P.newTracing . P.p2pOn
  -- Plutus (post-Voltaire profiles)
  , loopVolt   & P.name "plutus-volt-nomadperf"                           . P.dreps  10000 . P.newTracing . P.p2pOn
  ]
  ----------------------
  -- Testing benchmarks.
  ----------------------
  ++
  -- TODO: Inconsistency: "fast*" and "ci-test*" use `V.valueLocal` and "default*"/"oldtracing" use `V.valueCloud`.
  let valueCI  = P.empty & E.base . V.valueLocal . P.traceForwardingOn . nomadPerf
      fastNP = valueCI
        & E.fastDuration
        -- TODO: Inconsistency: "fast-nomadperf*" uses 52+Explorer nodes   and "ci-test-nomadperf" 2+Explorer nodes.
        . composeFiftytwo
        -- TODO: Inconsistency: "fast-nomadperf*" uses the last know epoch and "ci-test-nomadperf" epoch 300.
        . V.genesisVariantPreVoltaire
      ciNP = valueCI
        & E.ciTestDuration
        -- TODO: Inconsistency: "ci-test-nomadperf" uses 2+Explorer nodes and "fast-nomadperf*" 52+Explorer nodes.
        . P.torus . V.hosts 2 . P.withExplorerNode
        -- TODO: Inconsistency: "ci-test-nomadperf" uses epoch 300        and "fast-nomadperf*" the last know epoch.
        . V.genesisVariant300
  in [
    fastNP & P.name "fast-nomadperf"               . P.newTracing . P.p2pOn
  , fastNP & P.name "fast-nomadperf-nop2p"         . P.newTracing . P.p2pOff
  , ciNP   & P.name "ci-test-nomadperf"            . P.newTracing . P.p2pOn
  , ciNP   & P.name "ci-test-nomadperf-nop2p"      . P.newTracing . P.p2pOff
  -- TODO: FIXME: A non "nop2p" "nomadperf" profile without P2P???
  , ciNP   & P.name "ci-test-oldtracing-nomadperf" . P.oldTracing . P.p2pOff
  ]
  ++
  -- TODO: Inconsistency: "fast*" and "ci-test*" use `V.valueLocal` and "default*"/"oldtracing" use `V.valueCloud`.
  let defNP = P.empty & E.baseNoDataset . V.valueCloud . P.traceForwardingOn . nomadPerf
        . E.defaultDuration
        -- TODO: Inconsistency: "fast-nomadperf*" uses 52+Explorer nodes   and "ci-test-nomadperf" 2+Explorer nodes.
        . P.torus . V.hosts 6 . P.withExplorerNode
        -- TODO: Inconsistency: "ci-test-nomadperf" uses epoch 300        and "fast-nomadperf*" the last know epoch.
        . V.genesisVariant300
        -- TODO: The only ones without 0 delegators.
        --       Fix and remove `E.baseNoDataset` (Same `E.base` for all).
        . P.delegators 6
        . P.analysisUnitary
  in [
    defNP & P.name "default-nomadperf-nop2p"    . P.newTracing . P.p2pOff
  , defNP & P.name "default-nomadperf"          . P.newTracing . P.p2pOn
  , defNP & P.name "oldtracing-nomadperf"       . P.oldTracing . P.p2pOn
  , defNP & P.name "oldtracing-nomadperf-nop2p" . P.oldTracing . P.p2pOff
  ]
  ++
  let ciBench =
          P.empty
        & P.fixedLoaded
        . V.hosts 2 . P.torus . nomadPerf . P.withExplorerNode
        . V.timescaleCompressed
        . V.genesisVariant300
        . V.datasetMiniature . V.fundsDefault
        . P.shutdownOnBlock 15
        -- TODO: dummy "generator.epochs" ignored in favor of "--shutdown-on".
        . P.generatorEpochs 3 . P.initCooldown 5
        . P.analysisStandard
        . P.desc "Miniature dataset, CI-friendly duration, bench scale"
  -- 2 nodes, Nomad perf
  in [
    ciBench & P.name "ci-bench-nomadperf"            . V.valueLocal . P.dreps 0 . P.traceForwardingOn . P.newTracing . P.p2pOn
  , ciBench & P.name "ci-bench-nomadperf-nop2p"      . V.valueLocal . P.dreps 0 . P.traceForwardingOn . P.newTracing . P.p2pOff
  -- TODO: FIXME: A non "nop2p" "nomadperf" profile without P2P???
  , ciBench & P.name "ci-bench-oldtracing-nomadperf" . V.valueLocal . P.dreps 0 . P.traceForwardingOn . P.oldTracing . P.p2pOff
  ]

--------------------------------------------------------------------------------

nomadPerf :: Types.Profile -> Types.Profile
nomadPerf =
  P.regions
    [
      Types.AWS Types.EU_CENTRAL_1
    , Types.AWS Types.US_EAST_1
    , Types.AWS Types.AP_SOUTHEAST_2
    ]
  .
  P.nomadNamespace "perf"
  .
  P.nomadClass "perf"
  .
  P.nomadResources (Types.ByNodeType {
    Types.producer = Types.Resources 8 15400 16000
  , Types.explorer = Just $ Types.Resources 16 32000 64000
  })
  .
  P.nomadSSHLogsOn
  .
  P.clusterKeepRunningOn
  .
  P.awsInstanceTypes (Types.ByNodeType {
    Types.producer = "c5.2xlarge"
  , Types.explorer = Just "m5.4xlarge"
  })
  .
  P.usePublicRouting
  .
  P.clusterMinimunStorage (Just $ Types.ByNodeType {
    Types.producer = 12582912
  , Types.explorer = Just 14155776
  })
