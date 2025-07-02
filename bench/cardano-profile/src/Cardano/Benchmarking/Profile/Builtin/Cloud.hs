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
import qualified Cardano.Benchmarking.Profile.Builtin.Scenario.Base as B
import qualified Cardano.Benchmarking.Profile.Playground as Pl (calibrateLoopBlockMemx15, calibrateLoopBlockMemx2)
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V
import qualified Cardano.Benchmarking.Profile.Workload.CGroupMemory as C
import qualified Cardano.Benchmarking.Profile.Workload.Voting as W
import qualified Cardano.Benchmarking.Profile.Workload.Latency as L

--------------------------------------------------------------------------------

baseInternal :: Types.Profile -> Types.Profile
baseInternal =
    P.fixedLoaded
  . composeFiftytwo
  . P.maxBlockSize 88000
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

baseVoting :: Types.Profile -> Types.Profile
baseVoting =
    baseVoltaire
  . P.voting . P.v10Preview

--------------------------------------------------------------------------------

composeFiftytwo :: Types.Profile -> Types.Profile
composeFiftytwo = P.torusDense . V.hosts 52 . P.withExplorerNode

--------------------------------------------------------------------------------

-- Value / full blocks, 7 epochs.
valueDurationBase :: Types.Profile -> Types.Profile
valueDurationBase =
    V.timescaleModel
    -- Eight epochs.
  . P.shutdownOnSlot 64000 . P.generatorEpochs 8
  . P.analysisSizeFull

valueDuration :: Types.Profile -> Types.Profile
valueDuration =
  valueDurationBase . P.analysisEpoch3Plus

valueDurationVoting :: Types.Profile -> Types.Profile
valueDurationVoting =
  -- The extra splitting needs two more epochs before the benchmarking phase.
  valueDurationBase . P.analysisEpoch5Plus

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

-- Replaces "nomad_perf_plutussecp_base".
plutusSecpBase :: Types.Profile -> Types.Profile
plutusSecpBase =
    P.tps 2

-- Replaces "nomad_perf_plutusv3blst_base".
plutusBlstBase :: Types.Profile -> Types.Profile
plutusBlstBase =
    P.tps 2

-- Replaces "nomad_perf_plutusv3blst_base".
plutusRipemdBase :: Types.Profile -> Types.Profile
plutusRipemdBase =
    P.tps 2

--------------------------------------------------------------------------------

profilesNoEraCloud :: [Types.Profile]
profilesNoEraCloud =
  ----------------------
  -- Release benchmarks.
  ----------------------
  let valueDesc  = P.desc "AWS c5-2xlarge cluster dataset, 7 ep, value workload"
      plutusDesc = P.desc "AWS c5-2xlarge cluster dataset, 9 ep, Plutus workload"

      value      = P.empty & base         . V.valueCloud . V.datasetOct2021 . V.fundsDouble . valueDuration  . nomadPerf
                 . valueDesc
      plutus     = P.empty & base         . V.plutusBase . V.datasetOct2021 . V.fundsDouble . plutusDuration . nomadPerf
                 . plutusDesc
      valueVolt  = P.empty & baseVoltaire . V.valueCloud . V.datasetOct2021 . V.fundsDouble . valueDuration  . nomadPerf
                 . valueDesc
      plutusVolt = P.empty & baseVoltaire . V.plutusBase . V.datasetOct2021 . V.fundsDouble . plutusDuration . nomadPerf
                 . plutusDesc
      -- memory-constrained
      loop       = plutus     & plutusLoopBase   . V.plutusTypeLoop     . P.analysisSizeSmall
      loop2024   = plutus     & plutusLoopBase   . V.plutusTypeLoop2024 . P.analysisSizeSmall
      loopVolt   = plutusVolt & plutusLoopBase   . V.plutusTypeLoop     . P.analysisSizeSmall
      loopV3Volt = plutusVolt & plutusLoopBase   . V.plutusTypeLoopV3   . P.analysisSizeSmall
      -- steps-constrained
      ecdsa    = plutus     & plutusSecpBase   . V.plutusTypeECDSA    . P.analysisSizeModerate
      schnorr  = plutus     & plutusSecpBase   . V.plutusTypeSchnorr  . P.analysisSizeModerate
      blst     = plutusVolt & plutusBlstBase   . V.plutusTypeBLST     . P.analysisSizeModerate2
      ripemd   = plutusVolt & plutusRipemdBase . V.plutusTypeRIPEMD   . P.analysisSizeSmall
      -- PParams overlays and calibration for 4 tx per block memory full.
      blockMem15x = P.budgetBlockMemoryOneAndAHalf . P.overlay Pl.calibrateLoopBlockMemx15
      blockMem2x  = P.budgetBlockMemoryDouble      . P.overlay Pl.calibrateLoopBlockMemx2
      -- LMDB helper. Node config add the "hostvolume"s as a cluster constraint.
      lmdb =   P.lmdb
             -- The name of the defined volume in the Nomad Client config and
             -- where to mount it inside the isolated chroot.
             -- If the volume is not present the deployment will fail!
             . P.appendNomadHostVolume (Types.ByNodeType {
                 Types.producer = [Types.HostVolume "/ephemeral" False "ephemeral"]
               , Types.explorer = Nothing
               })
             . P.ssdDirectory "/ephemeral"
  in [
  -- Value (pre-Voltaire profiles)
    value     & P.name "value-nomadperf"                                   . P.dreps      0 . P.newTracing . P.p2pOn
  , value     & P.name "value-nomadperf-nop2p"                             . P.dreps      0 . P.newTracing . P.p2pOff
  , value     & P.name "value-drep1k-nomadperf"                            . P.dreps   1000 . P.newTracing . P.p2pOn
  , value     & P.name "value-drep10k-nomadperf"                           . P.dreps  10000 . P.newTracing . P.p2pOn
  , value     & P.name "value-drep100k-nomadperf"                          . P.dreps 100000 . P.newTracing . P.p2pOn
  , value     & P.name "value-oldtracing-nomadperf"                        . P.dreps      0 . P.oldTracing . P.p2pOn
  , value     & P.name "value-oldtracing-nomadperf-nop2p"                  . P.dreps      0 . P.oldTracing . P.p2pOff
  -- Value (post-Voltaire profiles)
  , valueVolt & P.name "value-volt-nomadperf"                              . P.dreps  10000 . P.newTracing . P.p2pOn
              . P.workloadAppend C.cgroupMemoryWorkload
  , valueVolt & P.name "value-volt-rtsqg1-nomadperf"                       . P.dreps  10000 . P.newTracing . P.p2pOn . P.rtsGcParallel . P.rtsGcLoadBalance
  , valueVolt & P.name "value-volt-lmdb-nomadperf"                         . P.dreps  10000 . P.newTracing . P.p2pOn . lmdb
  -- Plutus (pre-Voltaire profiles)
  , loop      & P.name "plutus-nomadperf"                                  . P.dreps      0 . P.newTracing . P.p2pOn
  , loop      & P.name "plutus-nomadperf-nop2p"                            . P.dreps      0 . P.newTracing . P.p2pOff
  , loop      & P.name "plutus-drep1k-nomadperf"                           . P.dreps   1000 . P.newTracing . P.p2pOn
  , loop      & P.name "plutus-drep10k-nomadperf"                          . P.dreps  10000 . P.newTracing . P.p2pOn
  , loop      & P.name "plutus-drep100k-nomadperf"                         . P.dreps 100000 . P.newTracing . P.p2pOn
  , loop2024  & P.name "plutus24-nomadperf"                                . P.dreps      0 . P.newTracing . P.p2pOn
  , ecdsa     & P.name "plutus-secp-ecdsa-nomadperf"                       . P.dreps      0 . P.newTracing . P.p2pOn
  , schnorr   & P.name "plutus-secp-schnorr-nomadperf"                     . P.dreps      0 . P.newTracing . P.p2pOn
  -- Plutus (post-Voltaire profiles)
  , loopVolt    & P.name "plutus-volt-nomadperf"                           . P.dreps  10000 . P.newTracing . P.p2pOn
  , loopV3Volt  & P.name "plutusv3-volt-nomadperf"                         . P.dreps  10000 . P.newTracing . P.p2pOn
  , loopVolt    & P.name "plutus-volt-memx15-nomadperf"                    . P.dreps  10000 . P.newTracing . P.p2pOn . blockMem15x
  , loopVolt    & P.name "plutus-volt-memx2-nomadperf"                     . P.dreps  10000 . P.newTracing . P.p2pOn . blockMem2x
  , loopVolt    & P.name "plutus-volt-rtsqg1-nomadperf"                    . P.dreps  10000 . P.newTracing . P.p2pOn . P.rtsGcParallel . P.rtsGcLoadBalance
  , loopVolt    & P.name "plutus-volt-lmdb-nomadperf"                      . P.dreps  10000 . P.newTracing . P.p2pOn . lmdb
  -- TODO: scaling the BLST workload only works well for 4 txns/block instead of 8. However, comparing it to other steps-constrained workloads, requires 8txns/block (like all of those).
  , blst      & P.name "plutusv3-blst-nomadperf"                           . P.dreps  10000 . P.newTracing . P.p2pOn . P.v10Preview
  , blst      & P.name "plutusv3-blst-stepx15-nomadperf"                   . P.dreps  10000 . P.newTracing . P.p2pOn . P.v10Preview . P.budgetBlockStepsOneAndAHalf
  , blst      & P.name "plutusv3-blst-stepx2-nomadperf"                    . P.dreps  10000 . P.newTracing . P.p2pOn . P.v10Preview . P.budgetBlockStepsDouble
  , ripemd    & P.name "plutusv3-ripemd-nomadperf"                         . P.dreps  10000 . P.newTracing . P.p2pOn . P.v10Preview
  , ripemd    & P.name "plutusv3-ripemd-stepx15-nomadperf"                 . P.dreps  10000 . P.newTracing . P.p2pOn . P.v10Preview . P.budgetBlockStepsOneAndAHalf
  , ripemd    & P.name "plutusv3-ripemd-stepx2-nomadperf"                  . P.dreps  10000 . P.newTracing . P.p2pOn . P.v10Preview . P.budgetBlockStepsDouble
  ]
  ----------
  -- Voting.
  ----------
  ++
  let valueVoting  = P.empty & baseVoting . V.valueCloud . V.datasetOct2021 . V.fundsVoting . valueDurationVoting . nomadPerf
             . P.descAdd "+ voting" . valueDesc
      plutusVoting = P.empty & baseVoting . V.plutusBase . V.datasetOct2021 . V.fundsVoting . plutusDuration      . nomadPerf
             . P.descAdd "+ voting" . plutusDesc
      loopVoting   = plutusVoting & plutusLoopBase . V.plutusTypeLoop . P.analysisSizeSmall
  in [
  -- Voting
    valueVoting & P.name "value-voting-utxo-volt-nomadperf"              . P.dreps  10000 . P.newTracing . P.p2pOn . P.workloadAppend W.votingWorkloadUtxo
  , valueVoting & P.name "value-voting-volt-nomadperf"                   . P.dreps  10000 . P.newTracing . P.p2pOn . P.workloadAppend W.votingWorkloadx1
  , valueVoting & P.name "value-voting-double-volt-nomadperf"            . P.dreps  10000 . P.newTracing . P.p2pOn . P.workloadAppend W.votingWorkloadx2
  , loopVoting  & P.name "plutus-voting-utxo-volt-nomadperf"             . P.dreps  10000 . P.newTracing . P.p2pOn . P.workloadAppend W.votingWorkloadUtxo
  , loopVoting  & P.name "plutus-voting-volt-nomadperf"                  . P.dreps  10000 . P.newTracing . P.p2pOn . P.workloadAppend W.votingWorkloadx1
  , loopVoting  & P.name "plutus-voting-double-volt-nomadperf"           . P.dreps  10000 . P.newTracing . P.p2pOn . P.workloadAppend W.votingWorkloadx2
  ]
  -----------
  -- Latency.
  -----------
  ++
  let latency =
          P.empty & B.base
        . P.fixedLoaded
        . composeFiftytwo
        -- TODO: Use `genesisVariant300` like the others and to "Scenario.Base".
        . V.genesisVariantPreVoltaire
        . V.timescaleCompressed
         -- TODO: "tracer-only" and "idle" have `P.delegators 6`.
         --       Remove and use `V.datasetEmpty` in module "Scenario.Base".
        . P.delegators 0
        . P.workloadAppend L.latencyWorkload
        . P.analysisStandard
  in (
    latency & P.name "latency-nomadperf"
            . P.desc "AWS perf class cluster, stop when all latency services stop"
            . P.traceForwardingOn . P.newTracing . P.p2pOn . nomadPerf
  )
  ----------------------
  -- Testing benchmarks.
  ----------------------
  :
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
  , ciBench & P.name "ci-bench-oldtracing-nomadperf" . V.valueLocal . P.dreps 0 .                       P.oldTracing . P.p2pOn
  ]


--------------------------------------------------------------------------------

nomadPerfBase :: Types.Profile -> Types.Profile
nomadPerfBase =
  -- Exact regions with availability zone (AZ) to match.
  P.regions
    [
      Types.AWS Types.EU_CENTRAL_1
    , Types.AWS Types.US_EAST_1
    , Types.AWS Types.AP_SOUTHEAST_2
    ]
  .
  -- Logical cluster separation. To avoid conflicts with same-server machines.
  P.nomadNamespace "perf" . P.nomadClass "perf"
  .
  -- Instance types will be used as Group "constraints".
  P.awsInstanceTypes (Types.ByNodeType {
    Types.producer = "c5d.2xlarge"
  , Types.explorer = Just "m5.4xlarge"
  })
  .
  -- Force all network related stuff to "attr.unique.platform.aws.public-ipv4".
  P.usePublicRouting
  .
  -- Nomad cloud backend Jobs won't start below these levels.
  P.clusterMinimunStorage (Just $ Types.ByNodeType {
    Types.producer = 12582912
  , Types.explorer = Just 14155776
  })
  .
  -- Require the cgroup fs mounted by default.
  P.appendNomadHostVolume (Types.ByNodeType {
    Types.producer = [Types.HostVolume "/sys/fs/cgroup" True "cgroup"]
  , Types.explorer = Nothing
  })
  .
  -- Flag to use SSH instead of `nomad exec` to fetch the logs.
  P.nomadSSHLogsOn
  .
  -- Don't stop the Nomad Job when finished.
  P.clusterKeepRunningOn

nomadPerf :: Types.Profile -> Types.Profile
nomadPerf =
  nomadPerfBase . nomadPerfResourcesAll

nomadPerfResourcesAll :: Types.Profile -> Types.Profile
nomadPerfResourcesAll =
  -- This will be used as constraints at the Task level.
  P.nomadResources (Types.ByNodeType {
    Types.producer = Types.Resources {
      Types.cores = 8
    , Types.memory = 15400
    , Types.memory_max = 16000
    }
  -- Explorer is unchanged between cloud profiles.
  , Types.explorer = Just resourcesExplorer
  })

resourcesExplorer :: Types.Resources
resourcesExplorer =
  Types.Resources {
    Types.cores = 16
  , Types.memory = 32000
  , Types.memory_max = 64000
  }
