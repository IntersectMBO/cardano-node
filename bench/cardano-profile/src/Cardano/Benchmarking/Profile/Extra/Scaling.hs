{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- UTxO scale / "-nomadperfssd" cluster.
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Extra.Scaling (
  profilesNoEraScalingLocal, profilesNoEraScalingCloud
, nomadSsd -- Cluster needed to define the "latency-*" profile.
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Builtin.Cloud as C
import qualified Cardano.Benchmarking.Profile.Builtin.Empty as E
import qualified Cardano.Benchmarking.Profile.Builtin.Scenario.Base as B
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V
import qualified Cardano.Benchmarking.Profile.Workload.Latency as L

--------------------------------------------------------------------------------

base :: Types.Profile -> Types.Profile
base =
    P.fixedLoaded
  . P.uniCircle . V.hosts 1
  . P.delegators 1200000 . P.dreps 0
  . P.initCooldown 5
  . P.traceForwardingOn . P.newTracing
  . P.analysisStandard

profilesNoEraScalingLocal :: [Types.Profile]
profilesNoEraScalingLocal =
  let fastStartup =
          P.empty & base
        . P.loopback
        . V.valueLocal
        . E.fastDuration
  in [
    fastStartup & P.name "faststartup-24M"                    . P.utxo 24000000 . V.fundsDefault . V.genesisVariant300
  ]

profilesNoEraScalingCloud :: [Types.Profile]
profilesNoEraScalingCloud =
  let utxoScale =
          P.empty & base
        . P.regions [Types.AWS Types.EU_CENTRAL_1]
        . V.timescaleSmall . P.maxBlockSize 88000
        . P.shutdownOnSlot 7200 . P.generatorEpochs 6
        . V.valueCloud
        . P.p2pOn
        . clusterNomadSsdNoRegions
        . P.analysisSizeFull . P.analysisEpoch3Plus
      fast  = P.empty & V.genesisVariantPreVoltaire . C.composeFiftytwo . E.base . E.fastDuration . nomadSsd
      value = P.empty & C.base . C.valueDuration . nomadSsd
  in [
    utxoScale   & P.name "utxoscale-solo-12M16G-nomadperfssd" . P.utxo 12000000 . V.fundsDouble  . V.genesisVariantPreVoltaire . P.rtsHeapLimit 16384 . P.heapLimit 16384
  , utxoScale   & P.name "utxoscale-solo-12M64G-nomadperfssd" . P.utxo 12000000 . V.fundsDouble  . V.genesisVariantPreVoltaire
  , utxoScale   & P.name "utxoscale-solo-24M64G-nomadperfssd" . P.utxo 24000000 . V.fundsDouble  . V.genesisVariantPreVoltaire
  , fast        & P.name "fast-nomadperfssd"  . V.valueLocal . P.traceForwardingOn . P.newTracing . P.p2pOn
  , value       & P.name "value-nomadperfssd" . V.valueCloud . V.datasetOct2021    . P.dreps 0 . V.fundsDouble . P.newTracing . P.p2pOn
  ]
  -----------
  -- Latency.
  -----------
  ++
  let latency =
          P.empty & B.base
        . P.fixedLoaded
        . C.composeFiftytwo
        -- TODO: Use `genesisVariant300` like the others and to "Scenario.Base".
        . V.genesisVariantPreVoltaire
        . V.timescaleCompressed
         -- TODO: "tracer-only" and "idle" have `P.delegators 6`.
         --       Remove and use `V.datasetEmpty` in module "Scenario.Base".
        . P.delegators 0
        . P.workloadAppend L.latencyWorkload
        . P.analysisStandard
  in [
    latency & P.name "latency-nomadperfssd"
            . P.desc "AWS perf-ssd class cluster, stop when all latency services stop"
            . P.traceForwardingOn . P.newTracing . P.p2pOn . nomadSsd
  ]

nomadSsd :: Types.Profile -> Types.Profile
nomadSsd = clusterNomadSsdNoRegions . nomadSsdRegions

clusterNomadSsdNoRegions :: Types.Profile -> Types.Profile
clusterNomadSsdNoRegions =
  P.nomadNamespace "perf-ssd"
  .
  P.nomadClass "perf-ssd"
  .
  P.nomadResources (Types.ByNodeType {
    Types.producer = Types.Resources 16 120000 124000
  , Types.explorer = Just $ Types.Resources 16 120000 124000
  })
  .
  P.nomadHostVolume (Types.HostVolume "/ssd2" False "ssd2")
  .
  P.nomadHostVolume (Types.HostVolume "/ssd1" False "ssd1")
  .
  P.nomadSSHLogsOn
  .
  P.clusterKeepRunningOn
  .
  P.awsInstanceTypes (Types.ByNodeType {
    Types.producer = "r5d.4xlarge"
  , Types.explorer = Just "r5d.4xlarge"
  })
  .
  P.usePublicRouting
  .
  P.clusterMinimunStorage Nothing

nomadSsdRegions :: Types.Profile -> Types.Profile
nomadSsdRegions =
  P.regions
    [
      Types.AWS Types.EU_CENTRAL_1
    , Types.AWS Types.US_EAST_1
    , Types.AWS Types.AP_SOUTHEAST_2
    ]
