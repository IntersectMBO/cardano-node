{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Map (
  byName, profiles
, profilesNoEra
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))

import qualified Data.Map.Strict as Map

import qualified Cardano.Benchmarking.Profile as P
import qualified Cardano.Benchmarking.Profile.Types as Types

--------------------------------------------------------------------------------

byName :: String -> Types.Profile
byName name = (Map.!) profiles name

--------------------------------------------------------------------------------

profiles :: Map.Map String Types.Profile
profiles = foldMap
  (\profile -> Map.fromList $
    let
        -- Add eras, like "ci-test-notracer-[alra|alzo|bage|coay|mary|shey]"
        addEra = \p era suffix ->
          let name = Types.name p
              newName = name ++ "-" ++ suffix
          in  (newName, p {Types.name = newName, Types.era = era})
    in 
        [ addEra profile Types.Allegra "alra"
        , addEra profile Types.Shelley "shey"
        , addEra profile Types.Mary    "mary"
        , addEra profile Types.Alonzo  "alzo"
        , addEra profile Types.Babbage "bage"
        , addEra profile Types.Conway  "coay"
        ]
  )
  profilesNoEra

-- Aux functions
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
  P.clusterMinimunStorage (Just $ Types.ByNodeType {
    Types.producer = 12582912
  , Types.explorer = Just 14155776
  })

nomadSsd :: Types.Profile -> Types.Profile
nomadSsd =
  P.regions
    [
      Types.AWS Types.EU_CENTRAL_1
    ]
  .
  P.nomadNamespace "perf-ssd"
  .
  P.nomadClass "perf-ssd"
  .
  P.nomadResources (Types.ByNodeType {
    Types.producer = Types.Resources 32 64000 64000
  , Types.explorer = Just $ Types.Resources 32 64000 64000
  })
  .
  P.nomadSSHLogsOn
  .
  P.clusterKeepRunningOn
  .
  P.awsInstanceTypes (Types.ByNodeType {
    Types.producer = "c5.9xlarge"
  , Types.explorer = Nothing
  })
  .
  P.clusterMinimunStorage Nothing

--------------------------------------------------------------------------------

-- TODO: forge-stress and forge-stress-light have the same .node.shutdown_on_slot_synced
-- Adding a P.nameSuffix was abandoned to keep the code `grep` friendly!
profilesNoEra :: Map.Map String Types.Profile
-- Names:
-- wb profile all-profiles | jq .[] | jq -r .name | sort | uniq | grep "\-bage"
profilesNoEra = Map.fromList $ map (\p -> (Types.name p, p)) $
  ------------------------------------------------------------------------------
  -- fast: 2 nodes, FixedLoaded and "--shutdown-on-block-synced 1"
  ------------------------------------------------------------------------------
  let fast =   P.defaults
             & P.utxo 0 . P.delegators 0
             . P.epochLength 600 . P.parameterK 3
             . P.fixedLoaded . P.generatorTps 15
             . P.shutdownOnBlock 1
             . P.analysisStandard
      fastLocal = fast & P.uniCircle  . P.hosts 2  . P.loopback
      fastPerf  = fast & P.torusDense . P.hosts 52 . nomadPerf  . P.withExplorerNode
      fastSsd   = fast & P.uniCircle  . P.hosts 1  . nomadSsd
  in [
    (fastLocal & P.name "fast"                  . P.tracerOn  . P.newTracing                                )
  , (fastLocal & P.name "fast-plutus"           . P.tracerOn  . P.newTracing           . P.analysisSizeSmall)
  , (fastLocal & P.name "fast-p2p"              . P.tracerOn  . P.newTracing . P.p2pOn                      )
  , (fastLocal & P.name "fast-oldtracing"       . P.tracerOn  . P.oldTracing                                )
  , (fastLocal & P.name "fast-notracer"         . P.tracerOff . P.newTracing                                )
  , (fastPerf  & P.name "fast-nomadperf"        . P.tracerOn  . P.newTracing . P.p2pOn                      )
  , (fastPerf  & P.name "fast-nomadperf-nop2p"  . P.tracerOn  . P.newTracing                                )
  , (fastSsd   & P.name "fast-nomadperfssd"     . P.tracerOn  . P.newTracing . P.p2pOn                      )
  ]
  ++
  ------------------------------------------------------------------------------
  -- ci-test: 2 nodes, FixedLoaded and "--shutdown-on-block-synced 3"
  ------------------------------------------------------------------------------
  let ciTest =   P.defaults
               & P.hosts 2
               . P.utxo 0 . P.delegators 0
               . P.epochLength 600 . P.parameterK 3
               . P.fixedLoaded . P.generatorTps 15
               . P.shutdownOnBlock 3
               . P.analysisStandard
      -- TODO: Why are not both using UniCircle ????
      ciTestLocal     = ciTest & P.uniCircle . P.loopback
      ciTestNomadPerf = ciTest & P.torus     . nomadPerf  . P.withExplorerNode
  in [
  -- Local
    (ciTestLocal     & P.name "ci-test"                      . P.tracerOn  . P.newTracing . P.p2pOff                  )
  , (ciTestLocal     & P.name "ci-test-plutus"               . P.tracerOn  . P.newTracing . P.p2pOff                  . P.analysisSizeSmall)
  , (ciTestLocal     & P.name "ci-test-rtview"               . P.tracerOn  . P.newTracing . P.p2pOff . P.tracerRtview )
  , (ciTestLocal     & P.name "ci-test-p2p"                  . P.tracerOn  . P.newTracing . P.p2pOn                   )
  , (ciTestLocal     & P.name "ci-test-notracer"             . P.tracerOff . P.newTracing . P.p2pOff                  )
  -- Nomad perf
  , (ciTestNomadPerf & P.name "ci-test-nomadperf"            . P.tracerOn  . P.newTracing . P.p2pOn                   )
  , (ciTestNomadPerf & P.name "ci-test-nomadperf-nop2p"      . P.tracerOn  . P.newTracing . P.p2pOff                  )
  -- TODO: FIXME: A non "nop2p" "nomadperf" profile without P2P???
  , (ciTestNomadPerf & P.name "ci-test-oldtracing-nomadperf" . P.tracerOn  . P.oldTracing . P.p2pOff                  )
  ]
  ++
  ------------------------------------------------------------------------------
  -- ci-test-dense: 10 pools, FixedLoaded and "--shutdown-on-block-synced 3"
  ------------------------------------------------------------------------------
  let ciTestDense =   P.defaults
                    & P.uniCircle . P.pools 10
                    . P.loopback
                    . P.p2pOff
                    . P.utxo 0 . P.delegators 0
                    . P.epochLength 600 . P.parameterK 3
                    . P.fixedLoaded . P.generatorTps 15
                    . P.shutdownOnBlock 3
                    . P.tracerOn  . P.newTracing
                    . P.analysisStandard
  in [
    (ciTestDense & P.name "ci-test-dense10")
  ]
  ++
  ------------------------------------------------------------------------------
  -- epoch transition: 2 nodes, FixedLoaded and "--shutdown-on-slot-synced 900"
  ------------------------------------------------------------------------------
  let epochTransition =   P.defaults
                        & P.uniCircle . P.hosts 2
                        . P.loopback
                        . P.utxo 0 . P.delegators 0
                        . P.epochLength 600 . P.parameterK 3
                        . P.fixedLoaded . P.generatorTps 15
                        . P.shutdownOnSlot 900
                        . P.tracerOn . P.newTracing
                        . P.analysisStandard
  in [
    (epochTransition & P.name "epoch-transition")
  ]
  ++
  ------------------------------------------------------------------------------
  -- ci-bench: 2|5|10 nodes, FixedLoaded and "--shutdown-on-block-synced 15"
  ------------------------------------------------------------------------------
  let ciBench =  P.defaults
               & P.fixedLoaded . P.generatorTps 15
               . P.epochLength 600 . P.parameterK 3
               . P.shutdownOnBlock 15
               . P.analysisStandard
      ciBench02  = ciBench & P.hosts  2 . P.utxo 500000 . P.delegators 100000
      ciBench06  = ciBench & P.hosts  6 . P.utxo      0 . P.delegators      0
      ciBench10  = ciBench & P.hosts 10 . P.utxo 500000 . P.delegators 100000
      -- TODO: Why are not all using UniCircle ????
      ciBench02Local     = ciBench02 & P.uniCircle . P.loopback
      ciBench02NomadPerf = ciBench02 & P.torus     . nomadPerf  . P.withExplorerNode
      ciBench06Trace     = ciBench06 & P.torus     . P.loopback . P.tracerWithresources
      ciBench10Local     = ciBench10 & P.uniCircle . P.loopback
  in [
  -- 2 nodes, local
    (ciBench02Local     & P.name "ci-bench"                      . P.tracerOn  . P.newTracing . P.p2pOff                  )
  , (ciBench02Local     & P.name "ci-bench-plutus"               . P.tracerOn  . P.newTracing . P.p2pOff                  . P.analysisSizeSmall)
  , (ciBench02Local     & P.name "ci-bench-plutus-secp-ecdsa"    . P.tracerOn  . P.newTracing . P.p2pOff                  . P.analysisSizeSmall)
  , (ciBench02Local     & P.name "ci-bench-plutus-secp-schnorr"  . P.tracerOn  . P.newTracing . P.p2pOff                  . P.analysisSizeSmall)
  , (ciBench02Local     & P.name "ci-bench-rtview"               . P.tracerOn  . P.newTracing . P.p2pOff . P.tracerRtview )
  , (ciBench02Local     & P.name "ci-bench-p2p"                  . P.tracerOn  . P.newTracing . P.p2pOn                   )
  , (ciBench02Local     & P.name "ci-bench-notracer"             . P.tracerOff . P.newTracing . P.p2pOff                  )
  -- 2 nodes, Nomad perf
  , (ciBench02NomadPerf & P.name "ci-bench-nomadperf"            . P.tracerOn  . P.newTracing . P.p2pOn                   )
  , (ciBench02NomadPerf & P.name "ci-bench-nomadperf-nop2p"      . P.tracerOn  . P.newTracing . P.p2pOff                  )
  -- TODO: FIXME: A non "nop2p" "nomadperf" profile without P2P???
  , (ciBench02NomadPerf & P.name "ci-bench-oldtracing-nomadperf" . P.tracerOn  . P.oldTracing . P.p2pOff                  )
  -- 6 nodes, local
  , (ciBench06Trace     & P.name "trace-bench"                   . P.tracerOn  . P.newTracing . P.p2pOff                  )
  , (ciBench06Trace     & P.name "trace-bench-rtview"            . P.tracerOn  . P.newTracing . P.p2pOff . P.tracerRtview )
  , (ciBench06Trace     & P.name "trace-bench-oldtracing"        . P.tracerOn  . P.oldTracing . P.p2pOff                  )
  , (ciBench06Trace     & P.name "trace-bench-notracer"          . P.tracerOff . P.newTracing . P.p2pOff                  )
  -- 10 nodes, local
  , (ciBench10Local     & P.name "10"                            . P.tracerOn  . P.newTracing . P.p2pOff                  )
  , (ciBench10Local     & P.name "10-plutus"                     . P.tracerOn  . P.newTracing . P.p2pOff                  . P.analysisSizeSmall)
  , (ciBench10Local     & P.name "10-p2p"                        . P.tracerOn  . P.newTracing . P.p2pOn                   )
  , (ciBench10Local     & P.name "10-notracer"                   . P.tracerOff . P.newTracing . P.p2pOff                  )
  ]
  ++
  ------------------------------------------------------------------------------
  -- forge-stress
  ------------------------------------------------------------------------------
  let forgeStress =   P.defaults
                    & P.uniCircle
                    . P.loopback
                    . P.p2pOff
                    . P.epochLength 600 . P.parameterK 3
                    . P.fixedLoaded . P.generatorTps 15
                    . P.newTracing
                    . P.analysisStandard
      -- "--shutdown-on-slot-synced 2400"
      forgeStress1      = P.hosts 1
      forgeStress3      = P.hosts 3
      forgeStress6      = P.hosts 6
      forgeStressPre    = P.utxo  4000000 . P.delegators 1000000
      forgeStressNonPre = P.utxo 10000000 . P.delegators 1300000
      forgeStressXS     = P.shutdownOnSlot 1200
      forgeStressM      = P.shutdownOnSlot 2400
      forgeStressXL     = P.shutdownOnSlot 4800
  in [
  -- 1 node versions (non-pre).
    (forgeStress & P.name "forge-stress-solo-xs"       . forgeStress1 . forgeStressNonPre . forgeStressXS . P.tracerOn                                         . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-solo"          . forgeStress1 . forgeStressNonPre . forgeStressM  . P.tracerOn                                         . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-plutus-solo"   . forgeStress1 . forgeStressNonPre . forgeStressM  . P.tracerOn                                         . P.analysisSizeSmall )
  -- 1 node versions (pre).
  , (forgeStress & P.name "forge-stress-pre-solo-xs"   . forgeStress1 . forgeStressPre    . forgeStressXS . P.tracerOn                                         . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-solo"      . forgeStress1 . forgeStressPre    . forgeStressM  . P.tracerOn                                         . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-solo-xl"   . forgeStress1 . forgeStressPre    . forgeStressXL . P.tracerOn                                         . P.analysisEpoch3Plus)
  -- 3 nodes versions (non-pre).
  , (forgeStress & P.name "forge-stress"               . forgeStress3 . forgeStressNonPre . forgeStressM  . P.tracerOn                                         . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-notracer"      . forgeStress3 . forgeStressNonPre . forgeStressM  . P.tracerOff                                        . P.analysisUnitary   )
  -- TODO: FIXME: "forge-stress-p2p" has no P2P enabled!
  , (forgeStress & P.name "forge-stress-p2p"           . forgeStress3 . forgeStressNonPre . forgeStressM  . P.tracerOn                                         . P.analysisSizeSmall )
  , (forgeStress & P.name "forge-stress-plutus"        . forgeStress3 . forgeStressNonPre . forgeStressM  . P.tracerOn                                         . P.analysisSizeSmall )
  -- 3 nodes versions (pre).
  , (forgeStress & P.name "forge-stress-pre"           . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOn                                         . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-plutus"    . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOn                                         . P.analysisSizeSmall )
  , (forgeStress & P.name "forge-stress-pre-rtsA4m"    . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOn                   . P.rtsGcAllocSize  4 . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-rtsA64m"   . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOn                   . P.rtsGcAllocSize 64 . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-rtsN3"     . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOn  . P.rtsThreads 3                       . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-rtsA4mN3"  . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOn  . P.rtsThreads 3 . P.rtsGcAllocSize  4 . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-rtsA64mN3" . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOn  . P.rtsThreads 3 . P.rtsGcAllocSize 64 . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-rtsxn"     . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOn                   . P.rtsGcNonMoving    . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-notracer"  . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOff                                        . P.analysisUnitary   )
  -- Double nodes and time running version.
  , (forgeStress & P.name "forge-stress-large"         . forgeStress6 . forgeStressNonPre . forgeStressXL . P.tracerOn                                         . P.analysisEpoch3Plus)
  ]
  ++
  ------------------------------------------------------------------------------
  -- TODO: This is a special case of forge-stress. Mix both? Still used?
  ------------------------------------------------------------------------------
  let dish =   P.defaults
             & P.uniCircle . P.hosts 3
             . P.loopback
             . P.p2pOff
             . P.epochLength 600 . P.parameterK 3
             . P.fixedLoaded . P.generatorTps 15
             . P.shutdownOnSlot 2400
             . P.tracerOn  . P.newTracing
             . P.analysisStandard
      dish30M = dish & P.utxo 30000000 . P.delegators 0
      dish10M = dish & P.utxo 10000000 . P.delegators 0
  in [
    (dish30M & P.name "dish"            . P.analysisUnitary  )
  , (dish30M & P.name "dish-plutus"     . P.analysisSizeSmall)
  , (dish10M & P.name "dish-10M"        . P.analysisUnitary  )
  , (dish10M & P.name "dish-10M-plutus" . P.analysisSizeSmall)
  ]
  ++
  ------------------------------------------------------------------------------
  -- k3: 3 nodes and no "--shutdown-on-slot-synced" and no "--shutdown-on-block-synced"
  ------------------------------------------------------------------------------
  let k3 =   P.defaults
           & P.uniCircle . P.hosts 3
           . P.loopback
           . P.p2pOff
           . P.utxo 10000000 . P.delegators 1300000
           . P.epochLength 600 . P.parameterK 3
           . P.fixedLoaded
           . P.shutdownOnOff
           . P.tracerOn  . P.newTracing
           . P.analysisStandard . P.analysisUnitary
  in [
    (k3 & P.name "k3-3ep-18kTx-10000kU-1300kD-64kbs-10tps-fixed-loaded" )
  , (k3 & P.name "k3-3ep-22kTx-10000kU-1300kD-64kbs-fixed-loaded"       )
  , (k3 & P.name "k3-3ep-5kTx-10000kU-1300kD-64kbs-fixed-loaded"        )
  , (k3 & P.name "k3-3ep-9kTx-10000kU-1300kD-64kbs-5tps-fixed-loaded"   )
  ]
  ++
  ------------------------------------------------------------------------------
  -- 6 nodes and no "--shutdown-on-slot-synced" and no "--shutdown-on-block-synced"
  ------------------------------------------------------------------------------
  let idle =   P.defaults
             & P.uniCircle . P.hosts 6
             . P.loopback
             . P.p2pOff
             . P.utxo 0 . P.delegators 6
             . P.idle
             . P.shutdownOnOff
             . P.analysisUnitary
  in [
    (idle & P.name "devops" . P.epochLength 1000 . P.parameterK 10 . P.tracerOn  . P.newTracing . P.p2pOff . P.analysisOff)
  , (idle & P.name "idle"   . P.epochLength 600 . P.parameterK   3 . P.tracerOn  . P.newTracing . P.p2pOff . P.analysisStandard)
  ]
  ++
  ------------------------------------------------------------------------------
  -- 6 nodes and no "--shutdown-on-slot-synced" and no "--shutdown-on-block-synced"
  ------------------------------------------------------------------------------
  let tracerOnly =   P.defaults
                   & P.uniCircle . P.hosts 6
                   . P.loopback
                   . P.p2pOff
                   . P.utxo 0 . P.delegators 6
                   . P.epochLength 600 . P.parameterK 3
                   . P.tracerOnly
                   . P.shutdownOnOff
                   . P.analysisStandard . P.analysisUnitary
  in [
    (tracerOnly & P.name "tracer-only" . P.tracerOn  . P.newTracing . P.p2pOff)
  ]
  ++
  ------------------------------------------------------------------------------
  -- 6 nodes and no "--shutdown-on-slot-synced" and no "--shutdown-on-block-synced"
  ------------------------------------------------------------------------------
  let noCliStop =   P.defaults
                  & P.hosts 6
                  . P.fixedLoaded
                  . P.utxo 0 . P.delegators 6
                  . P.epochLength 600 . P.parameterK 3
                  . P.shutdownOnOff
                  . P.analysisStandard
      -- TODO: Why are not all using Torus ????
      noCliStopLocal     = noCliStop & P.uniCircle . P.loopback
      noCliStopNomadPerf = noCliStop & P.torus     . nomadPerf  . P.withExplorerNode
  in [
    (noCliStopLocal     & P.name "default"                    . P.tracerOn  . P.newTracing . P.p2pOff . P.analysisUnitary  )
  , (noCliStopLocal     & P.name "default-p2p"                . P.tracerOn  . P.newTracing . P.p2pOn  . P.analysisUnitary  )
  , (noCliStopLocal     & P.name "oldtracing"                 . P.tracerOn  . P.oldTracing . P.p2pOff . P.analysisUnitary  )
  , (noCliStopLocal     & P.name "plutus"                     . P.tracerOn  . P.newTracing . P.p2pOff . P.analysisSizeSmall)
  , (noCliStopLocal     & P.name "plutus-secp-ecdsa"          . P.tracerOn  . P.newTracing . P.p2pOff . P.analysisSizeSmall)
  , (noCliStopLocal     & P.name "plutus-secp-schnorr"        . P.tracerOn  . P.newTracing . P.p2pOff . P.analysisSizeSmall)
  , (noCliStopNomadPerf & P.name "default-nomadperf"          . P.tracerOn  . P.newTracing . P.p2pOn  . P.analysisUnitary  )
  , (noCliStopNomadPerf & P.name "default-nomadperf-nop2p"    . P.tracerOn  . P.newTracing . P.p2pOff . P.analysisUnitary  )
  , (noCliStopNomadPerf & P.name "oldtracing-nomadperf"       . P.tracerOn  . P.oldTracing . P.p2pOn  . P.analysisUnitary  )
  , (noCliStopNomadPerf & P.name "oldtracing-nomadperf-nop2p" . P.tracerOn  . P.oldTracing . P.p2pOff . P.analysisUnitary  )
  ]
  ++
  ------------------------------------------------------------------------------
  -- model: 4 nodes, FixedLoaded and "--shutdown-on-slot-synced 56000"
  ------------------------------------------------------------------------------
  let model =    P.defaults
              & P.uniCircle . P.hosts 4
              . P.loopback
              . P.epochLength 8000 . P.parameterK 40
              . P.fixedLoaded
              . P.shutdownOnSlot 56000
              . P.tracerOn . P.newTracing
              . P.analysisStandard . P.analysisEpoch3Plus
  in [
    (model & P.name "model-secp-ecdsa-double" . P.analysisSizeModerate . P.utxo 10000000 . P.delegators 1300000)
  , (model & P.name "model-secp-ecdsa-half"   . P.analysisSizeModerate . P.utxo 10000000 . P.delegators 1300000)
  , (model & P.name "model-secp-ecdsa-plain"  . P.analysisSizeModerate . P.utxo 10000000 . P.delegators 1300000)
  , (model & P.name "model-value"             . P.analysisSizeFull     . P.utxo 10000000 . P.delegators 1300000)
  , (model & P.name "model-value-test"        . P.analysisSizeFull     . P.utxo  1000000 . P.delegators  200000)
  ]
  ++
  ------------------------------------------------------------------------------
  -- plutuscall: 6 nodes, FixedLoaded and "--shutdown-on-slot-synced 9000"
  ------------------------------------------------------------------------------
  let plutusCall =   P.defaults
                   & P.uniCircle . P.hosts 6
                   . P.loopback
                   . P.p2pOff
                   . P.utxo 1000000 . P.delegators 200000
                   . P.epochLength 600 . P.parameterK 3
                   . P.fixedLoaded
                   . P.shutdownOnSlot 9000
                   . P.tracerOn . P.newTracing
                   . P.analysisStandard . P.analysisSizeModerate . P.analysisEpoch3Plus
  in [
    (plutusCall & P.name "plutuscall-loop-double"        )
  , (plutusCall & P.name "plutuscall-loop-half"          )
  , (plutusCall & P.name "plutuscall-loop-plain"         )
  , (plutusCall & P.name "plutuscall-secp-ecdsa-double"  )
  , (plutusCall & P.name "plutuscall-secp-ecdsa-half"    )
  , (plutusCall & P.name "plutuscall-secp-ecdsa-plain"   )
  , (plutusCall & P.name "plutuscall-secp-schnorr-double")
  , (plutusCall & P.name "plutuscall-secp-schnorr-half"  )
  , (plutusCall & P.name "plutuscall-secp-schnorr-plain" )
  ]
  ++
  ------------------------------------------------------------------------------
  -- plutuscall: 6 nodes, FixedLoaded and "--shutdown-on-slot-synced 1200"
  ------------------------------------------------------------------------------
  let traceFull =   P.defaults
                  & P.torus . P.hosts 6
                  . P.loopback
                  . P.p2pOff
                  . P.utxo 0 . P.delegators 0
                  . P.epochLength 600 . P.parameterK 3
                  . P.fixedLoaded
                  . P.shutdownOnSlot 1200
                  . P.tracerOn . P.newTracing
                  . P.analysisStandard
  in [
    (traceFull & P.name "trace-full"        . P.tracerWithresources                 )
  , (traceFull & P.name "trace-full-rtview" . P.tracerWithresources . P.tracerRtview)
  ]
  ++
  ------------------------------------------------------------------------------
  -- cloud: (52 + 1) nodes, FixedLoaded and "--shutdown-on-slot-synced 56000"
  ------------------------------------------------------------------------------
  let cloud =   P.defaults
              & P.torusDense . P.hosts 52 . P.withExplorerNode
              . nomadPerf
              . P.utxo 4000000 . P.delegators 1000000
              . P.epochLength 8000 . P.parameterK 40
              . P.fixedLoaded
              . P.tracerOn
              . P.analysisStandard
      value  = cloud & P.shutdownOnSlot 64000
      plutus = cloud & P.shutdownOnSlot 72000
  in [
  -- Value
    (value  & P.name "value-nomadperf"                   . P.newTracing . P.p2pOn  . P.analysisSizeFull  . P.analysisEpoch3Plus)
  , (value  & P.name "value-nomadperf-nop2p"             . P.newTracing . P.p2pOff . P.analysisSizeFull  . P.analysisEpoch3Plus)
  , (value  & P.name "value-oldtracing-nomadperf"        . P.oldTracing . P.p2pOn  . P.analysisSizeFull  . P.analysisEpoch3Plus)
  , (value  & P.name "value-oldtracing-nomadperf-nop2p"  . P.oldTracing . P.p2pOff . P.analysisSizeFull  . P.analysisEpoch3Plus)
  -- Plutus
  , (plutus & P.name "plutus-nomadperf"                  . P.newTracing . P.p2pOn  . P.analysisSizeSmall . P.analysisEpoch3Plus)
  , (plutus & P.name "plutus-nomadperf-nop2p"            . P.newTracing . P.p2pOff . P.analysisSizeSmall . P.analysisEpoch3Plus)
  ]
  ++
  ------------------------------------------------------------------------------
  -- 
  ------------------------------------------------------------------------------
  let latency =   P.defaults
                & P.torusDense . P.hosts 52 . P.withExplorerNode
                . nomadPerf
                . P.utxo 0 . P.delegators 0
                . P.epochLength 600 . P.parameterK 3
                . P.latency . P.generatorTps 15
                . P.tracerOn . P.newTracing
                . P.analysisStandard
  in [
    (latency & P.name "latency-nomadperf" . P.p2pOn)
  ]
  ++
  ------------------------------------------------------------------------------
  -- chainsync
  ------------------------------------------------------------------------------
  let chainsync =   P.defaults
                  & P.uniCircle . P.hostsChainsync 1 . P.withChaindbServer . P.withExplorerNode
                  . P.loopback
                  . P.utxo 0 . P.delegators 0
                  . P.epochLength 432000 . P.parameterK 2160
                  . P.chainsync
                  . P.analysisPerformance
      byron  = chainsync & P.shutdownOnSlot   237599
      alonzo = chainsync & P.shutdownOnSlot 38901589
  in [
  -- Byron
    (byron & P.name "chainsync-early-byron"               . P.tracerOff . P.newTracing . P.p2pOff )
  , (byron & P.name "chainsync-early-byron-notracer"      . P.tracerOff . P.newTracing . P.p2pOff )
  , (byron & P.name "chainsync-early-byron-oldtracing"    . P.tracerOff . P.oldTracing . P.p2pOff )
  -- Alonzo
  , (alonzo  & P.name "chainsync-early-alonzo"            . P.tracerOff . P.newTracing . P.p2pOff )
  , (alonzo  & P.name "chainsync-early-alonzo-notracer"   . P.tracerOff . P.newTracing . P.p2pOff )
  , (alonzo  & P.name "chainsync-early-alonzo-p2p"        . P.tracerOff . P.newTracing . P.p2pOn  )
  , (alonzo  & P.name "chainsync-early-alonzo-oldtracing" . P.tracerOff . P.oldTracing . P.p2pOff )
  ]

--------------------------------------------------------------------------------

{--

  , generator:
    { add_tx_size:                    100
    , init_cooldown:                  5
    , inputs_per_tx:                  2
    , outputs_per_tx:                 2
    , tx_fee:                         1000000
    , epochs:                         3
    , tps:                            12
    , plutus:
      { type:   null
      , script: null
      }
    }

generatorDefault :: Types.Generator
generatorDefault = Types.Generator {
    Types.add_tx_size = 100
  , Types.init_cooldown = 5
  , Types.inputs_per_tx = 2
  , Types.outputs_per_tx = 2
  , Types.tx_fee = 1000000
  , Types.epochs = 3
  , Types.tps = 12
  , Types.plutus = Types.Plutus Nothing Nothing
  , Types.tx_count = mempty
}

--}
