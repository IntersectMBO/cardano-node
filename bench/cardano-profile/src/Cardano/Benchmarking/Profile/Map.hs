{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Map (
  names, namesNoEra
, byName
, profiles
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))

import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector

import qualified Cardano.Benchmarking.Profile as P
import qualified Cardano.Benchmarking.Profile.Types as Types

--------------------------------------------------------------------------------

names :: [String]
names = Map.keys profiles

namesNoEra :: [String]
namesNoEra = Map.keys profilesNoEra

byName :: String -> Types.Profile
byName name = (Map.!) profiles name

-- Vocabulary.
--------------------------------------------------------------------------------

-- Definition vocabulary: dataset size.
---------------------------------------

datasetEmpty :: Types.Profile -> Types.Profile
datasetEmpty = P.utxo 0 . P.delegators 0 . P.dreps 0

-- Definition vocabulary: timescale.
------------------------------------

timescaleCompressed :: Types.Profile -> Types.Profile
timescaleCompressed =
    P.slotDuration 1 . P.epochLength 600
  . P.activeSlotsCoeff 0.05 . P.parameterK 3

-- Used by "model", "value", and "plutus".
timescaleModel :: Types.Profile -> Types.Profile
timescaleModel =
    P.slotDuration 1 . P.epochLength 8000
  . P.activeSlotsCoeff 0.05 . P.parameterK 40

-- Definition vocabulary: genesis variants.
-------------------------------------------

-- See: data/genesis/epoch-timeline.json
genesisVariant300 :: Types.Profile -> Types.Profile
genesisVariant300 = P.pparamsEpoch 300

-- See: data/genesis/epoch-timeline.json
-- All uses of the last epoch include the "v8-preview" overlay.
genesisVariantLast :: Types.Profile -> Types.Profile
genesisVariantLast = P.pparamsEpoch 366 . P.v8Preview

-- Definition vocabulary: funds.
--------------------------------

-- Defined in the "genesis" property and it's for the tx-generator.
fundsDefault :: Types.Profile -> Types.Profile
fundsDefault = P.poolBalance 1000000000000000 . P.funds 10000000000000

-- Definition vocabulary: composition.
--------------------------------------

-- Sets the numbers of hosts and its analysis filter at the same time.
hosts :: Integer -> Types.Profile -> Types.Profile
hosts i =
    P.hosts i
  . P.cBlockMinimumAdoptions (i -1)

composeFiftytwo :: Types.Profile -> Types.Profile
composeFiftytwo = P.torusDense . hosts 52 . P.withExplorerNode

-- Definition vocabulary: workload.
-----------------------------------

-- current_tps_saturation_value
valueLocal :: Types.Profile -> Types.Profile
valueLocal =  P.tps 15    . P.txIn 2 . P.txOut 2 . P.txFee 1000000

-- nomad_perf_tps_saturation_value
valueCloud :: Types.Profile -> Types.Profile
valueCloud =  P.tps 12    . P.txIn 2 . P.txOut 2 . P.txFee 1000000

plutus ::     Types.Profile -> Types.Profile
plutus =      P.tps  0.20 . P.txIn 1 . P.txOut 1
            . P.analysisSizeSmall

-- TODO: Why "ci-bench-plutus-secp-*" changed the TPS ?
plutusV3 ::   Types.Profile -> Types.Profile
plutusV3 =    P.tps  0.48 . P.txIn 1 . P.txOut 1

plutusLoop ::         Types.Profile -> Types.Profile
plutusLoop =          plutus   . P.txFee 1360000
                    . plutusTypeLoop

plutusLoop2024 ::     Types.Profile -> Types.Profile
plutusLoop2024 =      plutus   . P.txFee 1412000     --TODO: fee change ???
                    . plutusTypeLoop2024

plutusECDSA ::        Types.Profile -> Types.Profile
plutusECDSA =         plutus   . P.txFee 1008000
                    . plutusTypeECDSA

-- TODO: Why "ci-bench-plutus-secp-*" changed the TPS ?
plutusECDSAV3 ::      Types.Profile -> Types.Profile
plutusECDSAV3 =       plutusV3 . P.txFee 1008000
                    . plutusTypeECDSA
                    . P.analysisSizeModerate

  -- TODO: TX fee went from 1025000 to 1004000 ????
plutusSchnorr ::      Types.Profile -> Types.Profile
plutusSchnorr =       plutus   . P.txFee 1004000
                    . plutusTypeSchnorr

-- TODO: Why "ci-bench-plutus-secp-*" changed the TPS ?
plutusSchnorrV3 ::    Types.Profile -> Types.Profile
plutusSchnorrV3 =     plutusV3 . P.txFee 1004000
                    . plutusTypeSchnorr
                    . P.analysisSizeModerate

plutusBLSTV3 ::       Types.Profile -> Types.Profile
plutusBLSTV3 =        plutusV3 . P.txFee 935000
                    . plutusTypeBLST
                    . P.analysisSizeModerate2

-- Plutus types ("type", "script" and "redeemer").
--------------------------------------------------

plutusTypeLoop :: Types.Profile -> Types.Profile
plutusTypeLoop =
    P.plutusType "LimitSaturationLoop" . P.plutusScript "Loop"
  . P.redeemerInt 1000000

plutusTypeLoop2024 :: Types.Profile -> Types.Profile
plutusTypeLoop2024 =
    P.plutusType "LimitSaturationLoop" . P.plutusScript "Loop2024"
  . P.redeemerInt 1000000

plutusTypeECDSA :: Types.Profile -> Types.Profile
plutusTypeECDSA =
    P.plutusType "LimitTxPerBlock_8" . P.plutusScript "EcdsaSecp256k1Loop"
  . P.redeemerFields [
      KeyMap.fromList [("int",   Aeson.Number 1000000.0)]
    , KeyMap.fromList [("bytes", Aeson.String "0392d7b94bc6a11c335a043ee1ff326b6eacee6230d3685861cd62bce350a172e0")]
    , KeyMap.fromList [("bytes", Aeson.String "16e0bf1f85594a11e75030981c0b670370b3ad83a43f49ae58a2fd6f6513cde9")]
    , KeyMap.fromList [("bytes", Aeson.String "5fb12954b28be6456feb080cfb8467b6f5677f62eb9ad231de7a575f4b6857512754fb5ef7e0e60e270832e7bb0e2f0dc271012fa9c46c02504aa0e798be6295")]
    ]

plutusTypeSchnorr :: Types.Profile -> Types.Profile
plutusTypeSchnorr =
    P.plutusType "LimitTxPerBlock_8"   . P.plutusScript "SchnorrSecp256k1Loop"
  . P.redeemerFields [
      KeyMap.fromList [("int",   Aeson.Number 1000000.0)]
    , KeyMap.fromList [("bytes", Aeson.String "599de3e582e2a3779208a210dfeae8f330b9af00a47a7fb22e9bb8ef596f301b")]
    , KeyMap.fromList [("bytes", Aeson.String "30303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030")]
    , KeyMap.fromList [("bytes", Aeson.String "5a56da88e6fd8419181dec4d3dd6997bab953d2fc71ab65e23cfc9e7e3d1a310613454a60f6703819a39fdac2a410a094442afd1fc083354443e8d8bb4461a9b")]
    ]

plutusTypeBLST :: Types.Profile -> Types.Profile
plutusTypeBLST =
    P.plutusType "LimitTxPerBlock_8"   . P.plutusScript "HashOntoG2AndAdd"
  . P.redeemerFields [
        KeyMap.fromList [("int",  Aeson.Number 1000000.0)]
      , KeyMap.fromList [("list", Aeson.Array $ Vector.fromList [
          Aeson.Object $ KeyMap.fromList [("bytes", Aeson.String "714805c6")]
        , Aeson.Object $ KeyMap.fromList [("bytes", Aeson.String "c413111e")]
        , Aeson.Object $ KeyMap.fromList [("bytes", Aeson.String "2d7eb870")]
        , Aeson.Object $ KeyMap.fromList [("bytes", Aeson.String "4ecbd6a1")]
        ])
      ]
    ]

{-

When defining profiles try keeping the same order as in the `Profile` type:
- Name and description
- Scenario
- Composition
- Era and genesis
- Node
- Generator
- Tracer
- Cluster
- Analysis

And common modifiers `P.tracerOn  . P.newTracing . P.p2pOff . P.tracerRtview`
next to the name.

-}
--------------------------------------------------------------------------------

-- All the `FixedLoaded`, `timescaleCompressed` and `datasetEmpty` profiles!
profileNoEraEmptyCI :: [Types.Profile]
profileNoEraEmptyCI = map
  (
    P.fixedLoaded
  -- Genesis:
  . timescaleCompressed . datasetEmpty . fundsDefault
  -- TODO: dummy "generator.epochs" ignored in favor of "--shutdown-on".
  --       Create a "time.epochs" or "time.blocks" or similar, IDK!
  -- This applies to all profiles!
  . P.generatorEpochs 3 . P.initCooldown 5
  . P.analysisStandard
  )
  $
  ------------------------------------------------------------------------------
  -- fast: FixedLoaded and "--shutdown-on-block-synced 1" with 2 nodes (52 + explorer for "nomadperf").
  ------------------------------------------------------------------------------
  let fast =   P.empty
             & P.shutdownOnBlock 1
      fastLocal = fast & genesisVariant300  . P.uniCircle  . hosts  2 . P.loopback . clusterDefault -- TODO: "cluster" should be "null" here.
      -- TODO: Inconsistency: "fast-nomadperf*" used 52+Explorer nodes   and "ci-test-nomadperf" 2+Explorer nodes.
      -- TODO: Inconsistency: "fast-nomadperf*" used the last know epoch and "ci-test-nomadperf" epoch 300.
      fastCloud = fast & genesisVariantLast . composeFiftytwo
      fastPerf  = fastCloud & nomadPerf
      fastSsd   = fastCloud & nomadSsd . clusterNomadSsd
  in [
  -- Local.
    (fastLocal & P.name "fast"                 . valueLocal . P.tracerOn  . P.newTracing . P.p2pOff)
  -- TODO: Remove and make `P.p2pOn` the default without adding a "-nop2p" profile.
  , (fastLocal & P.name "fast-p2p"             . valueLocal . P.tracerOn  . P.newTracing . P.p2pOn )
  , (fastLocal & P.name "fast-oldtracing"      . valueLocal . P.tracerOn  . P.oldTracing . P.p2pOff)
  , (fastLocal & P.name "fast-notracer"        . valueLocal . P.tracerOff . P.newTracing . P.p2pOff)
  , (fastLocal & P.name "fast-plutus"          . plutusLoop . P.tracerOn  . P.newTracing . P.p2pOff)
  -- Nomad perf.
  , (fastPerf  & P.name "fast-nomadperf"       . valueLocal . P.tracerOn  . P.newTracing . P.p2pOn )
  , (fastPerf  & P.name "fast-nomadperf-nop2p" . valueLocal . P.tracerOn  . P.newTracing . P.p2pOff)
  -- Nomad perf-ssd.
  , (fastSsd   & P.name "fast-nomadperfssd"    . valueLocal . P.tracerOn  . P.newTracing . P.p2pOn )
  ]
  ++
  ------------------------------------------------------------------------------
  -- ci-test: FixedLoaded and "--shutdown-on-block-synced 3" with 2 nodes (+ explorer for "nomadperf").
  ------------------------------------------------------------------------------
  let ciTest =   P.empty
               & hosts 2
               . genesisVariant300
               . P.shutdownOnBlock 3
      -- TODO: Why are not both using UniCircle ????
      ciTestLocal     = ciTest & P.uniCircle . P.loopback . clusterDefault -- TODO: "cluster" should be "null" here.
      -- TODO: Inconsistency: "ci-test-nomadperf" uses 2+Explorer nodes and "fast-nomadperf*" 52+Explorer nodes.
      -- TODO: Inconsistency: "ci-test-nomadperf" uses epoch 300        and "fast-nomadperf*" the last know epoch.
      ciTestNomadPerf = ciTest & P.torus     . nomadPerf . P.withExplorerNode
  in [
  -- Local
    (ciTestLocal     & P.name "ci-test"                      . valueLocal . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciTestLocal     & P.name "ci-test-rtview"               . valueLocal . P.tracerOn  . P.newTracing . P.p2pOff . P.tracerRtview)
  , (ciTestLocal     & P.name "ci-test-notracer"             . valueLocal . P.tracerOff . P.newTracing . P.p2pOff                 )
  -- TODO: Remove and make `P.p2pOn` the default without adding a "-nop2p" profile.
  , (ciTestLocal     & P.name "ci-test-p2p"                  . valueLocal . P.tracerOn  . P.newTracing . P.p2pOn                  )
  , (ciTestLocal     & P.name "ci-test-plutus"               . plutusLoop . P.tracerOn  . P.newTracing . P.p2pOff                 )
  -- Nomad perf
  , (ciTestNomadPerf & P.name "ci-test-nomadperf"            . valueLocal . P.tracerOn  . P.newTracing . P.p2pOn                  )
  , (ciTestNomadPerf & P.name "ci-test-nomadperf-nop2p"      . valueLocal . P.tracerOn  . P.newTracing . P.p2pOff                 )
  -- TODO: FIXME: A non "nop2p" "nomadperf" profile without P2P???
  , (ciTestNomadPerf & P.name "ci-test-oldtracing-nomadperf" . valueLocal . P.tracerOn  . P.oldTracing . P.p2pOff                 )
  ]
  ++
  ------------------------------------------------------------------------------
  -- trace-*: FixedLoaded and "tracer.withresources = true" with 6 nodes.
  ------------------------------------------------------------------------------
  let trace = P.empty
            & P.torus . hosts 6 . P.loopback
            . genesisVariant300
            . P.tracerWithresources
            . clusterDefault -- TODO: "cluster" should be "null" here.
      traceBench = trace & P.shutdownOnBlock   15
      traceFull  = trace & P.shutdownOnSlot  1200
  in [
  -- "--shutdown-on-block-synced 15"
    (traceBench & P.name "trace-bench"            . valueLocal . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (traceBench & P.name "trace-bench-rtview"     . valueLocal . P.tracerOn  . P.newTracing . P.p2pOff . P.tracerRtview)
  , (traceBench & P.name "trace-bench-oldtracing" . valueLocal . P.tracerOn  . P.oldTracing . P.p2pOff                 )
  , (traceBench & P.name "trace-bench-notracer"   . valueLocal . P.tracerOff . P.newTracing . P.p2pOff                 )
  -- "--shutdown-on-slot-synced 1200"
  , (traceFull  & P.name "trace-full"             . valueLocal . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (traceFull  & P.name "trace-full-rtview"      . valueLocal . P.tracerOn  . P.newTracing . P.p2pOff . P.tracerRtview)
  ]
  ++
  ------------------------------------------------------------------------------
  -- epoch transition: FixedLoaded and "--shutdown-on-slot-synced 900" with 2 nodes.
  ------------------------------------------------------------------------------
  let epochTransition =   P.empty
                        & P.uniCircle . hosts 2 . P.loopback
                        . P.pparamsEpoch 300
                        . P.shutdownOnSlot 900
                        . clusterDefault -- TODO: "cluster" should be "null" here.
  in [
    (epochTransition & P.name "epoch-transition" . valueLocal . P.tracerOn . P.newTracing . P.p2pOff)
  ]
  ++
  ------------------------------------------------------------------------------
  -- ci-test-dense: FixedLoaded and "--shutdown-on-block-synced 3" with 10 pools.
  ------------------------------------------------------------------------------
  let ciTestDense =   P.empty
                    & P.uniCircle . P.pools 10 . P.loopback
                    . genesisVariant300
                    . P.shutdownOnBlock 3
                    . clusterDefault -- TODO: "cluster" should be "null" here.
                    . P.cBlockMinimumAdoptions 9
  in [
    (ciTestDense & P.name "ci-test-dense10" . valueLocal . P.tracerOn  . P.newTracing . P.p2pOff)
  ]

--------------------------------------------------------------------------------

-- All the non-`FixedLoaded` with an almost `datasetEmpty` profiles!
profileNoEraEmptyOthers :: [Types.Profile]
profileNoEraEmptyOthers = map
  (
    -- TODO: "tracer-only" and "idle" have `P.delegators 6`. Needed ???
    P.utxo 0 . P.dreps 0
  . fundsDefault
  . valueCloud -- TODO: Why TPS=12 ? make for all these generator=null!
  -- TODO: dummy "generator.epochs" ignored in favor of "--shutdown-on".
  --       Create a "time.epochs" or "time.blocks" or similar, IDK!
  -- This applies to all profiles!
  . P.generatorEpochs 3 . P.initCooldown 5
  )
  $
  ------------------------------------------------------------------------------
  -- "idle" scenario.
  ------------------------------------------------------------------------------
  let idle =   P.empty
             & P.idle
             . P.uniCircle . hosts 6 . P.loopback
             . genesisVariant300
             . P.delegators 6
             . P.shutdownOnOff
             . P.analysisUnitary
             . clusterDefault -- TODO: "cluster" should be "null" here.
      updateQuorum = P.shelley (KeyMap.insert "updateQuorum" (Aeson.Number 1))
      timeScaleDevops = P.slotDuration 0.2 . P.epochLength 1000 . P.activeSlotsCoeff 0.1  . P.parameterK 10
  in [
    (idle & P.name "devops" . timeScaleDevops     . P.extraFutureOffset 10 . updateQuorum . P.tracerOn  . P.newTracing . P.p2pOff . P.analysisOff     )
  , (idle & P.name "idle"   . timescaleCompressed . P.extraFutureOffset  0                . P.tracerOn  . P.newTracing . P.p2pOff . P.analysisStandard)
  ]
  ++
  ------------------------------------------------------------------------------
  -- "tracer-only" scenario.
  ------------------------------------------------------------------------------
  let tracerOnly =   P.empty
                   & P.tracerOnly
                   . P.uniCircle . hosts 6 . P.loopback
                   . genesisVariant300
                   . timescaleCompressed
                   . P.delegators 6
                   . P.shutdownOnOff
                   . P.analysisStandard . P.analysisUnitary
                   . clusterDefault -- TODO: "cluster" should be "null" here.
  in [
    (tracerOnly & P.name "tracer-only" . P.tracerOn  . P.newTracing . P.p2pOff)
  ]
  ++
  ------------------------------------------------------------------------------
  -- "latency" scenario.
  ------------------------------------------------------------------------------
  let latency =   P.empty
                & P.latency
                . composeFiftytwo
                . genesisVariantLast
                . timescaleCompressed
                . P.delegators 0
                . P.shutdownOnOff
                . P.analysisStandard
  in [
    (latency & P.name "latency-nomadperf"    . P.tracerOn . P.newTracing . P.p2pOn . nomadPerf                 )
  , (latency & P.name "latency-nomadperfssd" . P.tracerOn . P.newTracing . P.p2pOn . nomadSsd . clusterNomadSsd)
  ]
  ++
  ------------------------------------------------------------------------------
  -- "chainsync" scenario.
  ------------------------------------------------------------------------------
  let chainsync =   P.empty
                  & P.chainsync
                  . P.uniCircle . P.withExplorerNode . P.loopback
                  . P.hostsChainsync 1 . P.withChaindbServer
                  . genesisVariant300
                  . P.slotDuration 1 . P.epochLength 432000 . P.activeSlotsCoeff 0.05 . P.parameterK 2160
                  . P.delegators 0
                  . P.analysisPerformance
                  . clusterDefault -- TODO: "cluster" should be "null" here.
                  . P.preset "mainnet"
      {-- TODO:
      chainsync-early-byron-bage
        - three map entries removed:
          chaindb:
          │ ledger_snapshot:
          │ │ chaindb_server: 237599
          │ │ explorer: 0
          │ mainnet_chunks:
          │ │ chaindb_server: 10
          │ │ explorer: 0
          extra_desc: "without cardano-tracer"
          suffix: notrc
      --}
      byron  = chainsync & P.shutdownOnSlot   237599
      {-- TODO:
      chainsync-early-alonzo-bage
        - three map entries removed:
          chaindb:
          │ ledger_snapshot:
          │ │ chaindb_server: 3.8901589e+07
          │ │ explorer: 3.717365e+07
          │ mainnet_chunks:
          │ │ chaindb_server: 1800
          │ │ explorer: 1799
          extra_desc: "without cardano-tracer"
          suffix: notrc
      --}
      alonzo = chainsync & P.shutdownOnSlot 38901589
  in [
  -- Byron
    (byron & P.name "chainsync-early-byron"               . P.tracerOff . P.newTracing . P.p2pOff)
  , (byron & P.name "chainsync-early-byron-notracer"      . P.tracerOff . P.newTracing . P.p2pOff)
  , (byron & P.name "chainsync-early-byron-oldtracing"    . P.tracerOff . P.oldTracing . P.p2pOff)
  -- Alonzo
  , (alonzo  & P.name "chainsync-early-alonzo"            . P.tracerOff . P.newTracing . P.p2pOff)
  , (alonzo  & P.name "chainsync-early-alonzo-notracer"   . P.tracerOff . P.newTracing . P.p2pOff)
  -- TODO: Remove and make `P.p2pOn` the default without adding a "-nop2p" profile.
  , (alonzo  & P.name "chainsync-early-alonzo-p2p"        . P.tracerOff . P.newTracing . P.p2pOn )
  , (alonzo  & P.name "chainsync-early-alonzo-oldtracing" . P.tracerOff . P.oldTracing . P.p2pOff)
  ]

--------------------------------------------------------------------------------

-- "forge-stress-*" family of profiles!
profilesNoEraForgeStress :: [Types.Profile]
profilesNoEraForgeStress =
  ------------------------------------------------------------------------------
  -- forge-stress
  ------------------------------------------------------------------------------
  let forgeStress =   P.empty
                    & P.fixedLoaded
                    . P.uniCircle . P.loopback
                    . timescaleCompressed . fundsDefault
                    . P.p2pOff . P.newTracing
                    . P.generatorEpochs 3  . P.initCooldown 5
                    . P.analysisStandard
                    . clusterDefault -- TODO: "cluster" should be "null" here.
      -- Helpers by size:
      -- TODO: Why `forgeStress1` is the only "forge-stress-*" not using epoch 300 ???
      forgeStress1       = genesisVariantLast . hosts 1
      forgeStress3       = genesisVariant300  . hosts 3
      forgeStress6       = genesisVariant300  . hosts 6
      -- Helpers by dataset:
      forgeStressPre     = P.utxo  4000000 . P.delegators 1000000
      forgeStressNonPre  = P.utxo 10000000 . P.delegators 1300000
      -- Helpers by run time:
      forgeStressXS      = P.shutdownOnSlot 1200
      forgeStressM       = P.shutdownOnSlot 2400
      forgeStressXL      = P.shutdownOnSlot 4800
  in [
  -- 1 node versions (non-pre).
    (forgeStress & P.name "forge-stress-solo-xs"       . valueLocal . forgeStress1 . forgeStressNonPre . forgeStressXS . P.tracerOn                                         . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-solo"          . valueLocal . forgeStress1 . forgeStressNonPre . forgeStressM  . P.tracerOn                                         . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-plutus-solo"   . plutusLoop . forgeStress1 . forgeStressNonPre . forgeStressM  . P.tracerOn                                                               )
  -- 1 node versions (pre).
  , (forgeStress & P.name "forge-stress-pre-solo-xs"   . valueLocal . forgeStress1 . forgeStressPre    . forgeStressXS . P.tracerOn                                         . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-solo"      . valueLocal . forgeStress1 . forgeStressPre    . forgeStressM  . P.tracerOn                                         . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-solo-xl"   . valueLocal . forgeStress1 . forgeStressPre    . forgeStressXL . P.tracerOn                                         . P.analysisEpoch3Plus)
  -- 3 nodes versions (non-pre).
  , (forgeStress & P.name "forge-stress"               . valueLocal . forgeStress3 . forgeStressNonPre . forgeStressM  . P.tracerOn                                         . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-notracer"      . valueLocal . forgeStress3 . forgeStressNonPre . forgeStressM  . P.tracerOff                                        . P.analysisUnitary   )
  -- TODO: FIXME: "forge-stress-p2p" has no P2P enabled and Plutus TPS!!!!
  , (forgeStress & P.name "forge-stress-p2p"           . plutusLoop . forgeStress3 . forgeStressNonPre . forgeStressM  . P.tracerOn                                                               )
  , (forgeStress & P.name "forge-stress-plutus"        . plutusLoop . forgeStress3 . forgeStressNonPre . forgeStressM  . P.tracerOn                                                               )
  -- 3 nodes versions (pre).
  , (forgeStress & P.name "forge-stress-pre"           . valueLocal . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOn                                         . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-rtsA4m"    . valueLocal . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOn                   . P.rtsGcAllocSize  4 . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-rtsA64m"   . valueLocal . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOn                   . P.rtsGcAllocSize 64 . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-rtsN3"     . valueLocal . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOn  . P.rtsThreads 3                       . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-rtsA4mN3"  . valueLocal . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOn  . P.rtsThreads 3 . P.rtsGcAllocSize  4 . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-rtsA64mN3" . valueLocal . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOn  . P.rtsThreads 3 . P.rtsGcAllocSize 64 . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-rtsxn"     . valueLocal . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOn                   . P.rtsGcNonMoving    . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-notracer"  . valueLocal . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOff                                        . P.analysisUnitary   )
  , (forgeStress & P.name "forge-stress-pre-plutus"    . plutusLoop . forgeStress3 . forgeStressPre    . forgeStressM  . P.tracerOn                                                               )
  -- Double nodes and double run time version.
  , (forgeStress & P.name "forge-stress-large"         . valueLocal . forgeStress6 . forgeStressNonPre . forgeStressXL . P.tracerOn                                         . P.analysisEpoch3Plus)
  ]

--------------------------------------------------------------------------------

profilesNoEraTheRest :: [Types.Profile]
profilesNoEraTheRest =
  ------------------------------------------------------------------------------
  -- ci-bench: 2|10 nodes, FixedLoaded and "--shutdown-on-block-synced 15"
  ------------------------------------------------------------------------------
  let ciBench =  P.empty
               & P.fixedLoaded
               . timescaleCompressed . fundsDefault
               . P.shutdownOnBlock 15
               -- TODO: dummy "generator.epochs" ignored in favor of "--shutdown-on".
               . P.generatorEpochs 3 . P.initCooldown 5
               . P.analysisStandard
      -- Helpers by size:
      ciBench02  = ciBench & hosts  2 . P.utxo 500000 . P.delegators 100000
      ciBench10  = ciBench & hosts 10 . P.utxo 500000 . P.delegators 100000
      -- Helpers by workload:
      ciBench02Local       = ciBench02 & P.uniCircle . P.loopback . clusterDefault -- TODO: "cluster" should be "null" here.
      -- Helpers by workload:
      ciBench02LocalValue  = ciBench02Local & genesisVariant300
      ciBench02LocalPlutus = ciBench02Local & genesisVariantLast
      ciBench02NomadPerf   = ciBench02      & genesisVariant300  . P.torus     . nomadPerf  . P.withExplorerNode
      ciBench10Local       = ciBench10      & genesisVariant300  . P.uniCircle . P.loopback . clusterDefault -- TODO: "cluster" should be "null" here.
  in [
  -- 2 nodes, local
    (ciBench02LocalValue  & P.name "ci-bench"                      . valueLocal      . P.dreps  0 . P.tracerOn  . P.newTracing . P.p2pOff                                 )
  , (ciBench02LocalValue  & P.name "ci-bench-lmdb"                 . valueLocal      . P.dreps  0 . P.tracerOn  . P.newTracing . P.p2pOn  . P.lmdb . P.ssdDirectory "/tmp")
  , (ciBench02LocalValue  & P.name "ci-bench-rtview"               . valueLocal      . P.dreps  0 . P.tracerOn  . P.newTracing . P.p2pOff . P.tracerRtview                )
  , (ciBench02LocalValue  & P.name "ci-bench-p2p"                  . valueLocal      . P.dreps  0 . P.tracerOn  . P.newTracing . P.p2pOn                                  )
  , (ciBench02LocalValue  & P.name "ci-bench-notracer"             . valueLocal      . P.dreps  0 . P.tracerOff . P.newTracing . P.p2pOff                                 )
  , (ciBench02LocalValue  & P.name "ci-bench-drep"                 . valueLocal      . P.dreps 10 . P.tracerOn  . P.newTracing . P.p2pOff                                 )
  , (ciBench02LocalPlutus & P.name "ci-bench-plutus"               . plutusLoop      . P.dreps  0 . P.tracerOn  . P.newTracing . P.p2pOff                                 )
  , (ciBench02LocalPlutus & P.name "ci-bench-plutus24"             . plutusLoop2024  . P.dreps  0 . P.tracerOn  . P.newTracing . P.p2pOff                                 )
  , (ciBench02LocalPlutus & P.name "ci-bench-plutus-secp-ecdsa"    . plutusECDSAV3   . P.dreps  0 . P.tracerOn  . P.newTracing . P.p2pOff                                 )
  , (ciBench02LocalPlutus & P.name "ci-bench-plutus-secp-schnorr"  . plutusSchnorrV3 . P.dreps  0 . P.tracerOn  . P.newTracing . P.p2pOff                                 )
  , (ciBench02LocalPlutus & P.name "ci-bench-plutusv3-blst"        . plutusBLSTV3    . P.dreps  0 . P.tracerOn  . P.newTracing . P.p2pOff . P.v9Preview                   )
  -- 2 nodes, Nomad perf
  , (ciBench02NomadPerf   & P.name "ci-bench-nomadperf"            . valueLocal      . P.dreps  0 . P.tracerOn  . P.newTracing . P.p2pOn                                  )
  , (ciBench02NomadPerf   & P.name "ci-bench-nomadperf-nop2p"      . valueLocal      . P.dreps  0 . P.tracerOn  . P.newTracing . P.p2pOff                                 )
  -- TODO: FIXME: A non "nop2p" "nomadperf" profile without P2P???
  , (ciBench02NomadPerf   & P.name "ci-bench-oldtracing-nomadperf" . valueLocal      . P.dreps  0 . P.tracerOn  . P.oldTracing . P.p2pOff                                 )
  -- 10 nodes, local
  , (ciBench10Local       & P.name "10"                            . valueLocal      . P.dreps  0 . P.tracerOn  . P.newTracing . P.p2pOff                                 )
  , (ciBench10Local       & P.name "10-p2p"                        . valueLocal      . P.dreps  0 . P.tracerOn  . P.newTracing . P.p2pOn                                  )
  , (ciBench10Local       & P.name "10-notracer"                   . valueLocal      . P.dreps  0 . P.tracerOff . P.newTracing . P.p2pOff                                 )
  , (ciBench10Local       & P.name "10-plutus"                     . plutusLoop      . P.dreps  0 . P.tracerOn  . P.newTracing . P.p2pOff                                 )
  ]
  ++
  ------------------------------------------------------------------------------
  -- TODO: This is a special case of forge-stress. Mix both? Still used?
  ------------------------------------------------------------------------------
  let dish =   P.empty
             & P.fixedLoaded
             . P.uniCircle . hosts 3 . P.loopback
             . genesisVariant300
             . timescaleCompressed
             . P.delegators 0 . P.dreps 0
             . fundsDefault
             . P.shutdownOnSlot 2400
             . P.generatorEpochs 3  . P.initCooldown 5
             . P.p2pOff
             . P.tracerOn  . P.newTracing
             . P.analysisStandard
             . clusterDefault -- TODO: "cluster" should be "null" here.
      dish30M = dish & P.utxo 30000000
      dish10M = dish & P.utxo 10000000
  in [
    (dish30M & P.name "dish"            . P.analysisUnitary   . P.tps 138.88888888888889 . P.txIn 2 . P.txOut 2 . P.txFee 1000000)
  , (dish30M & P.name "dish-plutus"     . plutusLoop)
  , (dish10M & P.name "dish-10M"        . P.analysisUnitary   . P.tps 138.88888888888889 . P.txIn 2 . P.txOut 2 . P.txFee 1000000)
  , (dish10M & P.name "dish-10M-plutus" . plutusLoop)
  ]
  ++
  ------------------------------------------------------------------------------
  -- k3: 3 nodes and no "--shutdown-on-slot-synced" and no "--shutdown-on-block-synced"
  ------------------------------------------------------------------------------
  let k3 =   P.empty
           & P.fixedLoaded
           . P.uniCircle . hosts 3 . P.loopback
           . genesisVariant300
           . P.epochLength 600 . P.activeSlotsCoeff 0.05 . P.parameterK 3
           . P.utxo 10000000 . P.delegators 1300000 . P.dreps 0
           . fundsDefault
           . P.txIn 2 . P.txOut 2 . P.txFee 1000000
           . P.shutdownOnOff . P.generatorEpochs 3  . P.initCooldown 5
           . P.p2pOff
           . P.tracerOn  . P.newTracing
           . P.analysisStandard . P.analysisUnitary
           . clusterDefault -- TODO: "cluster" should be "null" here.
  in [
    (k3 & P.name "k3-3ep-5kTx-10000kU-1300kD-64kbs-fixed-loaded"        . P.slotDuration 0.2 . P.tps 12)
  , (k3 & P.name "k3-3ep-9kTx-10000kU-1300kD-64kbs-5tps-fixed-loaded"   . P.slotDuration 1   . P.tps  5)
  , (k3 & P.name "k3-3ep-18kTx-10000kU-1300kD-64kbs-10tps-fixed-loaded" . P.slotDuration 1   . P.tps 10)
  , (k3 & P.name "k3-3ep-22kTx-10000kU-1300kD-64kbs-fixed-loaded"       . P.slotDuration 1   . P.tps 12)
  ]
  ++
  ------------------------------------------------------------------------------
  -- 6 nodes and no "--shutdown-on-slot-synced" and no "--shutdown-on-block-synced"
  ------------------------------------------------------------------------------
  let noCliStop =   P.empty
                  & P.fixedLoaded
                  . hosts 6
                  . timescaleCompressed
                  . P.utxo 0 . P.delegators 6 . P.dreps 0
                  . fundsDefault
                  . P.shutdownOnOff . P.generatorEpochs 3  . P.initCooldown 5
                  . P.analysisStandard
      -- TODO: Why are not all using Torus ????
      -- TODO: Why all using the cloud saturation value ???
      noCliStopLocal     = noCliStop      & P.uniCircle . P.loopback . clusterDefault -- TODO: "cluster" should be "null" here.
      noCliStopLocal120  = noCliStopLocal & genesisVariant300
      noCliStopLocal002  = noCliStopLocal & genesisVariantLast
      noCliStopNomadPerf = noCliStop      & genesisVariant300  . P.torus . nomadPerf  . P.withExplorerNode
  in [
  -- TODO: TX fee went from 1025000 to 1008000 ????
    (noCliStopLocal120  & P.name "default"                    . valueCloud    . P.tracerOn  . P.newTracing . P.p2pOff . P.analysisUnitary)
  , (noCliStopNomadPerf & P.name "default-nomadperf-nop2p"    . valueCloud    . P.tracerOn  . P.newTracing . P.p2pOff . P.analysisUnitary)
  , (noCliStopLocal120  & P.name "default-p2p"                . valueCloud    . P.tracerOn  . P.newTracing . P.p2pOn  . P.analysisUnitary)
  , (noCliStopNomadPerf & P.name "default-nomadperf"          . valueCloud    . P.tracerOn  . P.newTracing . P.p2pOn  . P.analysisUnitary)
  , (noCliStopLocal120  & P.name "oldtracing"                 . valueCloud    . P.tracerOn  . P.oldTracing . P.p2pOff . P.analysisUnitary)
  , (noCliStopNomadPerf & P.name "oldtracing-nomadperf"       . valueCloud    . P.tracerOn  . P.oldTracing . P.p2pOn  . P.analysisUnitary)
  , (noCliStopNomadPerf & P.name "oldtracing-nomadperf-nop2p" . valueCloud    . P.tracerOn  . P.oldTracing . P.p2pOff . P.analysisUnitary)
  , (noCliStopLocal002  & P.name "plutus"                     . plutusLoop    . P.tracerOn  . P.newTracing . P.p2pOff                    )
  , (noCliStopLocal002  & P.name "plutus-secp-ecdsa"          . plutusECDSA   . P.tracerOn  . P.newTracing . P.p2pOff                    )
  -- TODO: Only "ci-bench-plutus-secp-schnorr" changed to `plutusSchnorrV3` ????
  , (noCliStopLocal002  & P.name "plutus-secp-schnorr"        . plutusSchnorr . P.tracerOn  . P.newTracing . P.p2pOff                    )
  ]
  ++
  ------------------------------------------------------------------------------
  -- model: 4 nodes, FixedLoaded and "--shutdown-on-slot-synced 56000"
  ------------------------------------------------------------------------------
  let model =    P.empty
              & P.fixedLoaded
              . P.uniCircle . hosts 4 . P.loopback
              . timescaleModel
              . P.dreps 0
              . P.poolBalance 1000000000000000 . P.funds 20000000000000
              . P.generatorEpochs 7  . P.initCooldown 45
              . P.shutdownOnSlot 56000
              . P.tracerOn . P.newTracing
              . P.analysisStandard . P.analysisEpoch3Plus
              . clusterDefault -- TODO: "cluster" should be "null" here.
  in [
    -- TODO: Different pparamsEpoch introduced in last changes!
    -- TODO: TX fee went from 1025000 to 1008000 ????
    -- TODO: Same as `plutusECDSAV3` but tps is 0.4 instead of 0.48
    (model & P.name "model-secp-ecdsa-double" . plutusTypeECDSA . genesisVariant300  . P.doubleBudget . P.v8Preview . P.utxo 10000000 . P.delegators 1300000 . P.tps 0.4 . P.txIn 1 . P.txOut 1 . P.txFee 1008000 . P.analysisSizeModerate)
  , (model & P.name "model-secp-ecdsa-half"   . plutusTypeECDSA . genesisVariant300  . P.stepHalf     . P.v8Preview . P.utxo 10000000 . P.delegators 1300000 . P.tps 0.4 . P.txIn 1 . P.txOut 1 . P.txFee 1008000 . P.analysisSizeModerate)
  , (model & P.name "model-secp-ecdsa-plain"  . plutusTypeECDSA . genesisVariantLast                                . P.utxo 10000000 . P.delegators 1300000 . P.tps 0.4 . P.txIn 1 . P.txOut 1 . P.txFee 1008000 . P.analysisSizeModerate)
  , (model & P.name "model-value"                               . genesisVariantLast                                . P.utxo 10000000 . P.delegators 1300000 . P.tps 9   . P.txIn 2 . P.txOut 2 . P.txFee 1000000 . P.analysisSizeFull    )
  , (model & P.name "model-value-test"                          . genesisVariantLast                                . P.utxo  1000000 . P.delegators  200000 . P.tps 9   . P.txIn 2 . P.txOut 2 . P.txFee 1000000 . P.analysisSizeFull    )
  ]
  ++
  ------------------------------------------------------------------------------
  -- plutuscall: 6 nodes, FixedLoaded and "--shutdown-on-slot-synced 9000"
  ------------------------------------------------------------------------------
  let plutusCall =   P.empty
                   & P.fixedLoaded
                   . P.uniCircle . hosts 6 . P.loopback
                   . timescaleCompressed
                   . P.utxo 1000000 . P.delegators 200000 . P.dreps 0
                   . fundsDefault
                   -- TODO: Same as `plutusECDSAV3` but tps is 0.4 instead of 0.48
                   . P.tps 0.4 . P.txIn 1 . P.txOut 1
                   . P.generatorEpochs 15  . P.initCooldown 5
                   . P.shutdownOnSlot 9000
                   . P.p2pOff
                   . P.tracerOn . P.newTracing
                   . P.analysisStandard . P.analysisSizeModerate . P.analysisEpoch3Plus
                   . clusterDefault -- TODO: "cluster" should be "null" here.
      loop    = plutusCall & plutusTypeLoop
      ecdsa   = plutusCall & plutusTypeECDSA
      schnorr = plutusCall & plutusTypeSchnorr
  in [
  -- TODO: Different pparamsEpoch introduced in last changes!
    (loop    & P.name "plutuscall-loop-plain"          . genesisVariantLast                                . P.txFee 1360000)
  , (loop    & P.name "plutuscall-loop-half"           . genesisVariant300  . P.stepHalf     . P.v8Preview . P.txFee 1360000)
  , (loop    & P.name "plutuscall-loop-double"         . genesisVariant300  . P.doubleBudget . P.v8Preview . P.txFee 1360000)
  , (ecdsa   & P.name "plutuscall-secp-ecdsa-plain"    . genesisVariantLast                                . P.txFee 1008000)
  , (ecdsa   & P.name "plutuscall-secp-ecdsa-half"     . genesisVariant300  . P.stepHalf     . P.v8Preview . P.txFee 1008000)
  , (ecdsa   & P.name "plutuscall-secp-ecdsa-double"   . genesisVariant300  . P.doubleBudget . P.v8Preview . P.txFee 1008000)
  , (schnorr & P.name "plutuscall-secp-schnorr-plain"  . genesisVariantLast                                . P.txFee 1004000)
  , (schnorr & P.name "plutuscall-secp-schnorr-half"   . genesisVariant300  . P.stepHalf     . P.v8Preview . P.txFee 1004000)
  , (schnorr & P.name "plutuscall-secp-schnorr-double" . genesisVariant300  . P.doubleBudget . P.v8Preview . P.txFee 1004000)
  ]
  ++
  ------------------------------------------------------------------------------
  -- cloud: (52 + 1) nodes, FixedLoaded "value" and "plutus" workloads.
  ------------------------------------------------------------------------------
  let cloud =   P.empty
              & P.fixedLoaded
              . composeFiftytwo . nomadPerf
              . genesisVariantLast
              . timescaleModel
              . P.utxo 4000000 . P.delegators 1000000
              . P.poolBalance 1000000000000000 . P.funds 20000000000000
              . P.tracerOn
              . P.analysisStandard . P.analysisEpoch3Plus
      valueNP          = cloud    & P.shutdownOnSlot 64000 . P.generatorEpochs 8 . P.initCooldown 45 . valueCloud . P.analysisSizeFull
      plutusNP         = cloud    & P.shutdownOnSlot 72000 . P.generatorEpochs 9 . P.initCooldown 45 . P.txIn 1 . P.txOut 1
      plutusNPLoop     = plutusNP & P.tps  0.85 . P.txFee 1360000 . plutusTypeLoop     . P.analysisSizeSmall
      plutusNPLoop2024 = plutusNP & P.tps  0.85 . P.txFee 1412000 . plutusTypeLoop2024 . P.analysisSizeSmall
      plutusNPECDSA    = plutusNP & P.tps  2    . P.txFee 1008000 . plutusTypeECDSA    . P.analysisSizeModerate
      plutusNPSchnorr  = plutusNP & P.tps  2    . P.txFee 1004000 . plutusTypeSchnorr  . P.analysisSizeModerate
      plutusNPBLST     = plutusNP & P.tps  2    . P.txFee  935000 . plutusTypeBLST     . P.analysisSizeModerate2 . P.v9Preview
  in [
  -- Value
    (valueNP          & P.name "value-nomadperf"                                 . P.dreps      0 . P.newTracing . P.p2pOn  )
  , (valueNP          & P.name "value-nomadperfssd" . clusterNomadSsd            . P.dreps      0 . P.newTracing . P.p2pOn  )
  , (valueNP          & P.name "value-nomadperf-nop2p"                           . P.dreps      0 . P.newTracing . P.p2pOff )
  , (valueNP          & P.name "value-drep1k-nomadperf"                          . P.dreps   1000 . P.newTracing . P.p2pOn  )
  , (valueNP          & P.name "value-drep2k-nomadperf"                          . P.dreps   2000 . P.newTracing . P.p2pOn  )
  , (valueNP          & P.name "value-drep10k-nomadperf"                         . P.dreps  10000 . P.newTracing . P.p2pOn  )
  , (valueNP          & P.name "value-drep100k-nomadperf"                        . P.dreps 100000 . P.newTracing . P.p2pOn  )
  , (valueNP          & P.name "value-oldtracing-nomadperf"                      . P.dreps      0 . P.oldTracing . P.p2pOn  )
  , (valueNP          & P.name "value-oldtracing-nomadperf-nop2p"                . P.dreps      0 . P.oldTracing . P.p2pOff )
  -- Plutus
  , (plutusNPLoop     & P.name "plutus-nomadperf"                                . P.dreps      0 . P.newTracing . P.p2pOn  )
  , (plutusNPLoop     & P.name "plutus-nomadperf-nop2p"                          . P.dreps      0 . P.newTracing . P.p2pOff )
  , (plutusNPLoop     & P.name "plutus-drep1k-nomadperf"                         . P.dreps   1000 . P.newTracing . P.p2pOn  )
  , (plutusNPLoop     & P.name "plutus-drep2k-nomadperf"                         . P.dreps   2000 . P.newTracing . P.p2pOn  )
  , (plutusNPLoop     & P.name "plutus-drep10k-nomadperf"                        . P.dreps  10000 . P.newTracing . P.p2pOn  )
  , (plutusNPLoop     & P.name "plutus-drep100k-nomadperf"                       . P.dreps 100000 . P.newTracing . P.p2pOn  )
  , (plutusNPLoop2024 & P.name "plutus24-nomadperf"                              . P.dreps      0 . P.newTracing . P.p2pOn  )
  , (plutusNPECDSA    & P.name "plutus-secp-ecdsa-nomadperf"                     . P.dreps      0 . P.newTracing . P.p2pOn  )
  , (plutusNPSchnorr  & P.name "plutus-secp-schnorr-nomadperf"                   . P.dreps      0 . P.newTracing . P.p2pOn  )
  , (plutusNPBLST     & P.name "plutusv3-blst-nomadperf"                         . P.dreps      0 . P.newTracing . P.p2pOn  )
  , (plutusNPBLST     & P.name "plutusv3-blst-double-nomadperf" . P.doubleBudget . P.dreps      0 . P.newTracing . P.p2pOn  )
  , (plutusNPBLST     & P.name "plutusv3-blst-half-nomadperf"   . P.stepHalf     . P.dreps      0 . P.newTracing . P.p2pOn  )
  ]
  ++
  ------------------------------------------------------------------------------
  -- UTxO scale
  ------------------------------------------------------------------------------
  let utxoScale =   P.empty
                  & P.fixedLoaded
                  . P.uniCircle . hosts 1
                  . P.slotDuration 1 . P.activeSlotsCoeff 0.05
                  . P.dreps 0
                  . P.initCooldown 5
                  . P.tracerOn . P.newTracing
                  . P.analysisStandard
      euCentral1 =   P.regions [Types.AWS Types.EU_CENTRAL_1] . P.shutdownOnSlot 7200 . P.p2pOn
                   . P.generatorEpochs 6 . valueCloud
                   . clusterNomadSsd
                   . P.analysisSizeFull . P.analysisEpoch3Plus
      fast       =   P.loopback
                   . P.shutdownOnBlock 1 . P.generatorEpochs 3 . valueLocal . clusterDefault -- TODO: "cluster" should be "null" here.
  in [
    (utxoScale & P.name "utxoscale-solo-12M16G-nomadperfssd" . euCentral1 . P.epochLength 1200 . P.parameterK 6 . P.utxo 12000000 . P.delegators 1200000 . P.poolBalance 1000000000000000 . P.funds 20000000000000 . genesisVariantLast . P.rtsHeapLimit 16384 . P.heapLimit 16384)
  , (utxoScale & P.name "utxoscale-solo-12M64G-nomadperfssd" . euCentral1 . P.epochLength 1200 . P.parameterK 6 . P.utxo 12000000 . P.delegators 1200000 . P.poolBalance 1000000000000000 . P.funds 20000000000000 . genesisVariantLast                                           )
  , (utxoScale & P.name "utxoscale-solo-24M64G-nomadperfssd" . euCentral1 . P.epochLength 1200 . P.parameterK 6 . P.utxo 24000000 . P.delegators 1200000 . P.poolBalance 1000000000000000 . P.funds 20000000000000 . genesisVariantLast                                           )
  , (utxoScale & P.name "faststartup-24M"                    . fast       . P.epochLength  600 . P.parameterK 3 . P.utxo 24000000 . P.delegators 1200000 . P.poolBalance 1000000000000000 . P.funds 10000000000000 . genesisVariant300                                            )
  ]

--------------------------------------------------------------------------------

profiles :: Map.Map String Types.Profile
profiles = foldMap
  (\profile -> Map.fromList $
    let
        -- First fill the genesis' "shelley", "alonzo" and "conway" properties
        -- using the provided epoch number and overlay names.
        profile' = P.shelleyAlonzoConway
          (Types.pparamsEpoch $ Types.genesis profile)
          profile
        -- Second fill the "derive" property.
        -- "derive" needs above "shelley", "alonzo" and "conway" properties.
        profile'' = P.derive profile'
        -- Things not in "derived" and also don't make sense to be a default.
        profile''' =
          (\p -> p {
              -- Genesis "fixes".
              Types.genesis =
                let genesis = Types.genesis p
                in genesis {
                       -- TODO: Remove or move to derive ?
                       Types.pool_coin =
                         if Types.n_pools (Types.composition p) == 0
                         then 0
                         else Types.per_pool_balance genesis
                       -- TODO: Remove or move to derive ?
                     , Types.delegator_coin =
                         if Types.delegators genesis == (Just 0)
                         then 0
                         else Types.per_pool_balance genesis
                     }
            -- Generator "fixes".
            , Types.generator = (Types.generator p) {
                -- TODO: Remove or move to derive ?
                Types.tx_count = Just $ Types.generator_tx_count $ Types.derived p
            }
          })
          profile''
        -- The "cli_args" property need the "derived" property and above fixes.
        profile'''' = P.cliArgs profile'''
        -- Add eras, like "ci-test-notracer-[alra|alzo|bage|coay|mary|shey]"
        addEra = \p era suffix ->
          let name = Types.name p
              newName = name ++ "-" ++ suffix
          in  (newName, p {Types.name = newName, Types.era = era})
    in 
        [ addEra profile'''' Types.Allegra "alra"
        , addEra profile'''' Types.Shelley "shey"
        , addEra profile'''' Types.Mary    "mary"
        , addEra profile'''' Types.Alonzo  "alzo"
        , addEra profile'''' Types.Babbage "bage"
        , addEra profile'''' Types.Conway  "coay"
        ]
  )
  profilesNoEra

--------------------------------------------------------------------------------

-- Adding a P.nameSuffix was abandoned to keep the code `grep` friendly!
profilesNoEra :: Map.Map String Types.Profile
-- Names:
-- wb profile all-profiles | jq .[] | jq -r .name | sort | uniq | grep "\-bage"
profilesNoEra = Map.fromList $ map
  (\p ->
    (
      Types.name p
    -- TODO: Always the same values ???
    , p { Types.genesis =
          let genesis = Types.genesis p
          in genesis {
                Types.network_magic = 42
              -- TODO: Remove property?
              , Types.single_shot = True
              }
        , Types.generator =
          let generator = Types.generator p
          in generator {
              -- TODO: Remove property?
              Types.add_tx_size = 100
            }
        , Types.analysis =
          let analysis = Types.analysis p
          in  analysis {
                Types.cluster_base_startup_overhead_s = 40
              , Types.start_log_spread_s = 120
              , Types.last_log_spread_s = 120
              , Types.silence_since_last_block_s = 120
              , Types.tx_loss_ratio = 0.02
              , Types.finish_patience = 21
              {-- TODO: These two are set when constructing "derive"
              , Types.minimum_chain_density = active_slots_coeff * 0.5
              , Types.cluster_startup_overhead_s = dataset_induced_startup_delay_conservative
              --}
              }
        , Types.overlay = mempty
      }
    )
  )
  $
  profileNoEraEmptyCI ++ profileNoEraEmptyOthers ++ profilesNoEraForgeStress ++ profilesNoEraTheRest

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
  P.usePublicRouting
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
    , Types.AWS Types.US_EAST_1
    , Types.AWS Types.AP_SOUTHEAST_2
    ]

clusterNomadSsd :: Types.Profile -> Types.Profile
clusterNomadSsd =
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

clusterDefault :: Types.Profile -> Types.Profile
clusterDefault p =
  p {Types.cluster =
    Types.Cluster {
      Types.nomad = Types.ClusterNomad {
        Types.namespace = "default"
      , Types.nomad_class = ""
      , Types.resources = Types.ByNodeType {
          Types.producer = Types.Resources 2 15000 16000
        , Types.explorer = Just $ Types.Resources 2 15000 16000
        }
      , Types.host_volumes = Nothing
      , Types.fetch_logs_ssh = False
      }
    , Types.aws = Types.ClusterAWS {
        Types.instance_type = Types.ByNodeType {
          Types.producer = "c5.2xlarge"
        , Types.explorer = Just "m5.4xlarge"
        }
      , Types.use_public_routing = False
      }
    , Types.minimun_storage = Just $ Types.ByNodeType {
        Types.producer = 12582912
      , Types.explorer = Just 14155776
      }
    , Types.ssd_directory = Nothing
    , Types.keep_running = False
    }
  }
