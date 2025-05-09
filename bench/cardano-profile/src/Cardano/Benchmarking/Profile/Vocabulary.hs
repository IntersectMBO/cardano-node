{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Vocabulary (
  datasetEmpty
, datasetSmall, datasetMiniature, datasetCurrent, datasetOct2021

, timescaleCompressed, timescaleSmall, timescaleModel
, timescaleMainnet, timescaleDevops

, genesisVariant300, genesisVariantLatest, genesisVariantPreVoltaire, genesisVariantVoltaire
, fundsDefault, fundsDouble, fundsVoting

, hosts

, valueBase, valueLocal, valueCloud
, plutusBase, plutusLoop
, plutusSaturation, plutusDoubleSaturation, plutusDoublePlusSaturation

, plutusTypeLoop, plutusTypeLoop2024, plutusTypeECDSA, plutusTypeSchnorr
, plutusTypeBLST, plutusTypeRIPEMD

, clusterDefault
) where

--------------------------------------------------------------------------------

import           Prelude
-- Package: aeson.
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
-- Package: vector.
import qualified Data.Vector as Vector
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types

--------------------------------------------------------------------------------

-- Definition vocabulary: dataset size.
---------------------------------------

-- If zero delegators, numbers of pools is used (same as nodes for non-dense).
datasetEmpty :: Types.Profile -> Types.Profile
datasetEmpty =     P.utxo        0 . P.delegators       0 . P.dreps 0

datasetSmall :: Types.Profile -> Types.Profile
datasetSmall =     P.utxo  1000000 . P.delegators  200000 . P.dreps 0

-- Used only by "ci-bench*" and "10-*", including one profile with DReps.
datasetMiniature :: Types.Profile -> Types.Profile
datasetMiniature = P.utxo   500000 . P.delegators  100000

datasetCurrent :: Types.Profile -> Types.Profile
datasetCurrent =   P.utxo 10000000 . P.delegators 1300000 . P.dreps 0

-- Dataset used with variable DRep count.
datasetOct2021 :: Types.Profile -> Types.Profile
datasetOct2021 =   P.utxo  4000000 . P.delegators 1000000

-- Definition vocabulary: timescale.
------------------------------------

timescaleCompressed :: Types.Profile -> Types.Profile
timescaleCompressed =
    P.slotDuration 1 . P.epochLength 600
  . P.activeSlotsCoeff 0.05 . P.parameterK 3

timescaleSmall :: Types.Profile -> Types.Profile
timescaleSmall =
    P.slotDuration 1 . P.activeSlotsCoeff 0.05
  . P.epochLength 1200 . P.parameterK 6

-- Used by "model", "value", and "plutus".
timescaleModel :: Types.Profile -> Types.Profile
timescaleModel =
    P.slotDuration 1 . P.epochLength 8000
  . P.activeSlotsCoeff 0.05 . P.parameterK 40

timescaleMainnet :: Types.Profile -> Types.Profile
timescaleMainnet =
    P.slotDuration 1 . P.epochLength 432000
  . P.activeSlotsCoeff 0.05 . P.parameterK 2160

-- Only used in "devops".
timescaleDevops :: Types.Profile -> Types.Profile
timescaleDevops =
    P.slotDuration 0.2 . P.epochLength 1000
  . P.activeSlotsCoeff 0.1 . P.parameterK 10

-- Definition vocabulary: genesis variants.
-------------------------------------------

-- See: data/genesis/epoch-timeline.json
genesisVariant300 :: Types.Profile -> Types.Profile
genesisVariant300 = P.pparamsEpoch 300

-- should always reference the latest entry in data/genesis/epoch-timeline.json
genesisVariantLatest :: Types.Profile -> Types.Profile
genesisVariantLatest = P.pparamsEpoch 507

-- references the latest Babbage epoch in data/genesis/epoch-timeline.json
genesisVariantBabbageLatest :: Types.Profile -> Types.Profile
genesisVariantBabbageLatest = P.pparamsEpoch 492

-- Latest Babbage epoch in data/genesis/epoch-timeline.json, including the "v8-preview" overlay.
genesisVariantPreVoltaire :: Types.Profile -> Types.Profile
genesisVariantPreVoltaire = genesisVariantBabbageLatest . P.v8Preview

-- First Conway epoch in data/genesis/epoch-timeline.json; implicitly includes v8Preview and v9Prievew already
genesisVariantVoltaire :: Types.Profile -> Types.Profile
genesisVariantVoltaire = genesisVariantLatest

-- Definition vocabulary: funds.
--------------------------------

-- Defined in the "genesis" property and it's for the tx-generator.
fundsDefault :: Types.Profile -> Types.Profile
fundsDefault = P.poolBalance 1000000000000000 . P.funds 10000000000000 . P.utxoKeys 1

-- Some profiles have a higher `funds_balance` in `Genesis`. Needed? Fix it?
fundsDouble :: Types.Profile -> Types.Profile
fundsDouble =  P.poolBalance 1000000000000000 . P.funds 20000000000000 . P.utxoKeys 1

fundsVoting :: Types.Profile -> Types.Profile
fundsVoting =  P.poolBalance 1000000000000000 . P.funds 40000000000000 . P.utxoKeys 2

-- Definition vocabulary: composition.
--------------------------------------

-- Helper: Sets the numbers of hosts and its analysis filter at the same time.
hosts :: Integer -> Types.Profile -> Types.Profile
hosts i =
    P.hosts i
  . P.cBlockMinimumAdoptions (i -1)

-- Definition vocabulary: workload (value / full blocks).
---------------------------------------------------------

valueBase :: Types.Profile -> Types.Profile
valueBase = P.txIn 2 . P.txOut 2 . P.txFee 1000000

-- current_tps_saturation_value
-- Helper base for the common value workload case.
valueLocal :: Types.Profile -> Types.Profile
valueLocal = valueBase . P.tps 15

-- nomad_perf_tps_saturation_value
-- Helper base for the common value workload case.
-- Also used by non-cloud profiles "default" and "oldtracing".
valueCloud :: Types.Profile -> Types.Profile
valueCloud = valueBase . P.tps 12

-- Definition vocabulary: workload (plutus / small blocks).
-----------------------------------------------------------

{- TODO: Add filters, was part of "plutus_base"
    , analysis:
      { filters:                        ["size-small"]
      }
-}
plutusBase :: Types.Profile -> Types.Profile
plutusBase = P.txIn 1 . P.txOut 1

-- Helper base for the common plutus loop workload case.
plutusLoop :: Types.Profile -> Types.Profile
plutusLoop =
    plutusSaturation
  . plutusTypeLoop

plutusSaturation :: Types.Profile -> Types.Profile
plutusSaturation =
    plutusBase . P.tps 0.20

-- Used by "plutuscall*" and "model*" saturation.
plutusDoubleSaturation :: Types.Profile -> Types.Profile
plutusDoubleSaturation =
    plutusBase . P.tps 0.40

-- Used by "ci-bench*" and "plutuscall-volt*" for "secp" and "blst"
plutusDoublePlusSaturation :: Types.Profile -> Types.Profile
plutusDoublePlusSaturation =
    plutusBase . P.tps 0.48

-- Plutus types ("type", "script" and "redeemer").
--------------------------------------------------

-- Replaces jq's "plutus_loop_counter".
plutusTypeLoop :: Types.Profile -> Types.Profile
plutusTypeLoop =
    P.plutusType "LimitSaturationLoop" . P.plutusScript "Loop"
  . P.redeemerInt 1000000
  . P.txFee 1360000

-- Replaces jq's "plutus_loop2024_counter".
plutusTypeLoop2024 :: Types.Profile -> Types.Profile
plutusTypeLoop2024 =
    P.plutusType "LimitSaturationLoop" . P.plutusScript "Loop2024"
  . P.redeemerInt 1000000
  . P.txFee 1412000

-- Replaces jq's "plutus_loop_secp_ecdsa".
plutusTypeECDSA :: Types.Profile -> Types.Profile
plutusTypeECDSA =
    P.plutusType "LimitTxPerBlock_8" . P.plutusScript "EcdsaSecp256k1Loop"
  . P.redeemerFields [
      KeyMap.fromList [("int",   Aeson.Number 1000000.0)]
    , KeyMap.fromList [("bytes", Aeson.String "0392d7b94bc6a11c335a043ee1ff326b6eacee6230d3685861cd62bce350a172e0")]
    , KeyMap.fromList [("bytes", Aeson.String "16e0bf1f85594a11e75030981c0b670370b3ad83a43f49ae58a2fd6f6513cde9")]
    , KeyMap.fromList [("bytes", Aeson.String "5fb12954b28be6456feb080cfb8467b6f5677f62eb9ad231de7a575f4b6857512754fb5ef7e0e60e270832e7bb0e2f0dc271012fa9c46c02504aa0e798be6295")]
    ]
  . P.txFee 1008000

-- Replaces jq's "plutus_loop_secp_schnorr".
plutusTypeSchnorr :: Types.Profile -> Types.Profile
plutusTypeSchnorr =
    P.plutusType "LimitTxPerBlock_8"   . P.plutusScript "SchnorrSecp256k1Loop"
  . P.redeemerFields [
      KeyMap.fromList [("int",   Aeson.Number 1000000.0)]
    , KeyMap.fromList [("bytes", Aeson.String "599de3e582e2a3779208a210dfeae8f330b9af00a47a7fb22e9bb8ef596f301b")]
    , KeyMap.fromList [("bytes", Aeson.String "30303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030")]
    , KeyMap.fromList [("bytes", Aeson.String "5a56da88e6fd8419181dec4d3dd6997bab953d2fc71ab65e23cfc9e7e3d1a310613454a60f6703819a39fdac2a410a094442afd1fc083354443e8d8bb4461a9b")]
    ]
  . P.txFee 1004000

-- Replaces jq's "plutus_loop_blst".
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
  . P.txFee 940000

-- the bytes content is arbitrary, but should be of the same size as RIPEMD-160 output, i.e. 160 bits
plutusTypeRIPEMD :: Types.Profile -> Types.Profile
plutusTypeRIPEMD =
    P.plutusType "LimitTxPerBlock_8"   . P.plutusScript "Ripemd160"
  . P.redeemerFields [
      KeyMap.fromList [("int",   Aeson.Number 1000000.0)]
    , KeyMap.fromList [("bytes", Aeson.String "5a56da88e6fd8419181dec4d3dd6997bab953d2f")]
    ]
  . P.txFee 940000

-- Definition vocabulary: cluster.
----------------------------------

-- TODO: Should not exists, should be null instead!
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
