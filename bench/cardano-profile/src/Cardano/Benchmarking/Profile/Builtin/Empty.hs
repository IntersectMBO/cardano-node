{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Use :" -}

-- All the `FixedLoaded`, `timescaleCompressed` and `datasetEmpty` profiles!
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Builtin.Empty (
  base, baseNoDataset
, fastDuration
, ciTestDuration
, traceBenchDuration, traceFullDuration
, defaultDuration
, epochTransitionDuration
, profilesNoEraEmpty
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V

--------------------------------------------------------------------------------

-- Use `base` and pick a duration.
base :: Types.Profile -> Types.Profile
base =
    baseNoDataset
  -- Genesis:
  . V.datasetEmpty

-- TODO: "default*", "oldtracing" and "plutus*" below have `P.delegators 6`.
--       Remove them, make delegators zero by using `V.datasetEmpty` here.
--       It was kept to make test pass because "prof0-defaults.jq" set it,
--       `n_pools` used as "effective_delegators" when delegators zero/null.
baseNoDataset :: Types.Profile -> Types.Profile
baseNoDataset =
    P.fixedLoaded
  -- Genesis:
  . V.fundsDefault
  . P.initCooldown 5
  . P.analysisStandard

-- TODO: Move to `base` when "default*" and "oldtracing" genesis are the same.
genesis :: Types.Profile -> Types.Profile
genesis = V.genesisVariant300

--------------------------------------------------------------------------------

fastDuration :: Types.Profile -> Types.Profile
fastDuration =
    V.timescaleCompressed . P.shutdownOnBlock 1
  -- TODO: dummy "generator.epochs" ignored in favor of "--shutdown-on".
  --       Create a "time.epochs" or "time.blocks" or similar, IDK!
  -- This applies to all profiles!
  . P.generatorEpochs 3
  . P.desc "Stop as soon as we've seen a single block"

ciTestDuration :: Types.Profile -> Types.Profile
ciTestDuration =
    V.timescaleCompressed . P.shutdownOnBlock 8
  -- TODO: dummy "generator.epochs" ignored in favor of "--shutdown-on".
  --       Create a "time.epochs" or "time.blocks" or similar, IDK!
  -- This applies to all profiles!
  . P.generatorEpochs 2
  . P.desc "Miniature dataset, CI-friendly duration (2-3min), test scale"

traceBenchDuration :: Types.Profile -> Types.Profile
traceBenchDuration =
    V.timescaleCompressed . P.shutdownOnBlock 15
  -- TODO: dummy "generator.epochs" ignored in favor of "--shutdown-on".
  --       Create a "time.epochs" or "time.blocks" or similar, IDK!
  -- This applies to all profiles!
  . P.generatorEpochs 3
  . P.desc "6 low-footprint nodes in a torus topology, 5 minutes runtime"

traceFullDuration :: Types.Profile -> Types.Profile
traceFullDuration =
    V.timescaleCompressed . P.shutdownOnSlot 1200
  -- TODO: dummy "generator.epochs" ignored in favor of "--shutdown-on".
  --       Create a "time.epochs" or "time.blocks" or similar, IDK!
  -- This applies to all profiles!
  . P.generatorEpochs 3
  . P.desc "6 low-footprint nodes in a torus topology, 20 minutes runtime"

defaultDuration :: Types.Profile -> Types.Profile
defaultDuration =
    V.timescaleCompressed . P.shutdownOnOff
  . P.generatorEpochs 3
  -- . P.desc "2 low-footprint nodes, 15 minutes runtime"

epochTransitionDuration :: Types.Profile -> Types.Profile
epochTransitionDuration =
    V.timescaleCompressed . P.shutdownOnSlot 900
  -- TODO: dummy "generator.epochs" ignored in favor of "--shutdown-on".
  --       Create a "time.epochs" or "time.blocks" or similar, IDK!
  -- This applies to all profiles!
  . P.generatorEpochs 3
  . P.desc "2 low-footprint nodes, 15 minutes runtime"

--------------------------------------------------------------------------------

profilesNoEraEmpty :: [Types.Profile]
profilesNoEraEmpty = map baseNoDataset
  $
  ------------------------------------------------------------------------------
  -- fast: FixedLoaded and "--shutdown-on-block-synced 1" with 1 or 2 nodes.
  ------------------------------------------------------------------------------
  let fast =
          P.empty & V.datasetEmpty . genesis . fastDuration
        . P.uniCircle . P.loopback
      fast1 = fast & V.hosts 1
      fast2 = fast & V.hosts 2
  in [
  -- Local.
    fast1 & P.name "fast-solo"       . V.valueLocal . P.traceForwardingOn  . P.newTracing . P.p2pOff
  , fast2 & P.name "fast"            . V.valueLocal . P.traceForwardingOn  . P.newTracing . P.p2pOff
  -- TODO: Remove and make `P.p2pOn` the default without adding a "-nop2p" profile.
  , fast2 & P.name "fast-p2p"        . V.valueLocal . P.traceForwardingOn  . P.newTracing . P.p2pOn
  , fast2 & P.name "fast-oldtracing" . V.valueLocal . P.traceForwardingOn  . P.oldTracing . P.p2pOff
  , fast2 & P.name "fast-notracer"   . V.valueLocal . P.traceForwardingOff . P.newTracing . P.p2pOff
  , fast2 & P.name "fast-plutus"     . V.plutusLoop . P.traceForwardingOn  . P.newTracing . P.p2pOff . P.analysisSizeSmall
  ]
  ++
  ------------------------------------------------------------------------------
  -- ci-test: FixedLoaded and "--shutdown-on-block-synced 3" with 2 nodes.
  ------------------------------------------------------------------------------
  let ciTest =
          P.empty & V.datasetEmpty . genesis . ciTestDuration
        . P.uniCircle . V.hosts 2 . P.loopback
  in [
  -- Local
    ciTest & P.name "ci-test"          . V.valueLocal . P.traceForwardingOn  . P.newTracing . P.p2pOff
  , ciTest & P.name "ci-test-rtview"   . V.valueLocal . P.traceForwardingOn  . P.newTracing . P.p2pOff . P.tracerRtview
  , ciTest & P.name "ci-test-notracer" . V.valueLocal . P.traceForwardingOff . P.newTracing . P.p2pOff
  -- TODO: Remove and make `P.p2pOn` the default without adding a "-nop2p" profile.
  , ciTest & P.name "ci-test-p2p"      . V.valueLocal . P.traceForwardingOn  . P.newTracing . P.p2pOn
  , ciTest & P.name "ci-test-plutus"   . V.plutusLoop . P.traceForwardingOn  . P.newTracing . P.p2pOff . P.analysisSizeSmall
  ]
  ++
  ------------------------------------------------------------------------------
  -- ci-test-hydra: FixedLoaded and "--shutdown-on-block-synced 3" with 2 nodes.
  ------------------------------------------------------------------------------
  let ciTestHydra =
          P.empty & V.datasetEmpty . V.genesisVariantPreVoltaire . ciTestDuration
        . P.uniCircle . V.hosts 2 . P.loopback
        . P.analysisSizeSmall
  in [
     -- intricacies of fee calculation..., default fee works for ci-test-plutus and ci-bench-plutus
    ciTestHydra & P.name "ci-test-hydra" . P.txFeeOverwrite 1380000 . V.plutusLoop . P.traceForwardingOn  . P.newTracing . P.p2pOn . P.blocksize64k
  ]
  ++
  ------------------------------------------------------------------------------
  -- trace-*: FixedLoaded and "tracer.withresources = true" with 6 nodes.
  ------------------------------------------------------------------------------
  let trace =
          P.empty & V.datasetEmpty . genesis
        -- TODO: "default-*" uses 6 nodes and `uniCircle`.
        . P.torus . V.hosts 6 . P.loopback
        . P.tracerWithresources
      bench = trace & traceBenchDuration
      full  = trace & traceFullDuration
  in [
  -- "--shutdown-on-block-synced 15"
    bench & P.name "trace-bench"            . V.valueLocal . P.traceForwardingOn  . P.newTracing . P.p2pOff
  , bench & P.name "trace-bench-rtview"     . V.valueLocal . P.traceForwardingOn  . P.newTracing . P.p2pOff . P.tracerRtview
  , bench & P.name "trace-bench-oldtracing" . V.valueLocal . P.traceForwardingOn  . P.oldTracing . P.p2pOff
  , bench & P.name "trace-bench-notracer"   . V.valueLocal . P.traceForwardingOff . P.newTracing . P.p2pOff
  -- "--shutdown-on-slot-synced 1200"
  , full  & P.name "trace-full"             . V.valueLocal . P.traceForwardingOn  . P.newTracing . P.p2pOff
  , full  & P.name "trace-full-rtview"      . V.valueLocal . P.traceForwardingOn  . P.newTracing . P.p2pOff . P.tracerRtview
  ]
  ++
  ------------------------------------------------------------------------------
  -- 6 nodes and no "--shutdown-on-slot-synced" and no "--shutdown-on-block-synced"
  ------------------------------------------------------------------------------
  let noCliStop =
          P.empty & defaultDuration
        -- TODO: "trace-*" uses 6 nodes and `torus`.
        . P.uniCircle . V.hosts 6 . P.loopback
        -- TODO: The only one without 0 delegators.
        --       Fix and remove `baseNoDataset` (Same `base` for all).
        . P.utxo 0 . P.delegators 6 . P.dreps 0
      value  = noCliStop & V.genesisVariant300
      -- TODO: "fast-plutus" and "ci-test-plutus" are using `genesisVariant300`.
      plutus = noCliStop & V.genesisVariantPreVoltaire
      -- intricacies of fee calculation..., default fee works for ci-test-plutus and ci-bench-plutus
      loop    = P.txFeeOverwrite 1380000 . V.plutusLoop
      ecdsa   = V.plutusSaturation . V.plutusTypeECDSA
      schnorr = V.plutusSaturation . V.plutusTypeSchnorr
  in [
  -- TODO: TX fee went from 1025000 to 1008000 ????
    value  & P.name "default"             . V.valueCloud . P.traceForwardingOn  . P.newTracing . P.p2pOff . P.analysisUnitary
  , value  & P.name "default-p2p"         . V.valueCloud . P.traceForwardingOn  . P.newTracing . P.p2pOn  . P.analysisUnitary
  , value  & P.name "oldtracing"          . V.valueCloud . P.traceForwardingOn  . P.oldTracing . P.p2pOff . P.analysisUnitary
  , plutus & P.name "plutus"              . loop         . P.traceForwardingOn  . P.newTracing . P.p2pOff . P.analysisSizeSmall
  , plutus & P.name "plutus-secp-ecdsa"   . ecdsa        . P.traceForwardingOn  . P.newTracing . P.p2pOff . P.analysisSizeSmall
  , plutus & P.name "plutus-secp-schnorr" . schnorr      . P.traceForwardingOn  . P.newTracing . P.p2pOff . P.analysisSizeSmall
  ]
  ++
  ------------------------------------------------------------------------------
  -- epoch transition: FixedLoaded and "--shutdown-on-slot-synced 900" with 2 nodes.
  ------------------------------------------------------------------------------
  let ep =
          P.empty & V.datasetEmpty . genesis . epochTransitionDuration
        . P.uniCircle . V.hosts 2 . P.loopback
  in [
    ep & P.name "epoch-transition" . V.valueLocal . P.traceForwardingOn . P.newTracing . P.p2pOff
  ]
