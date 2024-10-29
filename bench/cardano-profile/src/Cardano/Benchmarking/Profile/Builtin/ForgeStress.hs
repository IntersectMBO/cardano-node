{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- "forge-stress-*" family of profiles!
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Builtin.ForgeStress (
  base
, durationXS, durationM, durationXL
, profilesNoEraForgeStress
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
  . P.uniCircle . P.loopback
  . V.fundsDefault
  . P.p2pOff . P.newTracing
  . P.generatorEpochs 3 . P.initCooldown 5
  . P.analysisStandard
  . V.clusterDefault -- TODO: "cluster" should be "null" here.

-- Helpers by timescale:

-- Two epochs.
durationXS :: Types.Profile -> Types.Profile
durationXS = V.timescaleCompressed . P.shutdownOnSlot 1200

-- Four epochs.
durationM :: Types.Profile -> Types.Profile
durationM = V.timescaleCompressed . P.shutdownOnSlot 2400

-- Eight epochs.
durationXL :: Types.Profile -> Types.Profile
durationXL = V.timescaleCompressed . P.shutdownOnSlot 4800
           . P.analysisEpoch3Plus

--------------------------------------------------------------------------------

profilesNoEraForgeStress :: [Types.Profile]
profilesNoEraForgeStress =
  let fs = P.empty & base
      -- Helpers by composition size:
      -- TODO: after dropping jq profiles, bump all to genesisVariantVoltaire.
      -- TODO: Except for the solo profile, forge-stress is not supposed to bump blocksize. Possibly solve this via overlay.
      n1 = V.genesisVariantPreVoltaire  . V.hosts 1
      n3 = V.genesisVariant300          . V.hosts 3
      n6 = V.genesisVariant300          . V.hosts 6
  in [
  -- 1 node versions (non-pre).
    fs & P.name "forge-stress-solo-xs"       . V.valueLocal . n1 . V.datasetCurrent . durationXS . P.traceForwardingOn                                         . P.analysisUnitary
  , fs & P.name "forge-stress-solo"          . V.valueLocal . n1 . V.datasetCurrent . durationM  . P.traceForwardingOn                                         . P.analysisUnitary
  , fs & P.name "forge-stress-plutus-solo"   . V.plutusLoop . n1 . V.datasetCurrent . durationM  . P.traceForwardingOn                                                            
  -- 1 node versions (pre).
  , fs & P.name "forge-stress-pre-solo-xs"   . V.valueLocal . n1 . V.datasetOct2021 . durationXS . P.traceForwardingOn                                         . P.analysisUnitary
  , fs & P.name "forge-stress-pre-solo"      . V.valueLocal . n1 . V.datasetOct2021 . durationM  . P.traceForwardingOn                                         . P.analysisUnitary
  , fs & P.name "forge-stress-pre-solo-xl"   . V.valueLocal . n1 . V.datasetOct2021 . durationXL . P.traceForwardingOn                                         
  -- 3 nodes versions (non-pre).
  , fs & P.name "forge-stress"               . V.valueLocal . n3 . V.datasetCurrent . durationM  . P.traceForwardingOn                                         . P.analysisUnitary
  , fs & P.name "forge-stress-notracer"      . V.valueLocal . n3 . V.datasetCurrent . durationM  . P.traceForwardingOff                                        . P.analysisUnitary
  -- TODO: FIXME: "forge-stress-p2p" has no P2P enabled and Plutus TPS!!!!
  , fs & P.name "forge-stress-p2p"           . V.plutusLoop . n3 . V.datasetCurrent . durationM  . P.traceForwardingOn
  , fs & P.name "forge-stress-plutus"        . V.plutusLoop . n3 . V.datasetCurrent . durationM  . P.traceForwardingOn
  -- 3 nodes versions (pre).
  , fs & P.name "forge-stress-pre"           . V.valueLocal . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOn                                         . P.analysisUnitary
  , fs & P.name "forge-stress-pre-rtsA4m"    . V.valueLocal . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOn                   . P.rtsGcAllocSize  4 . P.analysisUnitary
  , fs & P.name "forge-stress-pre-rtsA64m"   . V.valueLocal . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOn                   . P.rtsGcAllocSize 64 . P.analysisUnitary
  , fs & P.name "forge-stress-pre-rtsN3"     . V.valueLocal . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOn  . P.rtsThreads 3                       . P.analysisUnitary
  , fs & P.name "forge-stress-pre-rtsA4mN3"  . V.valueLocal . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOn  . P.rtsThreads 3 . P.rtsGcAllocSize  4 . P.analysisUnitary
  , fs & P.name "forge-stress-pre-rtsA64mN3" . V.valueLocal . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOn  . P.rtsThreads 3 . P.rtsGcAllocSize 64 . P.analysisUnitary
  , fs & P.name "forge-stress-pre-rtsxn"     . V.valueLocal . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOn                   . P.rtsGcNonMoving    . P.analysisUnitary
  , fs & P.name "forge-stress-pre-notracer"  . V.valueLocal . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOff                                        . P.analysisUnitary
  , fs & P.name "forge-stress-pre-plutus"    . V.plutusLoop . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOn
  -- Double nodes and double run time version.
  , fs & P.name "forge-stress-large"         . V.valueLocal . n6 . V.datasetCurrent . durationXL . P.traceForwardingOn                                         
  ]
