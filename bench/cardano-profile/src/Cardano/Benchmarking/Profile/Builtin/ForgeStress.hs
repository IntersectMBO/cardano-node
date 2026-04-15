{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- "forge-stress-*" family of profiles!
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Builtin.ForgeStress (
  base
, durationXS, durationM, durationXL
, profilesForgeStress
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
  . P.initCooldown 5
  . P.analysisStandard

-- Helpers by timescale:

-- Two epochs.
durationXS :: Types.Profile -> Types.Profile
durationXS = V.timescaleCompressed . P.shutdownOnSlot 1200
          . P.generatorEpochs 1

-- Four epochs.
durationM :: Types.Profile -> Types.Profile
durationM = V.timescaleCompressed . P.shutdownOnSlot 2400
          . P.generatorEpochs 3

-- Eight epochs.
durationXL :: Types.Profile -> Types.Profile
durationXL = V.timescaleCompressed . P.shutdownOnSlot 4800
           . P.generatorEpochs 7
           . P.analysisEpoch3Plus

-- Chain fragment creation with large blocks - short chain.
-- Overall tx count for submission needs adjustment,
-- however, duplicating some values allows for modding this
-- without interfering with regular forge-stres-* profiles.
durationChain :: Types.Profile -> Types.Profile
durationChain x = let 
  p =   timescaleXLBlock
      . P.shutdownOnSlot 1200
      . P.generatorEpochs 1
      $ x
  g = Types.generator p
  in p {Types.generator = g {Types.tx_count = Just 90000}}

-- Chain fragment creation with large blocks - medium length chain.
-- Rest, see above.
durationChainM :: Types.Profile -> Types.Profile
durationChainM x = let 
  p =   timescaleXLBlock
      . P.shutdownOnSlot 2400
      . P.generatorEpochs 3
      $ x
  g = Types.generator p
  in p {Types.generator = g {Types.tx_count = Just 90000}}

-- This is timescaleCompressed with a changed slot filling constraint:
-- fill on avg every 66th slot instead of every 20th.
-- Goes hand in hand with changed submission pressure to fill large blocks, and bumped tx counts.
timescaleXLBlock :: Types.Profile -> Types.Profile
timescaleXLBlock =
    P.slotDuration 1 . P.epochLength 600
  . P.activeSlotsCoeff 0.015 . P.parameterK 3

-- much higher submission pressure needed to fill large blocks
valueXLBlock :: Types.Profile -> Types.Profile
valueXLBlock = V.valueBase . P.tps 100

-- Chain framegment creation with varying tx in and out counts.
-- This duplicates valueLocal, parametrizing ins and outs.
valueInOut :: Integer -> Types.Profile -> Types.Profile
valueInOut x = P.txIn x . P.txOut x . P.txFee 1000000 . P.tps 15

--------------------------------------------------------------------------------

profilesForgeStress :: [Types.Profile]
profilesForgeStress =
  let fs = P.empty & base
      -- Helpers by composition size:
      -- Except for the solo profiles (which serve double-use to create chain fragments),
      -- forge-stress works better with a smaller block size (64k) for comparative benchmarks.
      n1 = V.genesisVariantVoltaire    . V.hosts 1
      n3 = V.genesisVariantVoltaire64k . V.hosts 3
      v6 = V.genesisVariantVoltaire64k . V.hosts 6
  in [
  -- 1 node versions (non-pre).
    fs & P.name "forge-stress-solo-xs"          . V.valueLocal . n1 . V.datasetCurrent . durationXS . P.traceForwardingOn                                         . P.analysisUnitary
  , fs & P.name "forge-stress-solo"             . V.valueLocal . n1 . V.datasetCurrent . durationM  . P.traceForwardingOn                                         . P.analysisUnitary
  , fs & P.name "forge-stress-plutus-solo"      . V.plutusLoop . n1 . V.datasetCurrent . durationM  . P.traceForwardingOn                                         . P.analysisSizeSmall
  -- 1 node versions (pre)
  , fs & P.name "forge-stress-pre-solo-xs"      . V.valueLocal . n1 . V.datasetOct2021 . durationXS . P.traceForwardingOn                                         . P.analysisUnitary
  , fs & P.name "forge-stress-pre-solo"         . V.valueLocal . n1 . V.datasetOct2021 . durationM  . P.traceForwardingOn                                         . P.analysisUnitary
  , fs & P.name "forge-stress-pre-solo-xl"      . V.valueLocal . n1 . V.datasetOct2021 . durationXL . P.traceForwardingOn
  , fs & P.name "forge-stress-pre-plutus-solo"  . V.plutusLoop . n1 . V.datasetOct2021 . durationM  . P.traceForwardingOn                                         . P.analysisSizeSmall
  -- chain creation
  , fs & P.name "fschain-768k-xs"               . valueXLBlock . n1 . V.datasetOct2021 . durationChain  . P.traceForwardingOn                                     . P.analysisUnitary  . P.blocksize768k
  , fs & P.name "fschain-768k"                  . valueXLBlock . n1 . V.datasetOct2021 . durationChainM . P.traceForwardingOn                                     . P.analysisUnitary  . P.blocksize768k
  , fs & P.name "fschain-8io"                   . valueInOut 8 . n1 . V.datasetOct2021 . durationXL     . P.traceForwardingOn
  -- 3 nodes versions (non-pre)
  , fs & P.name "forge-stress"                  . V.valueLocal . n3 . V.datasetCurrent . durationM  . P.traceForwardingOn                                         . P.analysisUnitary
  , fs & P.name "forge-stress-notracer"         . V.valueLocal . n3 . V.datasetCurrent . durationM  . P.traceForwardingOff                                        . P.analysisUnitary
  , fs & P.name "forge-stress-plutus"           . V.plutusLoop . n3 . V.datasetCurrent . durationM  . P.traceForwardingOn                                         . P.analysisSizeSmall
  -- -large: voltaire variant, double nodes and double runtime. This needs >64GB RAM.
  , fs & P.name "forge-stress-large"            . V.valueLocal . v6 . V.datasetCurrent . durationXL . P.traceForwardingOn
  -- 3 nodes versions (pre)
  , fs & P.name "forge-stress-pre"              . V.valueLocal . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOn                                         . P.analysisUnitary
  , fs & P.name "forge-stress-pre-rtsA4m"       . V.valueLocal . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOn                   . P.rtsGcAllocSize  4 . P.analysisUnitary
  , fs & P.name "forge-stress-pre-rtsA64m"      . V.valueLocal . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOn                   . P.rtsGcAllocSize 64 . P.analysisUnitary
  , fs & P.name "forge-stress-pre-rtsN3"        . V.valueLocal . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOn  . P.rtsThreads 3                       . P.analysisUnitary
  , fs & P.name "forge-stress-pre-rtsA4mN3"     . V.valueLocal . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOn  . P.rtsThreads 3 . P.rtsGcAllocSize  4 . P.analysisUnitary
  , fs & P.name "forge-stress-pre-rtsA64mN3"    . V.valueLocal . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOn  . P.rtsThreads 3 . P.rtsGcAllocSize 64 . P.analysisUnitary
  , fs & P.name "forge-stress-pre-rtsxn"        . V.valueLocal . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOn                   . P.rtsGcNonMoving    . P.analysisUnitary
  , fs & P.name "forge-stress-pre-notracer"     . V.valueLocal . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOff                                        . P.analysisUnitary
  , fs & P.name "forge-stress-pre-plutus"       . V.plutusLoop . n3 . V.datasetOct2021 . durationM  . P.traceForwardingOn                                         . P.analysisSizeSmall
  -- -large: voltaire variant, double nodes and double runtime. This needs a 64GB RAM machine.
  , fs & P.name "forge-stress-pre-large"           . V.valueLocal . v6 . V.datasetOct2021 . durationXL . P.traceForwardingOn
  , fs & P.name "forge-stress-pre-large-rtsN3"     . V.valueLocal . v6 . V.datasetOct2021 . durationXL . P.traceForwardingOn  . P.rtsThreads 3
  , fs & P.name "forge-stress-pre-large-rtsN4"     . V.valueLocal . v6 . V.datasetOct2021 . durationXL . P.traceForwardingOn  . P.rtsThreads 4
  ]
