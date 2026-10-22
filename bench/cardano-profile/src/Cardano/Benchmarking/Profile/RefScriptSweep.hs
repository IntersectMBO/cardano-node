{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Cardano.Benchmarking.Profile.RefScriptSweep
Description : Profiles sweeping the cost of reference scripts.

A sweep to price reference scripts by parity with Plutus execution: measure
the node time a byte of reference script costs, and the node time an
execution step costs, in the same runs, and charge a byte the steps it costs.
The benchmarking transactions spend inputs locked at variants of LoopV3 (see
"Cardano.TxGenerator.ScriptVariants"), each input at a variant of its own,
running the loop a fixed number of times:

  * @refscript-a-*@: the reference script bytes per transaction, with the
    execution minimal and the transactions per block fixed; and as a check,
    the same scripts in the witnesses.
  * @refscript-e-*@: the execution steps per transaction, with the reference
    scripts minimal, to convert time into steps.
  * @refscript-l-*@: blocks full of reference scripts, at the per-block limit
    of 1 MiB and at limits raised to 2 and 4 MiB.

Each comes in a local version, 2 nodes on loopback for 4 compressed epochs
and a quarter of the variants, to check its setup, and a cloud version on the perf cluster. They are
playground profiles: not in "data/all-profiles.json".
-}
module Cardano.Benchmarking.Profile.RefScriptSweep (
  profilesRefScriptSweep
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
import           Data.Maybe (fromMaybe)
-- Package: scientific.
import qualified Data.Scientific as Scientific
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Builtin.Cloud as C
import qualified Cardano.Benchmarking.Profile.Builtin.Empty as E
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V

--------------------------------------------------------------------------------

-- | The benchmarking transactions of a sweep point.
data Workload = Workload
  { inputs   :: Integer
    -- ^ Inputs per transaction, each locked at a variant of its own.
  , size     :: Maybe Integer
    -- ^ Bytes per variant; without, as small as can be (LoopV3 is 113 bytes).
  , source   :: Types.ScriptSource
  , loops    :: Integer
    -- ^ Iterations of the loop per input.
  , variants :: Integer
    -- ^ Enough for the transactions of a block not to share any.
  , tps      :: Scientific.Scientific
  }

-- | About 2 transactions per block, at a block every 20 seconds on average:
-- 390 KB of reference scripts with 13 inputs of 15 KB, well below the limit
-- of 1 MiB per block (at 4 transactions per block, the blocks after longer
-- gaps reach it), and room for 13 variants each in the 200.
belowLimit :: Integer -> Maybe Integer -> Types.ScriptSource -> Integer -> Workload
belowLimit k bytes src n = Workload
  { inputs = k, size = bytes, source = src, loops = n, variants = 200, tps = 0.1 }

-- | More transactions than a block can carry at 4 MiB of reference scripts
-- (21 of 195 KB), and variants for 2 such blocks.
aboveLimit :: Workload
aboveLimit = Workload
  { inputs = 13, size = Just 15_000, source = Types.ScriptByReference, loops = 0, variants = 600, tps = 1.5 }

-- | The fee of a transaction of the workload, with margins: its reference
-- scripts at the v12 tiers, its execution at mainnet prices (LoopV3 takes
-- about 614,000 steps and 2,405 memory units per iteration, 183 lovelace),
-- and its size.
fee :: Workload -> Integer
fee w =
    500_000 + 100_000 * inputs w
  + 200 * inputs w * loops w
  + case source w of
      Types.ScriptByReference -> refScriptFee $ inputs w * fromMaybe 200 (size w)
      Types.ScriptInWitness   -> 44 * inputs w * fromMaybe 200 (size w)

-- | The reference script fee of the v12 genesis: 15 lovelace per byte for the
-- first 25,600 bytes, and 1.2 times as much for each next 25,600.
refScriptFee :: Integer -> Integer
refScriptFee bytes = ceiling (go (fromInteger bytes) 15)
 where
  go :: Rational -> Rational -> Rational
  go n price
    | n <= stride = n * price
    | otherwise   = stride * price + go (n - stride) (price * 6 / 5)
  stride = 25_600

workload :: Workload -> Types.Profile -> Types.Profile
workload w =
    P.plutusVariantGroup 1 . P.plutusVariants (source w) (variants w) (size w)
  . P.plutusType "FixedLoop" . P.plutusScript "LoopV3" . P.redeemerInt (1_000_000 + loops w)
  . P.txIn (inputs w) . P.txOut 1 . P.txFee (fee w) . P.tps (tps w)

--------------------------------------------------------------------------------

-- | 2 nodes on loopback, the v12 genesis with blocks of 64 KB, 4 epochs of 10
-- minutes. Publishing the variants takes a block of 64 KB for every 4 of
-- 15 KB: 200 of them take about 16 minutes.
local :: Types.Profile -> Types.Profile
local =
    E.baseNoDataset . V.datasetEmpty
  . P.v12Preview . P.v11Preview . V.genesisVariantVoltaire64k
  . V.timescaleCompressed . P.generatorEpochs 4 . P.shutdownOnOff
  . P.uniCircle . V.hosts 2 . P.loopback
  . P.traceForwardingOn
  . P.analysisSizeSmall
  . P.desc "Reference script sweep, 2 nodes, 4 compressed epochs"

-- | The perf cluster with the v12 genesis, 5 epochs, the last 2 analysed.
cloud :: Types.Profile -> Types.Profile
cloud =
    C.baseVoltaire . V.datasetOct2021 . V.fundsDouble
  . V.timescaleModel . P.shutdownOnSlot 40_000 . P.generatorEpochs 5 . P.analysisEpoch3Plus
  . C.nomadPerf . P.dreps 10_000
  . P.v12Preview . P.v11Preview
  . P.analysisSizeSmall
  . P.desc "AWS c5-2xlarge cluster dataset, 5 ep, reference script sweep"

profilesRefScriptSweep :: [Types.Profile]
profilesRefScriptSweep =
  -- The point's genesis overlays after the cluster's, which set the limits it raises.
  [ P.empty & genesis . cluster . P.name ("refscript-" ++ point ++ "-" ++ suffix) . workload (scale w)
  | (cluster, suffix, scale) <- [(local, "local", localScale), (cloud, "nomadperf", id)]
  , (point, w, genesis) <- points
  ]
 where
  -- Locally, a quarter of the variants, to publish them in a few minutes; the
  -- blocks of family L then share some, which the ledger decodes only once.
  localScale w = w {variants = variants w `div` 4}
  byRef  = Types.ScriptByReference
  inline = Types.ScriptInWitness
  points =
    -- A: the reference script bytes per transaction.
    [ ("a-k1-min",        belowLimit 1  Nothing       byRef  0, id)
    , ("a-k1-15k",        belowLimit 1  (Just 15_000) byRef  0, id)
    , ("a-k4-min",        belowLimit 4  Nothing       byRef  0, id)
    , ("a-k4-15k",        belowLimit 4  (Just 15_000) byRef  0, id)
    , ("a-k13-min",       belowLimit 13 Nothing       byRef  0, id)
    , ("a-k13-15k",       belowLimit 13 (Just 15_000) byRef  0, id)
    , ("a-k1-15k-inline", belowLimit 1  (Just 15_000) inline 0, id)
    -- E: the execution steps per transaction; "a-k1-min" is its point without loops.
    , ("e-loops800",      belowLimit 1  Nothing       byRef  800,  id)
    , ("e-loops3200",     belowLimit 1  Nothing       byRef  3200, id)
    , ("e-loops6400",     belowLimit 1  Nothing       byRef  6400, id)
    -- L: blocks full of reference scripts, at the limit and above.
    , ("l-1mib",          aboveLimit, id)
    , ("l-2mib",          aboveLimit, P.refScriptBlockDouble)
    , ("l-4mib",          aboveLimit, P.refScriptBlockQuadruple)
    ]
