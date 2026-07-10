{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Fixed-iteration benchmark loop exercising the 'expModInteger' builtin,
-- ported from plutus-scripts-e2e's PlutusScripts.Batch6.ExpModInteger.Common.
module Cardano.Benchmarking.PlutusScripts.ExpModInteger
  ( mkSimpleExpModIntegerPolicyBench
  , compiledSimpleExpModIntegerPolicyBench
  , script
  ) where

import           Cardano.Api (PlutusScriptVersion (PlutusScriptV3))
import           Cardano.Benchmarking.ScriptAPI (PlutusBenchScript, mkPlutusBenchScript)
import           Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL), Loc (loc_module), qLocation)
import           PlutusLedgerApi.Common (serialiseCompiledCode)
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusTx
import           PlutusTx.Code (CompiledCode)
import qualified PlutusTx.Builtins as BI (expModInteger)
import qualified PlutusTx.Builtins.Internal as BI (BuiltinList, head, snd, tail,
                   unitval, unsafeDataAsConstr)
import           PlutusTx.Prelude as P hiding (Semigroup (..), (.), (<$>))
import           Prelude as Haskell ((.), (<$>))


data SimpleParams = SimpleParams
  { a      :: P.Integer -- Value to be exponentiated
  , e      :: P.Integer -- Exponent
  , m      :: P.Integer -- Modulus
  , result :: P.Integer -- Expected result, -1 for failing tests
  }

-- | `a` and `m`
data P = P Integer Integer
PlutusTx.unstableMakeIsData ''P

-- | `e` and `r`
data S = S Integer Integer

-- | Correctness by induction:
-- base case:
--   a^{e₀} mod m = r₀, established via a single `expModInteger` call
--   (e₀ = m - 2, so a^{e₀} mod m is a's modular inverse by Fermat's little
--   theorem, since m = 2^61 - 1 is prime -- a realistic, non-arbitrary exponent size)
-- Inductive step:
--   a^{e_{i+1}} mod m = r_{i+1}
--   a^{e_i + 1} mod m = r_{i+1}
--   (a^{e_i} mod m) * a mod m = r_{i+1}
--   r_i * a mod m = r_{i+1} ✔
--
-- `e` only ever increments by 1 per iteration (never doubles), so its
-- bit-length -- and thus `expModInteger`'s per-call cost -- stays essentially
-- constant across the whole benchmark run, however many iterations it runs for.
-- This also keeps every iteration's arguments genuinely distinct (dependent on
-- the loop counter), so no compiler optimisation (e.g. full laziness) can
-- legally hoist/common up the `expModInteger` call across iterations.
interp :: P -> S -> SimpleParams
interp (P a m) (S e r) = SimpleParams a e m r

seed :: P -> S
seed (P a m) =
  let e0 = m - 2
  in S e0 (BI.expModInteger a e0 m)

next :: P -> S -> S
next (P a m) (S e r) = S (e + 1) ((r * a) `modulo` m)


-- while n > 1000000, interp (param, st) and check expModInteger on that input. Fail if incorrect, otherwise loop
-- with n := n - 1, st := next param st
{-# INLINEABLE mkSimpleExpModIntegerPolicyBench #-}
mkSimpleExpModIntegerPolicyBench :: P.BuiltinData -> P.BuiltinUnit
mkSimpleExpModIntegerPolicyBench ctx = go (seed red_param) red_n
 where
  -- lazily decode script context up to redeemer, which is less expensive and results in much smaller tx size
  constrArgs :: P.BuiltinData -> BI.BuiltinList P.BuiltinData
  constrArgs = BI.snd . BI.unsafeDataAsConstr

  redeemerFollowedByScriptInfo :: BI.BuiltinList P.BuiltinData
  redeemerFollowedByScriptInfo = BI.tail (constrArgs ctx)

  redeemer :: P.BuiltinData
  redeemer = BI.head redeemerFollowedByScriptInfo

  -- `n` must come first in the redeemer's Data encoding: tx-generator's
  -- auto-budget calibration (Cardano.TxGenerator.PlutusContext.scriptDataModifyNumber)
  -- bumps whichever ScriptDataNumber it encounters first in a depth-first walk.
  red_n     :: Integer
  red_param :: P
  (red_n, red_param) = PlutusV3.unsafeFromBuiltinData redeemer

  go st n =
    if n P.== 1000000
      then BI.unitval
      else
        let SimpleParams{..} = interp red_param st
         in if BI.expModInteger a e m P.== result
              then let !st' = next red_param st in go st' (P.pred n)
              else P.traceError "mkSimpleExpModIntegerPolicyBench"

compiledSimpleExpModIntegerPolicyBench
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
compiledSimpleExpModIntegerPolicyBench =
  $$(PlutusTx.compile [|| mkSimpleExpModIntegerPolicyBench ||])

script :: PlutusBenchScript
script = mkPlutusBenchScript
           $(LitE . StringL . loc_module <$> qLocation)
           PlutusScriptV3
           (serialiseCompiledCode compiledSimpleExpModIntegerPolicyBench)
