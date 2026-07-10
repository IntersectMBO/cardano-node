{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Fixed-iteration benchmark loop exercising the 'expModInteger' builtin,
-- ported from plutus-scripts-e2e's PlutusScripts.Batch6.ExpModInteger.Common.
module Cardano.Benchmarking.PlutusScripts.ExpModInteger
  ( script
  ) where

import           Cardano.Api (PlutusScriptVersion (PlutusScriptV3))
import           Cardano.Benchmarking.ScriptAPI (PlutusBenchScript, mkPlutusBenchScript)
import           Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL), Loc (loc_module), qLocation)
import           PlutusLedgerApi.Common (serialiseCompiledCode)
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusTx
import qualified PlutusTx.Builtins as BI (expModInteger)
import qualified PlutusTx.Builtins.Internal as BI (BuiltinList, head, snd, tail,
                   unitval, unsafeDataAsConstr)
import           PlutusTx.Prelude as P hiding (Semigroup (..), (.), (<$>))
import           Prelude as Haskell ((.), (<$>))


-- while n > 1000000, force-evaluate expModInteger a e m (discarding the
-- result) and loop with n := n - 1, e := e + 1
{-# INLINEABLE mkSimpleExpModIntegerPolicyBench #-}
mkSimpleExpModIntegerPolicyBench :: P.BuiltinData -> P.BuiltinUnit
mkSimpleExpModIntegerPolicyBench ctx =
  if red_n P.< 1000000 -- large number ensures same bitsize for all counter values
    then P.traceError "redeemer is < 1000000"
    else go e0 red_n
 where
  -- lazily decode script context up to redeemer, which is less expensive and results in much smaller tx size
  constrArgs :: P.BuiltinData -> BI.BuiltinList P.BuiltinData
  constrArgs = BI.snd . BI.unsafeDataAsConstr

  redeemerFollowedByScriptInfo :: BI.BuiltinList P.BuiltinData
  redeemerFollowedByScriptInfo = BI.tail (constrArgs ctx)

  redeemer :: P.BuiltinData
  redeemer = BI.head redeemerFollowedByScriptInfo

  -- `n` must come first in the redeemer's (flat) Data encoding: tx-generator's
  -- auto-budget calibration (Cardano.TxGenerator.PlutusContext.scriptDataModifyNumber)
  -- bumps whichever ScriptDataNumber it encounters first in a depth-first walk.
  red_n :: Integer
  red_a :: Integer
  red_m :: Integer
  (red_n, red_a, red_m) = PlutusV3.unsafeFromBuiltinData redeemer

  -- e₀ = m - 2, a realistic, non-arbitrary exponent size (a's modular inverse
  -- exponent by Fermat's little theorem, since m = 2^61 - 1 is prime).
  --
  -- `e` only ever increments by 1 per iteration (never doubles), so its
  -- bit-length -- and thus `expModInteger`'s per-call cost -- stays essentially
  -- constant across the whole benchmark run, however many iterations it runs for.
  -- This also keeps every iteration's arguments genuinely distinct (dependent on
  -- the loop counter), so no compiler optimisation (e.g. full laziness) can
  -- legally hoist/common up the `expModInteger` call across iterations.
  e0 :: Integer
  e0 = red_m - 2

  go :: Integer -> Integer -> P.BuiltinUnit
  go e n =
    if n P.== 1000000
      then BI.unitval
      else let !_ = BI.expModInteger red_a e red_m in go (e P.+ 1) (P.pred n)

script :: PlutusBenchScript
script = mkPlutusBenchScript
           $(LitE . StringL . loc_module <$> qLocation)
           PlutusScriptV3
           (serialiseCompiledCode $$(PlutusTx.compile [|| mkSimpleExpModIntegerPolicyBench ||]))
