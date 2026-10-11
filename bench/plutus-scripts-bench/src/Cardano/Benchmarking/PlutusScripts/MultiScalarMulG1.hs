{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Benchmarking.PlutusScripts.MultiScalarMulG1 (script) where

import           Cardano.Api (PlutusScriptVersion (PlutusScriptV3))
import           Cardano.Benchmarking.ScriptAPI (PlutusBenchScript, mkPlutusBenchScript)
import           Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL), Loc (loc_module), qLocation)
import           PlutusLedgerApi.Common (serialiseCompiledCode)
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusTx (compile)
import qualified PlutusTx.Builtins.Internal as BI (BuiltinList, head, snd, tail, unitval,
                   unsafeDataAsConstr)
import           PlutusTx.Builtins as BI (bls12_381_G1_multiScalarMul)
import           PlutusTx.Prelude as Tx hiding (Semigroup (..), (.), (<$>))
import           Prelude as Haskell ((.), (<$>))


script :: PlutusBenchScript
script = mkPlutusBenchScript
           $(LitE . StringL . loc_module <$> qLocation)
           PlutusScriptV3
           (serialiseCompiledCode $$(PlutusTx.compile [|| mkValidator ||]))

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinUnit
mkValidator arg =
  if red_n < 1000000 -- large number ensures same bitsize for all counter values
    then traceError "redeemer is < 1000000"
    else loop (fmap Tx.bls12_381_G1_uncompress red_bss) red_is red_n
  where
    -- lazily decode script context up to redeemer, which is less expensive and results in much smaller tx size
    constrArgs :: BuiltinData -> BI.BuiltinList BuiltinData
    constrArgs = BI.snd . BI.unsafeDataAsConstr

    redeemerFollowedByScriptInfo :: BI.BuiltinList BuiltinData
    redeemerFollowedByScriptInfo = BI.tail (constrArgs arg)

    redeemer :: BuiltinData
    redeemer = BI.head redeemerFollowedByScriptInfo

    red_n   :: Integer
    red_is  :: [Integer]
    red_bss :: [BuiltinByteString]
    (red_n, red_is, red_bss) = PlutusV3.unsafeFromBuiltinData redeemer

    -- see Note[1]
    loop points scalars n
      | n == 1000000 = BI.unitval
      | otherwise    = let !_ = BI.bls12_381_G1_multiScalarMul (n : scalars) points in loop points scalars (pred n)

{-
Note[1]:

  The benchmarking loop's counter will always be used as a nonce, prepended to the list of scalars.
  Hence, make sure that in the redeemer args,
    >> THE LIST OF SCALARS IS ALWAYS 1 ELEMENT SHORTER THAN THE LIST OF POINTS <<

  == Reason for Nonce-as-Head ('n : scalars'):
  1. Defeats Pippenger Bucket-Caching: Mutating a single scalar
     head element breaks the windowed bit-partitioning configuration. This forces
     the 'blst' library to perform full, un-cached linear combination logic from
     scratch rather than reusing pre-computed bucket structures.
  2. Minmize execution units to achieve 1.: Prepending a head nonce element
     guarantees a predictable O(1) overhead, focusing execution cost purely on the underlying
     curve arithmetic. Also, this guarantees a stable memory footprint.
-}
