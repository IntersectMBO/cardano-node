{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

-- | LoopV3 on Plutus V4: the same loop, decoding the redeemer from the V4
-- script context.
module Cardano.Benchmarking.PlutusScripts.LoopV4 (script) where

import           Cardano.Api (PlutusScriptVersion (PlutusScriptV4))
import           Cardano.Benchmarking.ScriptAPI (PlutusBenchScript, mkPlutusBenchScript)
import           Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL), Loc (loc_module), qLocation)
import           PlutusLedgerApi.Common (serialiseCompiledCode)
import qualified PlutusTx (compile)
import           PlutusTx.Builtins (unsafeDataAsI)
import qualified PlutusTx.Builtins.Internal as BI (BuiltinList, head, tail, unitval,
                   unsafeDataAsList)
import           PlutusTx.Prelude as Plutus hiding (Semigroup (..), unless, (.), (<$>))
import           Prelude hiding (pred, ($), (&&), (<), (==))


script :: PlutusBenchScript
script = mkPlutusBenchScript
           $(LitE . StringL . loc_module <$> qLocation)
           PlutusScriptV4
           (serialiseCompiledCode $$(PlutusTx.compile [|| mkValidator ||]))


{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinUnit
mkValidator arg =
  if red_n < 1000000
    then traceError "redeemer is < 1000000" -- large number ensures same bitsize for all counter values
    else loop red_n
  where
    -- The V4 script context is a list, not a constructor:
    -- [txInfo, redeemer, scriptInfo, scriptHash]. Decode it lazily up to the
    -- redeemer, as LoopV3 does.
    fields :: BI.BuiltinList BuiltinData
    fields = BI.unsafeDataAsList arg

    redeemer :: BuiltinData
    redeemer = BI.head (BI.tail fields)

    red_n = unsafeDataAsI redeemer

    loop i = if i == 1000000 then BI.unitval else loop (pred i)
