{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Benchmarking.PlutusScripts.Ripemd160 (script) where

import           Cardano.Api (PlutusScriptVersion (PlutusScriptV3))
import           Cardano.Benchmarking.ScriptAPI
import           Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL), Loc (loc_module), qLocation)
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusTx (compile)
import qualified PlutusTx.Builtins.Internal as BI (BuiltinList, head, snd, tail, unitval,
                   unsafeDataAsConstr)
import           PlutusTx.Prelude as Tx hiding (Semigroup (..), (.), (<$>))
import           Prelude as Haskell ((.), (<$>))


script :: PlutusBenchScript
script = mkPlutusBenchScriptFromCompiled
           PlutusScriptV3
           $(LitE . StringL . loc_module <$> qLocation)
           $$(PlutusTx.compile [|| mkValidator ||])


{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinUnit
mkValidator arg =
  if red_n < 1000000 -- large number ensures same bitsize for all counter values
    then traceError "redeemer is < 1000000"
    else loop red_n red_b
  where
    -- lazily decode script context up to redeemer, which is less expensive and results in much smaller tx size
    constrArgs :: BuiltinData -> BI.BuiltinList BuiltinData
    constrArgs = BI.snd . BI.unsafeDataAsConstr

    redeemerFollowedByScriptInfo :: BI.BuiltinList BuiltinData
    redeemerFollowedByScriptInfo = BI.tail (constrArgs arg)

    redeemer :: BuiltinData
    redeemer = BI.head redeemerFollowedByScriptInfo

    red_n :: Integer
    red_b :: BuiltinByteString
    (red_n, red_b) = PlutusV3.unsafeFromBuiltinData redeemer

    loop i res
      | i == 1000000 = BI.unitval
      | otherwise    = let !res' = Tx.ripemd_160 res in loop (pred i) res'

