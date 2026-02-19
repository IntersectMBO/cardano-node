{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Benchmarking.PlutusScripts.LoopV3 (script) where

import           Cardano.Api (PlutusScriptVersion (PlutusScriptV3))
import           Cardano.Benchmarking.ScriptAPI
import qualified Data.ByteString.Short as SBS
import           Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL), Loc (loc_module), qLocation)
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusTx (compile)
import           PlutusTx.Builtins (unsafeDataAsI)
import qualified PlutusTx.Builtins.Internal as BI (BuiltinList, head, snd, tail, unitval,
                   unsafeDataAsConstr)
import           PlutusTx.Prelude as Plutus hiding (Semigroup (..), unless, (.), (<$>))
import           Prelude hiding (pred, ($), (&&), (<), (==))


script :: PlutusBenchScript
script = mkPlutusBenchScriptFromCompiled
           PlutusScriptV3
           $(LitE . StringL . loc_module <$> qLocation)
           loopScriptShortBs


{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinUnit
mkValidator arg =
  if red_n < 1000000
    then traceError "redeemer is < 1000000" -- large number ensures same bitsize for all counter values
    else loop red_n
  where
    -- lazily decode script context up to redeemer, which is less expensive and results in much smaller tx size
    constrArgs :: BuiltinData -> BI.BuiltinList BuiltinData
    constrArgs = BI.snd . BI.unsafeDataAsConstr

    redeemerFollowedByScriptInfo :: BI.BuiltinList BuiltinData
    redeemerFollowedByScriptInfo = BI.tail (constrArgs arg)

    redeemer :: BuiltinData
    redeemer = BI.head redeemerFollowedByScriptInfo

    red_n = unsafeDataAsI redeemer

    loop i = if i == 1000000 then BI.unitval else loop (pred i)

loopScriptShortBs :: SBS.ShortByteString
loopScriptShortBs = PlutusV3.serialiseCompiledCode $$(PlutusTx.compile [|| mkValidator ||])
