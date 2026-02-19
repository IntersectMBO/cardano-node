{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

-- PlutusV2 must be compiled using plc 1.0
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Cardano.Benchmarking.PlutusScripts.SchnorrSecp256k1Loop (script) where

import           Cardano.Api (PlutusScriptVersion (PlutusScriptV2))
import           Cardano.Benchmarking.ScriptAPI
import           Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL), Loc (loc_module), qLocation)
import qualified PlutusLedgerApi.V2 as PlutusV2
import qualified PlutusTx (compile)
import qualified PlutusTx.Builtins as BI
import           PlutusTx.Prelude as P hiding (Semigroup (..), (.), (<$>))
import           Prelude as Haskell ((.), (<$>))


script :: PlutusBenchScript
script = mkPlutusBenchScriptFromCompiled
           PlutusScriptV2
           $(LitE . StringL . loc_module <$> qLocation)
           $$(PlutusTx.compile [|| mkValidator ||])


{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _datum red _txContext =
  case PlutusV2.fromBuiltinData red of
    Nothing -> P.traceError "Trace error: Invalid redeemer"
    Just (n, vkey, msg, sig) ->
      if n < (1000000 :: Integer) -- large number ensures same bitsize for all counter values
      then traceError "redeemer is < 1000000"
      else loop n vkey msg sig
  where
    loop i v m s
      | i == 1000000 = ()
      | BI.verifySchnorrSecp256k1Signature v m s = loop (pred i) v m s
      | otherwise = P.traceError "Trace error: Schnorr validation failed"

