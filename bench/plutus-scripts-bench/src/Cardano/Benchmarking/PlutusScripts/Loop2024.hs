{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

-- PlutusV1 must be compiled using plc 1.0
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Cardano.Benchmarking.PlutusScripts.Loop2024 (script) where

import           Cardano.Api (PlutusScript (..), PlutusScriptV1,
                   PlutusScriptVersion (..), Script (..), toScriptInAnyLang)
import           Cardano.Benchmarking.ScriptAPI
import qualified Data.ByteString.Short as SBS
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified PlutusLedgerApi.V1 as PlutusV1
import           PlutusTx
import           PlutusTx.Builtins (unsafeDataAsI)
import           PlutusTx.Prelude hiding (Semigroup (..), unless, (.), (<$>))
import           Prelude hiding (pred, ($), (&&), (<), (==))


scriptName :: String
scriptName
  = prepareScriptName $(LitE . StringL . loc_module <$> qLocation)

script :: PlutusBenchScript
script = mkPlutusBenchScript scriptName (toScriptInAnyLang (PlutusScript PlutusScriptV1 scriptSerialized))


{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _datum redeemer _txContext
  = if n < 1000000
       then traceError "redeemer is < 1000000"
       else loop n
  where
    n = unsafeDataAsI redeemer
    loop i = if i == 1000000 then () else loop $ pred i

loopScriptShortBs :: SBS.ShortByteString
loopScriptShortBs = PlutusV1.serialiseCompiledCode $$(PlutusTx.compile [|| mkValidator ||])

scriptSerialized :: PlutusScript PlutusScriptV1
scriptSerialized = PlutusScriptSerialised loopScriptShortBs
