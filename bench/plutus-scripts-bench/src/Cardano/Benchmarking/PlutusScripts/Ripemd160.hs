{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Benchmarking.PlutusScripts.Ripemd160 (script) where

import           Cardano.Api (PlutusScript (..), PlutusScriptV3,
                   PlutusScriptVersion (..), Script (..), toScriptInAnyLang)
import           Cardano.Benchmarking.ScriptAPI
import qualified Data.ByteString.Short as SBS
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusTx
import qualified PlutusTx.Builtins.Internal as BI (BuiltinList, head, snd, tail, unitval,
                   unsafeDataAsConstr)
import           PlutusTx.Prelude as Tx hiding (Semigroup (..), (.), (<$>))
import           Prelude as Haskell (String, (.), (<$>))


scriptName :: Haskell.String
scriptName
  = prepareScriptName $(LitE . StringL . loc_module <$> qLocation)

script :: PlutusBenchScript
script = mkPlutusBenchScript scriptName (toScriptInAnyLang (PlutusScript PlutusScriptV3 scriptSerialized))


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

ripEmd160ShortBs :: SBS.ShortByteString
ripEmd160ShortBs = PlutusV3.serialiseCompiledCode $$(PlutusTx.compile [|| mkValidator ||])

scriptSerialized :: PlutusScript PlutusScriptV3
scriptSerialized = PlutusScriptSerialised ripEmd160ShortBs
