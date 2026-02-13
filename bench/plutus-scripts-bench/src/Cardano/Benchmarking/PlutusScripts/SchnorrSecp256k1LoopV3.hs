{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Benchmarking.PlutusScripts.SchnorrSecp256k1LoopV3 (script) where

import           Cardano.Api (PlutusScript (..), PlutusScriptV3,
                   PlutusScriptVersion (..), Script (..), toScriptInAnyLang)
import           Cardano.Benchmarking.ScriptAPI
import qualified Data.ByteString.Short as SBS
import           Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL), Loc (loc_module), qLocation)
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusTx (compile)
import qualified PlutusTx.Builtins as Builtins
import qualified PlutusTx.Builtins.Internal as BI (BuiltinList, head, snd, tail, unitval,
                   unsafeDataAsConstr)
import           PlutusTx.Prelude as P hiding (Semigroup (..), (.), (<$>))
import           Prelude as Haskell (String, (.), (<$>))


scriptName :: Haskell.String
scriptName
  = prepareScriptName $(LitE . StringL . loc_module <$> qLocation)

script :: PlutusBenchScript
script = mkPlutusBenchScript scriptName (toScriptInAnyLang (PlutusScript PlutusScriptV3 scriptSerialized))


{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinUnit
mkValidator arg =
  if red_n < (1000000 :: Integer) -- large number ensures same bitsize for all counter values
    then traceError "redeemer is < 1000000"
    else loop red_n red_vkey red_msg red_sig
  where
    -- lazily decode script context up to redeemer, which is less expensive and results in much smaller tx size
    constrArgs :: BuiltinData -> BI.BuiltinList BuiltinData
    constrArgs = BI.snd . BI.unsafeDataAsConstr

    redeemerFollowedByScriptInfo :: BI.BuiltinList BuiltinData
    redeemerFollowedByScriptInfo = BI.tail (constrArgs arg)

    redeemer :: BuiltinData
    redeemer = BI.head redeemerFollowedByScriptInfo

    red_n :: Integer
    red_vkey :: BuiltinByteString
    red_msg :: BuiltinByteString
    red_sig :: BuiltinByteString
    (red_n, red_vkey, red_msg, red_sig) = PlutusV3.unsafeFromBuiltinData redeemer

    loop i v m s
      | i == 1000000 = BI.unitval
      | Builtins.verifySchnorrSecp256k1Signature v m s = loop (pred i) v m s
      | otherwise = P.traceError "Trace error: Schnorr validation failed"

schnorrLoopScriptShortBs :: SBS.ShortByteString
schnorrLoopScriptShortBs = PlutusV3.serialiseCompiledCode $$(PlutusTx.compile [|| mkValidator ||])

scriptSerialized :: PlutusScript PlutusScriptV3
scriptSerialized = PlutusScriptSerialised schnorrLoopScriptShortBs

