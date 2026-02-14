{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.PlutusScripts.CustomCallV3 (script) where

import           Cardano.Api (PlutusScript (..), PlutusScriptV3,
                   PlutusScriptVersion (..),Script (..), toScriptInAnyLang)
import           Cardano.Benchmarking.PlutusScripts.CustomCallTypes
import           Cardano.Benchmarking.ScriptAPI
import qualified Data.ByteString.Short as SBS
import           Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL), Loc (loc_module), qLocation)
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusTx (compile)
import qualified PlutusTx.Builtins.Internal as BI (BuiltinList, head, snd, tail, unitval,
                   unsafeDataAsConstr)
import           PlutusTx.Foldable (sum)
import           PlutusTx.List (all, length)
import           PlutusTx.Prelude as Plutus hiding (Semigroup (..), (.), (<$>))
import           Prelude as Haskell (String, (.), (<$>))


script :: PlutusBenchScript
script = mkPlutusBenchScript scriptName (toScriptInAnyLang (PlutusScript PlutusScriptV3 scriptSerialized))

scriptName :: Haskell.String
scriptName
  = prepareScriptName $(LitE . StringL . loc_module <$> qLocation)


instance Plutus.Eq CustomCallData where
  CCNone            == CCNone           = True
  CCInteger i       == CCInteger i'     = i == i'
  CCSum i is        == CCSum i' is'     = i == i' && is == is'
  CCByteString s    == CCByteString s'  = s == s'
  CCConcat s ss     == CCConcat s' ss'  = s == s' && ss == ss'
  _                 == _                = False

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinUnit
mkValidator arg =
  let
    result = case cmd of
      EvalSpine       -> length redeemerArg == length datumArg
      EvalValues      -> redeemerArg == datumArg
      EvalAndValidate -> all validateValue redeemerArg && redeemerArg == datumArg
  in if result then BI.unitval else error ()
  where
    -- lazily decode script context up to datum and redeemer
    constrArgs :: BuiltinData -> BI.BuiltinList BuiltinData
    constrArgs = BI.snd . BI.unsafeDataAsConstr

    ctxFields :: BI.BuiltinList BuiltinData
    ctxFields = constrArgs arg

    datum :: BuiltinData
    datum = BI.head ctxFields

    redeemerFollowedByScriptInfo :: BI.BuiltinList BuiltinData
    redeemerFollowedByScriptInfo = BI.tail ctxFields

    redeemer :: BuiltinData
    redeemer = BI.head redeemerFollowedByScriptInfo

    datumArg            = snd (unwrap datum)
    (cmd, redeemerArg)  = unwrap redeemer

    validateValue :: CustomCallData -> Bool
    validateValue (CCSum i is)      = i == sum is
    validateValue (CCConcat s ss)   = s == mconcat ss
    validateValue _                 = True

{-# INLINABLE unwrap #-}
unwrap :: BuiltinData -> CustomCallArg
unwrap  = PlutusV3.unsafeFromBuiltinData

customCallScriptShortBs :: SBS.ShortByteString
customCallScriptShortBs = PlutusV3.serialiseCompiledCode $$(PlutusTx.compile [|| mkValidator ||])

scriptSerialized :: PlutusScript PlutusScriptV3
scriptSerialized = PlutusScriptSerialised customCallScriptShortBs
