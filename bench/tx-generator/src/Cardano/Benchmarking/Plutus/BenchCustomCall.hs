{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Benchmarking.Plutus.BenchCustomCall
    ( BenchCustomArg
    , BenchCommand(..)
    , BenchCustomData(..)
    , customCallScriptV1
    , customCallScriptV2
    ) where

import           Cardano.Api (PlutusScript, PlutusScriptV1, PlutusScriptV2)
import           Cardano.Api.Shelley (PlutusScript (..))
import           Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V1.Ledger.Api as PlutusV1
import qualified Plutus.V2.Ledger.Api as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude as Plutus hiding (Semigroup (..), (.))
import           Prelude as Haskell (Show, (.))


type BenchCustomArg = (BenchCommand, [BenchCustomData])

data BenchCommand
    = EvalSpine
    | EvalValues
    | EvalAndValidate
    deriving Haskell.Show

data BenchCustomData
    = BenchNone
    | BenchInt Integer
    | BenchString BuiltinByteString
    | BenchSum Integer [Integer]
    | BenchConcat BuiltinByteString [BuiltinByteString]

instance Plutus.Eq BenchCustomData where
  BenchNone         == BenchNone          = True
  BenchInt i        == BenchInt i'        = i == i'
  BenchSum i is     == BenchSum i' is'    = i == i' && is == is'
  BenchString s     == BenchString s'     = s == s'
  BenchConcat s ss  == BenchConcat s' ss' = s == s' && ss == ss'
  _                 == _                  = False

PlutusTx.unstableMakeIsData ''BenchCustomData
PlutusTx.unstableMakeIsData ''BenchCommand


{-# INLINEABLE mkValidator #-}
mkValidator :: (BuiltinData -> BenchCustomArg) -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator unwrap datum_ redeemer_ _txContext =
  let
    result = case cmd of
      EvalSpine       -> length redeemerArg == length datumArg
      EvalValues      -> redeemerArg == datumArg
      EvalAndValidate -> all validateValue redeemerArg && redeemerArg == datumArg
  in if result then () else error ()
  where
    datum, redeemer :: BenchCustomArg
    datum     = unwrap datum_
    redeemer  = unwrap redeemer_

    datumArg            = snd datum
    (cmd, redeemerArg)  = redeemer

    validateValue :: BenchCustomData -> Bool
    validateValue (BenchSum i is)     = i == sum is
    validateValue (BenchConcat s ss)  = s == mconcat ss
    validateValue _                   = True

{-# INLINEABLE mkValidatorV1 #-}
mkValidatorV1 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorV1 = mkValidator PlutusV1.unsafeFromBuiltinData

{-# INLINEABLE mkValidatorV2 #-}
mkValidatorV2 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorV2 = mkValidator PlutusV2.unsafeFromBuiltinData

validatorV1 :: PlutusV1.Validator
validatorV1 = PlutusV1.mkValidatorScript $$(PlutusTx.compile [|| mkValidatorV1 ||])

validatorV2 :: PlutusV2.Validator
validatorV2 = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkValidatorV2 ||])

scriptV1 :: PlutusV1.Script
scriptV1 = PlutusV1.unValidatorScript validatorV1

scriptV1SBS :: SBS.ShortByteString
scriptV1SBS = SBS.toShort . LBS.toStrict $ serialise scriptV1

customCallScriptV1 :: PlutusScript PlutusScriptV1
customCallScriptV1 = PlutusScriptSerialised scriptV1SBS

scriptV2 :: PlutusV2.Script
scriptV2 = PlutusV2.unValidatorScript validatorV2

scriptV2SBS :: SBS.ShortByteString
scriptV2SBS = SBS.toShort . LBS.toStrict $ serialise scriptV2

customCallScriptV2 :: PlutusScript PlutusScriptV2
customCallScriptV2 = PlutusScriptSerialised scriptV2SBS
