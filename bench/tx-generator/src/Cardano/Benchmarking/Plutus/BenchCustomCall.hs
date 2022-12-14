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
    , customCallScript
    , customCallScriptShortBs
    ) where

import           Cardano.Api (PlutusScript, PlutusScriptV2)
import           Cardano.Api.Shelley (PlutusScript (..))
import           Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
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
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator datum_ redeemer_ _txContext =
  let
    result = case cmd of
      EvalSpine       -> length redeemerArg == length datumArg
      EvalValues      -> redeemerArg == datumArg
      EvalAndValidate -> all validateValue redeemerArg && redeemerArg == datumArg
  in if result then () else error ()
  where
    datum, redeemer :: BenchCustomArg
    datum     = PlutusV2.unsafeFromBuiltinData datum_
    redeemer  = PlutusV2.unsafeFromBuiltinData redeemer_

    datumArg            = snd datum
    (cmd, redeemerArg)  = redeemer

    validateValue :: BenchCustomData -> Bool
    validateValue (BenchSum i is)     = i == sum is
    validateValue (BenchConcat s ss)  = s == mconcat ss
    validateValue _                   = True

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

script :: PlutusV2.Script
script = PlutusV2.unValidatorScript validator

customCallScriptShortBs :: SBS.ShortByteString
customCallScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

customCallScript :: PlutusScript PlutusScriptV2
customCallScript = PlutusScriptSerialised customCallScriptShortBs
