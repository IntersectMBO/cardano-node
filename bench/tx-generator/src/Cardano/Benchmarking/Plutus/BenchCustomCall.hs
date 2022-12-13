{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Benchmarking.Plutus.BenchCustomCall
    ( BenchCustomData(..) 
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
import           PlutusTx.Prelude as P hiding (Semigroup (..), unless, (.))
import           Prelude ((.))


data BenchCustomData
    = BenchNone
    | BenchInt Integer
    | BenchString BuiltinByteString
    | BenchSum Integer [Integer]
    | BenchConcat BuiltinByteString [BuiltinByteString]

instance P.Eq BenchCustomData where
  BenchNone         == BenchNone          = True
  BenchInt i        == BenchInt i'        = i == i'
  BenchSum i is     == BenchSum i' is'    = i == i' && is == is'
  BenchString s     == BenchString s'     = s == s'
  BenchConcat s ss  == BenchConcat s' ss' = s == s' && ss == ss'
  _                 == _                  = False

PlutusTx.unstableMakeIsData ''BenchCustomData


{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator datum redeemer _txContext =
  if all validateValue redeemerArg && redeemerArg == datumArg
  then ()
  else error ()
  where
    datumArg, redeemerArg :: [BenchCustomData]
    datumArg    = PlutusV2.unsafeFromBuiltinData datum
    redeemerArg = PlutusV2.unsafeFromBuiltinData redeemer

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
