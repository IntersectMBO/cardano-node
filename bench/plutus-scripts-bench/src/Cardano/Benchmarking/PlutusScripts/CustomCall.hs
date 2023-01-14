{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.PlutusScripts.CustomCall
  ( scriptName
  , scriptSerialized
  ) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Prelude as Haskell (String, (.), (<$>))

import           Cardano.Api (PlutusScript, PlutusScriptV2)
import           Cardano.Api.Shelley (PlutusScript (..))
import           Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V2.Ledger.Api as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude as Plutus hiding (Semigroup (..), (.), (<$>))

import           Cardano.Benchmarking.PlutusScripts.CustomCallTypes


scriptName :: Haskell.String
scriptName
  = $(LitE . StringL . loc_module <$> qLocation)


instance Plutus.Eq CustomCallData where
  CCNone            == CCNone           = True
  CCInteger i       == CCInteger i'     = i == i'
  CCSum i is        == CCSum i' is'     = i == i' && is == is'
  CCByteString s    == CCByteString s'  = s == s'
  CCConcat s ss     == CCConcat s' ss'  = s == s' && ss == ss'
  _                 == _                = False

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
    datum, redeemer :: CustomCallArg
    datum     = unwrap datum_
    redeemer  = unwrap redeemer_

    datumArg            = snd datum
    (cmd, redeemerArg)  = redeemer

    validateValue :: CustomCallData -> Bool
    validateValue (CCSum i is)      = i == sum is
    validateValue (CCConcat s ss)   = s == mconcat ss
    validateValue _                 = True

{-# INLINEABLE unwrap #-}
unwrap :: BuiltinData -> CustomCallArg
unwrap  = PlutusV2.unsafeFromBuiltinData
-- Note: type-constraining unsafeFromBuiltinData decreases script's execution units.

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

script :: PlutusV2.Script
script = PlutusV2.unValidatorScript validator

customCallScriptShortBs :: SBS.ShortByteString
customCallScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

scriptSerialized :: PlutusScript PlutusScriptV2
scriptSerialized = PlutusScriptSerialised customCallScriptShortBs
