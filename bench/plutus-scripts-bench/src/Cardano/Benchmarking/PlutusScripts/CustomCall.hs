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
import qualified Data.ByteString.Short as SBS
import qualified PlutusLedgerApi.V2 as PlutusV2
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

customCallScriptShortBs :: SBS.ShortByteString
customCallScriptShortBs = PlutusV2.serialiseCompiledCode $$(PlutusTx.compile [|| mkValidator ||])

scriptSerialized :: PlutusScript PlutusScriptV2
scriptSerialized = PlutusScriptSerialised customCallScriptShortBs
