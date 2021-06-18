{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.PlutusExample.Typed.DatumRedeemerGuess
  ( MyTypedDatum(..)
  , MyTypedRedeemer(..)
  , typeddatumRedeemerGuessScript
  , typeddatumRedeemerGuessScriptAsShortBs
  ) where

import           Prelude hiding (($), (&&), (==))

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import           Ledger.Contexts (ScriptContext (..))
import qualified Ledger.Typed.Scripts as Scripts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           PlutusTx (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)

newtype MyTypedDatum = MyTypedDatum Integer
newtype MyTypedRedeemer = MyTypedRedeemer Integer

PlutusTx.unstableMakeIsData ''MyTypedDatum
PlutusTx.unstableMakeIsData ''MyTypedRedeemer

data ExampleTypedValidator
instance Scripts.ValidatorTypes ExampleTypedValidator where
    type instance DatumType ExampleTypedValidator    = MyTypedDatum
    type instance RedeemerType ExampleTypedValidator = MyTypedRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: MyTypedDatum-> MyTypedRedeemer -> ScriptContext -> Bool
mkValidator (MyTypedDatum d) (MyTypedRedeemer r) _ =
  d == 42 && r == 42

inst :: Scripts.TypedValidator ExampleTypedValidator
inst = Scripts.mkTypedValidator @ExampleTypedValidator
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @MyTypedDatum @MyTypedRedeemer

validator :: Plutus.Validator
validator = Scripts.validatorScript inst

script :: Plutus.Script
script = Plutus.unValidatorScript validator

typeddatumRedeemerGuessScriptAsShortBs :: SBS.ShortByteString
typeddatumRedeemerGuessScriptAsShortBs = SBS.toShort . LBS.toStrict $ serialise script

typeddatumRedeemerGuessScript :: PlutusScript PlutusScriptV1
typeddatumRedeemerGuessScript = PlutusScriptSerialised typeddatumRedeemerGuessScriptAsShortBs

