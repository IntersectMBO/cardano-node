{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.PlutusExample.CustomDatumRedeemerGuess
  ( MyCustomDatum(..)
  , MyCustomRedeemer(..)
  , customGuessScript
  , customDatumRedeemerGuessScriptAsShortBs
  ) where

import           Prelude hiding (($), (&&), (==))

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import           Ledger.Contexts (ScriptContext (..))
import qualified Ledger.Typed.Scripts as Scripts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

newtype MyCustomDatum = MyCustomDatum Integer
newtype MyCustomRedeemer = MyCustomRedeemer Integer

PlutusTx.unstableMakeIsData ''MyCustomDatum
PlutusTx.unstableMakeIsData ''MyCustomRedeemer

data ExampleTypedValidator
instance Scripts.ValidatorTypes ExampleTypedValidator where
    type instance DatumType ExampleTypedValidator    = MyCustomDatum
    type instance RedeemerType ExampleTypedValidator = MyCustomRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: MyCustomDatum-> MyCustomRedeemer -> ScriptContext -> Bool
mkValidator (MyCustomDatum d) (MyCustomRedeemer r) _ =
  d == 42 && r == 42

inst :: Scripts.TypedValidator ExampleTypedValidator
inst = Scripts.mkTypedValidator @ExampleTypedValidator
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @MyCustomDatum @MyCustomRedeemer

validator :: Plutus.Validator
validator = Scripts.validatorScript inst

script :: Plutus.Script
script = Plutus.unValidatorScript validator

customDatumRedeemerGuessScriptAsShortBs :: SBS.ShortByteString
customDatumRedeemerGuessScriptAsShortBs = SBS.toShort . LBS.toStrict $ serialise script

customGuessScript :: PlutusScript PlutusScriptV1
customGuessScript = PlutusScriptSerialised customDatumRedeemerGuessScriptAsShortBs

