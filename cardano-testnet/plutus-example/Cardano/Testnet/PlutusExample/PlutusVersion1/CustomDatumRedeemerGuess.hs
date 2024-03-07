{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Testnet.PlutusExample.PlutusVersion1.CustomDatumRedeemerGuess
  ( MyCustomDatum(..)
  , MyCustomRedeemer(..)
  , customGuessScript
  , customDatumRedeemerGuessScriptAsShortBs
  ) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import qualified PlutusLedgerApi.V1.Contexts as V1

import           Prelude hiding (($), (&&), (==))

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import qualified Plutus.Script.Utils.Scripts as Plutus
import qualified Plutus.Script.Utils.Typed as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup ((<>)), unless, (.))

newtype MyCustomDatum = MyCustomDatum Integer
newtype MyCustomRedeemer = MyCustomRedeemer Integer

PlutusTx.unstableMakeIsData ''MyCustomDatum
PlutusTx.unstableMakeIsData ''MyCustomRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: MyCustomDatum -> MyCustomRedeemer -> V1.ScriptContext -> Bool
mkValidator (MyCustomDatum d) (MyCustomRedeemer r) _ =
  d == 42 && r == 42

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = Scripts.mkUntypedValidator mkValidator

script :: Plutus.Script
script = Plutus.unValidatorScript validator

customDatumRedeemerGuessScriptAsShortBs :: SBS.ShortByteString
customDatumRedeemerGuessScriptAsShortBs = SBS.toShort . LBS.toStrict $ serialise script

customGuessScript :: PlutusScript PlutusScriptV1
customGuessScript = PlutusScriptSerialised customDatumRedeemerGuessScriptAsShortBs
