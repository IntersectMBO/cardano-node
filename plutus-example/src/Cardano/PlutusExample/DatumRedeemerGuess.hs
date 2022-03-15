{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.PlutusExample.DatumRedeemerGuess
  ( guessScript
  , guessScriptStake
  , datumRedeemerGuessScriptShortBs
  ) where

import           Prelude hiding (($), (&&), (==))

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import qualified Plutus.V1.Ledger.Scripts as Plutus
import           PlutusTx (toBuiltinData)
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData  -> ()
mkValidator datum redeemer _txContext
  |    datum    == toBuiltinData (42 :: Integer)
    && redeemer == toBuiltinData (42 :: Integer) = ()
  | otherwise = traceError "Incorrect datum. Expected 42."

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

script :: Plutus.Script
script = Plutus.unValidatorScript validator

datumRedeemerGuessScriptShortBs :: SBS.ShortByteString
datumRedeemerGuessScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

guessScript :: PlutusScript PlutusScriptV1
guessScript = PlutusScriptSerialised datumRedeemerGuessScriptShortBs

{-# INLINEABLE mkValidatorStake #-}
mkValidatorStake :: BuiltinData -> BuiltinData -> ()
mkValidatorStake redeemer _txContext
  | redeemer == toBuiltinData (42 :: Integer) = ()
  | otherwise = traceError "Incorrect datum. Expected 42."

validatorStake :: Plutus.StakeValidator
validatorStake = Plutus.mkStakeValidatorScript $$(PlutusTx.compile [||mkValidatorStake||])

scriptStake :: Plutus.Script
scriptStake = Plutus.unStakeValidatorScript validatorStake

datumRedeemerGuessScriptStakeShortBs :: SBS.ShortByteString
datumRedeemerGuessScriptStakeShortBs = SBS.toShort . LBS.toStrict $ serialise scriptStake

guessScriptStake :: PlutusScript PlutusScriptV1
guessScriptStake = PlutusScriptSerialised datumRedeemerGuessScriptStakeShortBs
