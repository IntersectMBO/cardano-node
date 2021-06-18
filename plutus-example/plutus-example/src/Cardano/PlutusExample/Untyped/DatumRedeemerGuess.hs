{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.PlutusExample.Untyped.DatumRedeemerGuess
  ( datumRedeemerGuessScript
  , datumRedeemerGuessScriptShortBs
  ) where

import           Prelude hiding (($), (&&), (==))

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import qualified Plutus.V1.Ledger.Scripts as Plutus
import           PlutusTx (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)

{-# INLINABLE mkValidator #-}
mkValidator :: Data -> Data -> Data -> ()
mkValidator datum redeemer _txContext
  | datum == PlutusTx.I 42 && redeemer == PlutusTx.I 42 = ()
  | otherwise = traceError "Incorrect datum. Expected 42."

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

script :: Plutus.Script
script = Plutus.unValidatorScript validator

datumRedeemerGuessScriptShortBs :: SBS.ShortByteString
datumRedeemerGuessScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

datumRedeemerGuessScript :: PlutusScript PlutusScriptV1
datumRedeemerGuessScript = PlutusScriptSerialised datumRedeemerGuessScriptShortBs

