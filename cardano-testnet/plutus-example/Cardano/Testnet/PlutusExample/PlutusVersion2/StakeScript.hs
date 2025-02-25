{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Testnet.PlutusExample.PlutusVersion2.StakeScript
  ( v2StakeScript
  , v2StakeScriptShortBs
  ) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)

import           PlutusLedgerApi.V2.Contexts as V2

import           Prelude hiding (($), (&&))

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import qualified Plutus.Script.Utils.Scripts as Plutus
import           Plutus.Script.Utils.Typed as Scripts
import qualified PlutusTx
import           PlutusTx.Builtins
import           PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

{- HLINT ignore "Avoid lambda" -}

{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> V2.ScriptContext -> Bool
mkPolicy _redeemer _ctx = True

policy :: Plutus.StakeValidator
policy = Plutus.mkStakeValidatorScript $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.mkUntypedStakeValidator mkPolicy

plutusScript :: Plutus.Script
plutusScript =
  Plutus.unStakeValidatorScript policy

validator :: Plutus.Validator
validator = Plutus.Validator plutusScript

scriptAsCbor :: LBS.ByteString
scriptAsCbor = serialise validator

v2StakeScript :: PlutusScript PlutusScriptV2
v2StakeScript = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor

v2StakeScriptShortBs :: SBS.ShortByteString
v2StakeScriptShortBs = SBS.toShort . LBS.toStrict $ scriptAsCbor
