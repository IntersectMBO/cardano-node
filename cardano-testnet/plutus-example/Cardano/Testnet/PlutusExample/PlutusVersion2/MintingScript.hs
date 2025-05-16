{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Testnet.PlutusExample.PlutusVersion2.MintingScript
  ( v2mintingScript
  , v2mintingScriptShortBs
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

policy :: Plutus.MintingPolicy
policy = Plutus.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.mkUntypedMintingPolicy mkPolicy

plutusScript :: Plutus.Script
plutusScript =
  Plutus.unMintingPolicyScript policy

validator :: Plutus.Validator
validator = Plutus.Validator plutusScript

scriptAsCbor :: LBS.ByteString
scriptAsCbor = serialise validator

v2mintingScript :: PlutusScript PlutusScriptV2
v2mintingScript = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor

v2mintingScriptShortBs :: SBS.ShortByteString
v2mintingScriptShortBs = SBS.toShort . LBS.toStrict $ scriptAsCbor
