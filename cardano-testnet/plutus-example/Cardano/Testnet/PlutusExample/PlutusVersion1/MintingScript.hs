{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Testnet.PlutusExample.PlutusVersion1.MintingScript
  ( apiExamplePlutusMintingScript
  , mintingScriptShortBs
  ) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import qualified PlutusLedgerApi.V1 as V1

import           Prelude hiding (($))

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS

import qualified Plutus.Script.Utils.Scripts as Plutus
import qualified Plutus.Script.Utils.Typed as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

{- HLINT ignore "Avoid lambda" -}

{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> V1.ScriptContext -> Bool
mkPolicy _redeemer _ctx = True

policy :: Plutus.MintingPolicy
policy = Plutus.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = Scripts.mkUntypedMintingPolicy mkPolicy

plutusScript :: Plutus.Script
plutusScript =
  Plutus.unMintingPolicyScript policy

validator :: Plutus.Validator
validator =
  Plutus.Validator $ Plutus.unMintingPolicyScript policy

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

apiExamplePlutusMintingScript :: PlutusScript PlutusScriptV1
apiExamplePlutusMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

mintingScriptShortBs :: SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor
