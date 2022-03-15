{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}


module Cardano.PlutusExample.MintingScript
  ( apiExamplePlutusMintingScript
  , mintingScriptShortBs
  ) where

import           Prelude hiding (($))

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS

import           Ledger hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless, (.))


{- HLINT ignore "Avoid lambda" -}

{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> ScriptContext -> Bool
mkPolicy _redeemer _ctx = True


policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])


plutusScript :: Script
plutusScript =
  unMintingPolicyScript policy

validator :: Validator
validator =
  Validator $ unMintingPolicyScript policy

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

apiExamplePlutusMintingScript :: PlutusScript PlutusScriptV1
apiExamplePlutusMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

mintingScriptShortBs :: SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor

